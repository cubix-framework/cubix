{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Runtime support for the parsers that @gen-parser@ generates from
-- tree-sitter grammars (see @gen-mod/data/ParsePretty.hs.template@).
--
-- Tree-sitter does the real parsing, but its tree is untyped: a node is
-- just a symbol number and a list of children. The generated parser
-- turns that tree into a Cubix term bottom-up. Each node's children are
-- converted first; then the node's own grammar rule, compiled to a
-- Megaparsec parser, is run over them to pick the constructor and
-- assemble the term. So the "tokens" parsed here are not characters but
-- already-built child terms ('Input'), and a parser for one node only
-- ever sees that node's children.
--
-- This module provides:
--
--   * 'Input' and its Megaparsec stream instances, for parsing a list
--     of child terms (plus the node's source text, for leaf contents);
--
--   * 'pSort' and 'pFragmentSort', which accept a child of a given sort;
--
--   * combinators mirroring tree-sitter's rule forms (@optional@,
--     @repeat@, @repeat1@, @seq@, @choice@, and the usual @sepBy@
--     helpers), which build the Cubix container nodes ('MaybeF',
--     'ListF', 'PairF', 'EitherF') and handle the backtracking that
--     tree-sitter grammars need ('pItem', 'pLongest').
module Text.Megaparsec.Cubix where

import Control.Applicative (Alternative (empty))
import Data.ByteString as ByteString (ByteString, drop, take)
import Data.ByteString.Char8 qualified as ByteString.Char8 (unpack)
import Data.Either (partitionEithers)
import Data.List qualified as List (length, null, sortOn, span, splitAt)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (nonEmpty, toList)
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member, singleton)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error qualified as Text.Encoding (lenientDecode)
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (Typeable)
import Data.Void (Void)

import Control.Applicative.Combinators (between, eitherP, many, optional, sepEndBy, sepEndBy1, some)
-- TODO: CUBIX_NON_EMPTY
-- import Control.Applicative.Combinators.NonEmpty (some, sepBy1)
import Text.Megaparsec qualified (ErrorItem (Label), MonadParsec, Parsec, PosState (..), Stream (..), Token, Tokens, TraversableStream (..), VisualStream (..), getOffset, getParserState, observing, parseError, setParserState, sourceLine, stateInput, stateOffset, token, tokensLength, try)

import Data.Comp.Multi (Cxt, E (..), HFunctor, K, KOrd, KShow, NoHole, OrdHF, ShowHF, Sum, project, (:<:), (:&:))
import Data.Comp.Multi.Strategy.Classification (DynCase, KDynCase (..), caseE)

import Cubix.Language.Info (SourceRange (..), rangeLength)
import Cubix.Language.Parametric.Syntax (EitherF, InsertF (..), ListF, MaybeF, PairF, riLeftF, riPairF, riRightF)

pInsert
 :: (Functor m, Typeable l, InsertF f e)
 => m (f (e l)) -> m (e (f l))
pInsert = fmap insertF
{-# INLINABLE pInsert #-}

-- | An item of a repetition: backtracks on failure, and fails if it
-- consumes nothing. An item made only of invisible tokens (a hidden
-- rule whose children tree-sitter never emits) would otherwise be
-- repeated forever.
pItem :: Text.Megaparsec.MonadParsec e s m => m a -> m a
pItem p = Text.Megaparsec.try $ do
  before <- Text.Megaparsec.getOffset
  x <- p
  after <- Text.Megaparsec.getOffset
  if after > before then pure x else empty

pMaybe
  :: (Text.Megaparsec.MonadParsec e s m, MaybeF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a (Maybe l))
pMaybe = pInsert . optional . Text.Megaparsec.try
{-# INLINABLE pMaybe #-}

pPair
  :: (Applicative m, PairF :<: fs, Typeable l, Typeable l')
  => m (Cxt h fs a l) -> m (Cxt h fs a l') -> m (Cxt h fs a (l, l'))
pPair pa pb = riPairF <$> pa <*> pb
{-# INLINABLE pPair #-}

pEither
  :: (Alternative m, EitherF :<: fs, Typeable l, Typeable l')
  => m (Cxt h fs a l) -> m (Cxt h fs a l') -> m (Cxt h fs a (Either l l'))
pEither pa pb = either riLeftF riRightF <$> eitherP pa pb
{-# INLINABLE pEither #-}

-- TODO: CUBIX_NON_EMPTY
-- pSome
--   :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
--   => m (Cxt h fs a l) -> m (Cxt h fs a (NonEmpty l))
-- pSome = pInsert . some

pSome
  :: (Text.Megaparsec.MonadParsec e s m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a [l])
pSome = pInsert . some . pItem
{-# INLINABLE pSome #-}

pMany
  :: (Text.Megaparsec.MonadParsec e s m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a [l])
pMany = pInsert . many . pItem
{-# INLINABLE pMany #-}

-- tree-sitters sepBy is actually sepEndBy
pSepBy
  :: (Text.Megaparsec.MonadParsec e s m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a sep) -> m (Cxt h fs a [l])
pSepBy p sep = pInsert $ sepEndBy (pItem p) sep
{-# INLINABLE pSepBy #-}

-- TODO: CUBIX_NON_EMPTY
-- pSepBy1
--   :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
--   => m (Cxt h fs a l) -> m (Cxt h fs a sep) -> m (Cxt h fs a (NonEmpty l))
-- pSepBy1 p sep = pInsert $ sepBy1 p sep

-- tree-sitters sepBy1 is actually sepEndBy1
pSepBy1
  :: (Text.Megaparsec.MonadParsec e s m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a sep) -> m (Cxt h fs a [l])
pSepBy1 p sep = pInsert $ sepEndBy1 (pItem p) sep
{-# INLINABLE pSepBy1 #-}

-- | Maximal munch choice between alternatives; tie-broken by the first in the list
--
-- Has some caching to not infinite-loop when given left-recursive nodes, which grammars
-- have once some tokens are invisible
pLongest
  :: (HFunctor fs, OrdHF fs, KOrd a)
  => String -> [Parser' h fs a x] -> Parser' h fs a x
pLongest rule ps = do
  start <- Text.Megaparsec.getParserState

  let offset = Text.Megaparsec.stateOffset start
      outer = entered (Text.Megaparsec.stateInput start)
      active = if fst outer == offset then snd outer else Set.empty
      inside = withEntered (offset, Set.insert rule active) start

      -- Run each alternative from 'inside', keeping its result and end
      -- state, until one consumes all the input.
      runFrom p rest = do
        Text.Megaparsec.setParserState inside
        result <- Text.Megaparsec.observing $
          Text.Megaparsec.try ((,) <$> p <*> Text.Megaparsec.getParserState)
        (result :) <$> if any (consumedAll . snd) result then pure [] else rest

  if rule `Set.member` active then empty else do
    (errs, oks) <- partitionEithers <$> foldr runFrom (pure []) ps

    -- 'sortOn' is stable, so this is the first of the longest.
    case List.sortOn (Down . Text.Megaparsec.stateOffset . snd) oks of
      (x, end) : _ ->
        x <$ Text.Megaparsec.setParserState (withEntered outer end)

      -- All failed: report their errors merged, as 'choice' would.
      -- Rerunning them to get the errors instead would take time
      -- exponential in the depth of nested failing choices.
      [] -> do
        Text.Megaparsec.setParserState start
        maybe empty Text.Megaparsec.parseError (foldMap Just errs)
  where
    withEntered e st =
      st { Text.Megaparsec.stateInput = (Text.Megaparsec.stateInput st) { entered = e } }

    consumedAll = List.null . tokens . Text.Megaparsec.stateInput

pBetween
  :: Alternative m
  => m (Cxt h fs a open) -> m (Cxt h fs a close) -> m (Cxt h fs a l) -> m (Cxt h fs a l)
pBetween = between
{-# INLINABLE pBetween #-}

data Input h fs a = Input
  { content :: ByteString
  , tokens  :: [E (Cxt h fs a)]
  , entered :: (Int, Set String)
    -- ^ Rules 'pLongest' is currently trying at this input offset; see
    -- there. Kept in the input so that backtracking restores it.
  } deriving (Eq, Ord)

-- | The input for parsing a node: its source text and its children.
mkInput :: ByteString -> [E (Cxt h fs a)] -> Input h fs a
mkInput src children = Input src children (0, Set.empty)

instance (HFunctor fs, OrdHF fs, KOrd a) => Text.Megaparsec.Stream (Input h fs a) where
  type Token (Input h fs a) = E (Cxt h fs a)
  type Tokens (Input h fs a) = [E (Cxt h fs a)]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = List.length
  chunkEmpty Proxy = List.null

  take1_ (Input _ [] _) = Nothing
  take1_ input@(Input _ (t : ts) _) =
    Just (t, input { tokens = ts })

  takeN_ n input@(Input _ s _)
    | List.null s = Nothing
    | otherwise =
      let (x, s') = List.splitAt n s
       in Just (x, input { tokens = s' })

  takeWhile_ f input@(Input _ s _) =
    let (x, s') = List.span f s
     in (x, input { tokens = s' })

instance
  ( HFunctor fs, OrdHF fs, KOrd a, ShowHF fs, KShow a
  ) => Text.Megaparsec.VisualStream (Input h fs a)
  where
    showTokens Proxy = unwords
      . NonEmpty.toList
      . fmap show

    tokensLength Proxy = sum . fmap (const 1)

instance
  ( HFunctor fs, OrdHF fs, KOrd a, ShowHF fs, KShow a
  ) => Text.Megaparsec.TraversableStream (Input h fs a)
  where
    reachOffsetNoLine offset Text.Megaparsec.PosState {..} =
      Text.Megaparsec.PosState
        { pstateInput =
            pstateInput { tokens = post }
        , pstateOffset = max pstateOffset offset
        , pstateSourcePos = newSourcePos
        , pstateTabWidth = pstateTabWidth
        , pstateLinePrefix = prefix
        }
      where
        (pre, post) = List.splitAt (offset - pstateOffset) (tokens pstateInput)
        tokensConsumed = case NonEmpty.nonEmpty pre of
          Nothing -> 0
          Just nePre -> Text.Megaparsec.tokensLength (Proxy @(Input h fs a)) nePre
        -- those are wrong since tokensLength doesn't count proper, we would need to do the Annotated tokens
        preBS = ByteString.take tokensConsumed (content pstateInput)
        preStr = ByteString.Char8.unpack preBS
        newSourcePos = pstateSourcePos
        sameLine =
          (==)
            (Text.Megaparsec.sourceLine newSourcePos)
            (Text.Megaparsec.sourceLine pstateSourcePos)
        prefix =
          if sameLine
            then pstateLinePrefix ++ preStr
            else preStr

type Parser' h fs a t = Text.Megaparsec.Parsec Void (Input h fs a) t
type Parser sig t = Parser' NoHole (Sum sig) (K ()) t

-- | Parser whose tokens are annotated terms: each Cxt layer carries an
-- annotation of type @ann@. Used by the generated tree-sitter parsers
-- to thread source spans through the parse, attaching them at every
-- AST layer (mirrors what the hand-written Java parser does with
-- 'layer').
type ParserA sig ann t = Parser' NoHole (Sum sig :&: ann) (K ()) t

-- Use with TypeApplications, eg. `pSort @IdentL`
-- that is why I've put `l` variable first
pSort
  :: forall l h fs a
  .  (HFunctor fs, OrdHF fs, KOrd a, DynCase (Cxt h fs a) l)
  => NonEmpty Char -> Parser' h fs a (Cxt h fs a l)
pSort expected = Text.Megaparsec.token
  (caseE @_ @l)
  (Set.singleton $ Text.Megaparsec.Label expected)
{-# INLINABLE pSort #-}

-- | Like 'pSort', but for a child of sort @l@ that nodes of fragment @f@
-- have, and much cheaper to typecheck. Use with TypeApplications, e.g.
-- @pFragmentSort \@Ident \@IdentL@.
--
-- 2026.10.31:
--  Use of KDyncase, restricted to a single fragment, typechecks with far less memory
--  than DynCase on the whole signature. 
pFragmentSort
  :: forall f l h fs a
  .  (HFunctor fs, OrdHF fs, KOrd a, f :<: fs, KDynCase f l)
  => NonEmpty Char -> Parser' h fs a (Cxt h fs a l)
pFragmentSort expected = Text.Megaparsec.token
  (\(E t) -> do
      Refl <- kdyncase @f @l =<< project t
      pure t)
  (Set.singleton $ Text.Megaparsec.Label expected)
{-# INLINABLE pFragmentSort #-}

pSort'
  :: forall l h fs a
  .  (HFunctor fs, OrdHF fs, KOrd a, DynCase (Cxt h fs a) l)
  => Proxy l -> NonEmpty Char -> Parser' h fs a (Cxt h fs a l)
pSort' Proxy expected = Text.Megaparsec.token
  (caseE @_ @l)
  (Set.singleton $ Text.Megaparsec.Label expected)

instance IsString (NonEmpty Char) where
  fromString [] = error "NonEmpty.fromString: empty string"
  fromString (s:ss) = s :| ss

pContent
  :: forall h fs a
  .  (HFunctor fs, OrdHF fs, KOrd a)
  => Parser' h fs a Text
pContent = do
  src <- content . Text.Megaparsec.stateInput <$>
    Text.Megaparsec.getParserState
  pure $ Text.Encoding.decodeUtf8With Text.Encoding.lenientDecode src

getContent :: ByteString -> SourceRange -> ByteString
getContent src range =
  let len = rangeLength range
      start = _rangeStart range
   in ByteString.take len . ByteString.drop start $ src

{-# INLINABLE getContent #-}
