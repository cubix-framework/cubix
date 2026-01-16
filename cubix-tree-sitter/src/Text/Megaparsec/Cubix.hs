{-# OPTIONS_GHC -Wno-orphans #-}
module Text.Megaparsec.Cubix where

import Control.Applicative (Alternative)
import Control.Applicative.Combinators (between, eitherP, many, optional, sepEndBy, sepEndBy1, some)
import Data.ByteString as ByteString
-- TODO: CUBIX_NON_EMPTY
-- import Control.Applicative.Combinators.NonEmpty (some, sepBy1)
import Data.Comp.Multi (Cxt, E, HFunctor, K, KOrd, NoHole, OrdHF, Sum, KShow, (:<:), runE, kshow, unK)
import Data.Comp.Multi.Strategy.Classification (DynCase, caseE)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Encoding
import Data.Typeable (Typeable)
import Data.Void (Void)
import Text.Megaparsec qualified

import Cubix.Language.Info ( SourceRange (..), rangeLength )
import Cubix.Language.Parametric.Syntax
       ( EitherF
       , InsertF (..)
       , ListF
       , MaybeF
       , PairF
       , riLeftF
       , riPairF
       , riRightF
       )

pInsert
 :: (Functor m, Typeable l, InsertF f e)
 => m (f (e l)) -> m (e (f l))
pInsert = fmap insertF
{-# INLINABLE pInsert #-}

pMaybe
  :: (Alternative m, MaybeF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a (Maybe l))
pMaybe = pInsert . optional
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
  :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a [l])
pSome = pInsert . some
{-# INLINABLE pSome #-}

pMany
  :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a [l])
pMany = pInsert . many
{-# INLINABLE pMany #-}

-- tree-sitters sepBy is actually sepEndBy
pSepBy
  :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a sep) -> m (Cxt h fs a [l])
pSepBy p sep = pInsert $ sepEndBy p sep
{-# INLINABLE pSepBy #-}

-- TODO: CUBIX_NON_EMPTY
-- pSepBy1
--   :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
--   => m (Cxt h fs a l) -> m (Cxt h fs a sep) -> m (Cxt h fs a (NonEmpty l))
-- pSepBy1 p sep = pInsert $ sepBy1 p sep

-- tree-sitters sepBy1 is actually sepEndBy1
pSepBy1
  :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a sep) -> m (Cxt h fs a [l])
pSepBy1 p sep = pInsert $ sepEndBy1 p sep
{-# INLINABLE pSepBy1 #-}

pBetween
  :: Alternative m
  => m (Cxt h fs a open) -> m (Cxt h fs a close) -> m (Cxt h fs a l) -> m (Cxt h fs a l)
pBetween = between
{-# INLINABLE pBetween #-}

-- instance KShow f => Show (E f) where
--   show = runE (show . unK . kshow)

data Input h fs a = Input
  { content :: ByteString
  , tokens  :: [E (Cxt h fs a)]
  } deriving (Eq, Ord)

instance (HFunctor fs, OrdHF fs, KOrd a) => Text.Megaparsec.Stream (Input h fs a) where
  type Token (Input h fs a) = E (Cxt h fs a)
  type Tokens (Input h fs a) = [E (Cxt h fs a)]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = List.length
  chunkEmpty Proxy = List.null
  take1_ (Input _ []) = Nothing
  take1_ (Input src (t : ts)) =
    Just (t, Input src ts)
  takeN_ n (Input src s)
    | List.null s = Nothing
    | otherwise =
      let (x, s') = List.splitAt n s
       in Just (x, Input src s')
  takeWhile_ f (Input src s) =
    let (x, s') = List.span f s
     in (x, Input src s')

type Parser' h fs a t = Text.Megaparsec.Parsec Void (Input h fs a) t
type Parser sig t = Parser' NoHole (Sum sig) (K ()) t

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
