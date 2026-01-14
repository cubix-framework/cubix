{-# OPTIONS_GHC -Wno-orphans #-}
module Text.Megaparsec.Cubix where

import Control.Applicative (Alternative)
import Control.Applicative.Combinators (between, eitherP, many, optional, sepEndBy, sepEndBy1, some)
-- TODO: CUBIX_NON_EMPTY
-- import Control.Applicative.Combinators.NonEmpty (some, sepBy1)
import Data.Comp.Multi (Cxt, HFunctor, (:<:), OrdHF, KOrd)
import Data.Comp.Multi.HFunctor (E)
import Data.Comp.Multi.Strategy.Classification (DynCase, caseE)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Typeable (Typeable)
import Data.Void (Void)
import Text.Megaparsec qualified

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

pMaybe
  :: (Alternative m, MaybeF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a (Maybe l))
pMaybe = pInsert . optional

pPair
  :: (Applicative m, PairF :<: fs, Typeable l, Typeable l')
  => m (Cxt h fs a l) -> m (Cxt h fs a l') -> m (Cxt h fs a (l, l'))
pPair pa pb = riPairF <$> pa <*> pb

pEither
  :: (Alternative m, EitherF :<: fs, Typeable l, Typeable l')
  => m (Cxt h fs a l) -> m (Cxt h fs a l') -> m (Cxt h fs a (Either l l'))
pEither pa pb = either riLeftF riRightF <$> eitherP pa pb

-- TODO: CUBIX_NON_EMPTY
-- pSome
--   :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
--   => m (Cxt h fs a l) -> m (Cxt h fs a (NonEmpty l))
-- pSome = pInsert . some

pSome
  :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a [l])
pSome = pInsert . some

pMany
  :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a [l])
pMany = pInsert . many

-- tree-sitters sepBy is actually sepEndBy
pSepBy
  :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a sep) -> m (Cxt h fs a [l])
pSepBy p sep = pInsert $ sepEndBy p sep

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

pBetween
  :: Alternative m
  => m (Cxt h fs a open) -> m (Cxt h fs a close) -> m (Cxt h fs a l) -> m (Cxt h fs a l)
pBetween = between

type Tok h fs a = E (Cxt h fs a)
type Parser h fs a t = Text.Megaparsec.Parsec Void [Tok h fs a] t

-- Use with TypeApplications, eg. `pSort @IdentL`
-- that is why I've put `l` variable first
pSort
  :: forall l h fs a
  .  (HFunctor fs, OrdHF fs, KOrd a, DynCase (Cxt h fs a) l)
  => NonEmpty Char -> Parser h fs a (Cxt h fs a l)
pSort expected = Text.Megaparsec.token
  (caseE @_ @l)
  (Set.singleton $ Text.Megaparsec.Label expected)

instance IsString (NonEmpty Char) where
  fromString [] = error "NonEmpty.fromString: empty string"
  fromString (s:ss) = s :| ss
