module Text.Megaparsec.Cubix where

import Control.Applicative (Alternative)
import Control.Applicative.Combinators (eitherP, many, optional, some)
import Data.Comp.Multi (Cxt, HFunctor, (:<:))
import Data.Typeable (Typeable)
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

pSome
  :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a [l])
pSome = pInsert . some

pMany
  :: (Alternative m, ListF :<: fs, HFunctor fs, Typeable l)
  => m (Cxt h fs a l) -> m (Cxt h fs a [l])
pMany = pInsert . many
