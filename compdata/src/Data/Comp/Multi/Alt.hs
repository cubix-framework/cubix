{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Comp.Multi.Alt
       ( Alts
       , Alt
       , alt
       , (<|)
       , cons
       , nil
       , extractAt
       ) where

import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Unsafe.Coerce as U
import GHC.Types

import Data.Comp.Elem

newtype Alt f (a :: * -> *) e b = Alt (f a e -> b)

newtype Alts (fs :: [(* -> *) -> * -> *]) (a :: * -> *) e b =
  Alts (Vector (Alt Any a e b))

alt :: (f a e -> b) -> Alt f a e b
alt = Alt

infixr 6 <|

{-# INLINE (<|) #-}
(<|) :: Alt f a e b -> Alts fs a e b -> Alts (f ': fs) a e b
(<|) = cons

{-# INLINE cons #-}
cons :: Alt f a e b -> Alts fs a e b -> Alts (f ': fs) a e b
cons a (Alts as) = Alts (U.unsafeCoerce a `V.cons` as)

nil :: Alts '[] a e b
nil = Alts V.empty

extractAt :: Elem f fs -> Alts fs a e b -> (f a e -> b)
extractAt (Elem v) (Alts ms) = U.unsafeCoerce (ms V.! v)
