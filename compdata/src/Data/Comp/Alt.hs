{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}

module Data.Comp.Alt
       ( Alts
       , Alt
       , alt
       , (<|)
       , cons
       , nil
       , extractAt
       ) where

import qualified Data.Vector as V
import Data.Vector      (Vector)
import qualified Unsafe.Coerce as U
import GHC.Types

import Data.Comp.Elem

newtype Alt f e b = Alt (f e -> b)

newtype Alts (fs :: [(* -> *)]) e b =
  Alts (Vector (Alt Any e b))

alt :: (f e -> b) -> Alt f e b
alt = Alt

infixr 6 <|

{-# INLINE (<|) #-}
(<|) :: Alt f e b -> Alts fs e b -> Alts (f ': fs) e b
(<|) = cons

{-# INLINE cons #-}
cons :: Alt f e b -> Alts fs e b -> Alts (f ': fs) e b
cons a (Alts as) = Alts (U.unsafeCoerce a `V.cons` as)

nil :: Alts '[] e b
nil = Alts V.empty

extractAt :: Elem f fs -> Alts fs e b -> (f e -> b)
extractAt (Elem v) (Alts ms) = U.unsafeCoerce (ms V.! v)
