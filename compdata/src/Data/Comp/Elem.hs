{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Comp.Elem
       ( Elem (..)
       , Mem
       , RMem
       , witness
       , elemEq
       , comparePos
       , extend
       , contract
       , unsafeElem
       ) where

import Data.Proxy
import GHC.TypeLits
import Data.Type.Equality
import qualified Unsafe.Coerce as U

data Elem (f :: k) (fs :: [k]) where
  Elem :: Int -> Elem f fs

type family Position (f :: k) (fs :: [k]) where
  Position (f :: k) ((f :: k) ': fs) = 0
  Position f (g ': fs) = 1 + Position f fs

class (KnownNat (Position f fs)) => Mem (f :: k) (fs :: [k])
instance (KnownNat (Position f fs)) => Mem f fs

class (KnownNat (Position f fs)) => RMem (fs :: [k]) (f :: k)
instance (KnownNat (Position f fs)) => RMem fs f

{-# INLINE witness #-}
witness :: forall f fs. (Mem f fs) => Elem f fs
witness = Elem pos
  where pos = fromInteger (natVal (Proxy :: Proxy (Position f fs)))

{-# INLINE elemEq #-}
elemEq :: forall f g fs. Elem f fs -> Elem g fs -> Maybe (f :~: g)
elemEq (Elem v1) (Elem v2) = case v1 == v2 of
  True -> Just (U.unsafeCoerce Refl)
  False -> Nothing

{-# INLINE comparePos #-}
comparePos :: Elem f fs -> Elem g fs -> Ordering
comparePos (Elem v1) (Elem v2) = compare v1 v2

{-# INLINE extend #-}
extend :: Elem f fs -> Elem f (g ': fs)
extend (Elem i) = Elem (i + 1)

{-# INLINE contract #-}
contract :: Elem f (g ': fs) -> Either (f :~: g) (Elem f fs)
contract (Elem i)
  | i > 0     = Right (Elem (i - 1))
  | otherwise = Left (U.unsafeCoerce Refl)

-- NOTE: Completely unsafe. USE WITH CARE.
{-# INLINE unsafeElem #-}
unsafeElem :: Elem f fs -> Elem g gs
unsafeElem (Elem e) = Elem e
