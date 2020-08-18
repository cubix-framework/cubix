{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Elem
-- Copyright   :  (c) 2020 James Koppel
-- License     :  BSD3
--
-- Defines the `Elem` type so that @Elem f fs@ is type-level evidence that @f@ is a member of
-- the list of types @fs@
--
--------------------------------------------------------------------------------

module Data.Comp.Elem
       ( Elem
       , pattern Elem
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

-- |  @Elem f fs@ is type-level evidence that @f@ is a member of the list of types @fs@.
--    The runtime value is just an int representing the index of @f@ in @fs@.
--    The `Elem` pattern . The safe constructor is `witness`.
data Elem (f :: k) (fs :: [k]) where
  Elem# :: Int -> Elem f fs

-- | Access the underlying int of an `Elem`. Only usable as a destructor.
pattern Elem :: Int -> Elem f fs
pattern Elem n <- Elem# n


type family Position (f :: k) (fs :: [k]) where
  Position (f :: k) ((f :: k) ': fs) = 0
  Position f (g ': fs) = 1 + Position f fs

-- | `Mem f fs` holds if the typechecker can statically deduce that `f` is contained in `fs`
class (KnownNat (Position f fs)) => Mem (f :: k) (fs :: [k])
instance (KnownNat (Position f fs)) => Mem f fs

-- | Flipped version of `Mem`
class (KnownNat (Position f fs)) => RMem (fs :: [k]) (f :: k)
instance (KnownNat (Position f fs)) => RMem fs f

-- | Safe constructor for `Elem`. If the typechecker can deduce that @f@ is in @fs@,
--   then `witness` creates an `Elem f fs` witnessing that inclusion.
witness :: forall f fs. (Mem f fs) => Elem f fs
witness = Elem# pos
  where pos = fromInteger (natVal (Proxy :: Proxy (Position f fs)))
{-# INLINE witness #-}

{-# INLINE elemEq #-}
elemEq :: forall f g fs. Elem f fs -> Elem g fs -> Maybe (f :~: g)
elemEq (Elem v1) (Elem v2) = case v1 == v2 of
  True -> Just (U.unsafeCoerce Refl)
  False -> Nothing

{-# INLINE comparePos #-}
comparePos :: Elem f fs -> Elem g fs -> Ordering
comparePos (Elem v1) (Elem v2) = compare v1 v2

extend :: Elem f fs -> Elem f (g ': fs)
extend (Elem i) = Elem# (i + 1)
{-# INLINE extend #-}

{-# INLINE contract #-}
contract :: Elem f (g ': fs) -> Either (f :~: g) (Elem f fs)
contract (Elem i)
  | i > 0     = Right (Elem# (i - 1))
  | otherwise = Left (U.unsafeCoerce Refl)

-- | Completely unsafe. USE WITH CARE.
{-# INLINE unsafeElem #-}
unsafeElem :: Elem f fs -> Elem g gs
unsafeElem (Elem e) = Elem# e
