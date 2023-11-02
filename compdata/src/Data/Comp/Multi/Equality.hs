{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Equality
-- Copyright   :  (c) Patrick Bahr, 2011
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module defines equality for (higher-order) signatures, which lifts to
-- equality for (higher-order) terms and contexts. All definitions are
-- generalised versions of those in "Data.Comp.Equality".
--
--------------------------------------------------------------------------------
module Data.Comp.Multi.Equality
    (
     EqHF(..),
     KEq(..),
     heqMod
    ) where

import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Ops
import Data.Comp.Multi.Term
import Data.Comp.Dict
import Data.Comp.Elem
import Data.Type.Equality

class KEq f where
    keq :: f i -> f j -> Bool

{-| Signature equality. An instance @EqHF f@ gives rise to an instance
  @KEq (HTerm f)@. -}
class EqHF f where
    eqHF :: KEq g => f g i -> f g j -> Bool

instance Eq a => KEq (K a) where
    keq (K x) (K y) = x == y

instance KEq a => Eq (E a) where
     E x == E y = x `keq`  y

{-|
  'EqF' is propagated through sums.
-}
instance (All EqHF fs) => EqHF (Sum fs) where
    eqHF (Sum wit1 x) (Sum wit2 y) =
      case elemEq wit1 wit2 of
              Just Refl -> eqHF x y \\ dictFor @EqHF wit1
              Nothing   -> False

instance EqHF f => EqHF (Cxt h f) where
    eqHF (Term e1) (Term e2) = e1 `eqHF` e2
    eqHF (Hole h1) (Hole h2) = h1 `keq` h2
    eqHF _ _ = False

instance (EqHF f, KEq a) => KEq (Cxt h f a) where
    keq = eqHF

{-|
  From an 'EqF' functor an 'Eq' instance of the corresponding
  term type can be derived.
-}
instance (EqHF f, KEq a) => Eq (Cxt h f a i) where
    (==) = keq

{-| This function implements equality of values of type @f a@ modulo
the equality of @a@ itself. If two functorial values are equal in this
sense, 'eqMod' returns a 'Just' value containing a list of pairs
consisting of corresponding components of the two functorial
values. -}

heqMod :: (EqHF f, HFunctor f, HFoldable f) => f a i -> f b i -> Maybe [(E a, E b)]
heqMod s t
    | unit s `eqHF` unit' t = Just args
    | otherwise = Nothing
    where unit = hfmap (const $ K ())
          unit' = hfmap (const $ K ())
          args = htoList s `zip` htoList t
