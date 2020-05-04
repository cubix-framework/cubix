{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Ordering
-- Copyright   :  (c) 2011 Patrick Bahr, Tom Hvitved
-- License     :  BSD3
-- Maintainer  :  Tom Hvitved <hvitved@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module defines ordering of signatures, which lifts to ordering of
-- terms and contexts.
--
--------------------------------------------------------------------------------
module Data.Comp.Multi.Ordering
    (
     KOrd(..),
     OrdHF(..)
    ) where

import Data.Type.Equality
import Data.Comp.Multi.Equality
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Ops
import Data.Comp.Multi.Term
import Data.Comp.Dict
import Data.Comp.Elem

class KEq f => KOrd f where
    kcompare :: f i -> f j -> Ordering

{-| Signature ordering. An instance @OrdHF f@ gives rise to an instance
  @Ord (Term f)@. -}
class EqHF f => OrdHF f where
    compareHF :: KOrd a => f a i -> f a j -> Ordering

instance KOrd f => Ord (E f) where
    compare (E x) (E y) = kcompare x y

instance Ord a => KOrd (K a) where
    kcompare (K x) (K y) = compare x y

{-| 'OrdHF' is propagated through sums. -}
instance ( All OrdHF fs
         , EqHF (Sum fs)
         ) => OrdHF (Sum fs) where
    compareHF (Sum wit1 x) (Sum wit2 y) =
      case elemEq wit1 wit2 of
        Just Refl -> compareHF x y \\ dictFor @OrdHF wit1
        Nothing   -> comparePos wit1 wit2

{-| From an 'OrdHF' difunctor an 'Ord' instance of the corresponding term type
  can be derived. -}
instance (HFunctor f, OrdHF f) => OrdHF (Cxt h f) where
    compareHF (Term e1) (Term e2) = compareHF e1 e2
    compareHF (Hole h1) (Hole h2) = kcompare h1 h2
    compareHF (Term _) _ = LT
    compareHF (Hole _) (Term _) = GT

instance (HFunctor f, OrdHF f, KOrd a) => KOrd (Cxt h f a) where
    kcompare = compareHF

{-| Ordering of terms. -}
instance (HFunctor f, OrdHF f, KOrd a) => Ord (Cxt h f a i) where
    compare = kcompare
