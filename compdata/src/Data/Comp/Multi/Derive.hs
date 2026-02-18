{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Derive
-- Copyright   :  (c) 2010-2011 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module contains functionality for automatically deriving boilerplate
-- code using Template Haskell. Examples include instances of 'HFunctor',
-- 'HFoldable', and 'HTraversable'.
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Derive
    (
     derive,
     -- |Derive boilerplate instances for higher-order signatures, i.e.
     -- signatures for generalised compositional data types.

     -- ** HShowF
     module Data.Comp.Multi.Derive.Show,
     -- ** ConstrNameHF
     module Data.Comp.Multi.Derive.ConstrName,
     -- ** EqHF
     module Data.Comp.Multi.Derive.Equality,
     -- ** OrdHF
     module Data.Comp.Multi.Derive.Ordering,
     -- ** HFunctor
     module Data.Comp.Multi.Derive.HFunctor,
     -- ** HFoldable
     module Data.Comp.Multi.Derive.HFoldable,
     -- ** HTraversable
     module Data.Comp.Multi.Derive.HTraversable,
     -- ** Smart Constructors
     module Data.Comp.Multi.Derive.SmartConstructors,
     -- ** Lifting to Sums
     liftSum,
     -- ** Generic
     module Data.Comp.Multi.Derive.Generic
    ) where

import Data.Comp.Derive.Utils (derive, liftSumGen)
import Data.Comp.Dict (All)
import Data.Comp.Multi.Derive.ConstrName
import Data.Comp.Multi.Derive.Equality
import Data.Comp.Multi.Derive.Generic
import Data.Comp.Multi.Derive.HFoldable
import Data.Comp.Multi.Derive.HFunctor
import Data.Comp.Multi.Derive.HTraversable
import Data.Comp.Multi.Derive.Ordering
import Data.Comp.Multi.Derive.Show
import Data.Comp.Multi.Derive.SmartConstructors
import Data.Comp.Multi.Ops (Sum, caseCxt)

import Language.Haskell.TH

{-| Given the name of a type class, where the first parameter is a higher-order
  functor, lift it to sums of higher-order. Example: @class HShowF f where ...@
  is lifted as @instance (HShowF f, HShowF g) => HShowF (f :+: g) where ... @.
 -}
liftSum :: Name -> Q [Dec]
liftSum = liftSumGen 'caseCxt ''Sum ''All
