{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses,
DataKinds, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Examples.Multi.Common
-- Copyright   :  (c) 2011 Patrick Bahr, Tom Hvitved
-- License     :  BSD3
-- Maintainer  :  Tom Hvitved <hvitved@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Common example files.
--
--------------------------------------------------------------------------------

module Examples.Multi.Common where

import Data.Comp.Multi
import Data.Comp.Multi.Show ()
import Data.Comp.Multi.Equality ()
import Data.Comp.Multi.Ordering ()
import Data.Comp.Multi.Derive
import Data.Comp.Multi.Mutable

-- Signature for values and operators
data Value a i where
  Const ::        Int -> Value a Int
  Pair  :: a i -> a j -> Value a (i,j)
data Op a i where
  Add, Mult :: a Int -> a Int   -> Op a Int
  Fst       ::          a (i,j) -> Op a i
  Snd       ::          a (i,j) -> Op a j

-- Signature for the simple expression language
type Sig = '[Op, Value]
-- Derive boilerplate code using Template Haskell (GHC 7 needed)
$(derive [makeHFunctor, makeHFoldable, makeHTraversable, makeShowHF, makeEqHF,
          makeOrdHF, smartConstructors]
         [''Value, ''Op])