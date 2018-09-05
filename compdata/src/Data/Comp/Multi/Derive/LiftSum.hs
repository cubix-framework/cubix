{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Derive.LiftSum
-- Copyright   :  (c) 2011 Patrick Bahr, Tom Hvitved
-- License     :  BSD3
-- Maintainer  :  Tom Hvitved <hvitved@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Lift a class declaration for higher-order functors to sums of higher-order
-- functors.
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Derive.LiftSum
    (
     liftSum,
     caseH
    ) where

import Data.Comp.Derive.Utils
import Data.Comp.Multi.Ops ((:+:) (..))
import Data.Comp.Multi.Sum
import Language.Haskell.TH hiding (Cxt)

{-| Given the name of a type class, where the first parameter is a higher-order
  functor, lift it to sums of higher-order. Example: @class HShowF f where ...@
  is lifted as @instance (HShowF f, HShowF g) => HShowF (f :+: g) where ... @.
 -}
liftSum :: Name -> Q [Dec]
liftSum = liftSumGen 'caseH ''(:+:)

{-| Utility function to case on a higher-order functor sum, without exposing the
  internal representation of sums. -}
caseH :: (f a b -> c) -> (g a b -> c) -> (f :+: g) a b -> c
caseH f g x = case x of
                Inl x -> f x
                Inr x -> g x
