{-# OPTIONS_HADDOCK hide #-}

module Cubix.Transformations.Hoist (
   -- * Hoist transformations
   elementaryHoist
 , hoistDeclarations

   -- * Re-exports for language implementations
   -- ** From Custom
 , HoistState
 , seenIdents
 , SpecialHoistState
 , HoistStateConstraints(..)
 , VarInitToRhs(..)
 , VarDeclBinderToLhs(..)
 , SpecialHoist(..)
 , BuiltinSpecialIdents(..)
 , BlockHoisting(..)

   -- ** From Variation
 , StatSort
 , VariableInsertionVariation(..)
 , VariableInsertionVariationDict(..)
 , SingleDecInsertion
 , MultiDecInsertion
 ) where

import Cubix.Transformations.Hoist.Custom
import Cubix.Transformations.Hoist.Elementary
import Cubix.Transformations.Hoist.Hoisting
import Cubix.Transformations.Variation
  ( StatSort
  , VariableInsertionVariation(..)
  , VariableInsertionVariationDict(..)
  , SingleDecInsertion
  , MultiDecInsertion
  )

