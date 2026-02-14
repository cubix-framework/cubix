{-# OPTIONS_HADDOCK hide #-}

-- | Exports for implementing language-specific hoist instances in external packages.
module Cubix.Transformations.Hoist.External (
    -- * Custom instances for language support
    HoistState
  , seenIdents
  , HoistStateConstraints(..)
  , VarInitToRhs(..)
  , VarDeclBinderToLhs(..)
  , SpecialHoist(..)
  , BuiltinSpecialIdents(..)
  , BlockHoisting(..)
  , SpecialHoistState

    -- * Variation support
  , StatSort
  , VariableInsertionVariation(..)
  , VariableInsertionVariationDict(..)
  , SingleDecInsertion
  , MultiDecInsertion
  ) where

import Cubix.Transformations.Hoist.Custom
import Cubix.Transformations.Variation
