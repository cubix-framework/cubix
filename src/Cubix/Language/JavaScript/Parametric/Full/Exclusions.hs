{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.JavaScript.Parametric.Full.Exclusions () where
#else
module Cubix.Language.JavaScript.Parametric.Full.Exclusions (
    jsExcludedNames
  , excludedNamesSet
  ) where

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Comp.Trans ( standardExcludedNames )

import qualified Language.Haskell.TH.Syntax as TH
import Language.JavaScript.Parser.AST

-- | We handle these specially because they take type arguments

jsExcludedNames :: [TH.Name]
jsExcludedNames = [''JSCommaList, ''JSCommaTrailingList]

excludedNamesSet :: Set TH.Name
excludedNamesSet = Set.union standardExcludedNames $ Set.fromList jsExcludedNames

#endif