{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.JavaScript.Parametric.Full.Names () where
#else
module Cubix.Language.JavaScript.Parametric.Full.Names (
    origASTTypes
  , newASTTypes
  , jsSigNamesBase
  ) where

import qualified Language.Haskell.TH as TH ( Name )

import Language.JavaScript.Parser.AST

import Data.Comp.Trans ( runCompTrans, generateNameLists, withExcludedNames )

import Cubix.Language.JavaScript.Parametric.Full.Exclusions
import Cubix.Language.Parametric.Syntax.Functor

runCompTrans $ withExcludedNames excludedNamesSet $ generateNameLists ''JSAST

jsSigNamesBase :: [TH.Name]
jsSigNamesBase = newASTTypes ++ [''ListF, ''MaybeF]
#endif