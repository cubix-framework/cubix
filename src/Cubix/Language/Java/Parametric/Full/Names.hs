{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- This is in a separate file due to GHC's phase restriction

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Java.Parametric.Full.Names () where
#else
module Cubix.Language.Java.Parametric.Full.Names (
    origASTTypes
  , newASTTypes
  , javaSigNames
  ) where

import qualified Language.Haskell.TH as TH ( Name )
import Language.Java.Syntax

import Data.Comp.Trans ( runCompTrans, generateNameLists )

import Cubix.Language.Parametric.Syntax.Functor

runCompTrans $ generateNameLists ''CompilationUnit

javaSigNames :: [TH.Name]
javaSigNames = newASTTypes ++ [''PairF, ''ListF, ''MaybeF]
#endif