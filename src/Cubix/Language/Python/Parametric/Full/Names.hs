{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- This is in a separate file due to GHC's phase restriction

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Python.Parametric.Full.Names () where
#else
module Cubix.Language.Python.Parametric.Full.Names (
    origASTTypes
  , newASTTypes
  , pythonSigNames
  , makeSubsts
  ) where

import           Data.Map ( Map )
import qualified Data.Map as Map

import           Language.Haskell.TH hiding ( Name )
import qualified Language.Haskell.TH as TH
import           Language.Python.Common.AST

import           Data.Comp.Trans ( runCompTrans, generateNameLists, getTypeParamVars )

import           Cubix.Language.Parametric.Syntax.Base
import           Cubix.Language.Parametric.Syntax.Functor

runCompTrans $ generateNameLists ''Module

pythonSigNames :: [TH.Name]
pythonSigNames = newASTTypes ++ [''PairF, ''ListF, ''MaybeF, ''UnitF, ''CharF]

makeSubsts :: Q (Map TH.Name Type)
makeSubsts = do
  vars <- runCompTrans $ getTypeParamVars origASTTypes
  let substs = Map.fromList (zip vars (repeat $ TupleT 0))
  return substs
#endif