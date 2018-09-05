{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TemplateHaskell #-}

module Cubix.Language.Lua.Parametric.Full.Names (
    origASTTypes
  , newASTTypes
  , luaSigNamesBase
  , makeSubsts

  , isAnn
  , propAnn

  , annType
  , annotatedSourceType
  ) where


import           Data.Map ( Map )
import qualified Data.Map as Map

import           Language.Haskell.TH hiding ( Name )
import qualified Language.Haskell.TH as TH
import           Language.Lua.Annotated

import           Data.Comp.Trans ( runCompTrans, generateNameLists, withExcludedNames, getTypeParamVars, defaultPropAnn )

import           Cubix.Language.Info

import           Cubix.Language.Lua.Parametric.Full.Exclusions

import           Cubix.Language.Parametric.Syntax.Base
import           Cubix.Language.Parametric.Syntax.Functor

runCompTrans $ withExcludedNames excludedNamesSet $ generateNameLists ''Block

luaSigNamesBase :: [TH.Name]
luaSigNamesBase = newASTTypes ++ [''PairF, ''ListF, ''MaybeF, ''UnitF]


makeSubsts :: Q (Map TH.Name Type)
makeSubsts = do
  vars <- runCompTrans $ getTypeParamVars origASTTypes
  let substs = Map.fromList (zip vars (repeat $ AppT (ConT ''Maybe) (ConT ''SourceSpan)))
  return substs


isAnn :: TH.Type -> Bool
isAnn = (== (TH.AppT (TH.ConT ''Maybe) (TH.ConT ''SourceSpan)))

propAnn :: [(TH.Exp, TH.Type)] -> TH.Exp
propAnn = defaultPropAnn (TH.ConE 'Nothing)


annType :: TH.Type
annType = (TH.AppT (TH.ConT ''Maybe) (TH.ConT ''SourceSpan))

annotatedSourceType :: TH.Type
annotatedSourceType = TH.AppT (TH.ConT ''Block) annType