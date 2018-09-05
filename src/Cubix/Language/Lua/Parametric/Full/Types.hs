{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- This is a separate file due to GHC's phase restriction

module Cubix.Language.Lua.Parametric.Full.Types where

import Data.Comp.Multi ( Term, AnnTerm )

import qualified Language.Haskell.TH as TH
import qualified Language.Lua.Annotated as Lua

import Data.Comp.Trans ( runCompTrans, deriveMultiComp, makeSumType, withAnnotationProp, defaultUnpropAnn, withSubstitutions, withExcludedNames )

import Cubix.Language.Info
import Cubix.Language.Lua.Parametric.Full.Exclusions
import Cubix.Language.Lua.Parametric.Full.Names
import Cubix.Language.Parametric.Derive

-----------------------------------------------------------

data FunBodyL

do substs <- makeSubsts
   runCompTrans $ withAnnotationProp annType isAnn propAnn defaultUnpropAnn $ withSubstitutions substs $ withExcludedNames excludedNamesSet $ deriveMultiComp ''Lua.Block

deriveAll newASTTypes

data FunBody e l where
  FunBody :: e [NameL] -> Bool -> e BlockL -> FunBody e FunBodyL

deriveAll [''FunBody]

let luaSigNames = luaSigNamesBase ++ [''FunBody] in
    runCompTrans $ makeSumType "LuaSig" luaSigNames

luaSigNames :: [TH.Name]
luaSigNames = luaSigNamesBase ++ [''FunBody] -- Yes, this duplicates code. Template Haskell restrictions

type LuaTerm    = Term LuaSig
type LuaTermLab l = TermLab LuaSig l

type LuaTermOptAnn a = AnnTerm (Maybe a) LuaSig

annotatedTargType :: TH.Type
annotatedTargType = TH.AppT (TH.AppT (TH.ConT ''AnnTerm) annType) (TH.ConT ''LuaSig)
