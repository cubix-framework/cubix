{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- This is a separate file due to GHC's phase restriction

module Cubix.Language.Python.Parametric.Full.Types where

#ifndef ONLY_ONE_LANGUAGE
import Data.Comp.Multi ( Term, AnnTerm )

import qualified Language.Haskell.TH as TH
import qualified Language.Python.Common.AST as P

import Data.Comp.Trans ( runCompTrans, withSubstitutions, deriveMultiComp, makeSumType, withAnnotationProp, defaultUnpropAnn )

import Cubix.Language.Info
import Cubix.Language.Python.Parametric.Full.Names
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax.Base

-----------------------------------------------------------

do substs <- makeSubsts
   runCompTrans $ withAnnotationProp annType isAnn propAnn defaultUnpropAnn
                $ withSubstitutions substs
                $ deriveMultiComp ''P.Module

deriveAll newASTTypes
runCompTrans $ makeSumType "PythonSig" pythonSigNames

type PythonTerm = Term PythonSig
type PythonTermLab l = TermLab PythonSig l

type PythonTermAnn    a = AnnTerm        a  PythonSig
type PythonTermOptAnn a = AnnTerm (Maybe a) PythonSig


annotatedTargType :: TH.Type
annotatedTargType = TH.AppT (TH.AppT (TH.ConT ''AnnTerm) annType) (TH.ConT ''PythonSig)

#endif
