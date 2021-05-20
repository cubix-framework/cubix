{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- This is a separate file due to GHC's phase restriction

module Cubix.Language.Python.Parametric.Full.Types where

#ifndef ONLY_ONE_LANGUAGE
import Data.Comp.Multi ( Term )

import qualified Language.Python.Common.AST as P

import Data.Comp.Trans ( runCompTrans, withSubstitutions, deriveMultiComp, makeSumType )

import Cubix.Language.Info
import Cubix.Language.Python.Parametric.Full.Names
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax.Base

-----------------------------------------------------------

do substs <- makeSubsts
   runCompTrans $ withSubstitutions substs $ deriveMultiComp ''P.Module

deriveAll newASTTypes
runCompTrans $ makeSumType "PythonSig" pythonSigNames

type PythonTerm = Term PythonSig
type PythonTermLab l = TermLab PythonSig l
#endif
