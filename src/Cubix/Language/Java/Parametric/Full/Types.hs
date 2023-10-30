{-# OPTIONS_HADDOCK hide                     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -freduction-depth=120        #-}

{-# LANGUAGE CPP                             #-}
{-# LANGUAGE TemplateHaskell                 #-}
{-# LANGUAGE UndecidableInstances            #-}

-- This is a separate file due to GHC's phase restriction

module Cubix.Language.Java.Parametric.Full.Types where

#ifndef ONLY_ONE_LANGUAGE
import Data.Comp.Multi ( Term )

import qualified Language.Java.Syntax as J ( CompilationUnit )

import Data.Comp.Trans ( runCompTrans, deriveMultiComp, makeSumType )

import Cubix.Language.Info
import Cubix.Language.Java.Parametric.Full.Names
import Cubix.Language.Parametric.Derive

-----------------------------------------------------------

runCompTrans $ deriveMultiComp ''J.CompilationUnit
deriveAll newASTTypes
runCompTrans $ makeSumType "JavaSig" javaSigNames


type JavaTerm = Term JavaSig
type JavaTermLab l = TermLab JavaSig
#endif
