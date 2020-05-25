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

module Cubix.Language.C.Parametric.Full.Types where

#ifndef ONLY_ONE_LANGUAGE
import qualified Language.C.Syntax as C ( CTranslationUnit )

import Data.Comp.Multi ( Term )
import Data.Comp.Trans ( runCompTrans, withSubstitutions, deriveMultiComp, makeSumType )

import Cubix.Language.Info
import Cubix.Language.C.Parametric.Full.Names
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax.Base

-----------------------------------------------------------

do substs <- makeSubsts
   runCompTrans $ withSubstitutions substs $ deriveMultiComp ''C.CTranslationUnit

deriveAll newASTTypes
runCompTrans $ makeSumType "CSig" cSigNames

type CTerm = Term CSig
type CTermLab l = TermLab CSig l
#endif
