{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- This is a separate file due to GHC's phase restriction

module Cubix.Language.C.Parametric.Full.Types where

#ifndef ONLY_ONE_LANGUAGE
import qualified Language.C.Syntax as C ( CTranslationUnit )

import qualified Language.Haskell.TH as TH

import Data.Comp.Multi ( Term, AnnTerm )
import Data.Comp.Trans ( runCompTrans, withSubstitutions, withAnnotationProp, defaultUnpropAnn, deriveMultiComp, makeSumType )

import Cubix.Language.Info
import Cubix.Language.C.Parametric.Full.Names
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax.Base

-----------------------------------------------------------

do substs <- makeSubsts
   runCompTrans $ withAnnotationProp annType isAnn propAnn defaultUnpropAnn
                $ withSubstitutions substs
                $ deriveMultiComp ''C.CTranslationUnit

deriveAll newASTTypes
runCompTrans $ makeSumType "CSig" cSigNames

type CTerm = Term CSig
type CTermLab l = TermLab CSig l

type CTermAnn    a = AnnTerm        a  CSig
type CTermOptAnn a = AnnTerm (Maybe a) CSig

annotatedTargType :: TH.Type
annotatedTargType = TH.AppT (TH.AppT (TH.ConT ''AnnTerm) annType) (TH.ConT ''CSig)
#endif
