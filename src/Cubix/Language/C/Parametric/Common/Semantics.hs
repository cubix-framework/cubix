{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Language.C.Parametric.Common.Semantics () where

#ifndef ONLY_ONE_LANGUAGE
import Control.Monad ( liftM )
import Data.Maybe ( fromJust )

import Data.Comp.Multi ( AnnTerm, ContextS, Cxt(..), project, appCxt,  inject', (:-<:) )
import Data.Comp.Multi.Strategy.Classification ( dynProj )

import Cubix.Language.C.Parametric.Common.Types
import Cubix.Language.C.Parametric.Full
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Semantics.SemanticProperties
import Cubix.Language.Parametric.Syntax

import Cubix.Sin.Compdata.Annotation ( annotateM )

import Unsafe.Coerce ( unsafeCoerce )

--------------------------------------------------------------------------

----------------------------------------------------------------
---------------------- Strictness  -----------------------------
----------------------------------------------------------------

instance {-# OVERLAPPING #-} (CBinaryOp :-<: gs) => GetStrictness' gs CExpression where
  getStrictness'   (CCond _ _ _ _)     = [Strict, GuardedBy (Place 0), GuardedBy (NegPlace 0), NoEval]
  getStrictness'   (CSizeofExpr _ _)   = [NoEval, NoEval]
  getStrictness' t@(CBinary op _ _ _)  = case project op of
    Just CLndOp -> [NoEval, Strict, GuardedBy (Place 1), NoEval]
    Just CLorOp -> [NoEval, Strict, GuardedBy (NegPlace 1), NoEval]
    _           -> defaultGetStrictness t
  getStrictness' x                     = defaultGetStrictness x


----------------------------------------------------------------
---------------------- Insertion  -----------------------------
----------------------------------------------------------------

instance {-# OVERLAPPING #-} InsertAt' MCSig BlockItemL CStatement where

  -- I swear I remember writing something that could safely e.g.:
  -- cast a (CStatement e i) to (CStatement e CStatementL)
  insertAt' EnterEvalPoint t s =  liftM convertTerm $ liftM appCxt $ annotateM e
    where
      e :: ContextS MCSig (AnnTerm _ MCSig) CStatementL
      e = iCLabeledBlock NilF' (iBlock (insertF [Hole t, injF ((Hole $ fromJust $ dynProj $ inject' s) :: ContextS MCSig _ CStatementL)]) EmptyBlockEnd')

      convertTerm :: AnnTerm a MCSig i -> AnnTerm a MCSig j
      convertTerm = unsafeCoerce
  insertAt' _ _ s = return $ inject' s

  canInsertAt' EnterEvalPoint _ _ = True
  canInsertAt' _              _ _ = False

#endif
