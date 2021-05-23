{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Language.Java.Parametric.Common.Semantics () where

#ifndef ONLY_ONE_LANGUAGE
import Control.Monad ( liftM )
import Data.Maybe ( fromJust )

import Data.Comp.Multi ( project, appCxt, Context, Cxt(..), AnnTerm, inject', (:-<:), ContextS )
import Data.Comp.Multi.Strategy.Classification ( dynProj )

import Cubix.Language.Java.Parametric.Common.Types
import Cubix.Language.Java.Parametric.Full
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Semantics.SemanticProperties
import Cubix.Language.Parametric.Syntax.Functor
import Cubix.Language.Parametric.Syntax.VarDecl as V

import Cubix.Sin.Compdata.Annotation ( annotateM )

import Unsafe.Coerce ( unsafeCoerce )

instance {-# OVERLAPPING #-} (Op :-<: gs) => GetStrictness' gs Exp where
  getStrictness'   (Cond _ _ _)   = [Strict, GuardedBy (Place 0), GuardedBy (NegPlace 0)]
  getStrictness' t@(BinOp _ op _) = case project op of
    Just CAnd -> [Strict, NoEval, GuardedBy (Place 0)]
    Just COr  -> [Strict, NoEval, GuardedBy (NegPlace 0)]
    _           -> defaultGetStrictness t
  getStrictness' x              = defaultGetStrictness x

instance {-# OVERLAPPING #-} InsertAt' MJavaSig BlockItemL Stmt where
  -- I swear I remember writing something that could safely e.g.:
  -- cast a (CStatement e i) to (CStatement e CStatementL)
  insertAt' EnterEvalPoint t s =  liftM convertTerm $ liftM appCxt $ annotateM e
    where
      e :: ContextS MJavaSig (AnnTerm _ MJavaSig) StmtL
      e = V.iBlock (insertF [Hole t, injF ((Hole $ fromJust $ dynProj $ inject' s) :: Context _ _ StmtL)]) EmptyBlockEnd'

      convertTerm :: AnnTerm a MJavaSig i -> AnnTerm a MJavaSig j
      convertTerm = unsafeCoerce
  insertAt' _ _ t = return $ inject' t

  -- While's always start their own basic block. Kinda correct, but kinda not
  -- We're disabling this disablement as a workaround for a bug
  --canInsertAt' EnterEvalPoint _ (While _ _ :&: _) = False
  canInsertAt' EnterEvalPoint _ _ = True
  canInsertAt' _              _ _ = False

#endif
