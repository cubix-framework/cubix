{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Language.JavaScript.Parametric.Common.Semantics () where

#ifndef ONLY_ONE_LANGUAGE
import Control.Monad ( liftM )

import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy(..) )
import Data.Type.Equality ( (:~:)(..), gcastWith )

import Data.Comp.Multi ( project, project', inject', Cxt(..), appCxt, AnnTerm, (:-<:), ContextS )
import Data.Comp.Multi.Strategy.Classification ( KDynCase(..), kIsSort, dynProj )

import Cubix.Language.JavaScript.Parametric.Common.Types
import Cubix.Language.JavaScript.Parametric.Full
import Cubix.Language.Parametric.Semantics.SemanticProperties
import Cubix.Language.Parametric.Syntax

import Cubix.Sin.Compdata.Annotation ( annM, annotateM )

import Unsafe.Coerce ( unsafeCoerce )

instance {-# OVERLAPPING #-} (JSBinOp :-<: gs) => GetStrictness' gs JSExpression where
  getStrictness' (JSExpressionTernary _ _ _ _ _) = [Strict, NoEval, GuardedBy (Place 0), NoEval, GuardedBy (NegPlace 0)]
  getStrictness' t@(JSExpressionBinary _ op _)   = case project op of
    Just (JSBinOpAnd _) -> [Strict, NoEval, GuardedBy (Place 0)]
    Just (JSBinOpOr _)  -> [Strict, NoEval, GuardedBy (NegPlace 0)]
    _           -> defaultGetStrictness t
  getStrictness' x                  = defaultGetStrictness x

-- We can insert statements before list of statements, *or* before lists of BlockItem's
-- JS has both because scoping
instance {-# OVERLAPPING #-} InsertAt' MJSSig JSStatementL ListF where
  insertAt' EnterEvalPoint e t =
        case kdyncase t :: Maybe (_ :~: [JSStatementL]) of
            Just p  -> gcastWith p $ inject' <$> annM (ConsF e (inject' t))
            Nothing -> case kdyncase t :: (Maybe (_ :~: [BlockItemL])) of
                           Nothing -> return $ inject' t
                           Just p  ->
                             gcastWith p $ liftM inject' $ annM =<< (ConsF <$> (inject' <$> (annM $ JSStatementIsBlockItem e)) <*> return (inject' t))
  insertAt' _ _ t = return $ inject' t

  canInsertAt' EnterEvalPoint _ t =     kIsSort (Proxy :: Proxy [JSStatementL]) t
                                     || kIsSort (Proxy :: Proxy [BlockItemL]) t
  canInsertAt' _              _ _ = False

-- And here's inserting a block item into a list of statements/block items
instance {-# OVERLAPPING #-} InsertAt' MJSSig BlockItemL ListF where
  insertAt' p (project' -> Just (JSStatementIsBlockItem s)) t = insertAt' p s t

  canInsertAt' p  _ t = canInsertAt' p (Proxy :: Proxy JSStatementL) t


instance {-# OVERLAPPING #-} InsertAt' MJSSig JSStatementL JSStatement where
  insertAt' EnterEvalPoint t s = liftM convertTerm $ liftM appCxt $ annotateM e
    where
      e :: ContextS MJSSig (AnnTerm _ MJSSig) JSStatementL
      e = iJSStatementBlock iJSNoAnnot (insertF [Hole t, (Hole $ fromJust $ dynProj $ inject' s) :: ContextS MJSSig _ JSStatementL]) iJSNoAnnot iJSSemiAuto

      convertTerm :: AnnTerm a MJSSig i -> AnnTerm a MJSSig j
      convertTerm = unsafeCoerce
  insertAt' _ _ s = return $ inject' s

  canInsertAt' EnterEvalPoint _ _ = True
  canInsertAt' _              _ _ = False

instance {-# OVERLAPPING #-} InsertAt' MJSSig BlockItemL JSStatement where
  insertAt' p (project' -> Just (JSStatementIsBlockItem s)) t = insertAt' p s t

  canInsertAt' p  _ t = canInsertAt' p (Proxy :: Proxy JSStatementL) t

#endif
