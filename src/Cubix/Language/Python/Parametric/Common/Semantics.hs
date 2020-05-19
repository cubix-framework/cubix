{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cubix.Language.Python.Parametric.Common.Semantics () where

#ifndef ONLY_ONE_LANGUAGE
import Control.Monad ( liftM )

import Data.Proxy ( Proxy(..) )
import Data.Type.Equality ( (:~:)(..), gcastWith )

import Data.Comp.Multi ( Cxt(..), project, project', inject', (:&:)(..), (:-<:) )
import Data.Comp.Multi.Strategy.Classification ( KDynCase(..), kIsSort )

import Cubix.Language.Info
import Cubix.Language.Python.Parametric.Common.Types
import Cubix.Language.Python.Parametric.Full
import Cubix.Language.Parametric.Semantics.SemanticProperties
import Cubix.Language.Parametric.Syntax

import Cubix.Sin.Compdata.Annotation ( annM )

instance {-# OVERLAPPING #-} (Op :-<: gs) => GetStrictness' gs Expr where
  getStrictness' t@(BinaryOp op _ _ _) = case project op of
    Just (And _) -> [NoEval, Strict, GuardedBy (Place 1), NoEval]
    Just (Or _)  -> [NoEval, Strict, GuardedBy (NegPlace 1),    NoEval]
    _           -> defaultGetStrictness t

  getStrictness' x                  = defaultGetStrictness x

instance {-# OVERLAPPING #-} GetStrictness' g PyCondExpr where
  getStrictness' (PyCondExpr _ _ _) = [Strict, GuardedBy (Place 0), GuardedBy (NegPlace 0)]



instance {-# OVERLAPPING #-} InsertAt' MPythonSig StatementL Statement where
  insertAt' (BeforeIntermediateEvalPoint n) e (Conditional clauses els _ :&: l)
                | (n > 0) && (n < length (extractF clauses)) = do
      let (former, latter) = splitAt n (extractF clauses)
      innerCond <- annotateOuter $ iConditional (insertF $ map Hole latter) (Hole els) iUnitF
      outerCond <- annotateOuter $ iConditional (insertF $ map Hole former) (insertF [Hole e, Hole innerCond]) iUnitF
      return outerCond

  -- This case is hit when doing multiple inserts on the same conditional, causing some eval points to move into the else
  -- This doesn't really work because it just puts everything at the front, instead of where it's meant to go...
  insertAt' (BeforeIntermediateEvalPoint n) e (Conditional clauses els u :&: l)
                | n >= length (extractF clauses)
    = do els' <- insertBefore e els
         return $ inject' $ (Conditional clauses els' u) :&: l

  insertAt' _ _ t = return $ inject' t

  -- If = 0, use normal insertion at the list level, not this kind which will split an if/elif
  canInsertAt' (BeforeIntermediateEvalPoint n) _ (Conditional clauses _ _ :&: _) = (n > 0) && (n < length (extractF clauses))
  canInsertAt' _                               _ _                               = False

instance {-# OVERLAPPING #-} InsertAt' MPythonSig BlockItemL Statement where
  insertAt' p (project' -> Just (StatementIsBlockItem s)) t = insertAt' p s t
  canInsertAt' p _ t = canInsertAt' p (Proxy :: Proxy StatementL) t


instance {-# OVERLAPPING #-} InsertAt' MPythonSig StatementL PyWith where
  insertAt' (BeforeIntermediateEvalPoint n) e (PyWith binders body :&: l)
                  | (n > 0) && (n < length (extractF binders)) = do
      let (former, latter) = splitAt n (extractF binders)
      innerWith <- annotateOuter $ iPyWith (insertF $ map Hole latter) (Hole body)
      outerWith <- annotateOuter $ iPyWith (insertF $ map Hole former) (insertF [Hole e, Hole innerWith])
      return outerWith

  insertAt' (BeforeIntermediateEvalPoint n) e (PyWith binders body :&: l)
                 | (n >= length (extractF binders))
    = do body' <- insertBefore e body
         return $ inject' $ (PyWith binders body') :&: l

  insertAt' _ _ t = return $ inject' t


  canInsertAt' (BeforeIntermediateEvalPoint n) _ (PyWith binders _ :&: _) = (n > 0) && (n < length (extractF binders))
  canInsertAt' _                               _ _                        = False

instance {-# OVERLAPPING #-} InsertAt' MPythonSig BlockItemL PyWith where
  insertAt' p (project' -> Just (StatementIsBlockItem s)) t = insertAt' p s t
  canInsertAt' p _ t = canInsertAt' p (Proxy :: Proxy StatementL) t

-- We can insert statements before lists of statements, *or* before lists of BlockItem's
-- Python has both because scoping
instance {-# OVERLAPPING #-} InsertAt' MPythonSig StatementL ListF where
  insertAt' EnterEvalPoint e t =
                      case kdyncase t :: Maybe (_ :~: [StatementL]) of
                        Just p  -> gcastWith p $ inject' <$> annM (ConsF e (inject' t))
                        Nothing -> case kdyncase t :: (Maybe (_ :~: [BlockItemL])) of
                                     Nothing -> return $ inject' t
                                     Just p  -> gcastWith p $ liftM inject' $ annM =<< (ConsF <$> (inject' <$> (annM $ StatementIsBlockItem e)) <*> pure (inject' t))
  insertAt' _ _ t = return $ inject' t

  canInsertAt' EnterEvalPoint _  t =    kIsSort (Proxy :: Proxy [StatementL]) t
                                     || kIsSort (Proxy :: Proxy [BlockItemL]) t
  canInsertAt' _              _  _ = False

instance {-# OVERLAPPING #-} InsertAt' MPythonSig BlockItemL ListF where
  insertAt' p (project' -> Just (StatementIsBlockItem s)) t = insertAt' p s t
  canInsertAt' p _ t = canInsertAt' p (Proxy :: Proxy StatementL) t

#endif
