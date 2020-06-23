{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cubix.Language.Python.Parametric.Common.Cfg () where

#ifndef ONLY_ONE_LANGUAGE
import Control.Monad ( liftM )
import Control.Monad.State ( MonadState )

import Control.Lens ( (%=), makeLenses )

import Data.Traversable ( for )

import Data.Comp.Multi ( (:*:)(..), remA, stripA, project )

import Cubix.Language.Info

import Cubix.Language.Python.Parametric.Common.Types as C
import Cubix.Language.Python.Parametric.Full.Types as F
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Semantics.SemanticProperties
import Cubix.Language.Parametric.Syntax as P

data PythonCfgState = PythonCfgState {
                   _pcs_cfg       :: Cfg MPythonSig
                 , _pcs_labeler   :: LabelGen
                 , _pcs_stack     :: LoopStack
                 , _pcs_goto_labs :: LabelMap
                 }

makeLenses ''PythonCfgState

instance HasCurCfg PythonCfgState MPythonSig where cur_cfg = pcs_cfg
instance HasLabelGen PythonCfgState where labelGen = pcs_labeler
instance HasLoopStack PythonCfgState where loopStack = pcs_stack
instance HasLabelMap PythonCfgState where labelMap = pcs_goto_labs


type instance ComputationSorts MPythonSig = '[StatementL, ExprL, [BlockItemL], [StatementL]]

type instance SuspendedComputationSorts MPythonSig = '[]
type instance ContainerFunctors MPythonSig = '[PairF, ListF, MaybeF]
type instance CfgState MPythonSig = PythonCfgState


singleton :: a -> [a]
singleton = return

constructCfgWhileElse ::
  ( HasLoopStack s
  , MonadState s m
  , CfgComponent gs s
  ) => TermLab gs l -> m (EnterExitPair gs h) -> m (EnterExitPair gs i) -> m (EnterExitPair gs j) -> m (EnterExitPair gs k)
constructCfgWhileElse t mExp mBody mElse = do
  enterNode     <- addCfgNode t EnterNode
  loopEntryNode <- addCfgNode t LoopEntryNode
  exitNode      <- addCfgNode t ExitNode

  exp <- mExp >>= collapseEnterExit
  pushLoopNode loopEntryNode exitNode
  body <- mBody
  popLoopNode

  els <- mElse >>= collapseEnterExit

  end <- combineEnterExit els (identEnterExit exitNode)

  cur_cfg %= addEdge enterNode loopEntryNode
  cur_cfg %= addEdge loopEntryNode (enter exp)
  cur_cfg %= addEdge (exit exp) (enter body)
  cur_cfg %= addEdge (exit exp) (enter end)
  cur_cfg %= addEdge (exit body) loopEntryNode

  return $ EnterExitPair enterNode exitNode


-- When developing this, I ran afoul of a GHC bug that resulted in my program segfaulting (and changed when
-- I swapped the order of my instance declarations). Luckily, a "stack clean" fixed the problem

instance {-# OVERLAPPING #-} ConstructCfg MPythonSig PythonCfgState Statement where
  constructCfg (collapseFProd' -> (t :*: While cond body els _)) = HState $ constructCfgWhileElse t (unHState cond) (unHState body) (unHState els)
  constructCfg (collapseFProd' -> (t :*: For _ cond body els _)) = HState $ constructCfgWhileElse t (unHState cond) (unHState body) (unHState els)

  constructCfg (collapseFProd' -> t :*: Conditional clauses els _) = HState $ constructCfgIfElseIfElse t (liftM (map extractEEPPair . extractEEPList) $ unHState clauses) (liftM Just $ unHState els)

  constructCfg (collapseFProd' -> (t :*: Return e _)) = HState $ constructCfgReturn t (extractEEPMaybe $ unHState e)
  constructCfg (collapseFProd' -> (t :*: Try body handlers els finally _)) = HState $ do
    eepBody <- unHState body
    unHState handlers
    eepElse <- unHState els
    unHState finally

    eeMain <- combineEnterExit eepBody eepElse

    enterNode <- addCfgNode t EnterNode
    exitNode  <- addCfgNode t ExitNode

    p <- combineEnterExit (identEnterExit enterNode) eeMain
    combineEnterExit p (identEnterExit exitNode)

  constructCfg (collapseFProd' -> (t :*: Raise e _)) = HState $ constructCfgReturn t (liftM Just $ unHState e)
  constructCfg (collapseFProd' -> (t :*: Break _)) = HState $ constructCfgBreak t
  constructCfg (collapseFProd' -> (t :*: Continue _)) = HState $ constructCfgContinue t

  constructCfg t = constructCfgDefault t

-- Excludes evaluating default arguments from the CFG. I don't remember why I did this.
instance {-# OVERLAPPING #-} ConstructCfg MPythonSig PythonCfgState FunctionDef where
  constructCfg a@(collapseFProd' -> t :*: FunctionDef _ _ _ body) = HState (unHState body >> (constructCfgEmpty t))

-- | Control flow actually does flow through for classes; it's not a suspended computation.
-- This is a bit of a hack, used to prevent "del" statements in TAC from crossing a scope boundary,
-- so that "@foo(1+1) class Bar: pass" transforms to "t=1+1; @foo(t) class Bar: pass; del t" instead
-- of "t = 1+1; @foo(t) class Bar: del t; pass" . My overall conclusion is that we need a much
-- smarter architecture for building program transformations, but, in this case, we could do this
-- better by integrating the boundary condition of the CFG inserter with some name machinery

instance {-# OVERLAPPING #-} ConstructCfg MPythonSig PythonCfgState PyClass where
  constructCfg (collapseFProd' -> t :*: PyClass _ args body) = HState $ do
    unHState body
    a <- collapseEnterExit =<< unHState args
    enterNode <- addCfgNode t EnterNode
    exitNode  <- addCfgNode t ExitNode

    p <- combineEnterExit (identEnterExit enterNode) a
    combineEnterExit p (identEnterExit exitNode)

instance {-# OVERLAPPING #-} ConstructCfg MPythonSig PythonCfgState PyWith where
  constructCfg (collapseFProd' -> t :*: PyWith hBinders hBody) = HState $ do
    enterNode <- addCfgNode t EnterNode

    binders <- extractEEPList <$> unHState hBinders
    midNodes <- for [0..(length binders - 1)] $ \i ->
                          addCfgNode t (evalPointToNodeType (BeforeIntermediateEvalPoint i))

    exitNode <- addCfgNode t ExitNode

    body <- collapseEnterExit =<< unHState hBody

    cur_cfg %= addEdge enterNode (head midNodes)
    cur_cfg %= addEdge (exit (last binders)) (enter body)
    cur_cfg %= addEdge (exit body) exitNode

    for (zip midNodes binders) $ \(n, b) -> cur_cfg %= addEdge n (enter b)
    for (zip binders (tail midNodes)) $ \(b, n) -> cur_cfg %= addEdge (exit b) n

    return $ EnterExitPair enterNode exitNode

instance {-# OVERLAPPING #-} ConstructCfg MPythonSig PythonCfgState Expr where
  constructCfg t'@(remA -> (BinaryOp (op :*: _) _ _ _)) = do
    let (t :*: (BinaryOp _ el er _)) = collapseFProd' t'
    case extractOp op of
      And {} -> HState $ constructCfgShortCircuitingBinOp t (unHState el) (unHState er)
      Or {}  -> HState $ constructCfgShortCircuitingBinOp t (unHState el) (unHState er)
      _   -> constructCfgDefault t'

    where extractOp :: MPythonTermLab OpL -> Op MPythonTerm OpL
          extractOp (stripA -> project -> Just bp) = bp

  constructCfg (collapseFProd' -> (t :*: (Lambda _ e _))) = HState $ do
    -- NOTE: similar to FunctionDef, ignoring default arguments here.
    unHState e
    enterNode <- addCfgNode t EnterNode
    exitNode  <- addCfgNode t ExitNode

    combineEnterExit (identEnterExit enterNode) (identEnterExit exitNode)

  constructCfg t = constructCfgDefault t

instance {-# OVERLAPPING #-} ConstructCfg MPythonSig PythonCfgState PyCondExpr where
  constructCfg (collapseFProd' -> (t :*: (PyCondExpr test succ fail))) = HState $ do
    constructCfgCondOp t (unHState test) (unHState succ) (unHState fail)

instance CfgInitState MPythonSig where
  cfgInitState _ = PythonCfgState emptyCfg (unsafeMkCSLabelGen ()) emptyLoopStack emptyLabelMap
#endif

