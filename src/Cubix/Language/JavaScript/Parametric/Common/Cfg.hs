{-# OPTIONS_HADDOCK hide #-}
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

module Cubix.Language.JavaScript.Parametric.Common.Cfg () where

#ifndef ONLY_ONE_LANGUAGE
import Control.Monad ( liftM, liftM2, forM_ )

import Control.Lens (  makeLenses, (%=), (^.), use )

import qualified Data.Map as Map

import Data.Comp.Multi ( remA, stripA )
import Data.Comp.Multi.Ops ( (:*:)(..), fsnd )

import Cubix.Language.Info

import Cubix.Language.JavaScript.Parametric.Common.Types as C
import Cubix.Language.JavaScript.Parametric.Full.Types as F
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Syntax as P

data JSCfgState = JSCfgState {
                   _jcs_cfg         :: Cfg MJSSig
                 , _jcs_labeler     :: LabelGen
                 , _jcs_stack       :: LoopStack
                 , _jcs_scoped_labs :: ScopedLabelMap
                 }

makeLenses ''JSCfgState

instance HasCurCfg JSCfgState MJSSig where cur_cfg = jcs_cfg
instance HasLabelGen JSCfgState where labelGen = jcs_labeler
instance HasLoopStack JSCfgState where loopStack = jcs_stack
instance HasScopedLabelMap JSCfgState where scopedLabelMap = jcs_scoped_labs

type instance ComputationSorts MJSSig = '[JSStatementL, JSExpressionL, [BlockItemL], [JSStatementL]]
type instance SuspendedComputationSorts MJSSig = '[FunctionDefL]
type instance ContainerFunctors MJSSig = '[ListF, MaybeF]
type instance CfgState MJSSig = JSCfgState

singleton :: a -> [a]
singleton = return


instance ConstructCfg MJSSig JSCfgState JSStatement where
  constructCfg p@(remA -> JSStatementBlock _ (body :*: _) _ _) =
       case extractF body of
           [] -> constructCfgGeneric p
           _  -> HState $ runSubCfgs $ fsnd $ collapseFProd' p



  constructCfg (t@(remA -> JSBreak _ ((stripA -> JSIdent' targ) :*: _) _))    = HState $
    constructCfgScopedLabeledBreak (fprodFst' t) targ
  constructCfg (t@(remA -> JSContinue _ ((stripA -> JSIdent' targ) :*: _) _)) = HState $
    constructCfgScopedLabeledContinue (fprodFst' t) targ

  constructCfg (collapseFProd' -> (t :*: (JSBreak _ _ _))) = HState $ constructCfgBreak t
  constructCfg (collapseFProd' -> (t :*: (JSContinue _ _ _))) = HState $ constructCfgContinue t

  constructCfg (collapseFProd' -> (t :*: (JSDoWhile _ body _ _ cond _ _))) = HState $ constructCfgDoWhile t (unHState cond) (unHState body)
  constructCfg (collapseFProd' -> (t :*: (JSIf _ _ cond _ thn))) = HState $ constructCfgIfElseIfElse t (liftM singleton $ liftM2 (,) (unHState cond) (unHState thn)) (return Nothing)
  constructCfg (collapseFProd' -> (t :*: (JSIfElse _ _ cond _ thn _ els))) = HState $ constructCfgIfElseIfElse t (liftM singleton $ liftM2 (,) (unHState cond) (unHState thn)) (liftM Just $ unHState els)

  constructCfg tp@(remA -> JSLabelled ((stripA -> JSIdent' nam) :*: _) _ (s :*: mStmt)) = HState $ do
    let t = fprodFst' tp

    enterNode     <- addCfgNode t EnterNode
    loopEntryNode <- addCfgNode t LoopEntryNode
    exitNode      <- addCfgNode t ExitNode

    -- Using a label as a continue target is not valid unless the labeled statement is a loop
    -- We assume that this code is valid JS, and hence loopEntryNode is unused unless
    -- stmt is a loop
    let labMap = Map.fromList [ (LoopEntryNode, loopEntryNode ^. cfg_node_lab)
                              , (ExitNode, exitNode ^. cfg_node_lab)
                              ]

    stmt <- withScopedLabel nam labMap $ unHState mStmt

    cur_cfg %= addEdge enterNode (enter stmt)
    cur_cfg %= addEdge (exit stmt) exitNode

    -- loopEntryNode is just a temporary; contract it out
    gr <- use cur_cfg
    case cfgNodeForTerm gr LoopEntryNode s of
      Nothing -> return ()
      Just n  -> cur_cfg %= addEdge loopEntryNode n

    cur_cfg %= contractNode (loopEntryNode ^. cfg_node_lab)

    return (EnterExitPair enterNode exitNode)


  constructCfg (collapseFProd' -> (t :*: JSReturn _ e _)) = HState $ constructCfgReturn t (extractEEPMaybe $ unHState e)

  -- Consciously skipping switch's

  constructCfg (collapseFProd' -> (t :*: JSThrow _ e _)) = HState $ constructCfgReturn t (liftM Just $ unHState e)
  -- Again, pretending try/catch blocks are independent computation units
  constructCfg (collapseFProd' -> (t :*: JSTry _ block catchs finally)) = HState $ do
    unHState block
    unHState catchs
    unHState finally
    constructCfgEmpty t

  constructCfg (collapseFProd' -> (t :*: (JSWhile _ _ e _ s))) = HState $ constructCfgWhile t (unHState e) (unHState s)


  -- FIXME: Slightly hackish so we can get tests passing. Doesn't handle pass-through properly
  constructCfg (collapseFProd' -> (t :*: (JSSwitch _ _ exp _ _ switchParts _ _))) = HState $ do
    enterNode <- addCfgNode t EnterNode
    exitNode  <- addCfgNode t ExitNode

    expEE <- unHState exp
    cur_cfg %= addEdge enterNode (enter expEE)

    pushBreakNode exitNode

    blocks <- extractEEPList <$> unHState switchParts

    forM_ blocks $ \b -> case b of
                          -- EmptyEnterExit -> cur_cfg %= addEdge (exit expEE) exitNode
                           EnterExitPair bEnt bEx -> do
                             cur_cfg %= addEdge (exit expEE) bEnt
                             cur_cfg %= addEdge bEx exitNode

    popBreakNode

    return $ EnterExitPair enterNode exitNode

  constructCfg t = constructCfgDefault t

instance ConstructCfg MJSSig JSCfgState FunctionDef where
  constructCfg (collapseFProd' -> (t :*: (FunctionDef _ _ _ body))) = HState $ (unHState body >> constructCfgEmpty t)

instance ConstructCfg MJSSig JSCfgState JSExpression where
  constructCfg (collapseFProd' -> (t :*: (JSFunctionExpression _ _ _ _ _ body))) = HState (unHState body >> constructCfgEmpty t)
  constructCfg t = constructCfgDefault t

instance ConstructCfg MJSSig JSCfgState C.JSFor where
  constructCfg (collapseFProd' -> (t :*: C.JSFor init cond step body)) = HState $ constructCfgFor t (liftM Just $ unHState init) (liftM Just $ unHState cond) (liftM Just $ unHState step) (unHState body)
  constructCfg (collapseFProd' -> (t :*: C.JSForIn _ _ exp body)) = HState $ constructCfgWhile t (unHState exp) (unHState body)
  constructCfg (collapseFProd' -> (t :*: C.JSForVar init cond step body)) = HState $ constructCfgFor t (liftM Just $ unHState init) (liftM Just $ unHState cond) (liftM Just $ unHState step) (unHState body)
  constructCfg (collapseFProd' -> (t :*: C.JSForVarIn _ _ exp body)) = HState $ constructCfgWhile t (unHState exp) (unHState body)

instance CfgInitState MJSSig where
  cfgInitState _ = JSCfgState emptyCfg (unsafeMkCSLabelGen ()) emptyLoopStack emptyScopedLabelMap
#endif
