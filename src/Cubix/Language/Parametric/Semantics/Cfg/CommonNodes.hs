{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Cubix.Language.Parametric.Semantics.Cfg.CommonNodes (
    constructCfgReturn
  , constructCfgEmpty
  , constructCfgIfElseIfElse

  , LoopStack
  , emptyLoopStack
  , HasLoopStack(..)
  , pushContinueNode
  , popContinueNode
  , pushBreakNode
  , popBreakNode
  , pushLoopNode
  , popLoopNode
  , constructCfgWhile
  , constructCfgDoWhile
  , constructCfgFor
  , constructCfgBreak
  , constructCfgContinue

  , LabelMap
  , emptyLabelMap
  , HasLabelMap(..)
  , constructCfgGoto
  , constructCfgLabel

  , ScopedLabelMap
  , emptyScopedLabelMap
  , HasScopedLabelMap(..)
  , withScopedLabel
  , edgeToScopedLabel
  , constructCfgScopedLabeledBreak
  , constructCfgScopedLabeledContinue
  , constructCfgScopedLabel

  , constructCfgShortCircuitingBinOp
  , constructCfgCondOp

) where

import Control.Monad ( liftM, when )
import Control.Monad.State ( MonadState )

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Proxy ( Proxy(..) )
import Data.Traversable ( for )

import Control.Lens ( (^.), (%~), (%=), (.=), (.~), _2, at, use, makeClassy )

import Data.Comp.Multi ( TreeLike )

import Cubix.Language.Info
import Cubix.Language.Parametric.Semantics.Cfg.CfgConstruction
import Cubix.Language.Parametric.Semantics.Cfg.Graph
import Cubix.Language.Parametric.Semantics.SemanticProperties

--------------------------------------------------------------------------------------

eeNonEmpty :: EnterExitPair fs i -> Bool
eeNonEmpty (EnterExitPair _ _) = True
eeNonEmpty EmptyEnterExit      = False
eeNonEmpty _                   = error "Passed non-collapsed EnterExitPair to eeNonEmpty"

collapseMaybeEnterExit :: Maybe (EnterExitPair fs i) -> EnterExitPair fs i
collapseMaybeEnterExit (Just eep) = eep
collapseMaybeEnterExit Nothing    = EmptyEnterExit

mCombineEnterExit ::
  ( HasCurCfg s fs
  , MonadState s m
  , TreeLike fs
  ) => m (EnterExitPair fs i) -> EnterExitPair fs j -> m (EnterExitPair fs k)
mCombineEnterExit p1 p2 = p1 >>= (\r -> combineEnterExit r p2)

--------------------------------------------------------------------------------------

-- Because we don't actually have function start and end nodes,
-- for now we can model "return" as just a black hole with no outgoing edges
-- This can also model the computed goto in GCC
constructCfgReturn ::
  ( MonadState s m
  , CfgComponent gs s
  ) => TermLab gs l -> m (Maybe (EnterExitPair gs i)) -> m (EnterExitPair gs l)
constructCfgReturn t exp = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode
  e <- liftM collapseMaybeEnterExit exp
  combineEnterExit (identEnterExit enterNode) e
  return $ EnterExitPair enterNode exitNode

constructCfgEmpty :: (MonadState s m, CfgComponent gs s) => TermLab gs l -> m (EnterExitPair gs l)
constructCfgEmpty t = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode
  cur_cfg %= addEdge enterNode exitNode
  return $ EnterExitPair enterNode exitNode


constructCfgIfElseIfElse ::
  ( MonadState s m
  , CfgComponent gs s
  ) => TermLab gs l -> m [(EnterExitPair gs i, EnterExitPair gs j)] -> m (Maybe (EnterExitPair gs k)) -> m (EnterExitPair gs l)
constructCfgIfElseIfElse t clauses optElse = do
  enterNode <- addCfgNode t EnterNode
  evalledClauses <- clauses

  midNodes <- for [0..(length evalledClauses - 1)] $ \i ->
                        addCfgNode t (evalPointToNodeType (BeforeIntermediateEvalPoint i))

  exitNode  <- addCfgNode t ExitNode

  for evalledClauses $ \(c, b) -> do
    cur_cfg %= addEdge (exit c) (enter b)
    cur_cfg %= addEdge (exit b) exitNode

  let condNodes = zip midNodes (map fst evalledClauses)
  let condPairs = zip condNodes (tail condNodes)

  for condNodes $ \(n, x) -> cur_cfg %= addEdge n (enter x)

  for condPairs $ \((xn, x), (yn, y)) -> do
    cur_cfg %= addEdge (exit x) yn

  cur_cfg %= addEdge enterNode (head midNodes)

  evalledOptElse <- optElse
  let lastCondExit = exit $ fst $ last evalledClauses

  case evalledOptElse of
    Nothing             -> cur_cfg %= addEdge lastCondExit exitNode
    Just c              -> collapseEnterExit c >>= \c' -> case c' of
            EmptyEnterExit       -> cur_cfg %= addEdge lastCondExit exitNode
            EnterExitPair ent ex -> do
               cur_cfg %= addEdge lastCondExit ent
               cur_cfg %= addEdge ex exitNode

  return $ EnterExitPair enterNode exitNode


data LoopStack = LoopStack {
         _break_stack :: [Label]
       , _continue_stack :: [Label]
       }
  deriving ( Eq, Ord, Show )

emptyLoopStack :: LoopStack
emptyLoopStack = LoopStack [] []

makeClassy ''LoopStack

pushBreakNode :: (MonadState s m, HasLoopStack s) => CfgNode fs -> m ()
pushBreakNode n = break_stack %= ((n ^. cfg_node_lab):)

popBreakNode :: (MonadState s m, HasLoopStack s) => m ()
popBreakNode = break_stack %= tail

pushContinueNode :: (MonadState s m, HasLoopStack s) => CfgNode fs -> m ()
pushContinueNode n = continue_stack %= ((n ^. cfg_node_lab):)

popContinueNode :: (MonadState s m, HasLoopStack s) => m ()
popContinueNode = continue_stack %= tail

pushLoopNode :: (MonadState s m, HasLoopStack s) => CfgNode fs -> CfgNode fs -> m ()
pushLoopNode n1 n2 = pushContinueNode n1 >> pushBreakNode n2

popLoopNode :: (MonadState s m, HasLoopStack s) => m ()
popLoopNode = popContinueNode >> popBreakNode

constructCfgWhile ::
  ( HasLoopStack s
  , MonadState s m
  , CfgComponent gs s
  ) => TermLab gs l -> m (EnterExitPair gs i) -> m (EnterExitPair gs j) -> m (EnterExitPair gs k)
constructCfgWhile t mExp mBody = do
  enterNode     <- addCfgNode t EnterNode
  loopEntryNode <- addCfgNode t LoopEntryNode
  exitNode      <- addCfgNode t ExitNode

  exp <- mExp >>= collapseEnterExit
  pushLoopNode loopEntryNode exitNode
  body <- mBody
  popLoopNode

  cur_cfg %= addEdge enterNode loopEntryNode
  cur_cfg %= addEdge loopEntryNode (enter exp)
  cur_cfg %= addEdge (exit exp) (enter body)
  cur_cfg %= addEdge (exit exp) exitNode
  cur_cfg %= addEdge (exit body) loopEntryNode

  return $ EnterExitPair enterNode exitNode

constructCfgDoWhile ::
  ( HasLoopStack s
  , MonadState s m
  , CfgComponent gs s
  ) => TermLab gs l -> m (EnterExitPair gs i) -> m (EnterExitPair gs j) -> m (EnterExitPair gs k)
constructCfgDoWhile t mExp mBody = do
  enterNode     <- addCfgNode t EnterNode
  loopEntryNode <- addCfgNode t LoopEntryNode
  exitNode      <- addCfgNode t ExitNode

  exp <- mExp >>= collapseEnterExit
  pushLoopNode loopEntryNode exitNode
  body <- mBody
  popLoopNode

  cur_cfg %= addEdge enterNode     (enter body)
  cur_cfg %= addEdge (exit exp)    (enter body)
  cur_cfg %= addEdge (exit exp)    exitNode
  cur_cfg %= addEdge (exit body)   loopEntryNode
  cur_cfg %= addEdge loopEntryNode (enter exp)

  return $ EnterExitPair enterNode exitNode

constructCfgFor ::
  ( HasLoopStack s
  , MonadState s m
  , CfgComponent gs s
  ) => TermLab gs l
  -> m (Maybe (EnterExitPair gs h))
  -> m (Maybe (EnterExitPair gs i))
  -> m (Maybe (EnterExitPair gs j))
  -> m (EnterExitPair gs k)
  -> m (EnterExitPair gs l)
constructCfgFor t mInit mCond mStep mBody = do
  enterNode     <- addCfgNode t EnterNode
  loopEntryNode <- addCfgNode t LoopEntryNode
  exitNode      <- addCfgNode t ExitNode

  init <- collapseEnterExit =<< liftM collapseMaybeEnterExit mInit
  cond <- collapseEnterExit =<< liftM collapseMaybeEnterExit mCond
  step <- collapseEnterExit =<< liftM collapseMaybeEnterExit mStep

  initCond <- combineEnterExit init cond

  pushLoopNode loopEntryNode exitNode
  body <- mBody
  popLoopNode

  initCondBody <- combineEnterExit initCond body

  cur_cfg %= addEdge enterNode (enter initCondBody)

  (((body `combineEnterExit`  (identEnterExit loopEntryNode))
          `mCombineEnterExit` step)
          `mCombineEnterExit` cond)
          `mCombineEnterExit` body

  when (eeNonEmpty cond) $ cur_cfg %= addEdge (exit cond) exitNode

  return $ EnterExitPair enterNode exitNode



constructCfgBreak :: (HasLoopStack s, MonadState s m, CfgComponent gs s) => TermLab gs l -> m (EnterExitPair gs i)
constructCfgBreak t = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode

  l' <- use break_stack
  let (l:_) = l'
  n' <- nodeForLab l
  let Just n = n'
  cur_cfg %= addEdge enterNode n -- go to end of loop
  -- do not connect enter to exit

  return $ EnterExitPair enterNode exitNode

constructCfgContinue :: (HasLoopStack s, MonadState s m, CfgComponent gs s) => TermLab gs l -> m (EnterExitPair gs i)
constructCfgContinue t = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode
  l' <- use continue_stack
  let (l:_) = l'
  n' <- nodeForLab l
  let Just n = n'
  cur_cfg %= addEdge enterNode n -- go to beginning of loop
  -- do not connect enter to exit

  return $ EnterExitPair enterNode exitNode

-- For goto label nodes, we create a label for the node the first time it's referenced,
-- and accumulate a list of nodes that want to connect to it. When it's added for real, we clear this
--
-- "Label" can refer to both the annotation on nodes, and the program construct used as a goto target.
-- This is confusing.
data LabelMap = LabelMap {
                           _label_map :: Map String (Label, [Label])
                         }
  deriving ( Eq, Ord, Show )

emptyLabelMap :: LabelMap
emptyLabelMap = LabelMap Map.empty

makeClassy ''LabelMap

speculativeGetLabel :: (MonadState s m, HasLabelMap s, HasLabelGen s) => String -> m Label
speculativeGetLabel s = do
  lm <- use label_map
  case Map.lookup s lm of
    Just (lab, _) -> return lab
    Nothing -> do
      lab <- nextLabel
      label_map %= Map.insert s (lab, [])
      return lab

addGotoEdge :: (MonadState s m, HasLabelMap s, CfgComponent gs s) => CfgNode gs -> String -> m ()
addGotoEdge n targName = do
  targL <- speculativeGetLabel targName
  targNode <- nodeForLab targL
  case targNode of
    Just n' -> cur_cfg %= addEdge n n'
    Nothing -> label_map . at targName %= fmap (_2 %~ ((n ^. cfg_node_lab):))

constructCfgGoto :: (MonadState s m, HasLabelMap s, CfgComponent gs s) => TermLab gs l -> String -> m (EnterExitPair gs i)
constructCfgGoto t targ = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode

  addGotoEdge enterNode targ
  -- do not connect enter to exit

  return $ EnterExitPair enterNode exitNode

constructCfgLabel :: forall gs s m l i. (MonadState s m, HasLabelMap s, CfgComponent gs s) => TermLab gs l -> String -> m (EnterExitPair gs i)
constructCfgLabel t name = do
  lm <- use label_map

  enterNode <- case Map.lookup name lm of
    Nothing -> do
      l <- nextLabel
      n <- addCfgNodeWithLabel t l EnterNode
      label_map %= Map.insert name (l, [])
      pure n

    Just (l, prevs) -> do
      n <- addCfgNodeWithLabel t l EnterNode
      for prevs $ \p -> cur_cfg %= addEdgeLab (Proxy :: Proxy gs) p l
      label_map . at name %= fmap (_2 .~ [])
      return n


  exitNode <- addCfgNode t ExitNode

  cur_cfg %= addEdge enterNode exitNode
  return $ EnterExitPair enterNode exitNode

-- |
-- Use this if labels are lexically scoped and may not be shadowed
-- In accordance with the representable/valid principle, we are not reusing LabelMap
-- for this purpose. LabelMap is for C's goto labels; ScopedLabelMap is for Java/JS labeled break/continue
data ScopedLabelMap = ScopedLabelMap {
                           _scoped_label_map :: Map String (Map CfgNodeType Label)
                         }
  deriving ( Eq, Ord, Show )

emptyScopedLabelMap :: ScopedLabelMap
emptyScopedLabelMap = ScopedLabelMap Map.empty

makeClassy ''ScopedLabelMap

withScopedLabel :: (MonadState s m, HasScopedLabelMap s) => String -> Map CfgNodeType Label -> m a -> m a
withScopedLabel s labMap m = do
  oldLabMap <- use scoped_label_map
  scoped_label_map %= Map.insert s labMap
  res <- m
  scoped_label_map .= oldLabMap
  return res


nodeForScopedLabel :: (MonadState s m, HasScopedLabelMap s, CfgComponent fs s) => String -> CfgNodeType -> m (Maybe (CfgNode fs))
nodeForScopedLabel nm tp = do
  slm <- use scoped_label_map
  gr <- use cur_cfg
  return (Map.lookup nm slm >>= Map.lookup tp >>= safeLookupCfg gr)

edgeToScopedLabel :: (MonadState s m, HasScopedLabelMap s, CfgComponent fs s) => CfgNode fs -> String -> CfgNodeType -> m ()
edgeToScopedLabel n targName targTp = do
  targNode <- nodeForScopedLabel targName targTp
  case targNode of
    Nothing -> error $ "Label " ++ show targName ++ " has no node of type " ++ show targTp
    Just n' -> cur_cfg %= addEdge n n'



constructCfgScopedLabeledBreak :: (HasScopedLabelMap s, MonadState s m, CfgComponent gs s) => TermLab gs l -> String -> m (EnterExitPair gs i)
constructCfgScopedLabeledBreak t labStr = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode

  edgeToScopedLabel enterNode labStr ExitNode
  -- do not connect enter to exit

  return $ EnterExitPair enterNode exitNode


constructCfgScopedLabeledContinue :: (HasScopedLabelMap s, MonadState s m, CfgComponent gs s) => TermLab gs l -> String -> m (EnterExitPair gs i)
constructCfgScopedLabeledContinue t labStr = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode

  edgeToScopedLabel enterNode labStr LoopEntryNode
  -- do not connect enter to exit

  return $ EnterExitPair enterNode exitNode

constructCfgScopedLabel ::
  ( HasScopedLabelMap s
  , MonadState s m
  , CfgComponent fs s
  ) => TermLab fs l -> String -> TermLab fs s0 -> m (EnterExitPair fs s0) -> m (EnterExitPair fs i)
constructCfgScopedLabel t labName s mStmt = do
  enterNode     <- addCfgNode t EnterNode
  loopEntryNode <- addCfgNode t LoopEntryNode
  exitNode      <- addCfgNode t ExitNode

  -- Using a label as a continue target is not valid unless the labeled statement is a loop
  -- We assume that this code is valid JS / Java, and hence loopEntryNode is unused unless
  -- stmt is a loop
  let labMap = Map.fromList [ (LoopEntryNode, loopEntryNode ^. cfg_node_lab)
                            , (ExitNode, exitNode ^. cfg_node_lab)
                            ]

  stmt <- withScopedLabel labName labMap mStmt
  cur_cfg %= addEdge enterNode (enter stmt)
  cur_cfg %= addEdge (exit stmt) exitNode

  -- loopEntryNode is just a temporary; contract it out
  gr <- use cur_cfg
  case cfgNodeForTerm gr LoopEntryNode s of
    Nothing -> return ()
    Just n  -> cur_cfg %= addEdge loopEntryNode n

  cur_cfg %= contractNode (loopEntryNode ^. cfg_node_lab)

  return (EnterExitPair enterNode exitNode)

constructCfgShortCircuitingBinOp ::
  ( MonadState s m
  , CfgComponent fs s
  ) => TermLab fs l -> m (EnterExitPair fs ls) -> m (EnterExitPair fs rs) -> m (EnterExitPair fs es)
constructCfgShortCircuitingBinOp t mlArg mrArg = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode
  lArg <- mlArg
  rArg <- mrArg
  cur_cfg %= addEdge enterNode (enter lArg)
  cur_cfg %= addEdge (exit lArg) (enter rArg)
  -- NOTE: short circuit edge.
  cur_cfg %= addEdge (exit lArg) exitNode
  cur_cfg %= addEdge (exit rArg) exitNode
  return (EnterExitPair enterNode exitNode)

constructCfgCondOp ::
  ( MonadState s m
  , CfgComponent fs s
  ) => TermLab fs l -> m (EnterExitPair fs ls) -> m (EnterExitPair fs rs) -> m (EnterExitPair fs es) -> m (EnterExitPair fs es)
constructCfgCondOp t mtest msucc mfail = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode
  test <- mtest
  succ <- msucc
  fail <- mfail
  cur_cfg %= addEdge enterNode (enter test)
  cur_cfg %= addEdge (exit test) (enter succ)
  cur_cfg %= addEdge (exit test) (enter fail)
  cur_cfg %= addEdge (exit succ) exitNode
  cur_cfg %= addEdge (exit fail) exitNode
  return (EnterExitPair enterNode exitNode)
