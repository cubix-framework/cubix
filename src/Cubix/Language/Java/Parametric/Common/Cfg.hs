{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Cubix.Language.Java.Parametric.Common.Cfg () where

#ifndef ONLY_ONE_LANGUAGE
import Control.Monad ( liftM, liftM2, forM_ )
import Control.Monad.State ( MonadState )

import Control.Lens ( makeLenses, (%=) )

import Data.Comp.Multi ( stripA, remA, (:*:)(..), ffst, proj, project', project )
import Data.Foldable

import Cubix.Language.Info

import Cubix.Language.Java.Parametric.Common.Types as J
import Cubix.Language.Java.Parametric.Full.Types as F hiding ( pattern Ident' )
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Syntax hiding (ExpL)

data JavaCfgState = JavaCfgState {
                   _jcs_cfg       :: Cfg MJavaSig
                 , _jcs_labeler   :: LabelGen
                 , _jcs_stack     :: LoopStack
                 , _jcs_scoped_labs :: ScopedLabelMap
                 }

makeLenses ''JavaCfgState

instance HasCurCfg JavaCfgState MJavaSig where cur_cfg = jcs_cfg
instance HasLabelGen JavaCfgState where labelGen = jcs_labeler
instance HasLoopStack JavaCfgState where loopStack = jcs_stack
instance HasScopedLabelMap JavaCfgState where scopedLabelMap = jcs_scoped_labs

type instance ComputationSorts MJavaSig = '[StmtL, ExpL, BlockStmtL, [BlockItemL]]

-- Putting catch's here is a hack
type instance SuspendedComputationSorts MJavaSig = '[MethodBodyL, ConstructorBodyL, LambdaExpressionL, CatchL, FunctionDefL]
type instance ContainerFunctors MJavaSig = '[PairF, ListF, MaybeF, SwitchBlock]
type instance CfgState MJavaSig = JavaCfgState

data Kleene = KTrue | KFalse | KMaybe

singleton :: a -> [a]
singleton = return

evaluateExp :: MJavaTermLab ExpL -> Kleene
evaluateExp (project' -> Just t@(Lit lit)) =
  case project' lit of
    Just (Boolean True) -> KTrue
    Just (Boolean False) -> KFalse
    _ -> KMaybe
evaluateExp _ = KMaybe

evaluateWhileExp :: MJavaTermLab l -> Kleene
evaluateWhileExp (project' -> Just t@(While exp body)) = evaluateExp exp

-- Java code won't compile if the compiler detects unreachable code. For
-- example, while (true); stmt; will not compile because stmt is unreachable,
-- and so we should construct our CFG graph such that we won't generate
-- unreachable code. Here we perform a very basic check whether the loop
-- condition is always true, always false, or could be either. If always true or
-- always false, certain CFG edges are omitted.
--
-- See JLS 7 section 14.21, "Unreachable Statements."
constructCfgWhileJava :: (HasLoopStack s, MonadState s m, CfgComponent MJavaSig s) => MJavaTermLab l -> m (EnterExitPair MJavaSig i) -> m (EnterExitPair MJavaSig j) -> m (EnterExitPair MJavaSig k)
constructCfgWhileJava t mExp mBody = do
  enterNode     <- addCfgNode t EnterNode
  loopEntryNode <- addCfgNode t LoopEntryNode
  exitNode      <- addCfgNode t ExitNode

  exp <- mExp >>= collapseEnterExit
  pushLoopNode loopEntryNode exitNode
  body <- mBody
  popLoopNode

  case evaluateWhileExp t of
    KTrue  -> cur_cfg %= addEdge (exit exp) (enter body)
    KFalse -> cur_cfg %= addEdge (exit exp) exitNode
    KMaybe -> do
      cur_cfg %= addEdge (exit exp) (enter body)
      cur_cfg %= addEdge (exit exp) exitNode

  cur_cfg %= addEdge enterNode loopEntryNode
  cur_cfg %= addEdge loopEntryNode (enter exp)
  cur_cfg %= addEdge (exit body) loopEntryNode

  return $ EnterExitPair enterNode exitNode

extractAndRunSwitchBlocks :: (MonadState s m, CfgComponent MJavaSig s) => m (EnterExitPair MJavaSig [SwitchBlockL]) -> m [EnterExitPair MJavaSig [BlockStmtL]]
extractAndRunSwitchBlocks switchBlocks = mapM collapseEnterExit =<< (map extractBlock <$> extractEEPList <$> switchBlocks)
  where
    extractBlock :: EnterExitPair MJavaSig SwitchBlockL -> EnterExitPair MJavaSig [BlockStmtL]
    extractBlock (SubPairs (proj -> Just (SwitchBlock _ body))) = body

nameString' :: MJavaTermLab F.IdentL -> String
nameString' = nameString . stripA

nameString :: MJavaTerm F.IdentL -> String
nameString (project -> Just (IdentIsIdent (Ident' s))) = s


instance ConstructCfg MJavaSig JavaCfgState Stmt where
  constructCfg (collapseFProd' -> (_ :*: subCfgs@(StmtBlock _))) = HState $ runSubCfgs subCfgs

  constructCfg (collapseFProd' -> (t :*: (IfThen cond thn))) = HState $ constructCfgIfElseIfElse t (liftM singleton $ liftM2 (,) (unHState cond) (unHState thn)) (return Nothing)
  constructCfg (collapseFProd' -> (t :*: (IfThenElse cond thn els))) = HState $ constructCfgIfElseIfElse t (liftM singleton $ liftM2 (,) (unHState cond) (unHState thn)) (liftM Just $ unHState els)
  constructCfg (collapseFProd' -> (t :*: (While e s))) = HState $ constructCfgWhileJava t (unHState e) (unHState s)
  constructCfg (collapseFProd' -> (t :*: (BasicFor init cond step body))) = HState $ constructCfgFor t (extractEEPMaybe $ unHState init) (extractEEPMaybe $ unHState cond) (extractEEPMaybe $ unHState step) (unHState body)
  constructCfg (collapseFProd' -> (t :*: (EnhancedFor _ _ _ e s))) = HState $ constructCfgWhile t (unHState e) (unHState s)

  constructCfg (collapseFProd' -> (t :*: (Switch exp switchBlocks))) = HState $ do
    enterNode <- addCfgNode t EnterNode
    exitNode  <- addCfgNode t ExitNode

    expEE <- unHState exp

    pushBreakNode exitNode
    blocks <- extractAndRunSwitchBlocks $ unHState switchBlocks
    popBreakNode

    cur_cfg %= addEdge enterNode (enter expEE)

    forM_ blocks $ \b -> case b of
                           EmptyEnterExit -> cur_cfg %= addEdge (exit expEE) exitNode
                           EnterExitPair bEnt bEx -> do
                             cur_cfg %= addEdge (exit expEE) bEnt

    -- NOTE: fallthrough
    blockEE <- foldlM combineEnterExit EmptyEnterExit blocks
    _ <- combineEnterExit blockEE (identEnterExit exitNode)

    return $ EnterExitPair enterNode exitNode


  constructCfg (collapseFProd' -> (t :*: (Do s e))) = HState $ constructCfgDoWhile t (unHState e) (unHState s)

  constructCfg t@(remA -> Break ((stripA -> Nothing') :*: _)) = HState $ constructCfgBreak (ffst $ collapseFProd' t)
  constructCfg t@(remA -> Continue ((stripA -> Nothing') :*: _)) = HState $ constructCfgContinue (ffst $ collapseFProd' t)
  constructCfg t@(remA -> Break ((stripA -> Just' targ) :*: _)) = HState $ constructCfgScopedLabeledBreak (fprodFst' t) (nameString targ)
  constructCfg t@(remA -> Continue ((stripA -> Just' targ) :*: _)) = HState $ constructCfgScopedLabeledContinue (fprodFst' t) (nameString targ)

  constructCfg (collapseFProd' -> (t :*: Return e)) = HState $ constructCfgReturn t (extractEEPMaybe $ unHState e)

  -- I think it will work to just pretend try-catch-finally blocks are separate computation units, and
  -- throw's go nowhere
  constructCfg (collapseFProd' -> (t :*: Throw e)) = HState $ constructCfgReturn t (liftM Just $ unHState e)
  constructCfg (collapseFProd' -> (t :*: Try block catchs finally)) = HState $ do
    unHState block
    unHState catchs
    unHState finally
    constructCfgEmpty t

  constructCfg tp@(remA -> Labeled (name :*: _) (s :*: mStmt)) = HState $  constructCfgScopedLabel (fprodFst' tp) (nameString' name) s (unHState mStmt)

  constructCfg t = constructCfgDefault t

instance ConstructCfg MJavaSig JavaCfgState Exp where
  constructCfg t'@(remA -> (BinOp _ (op :*: _) _)) = do
    let (t :*: (BinOp el _ er)) = collapseFProd' t'
    case extractOp op of
      CAnd -> HState $ constructCfgShortCircuitingBinOp t (unHState el) (unHState er)
      COr  -> HState $ constructCfgShortCircuitingBinOp t (unHState el) (unHState er)
      _   -> constructCfgDefault t'

    where extractOp :: MJavaTermLab OpL -> Op MJavaTerm OpL
          extractOp (stripA -> project -> Just bp) = bp

  constructCfg t'@(remA -> Cond {}) = HState $ do
    let (t :*: (Cond test succ fail)) = collapseFProd' t'
    constructCfgCondOp t (unHState test) (unHState succ) (unHState fail)

  constructCfg t = constructCfgDefault t

instance CfgInitState MJavaSig where
  cfgInitState _ = JavaCfgState emptyCfg (unsafeMkConcurrentSupplyLabelGen ()) emptyLoopStack emptyScopedLabelMap
#endif
