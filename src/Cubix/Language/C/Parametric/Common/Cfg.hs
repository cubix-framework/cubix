{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Cubix.Language.C.Parametric.Common.Cfg () where

#ifndef ONLY_ONE_LANGUAGE
import Control.Monad ( liftM, liftM2, forM_ )
import Control.Monad.State ( State, MonadState )

import Control.Lens ( makeLenses, (%=), (.=), use, uses )

import Data.List as List ( (\\) )
import Data.Map as Map ( Map, partitionWithKey )
import Data.Set as Set ( Set, member, empty, fromList )

import Data.Comp.Multi ( stripA, remA, (:*:)(..), ffst, fsnd, project, proj, E(..), (:&:)(..), subterms, (:-<:), Cxt (..) )

import Cubix.Language.Info

import Cubix.Language.C.Parametric.Common.Types as C
import Cubix.Language.C.Parametric.Full.Types as F hiding (CFor)
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Syntax as P

-------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------
--------------------- CFG-construction data structures ----------------------------
-----------------------------------------------------------------------------------

data CCfgState = CCfgState {
                   _ccs_cfg       :: Cfg MCSig
                 , _ccs_labeler   :: LabelGen
                 , _ccs_stack     :: LoopStack
                 , _ccs_goto_labs :: LabelMap
                 , _ccs_local_goto_labs :: LocalLabels
                 }

type LocalLabels = Set String

makeLenses ''CCfgState

------------------------------------------------------------
---------------------- Labelling mechanism -----------------
------------------------------------------------------------

-- With a GNU C extension it is possible to have nested function definitions.
-- 
-- From GCC docs:
--
--   GCC allows you to declare local labels in any nested block scope.
--   A local label is just like an ordinary label, but you can only reference it
--   (with a goto statement, or by taking its address) within the block in which it is declared.
--   Local label declarations also make the labels they declare visible to nested functions.

type LabelMap0 = Map.Map String (Label, [Label])

cLabeledBlockLabMap ::
  ( Monad m
  , MonadState CCfgState m
  ) => [String] -> m a -> m a
cLabeledBlockLabMap lls act = do
  let curLocalLabs = Set.fromList lls
  withExtendedLocalLabels curLocalLabs $ do
    -- NOTE: resets outer labels which shadows local labels in this block
    --       and after the work is done, restores it.
    (locLabMap, labMap) <- uses label_map (resetLabMap curLocalLabs)
    label_map .= labMap
    res <- act
    label_map %= restoreLocalLabMap curLocalLabs locLabMap
    return res

withExtendedLocalLabels :: (Monad m, MonadState CCfgState m) => LocalLabels -> m a -> m a
withExtendedLocalLabels lls act = do
  prevLocalLabs <- use ccs_local_goto_labs
  ccs_local_goto_labs .= prevLocalLabs <> lls
  res <- act
  ccs_local_goto_labs .= prevLocalLabs
  return res

-- NOTE: A Local label map is a label map which has
--       local labels as it's keys.

-- | Reset label map, returning (shadowed) local label map and rest.
resetLabMap :: LocalLabels -> LabelMap0 -> (LabelMap0, LabelMap0)
resetLabMap = splitLabMap

-- | Restores the (shadowed) local label map as it was previously.
restoreLocalLabMap :: LocalLabels -> LabelMap0 -> LabelMap0 -> LabelMap0
restoreLocalLabMap lls rlm lm = deleteLocalLabMap lls lm <> rlm

splitLabMap :: LocalLabels -> LabelMap0 -> (LabelMap0, LabelMap0)
splitLabMap lls lm = Map.partitionWithKey go lm
  where go k _ = k `Set.member` lls

getLocalLabMap :: LocalLabels -> LabelMap0 -> LabelMap0
getLocalLabMap lls lm = fst (splitLabMap lls lm)

deleteLocalLabMap :: LocalLabels -> LabelMap0 -> LabelMap0
deleteLocalLabMap lls lm = snd (splitLabMap lls lm)

functionDefLabelMap ::
  ( Monad m
  , MonadState CCfgState m
  ) => m a -> m a
functionDefLabelMap act = do
  oldLabMap <- use label_map
  localLabs <- use ccs_local_goto_labs
  -- NOTE: Let the (outer) local labels alone
  --       be seen inside the function.
  label_map %= getLocalLabMap localLabs
  res <- act
  -- NOTE: Propogate the local labels outwards
  --       while the restoring old map.
  label_map %= mappend oldLabMap . getLocalLabMap localLabs
  pure res

emptyLocalLabels :: LocalLabels
emptyLocalLabels = Set.empty

-----------------------------------------------------------------------------------
---------------           CfgConstruction Instances        ------------------------
-----------------------------------------------------------------------------------


instance HasCurCfg CCfgState MCSig where cur_cfg = ccs_cfg
instance HasLabelGen CCfgState where labelGen = ccs_labeler
instance HasLoopStack CCfgState where loopStack = ccs_stack
instance HasLabelMap CCfgState where labelMap = ccs_goto_labs


type instance ComputationSorts MCSig = '[CStatementL, CExpressionL, CCompoundBlockItemL, [BlockItemL]]
type instance SuspendedComputationSorts MCSig = '[FunctionDefL]
type instance ContainerFunctors MCSig = '[PairF, TripleF, ListF, MaybeF, EitherF]
type instance CfgState MCSig = CCfgState

nameString :: MCTermLab F.IdentL -> String
nameString (stripA -> projF -> Just (Ident' n)) = n

singleton :: a -> [a]
singleton = return

extractForInit :: (HasCurCfg s MCSig) => HState s (EnterExitPair MCSig) (Either (Maybe CExpressionL) CDeclarationL) -> State s (Maybe (EnterExitPair MCSig ()))
extractForInit m = do
  p1' <- unHState m
  let SubPairs p1 = p1'
  case kextractF2' p1 of
    Left x  -> mapM collapseEnterExit =<< (extractEEPMaybe $ return x)
    Right x -> Just <$> collapseEnterExit x


-- TODO: test this for Duff's device (once we have switches working)
instance ConstructCfg MCSig CCfgState CStatement where
  constructCfg t@(remA -> CLabel (nam :*: _) (_ :*: mStatEE) _ _) = HState $ do
    -- It's easiest to model it as if the label and the ensuing statement are separate
   labEE <- constructCfgLabel (ffst $ collapseFProd' t) (nameString nam)
   statEE <- unHState mStatEE
   combineEnterExit labEE statEE

  constructCfg (collapseFProd' -> (t :*: (CIf e thn optElse _))) = HState $ constructCfgIfElseIfElse t (liftM singleton $ liftM2 (,) (unHState e) (unHState thn)) (extractEEPMaybe $ unHState optElse)
  constructCfg (collapseFProd' -> (t :*: (CWhile e b False _))) = HState $ constructCfgWhile   t (unHState e) (unHState b)
  constructCfg (collapseFProd' -> (t :*: (CWhile e b True _)))  = HState $ constructCfgDoWhile t (unHState e) (unHState b)

  constructCfg t@(remA -> CGoto (nam :*: _) _) = HState $ constructCfgGoto (ffst $ collapseFProd' t) (nameString nam)
  constructCfg (collapseFProd' -> (t :*: (CGotoPtr e _))) = HState $ constructCfgReturn t (liftM Just $ unHState e)
  constructCfg (collapseFProd' -> (t :*: (CCont _))) = HState $ constructCfgContinue t
  constructCfg (collapseFProd' -> (t :*: (CBreak _))) = HState $ constructCfgBreak t
  constructCfg (collapseFProd' -> (t :*: (CReturn e _))) = HState $ constructCfgReturn t (extractEEPMaybe $ unHState e)

  constructCfg (collapseFProd' -> (t :*: (CSwitch exp body _))) = HState $ do
    enterNode <- addCfgNode t EnterNode
    exitNode  <- addCfgNode t ExitNode

    expEE <- unHState exp

    pushBreakNode exitNode
    bodyEE <- unHState body
    popBreakNode

    cur_cfg %= addEdge enterNode (enter expEE)
    cur_cfg %= addEdge (exit expEE) (enter bodyEE)
    cur_cfg %= addEdge (exit bodyEE) exitNode

    forM_ cases $ \(E case0) -> do
      ccfg <- use cur_cfg
      let Just enCase = cfgNodeForTerm ccfg EnterNode case0
      cur_cfg %= addEdge (exit expEE) enCase

    return $ EnterExitPair enterNode exitNode

      where cases = case project0 t of
              Just (remA -> CSwitch _ b0 _) -> extractCases b0

            extractCases t0 =
              let subs = subterms t0
                  cases0 = filter isCase subs
                  switches = filter isSwitch subs
                  subcases = filter isCase (concatMap (\(E e0) -> subterms e0) switches)
              in  cases0 List.\\ subcases

            isCase :: E MCTermLab -> Bool
            isCase (E (project0 -> Just (remA -> CCase {}))) = True
            isCase (E (project0 -> Just (remA -> CDefault {}))) = True
            isCase _ = False

            isSwitch :: E MCTermLab -> Bool
            isSwitch (E (project0 -> Just (remA -> CSwitch {}))) = True
            isSwitch _ = False

            project0 :: (f :-<: MCSig) => MCTermLab l -> Maybe ((f :&: Label) MCTermLab l)
            project0 (Term (s :&: l)) = fmap (:&: l) (proj s)

  constructCfg t = constructCfgDefault t

instance ConstructCfg MCSig CCfgState CFor where
  constructCfg (collapseFProd' -> (t :*: (CFor init cond step body))) = HState $ constructCfgFor t (Just <$> unHState init) (extractEEPMaybe $ unHState cond) (extractEEPMaybe $ unHState step) (unHState body)


instance ConstructCfg MCSig CCfgState CExpression where
  constructCfg t'@(remA -> (CBinary (op :*: _) _ _ _)) = do
    let (t :*: (CBinary _ el er _)) = collapseFProd' t'
    case extractOp op of
      CLndOp -> HState $ constructCfgShortCircuitingBinOp t (unHState el) (unHState er)
      CLorOp  -> HState $ constructCfgShortCircuitingBinOp t (unHState el) (unHState er)
      _   -> constructCfgDefault t'

    where extractOp :: MCTermLab CBinaryOpL -> CBinaryOp MCTerm CBinaryOpL
          extractOp (stripA -> project -> Just bp) = bp

  constructCfg t'@(remA -> CCond {}) = HState $ do
    let (t :*: (CCond test succ fail _)) = collapseFProd' t'
    constructCfgCCondOp t (unHState test) (extractEEPMaybe (unHState succ)) (unHState fail)

  constructCfg t = constructCfgDefault t

-- NOTE: because of gcc extension which allows things like x ? : y
constructCfgCCondOp ::
  ( MonadState s m
  , CfgComponent MCSig s
  ) => TermLab MCSig l -> m (EnterExitPair MCSig ls) -> m (Maybe (EnterExitPair MCSig rs)) -> m (EnterExitPair MCSig es) -> m (EnterExitPair MCSig es)
constructCfgCCondOp t mtest msucc mfail = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode

  test <- mtest
  fail <- mfail
  succ <- msucc

  case succ of
    Just succ0 -> do
      cur_cfg %= addEdge enterNode (enter test)
      cur_cfg %= addEdge (exit test) (enter succ0)
      cur_cfg %= addEdge (exit test) (enter fail)
      cur_cfg %= addEdge (exit succ0) exitNode
      cur_cfg %= addEdge (exit fail) exitNode
    Nothing -> do
      cur_cfg %= addEdge enterNode (enter test)
      cur_cfg %= addEdge (exit test) (enter fail)
      cur_cfg %= addEdge (exit test) exitNode
      cur_cfg %= addEdge (exit fail) exitNode

  return (EnterExitPair enterNode exitNode)

-- CLabelBlock's getting nodes is messing everything up
instance ConstructCfg MCSig CCfgState CLabeledBlock where
  constructCfg t@(remA -> (CLabeledBlock (idents :*: _) _)) = HState $ do
    cLabeledBlockLabMap labels $
      runSubCfgs (fsnd $ collapseFProd' t)

      where labels = map getIdent (extractF idents)
            getIdent (stripA -> projF -> Just (Ident' s)) = s

instance ConstructCfg MCSig CCfgState P.FunctionDef where
  constructCfg (collapseFProd' -> (_ :*: subCfgs)) = HState $ do
    functionDefLabelMap $ do
      runSubCfgs subCfgs
      pure EmptyEnterExit

instance CfgInitState MCSig where
  cfgInitState _ = CCfgState emptyCfg (unsafeMkConcurrentSupplyLabelGen ()) emptyLoopStack emptyLabelMap emptyLocalLabels
#endif
