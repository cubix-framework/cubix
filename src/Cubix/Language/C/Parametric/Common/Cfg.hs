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

module Cubix.Language.C.Parametric.Common.Cfg () where

#ifndef ONLY_ONE_LANGUAGE
import Control.Monad ( liftM, liftM2 )
import Control.Monad.State ( State )

import Control.Lens ( makeLenses )

import Data.Comp.Multi ( stripA, remA, (:*:)(..), ffst )

import Cubix.Language.Info

import Cubix.Language.C.Parametric.Common.Types as C
import Cubix.Language.C.Parametric.Full.Types as F
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Syntax as P

data CCfgState = CCfgState {
                   _ccs_cfg       :: Cfg MCSig
                 , _ccs_labeler   :: LabelGen
                 , _ccs_stack     :: LoopStack
                 , _ccs_goto_labs :: LabelMap
                 }

makeLenses ''CCfgState

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

  constructCfg (collapseFProd' -> (t :*: (CFor init cond step body _))) = HState $ constructCfgFor t (extractForInit init) (extractEEPMaybe $ unHState cond) (extractEEPMaybe $ unHState step) (unHState body)

  -- TODO: Stopgap to get tests to pass while we're waiting for the revamped break/continue support
  -- Doesn't handle fall-through properly
  constructCfg (collapseFProd' -> (t :*: (CSwitch exp body _))) = HState $ do
    enterNode <- addCfgNode t EnterNode
    exitNode  <- addCfgNode t ExitNode

    expEE <- unHState exp

    pushBreakNode exitNode
    bodyEE <- unHState body
    popBreakNode

    r1 <- combineEnterExit (identEnterExit enterNode) expEE
    r2 <- combineEnterExit r1 bodyEE
    combineEnterExit r2 (identEnterExit exitNode)


  -- Making a conscious choice to skip switch's

  constructCfg t = constructCfgDefault t


-- CLabelBlock's getting nodes is messing everything up
instance ConstructCfg MCSig CCfgState CLabeledBlock where
  constructCfg (collapseFProd' -> (_ :*: subCfgs)) = HState $ runSubCfgs subCfgs

instance CfgInitState MCSig where
  cfgInitState _ = CCfgState emptyCfg (unsafeMkCSLabelGen ()) emptyLoopStack emptyLabelMap
#endif
