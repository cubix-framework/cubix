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

module Cubix.Language.Lua.Parametric.Common.Cfg () where

import Control.Monad.State ( State )
import Data.Typeable ( Typeable )

import Control.Lens ( (%=), makeLenses )

import Data.Comp.Multi ( (:<:), project, project', stripA, remA )
import Data.Comp.Multi.Ops ( Sum, (:*:)(..), ffst )

import Cubix.Language.Info

import Cubix.Language.Lua.Parametric.Common.Types as C
import Cubix.Language.Lua.Parametric.Full.Types as F
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Syntax as P

data LuaCfgState = LuaCfgState {
                   _lcs_cfg       :: Cfg (Sum MLuaSig)
                 , _lcs_labeler   :: LabelGen
                 , _lcs_stack     :: LoopStack
                 , _lcs_goto_labs :: LabelMap
                 }

makeLenses ''LuaCfgState

instance HasCurCfg LuaCfgState (Sum MLuaSig) where cur_cfg = lcs_cfg
instance HasLabelGen LuaCfgState where labelGen = lcs_labeler
instance HasLoopStack LuaCfgState where loopStack = lcs_stack
instance HasLabelMap LuaCfgState where labelMap = lcs_goto_labs

type instance ComputationSorts (Sum MLuaSig) = '[StatL, ExpL, PrefixExpL, VarL, TableFieldL, FunCallL, [BlockItemL], AssignL]
type instance SuspendedComputationSorts (Sum MLuaSig) = '[P.FunctionDefL]
type instance ContainerFunctors (Sum MLuaSig) = '[PairF, ListF, MaybeF]
type instance CfgState (Sum MLuaSig) = LuaCfgState

nameString :: MLuaTermLab NameL -> String
nameString (stripA -> project -> Just (IdentIsName (Ident' s))) = s

extractClauses :: forall f s a b. (ListF :<: f, KExtractF2' (,) f, Typeable a, Typeable b)
                                                                   => HState s (EnterExitPair f) [(a, b)]
                                                                   -> State s [(EnterExitPair f a, EnterExitPair f b)]
extractClauses hs = do
    (extractEEPList -> cs) <- unHState hs
    return $ map extractClause cs
  where
    extractClause :: EnterExitPair f (a, b) -> (EnterExitPair f a, EnterExitPair f b)
    extractClause (SubPairs p) = kextractF2' p

-- Lua's for loop is weird
constructCfgLuaForRange :: MLuaTermLab h -> State LuaCfgState (EnterExitPair (Sum MLuaSig) i)
                                         -> State LuaCfgState (EnterExitPair (Sum MLuaSig) j)
                                         -> State LuaCfgState (EnterExitPair (Sum MLuaSig) k)
                                         -> State LuaCfgState (EnterExitPair (Sum MLuaSig) l)
                                         -> State LuaCfgState (EnterExitPair (Sum MLuaSig) m)
constructCfgLuaForRange t mInit mFinal mOptStep mBody = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode

  init <- mInit
  final <- mFinal
  step <- mOptStep

  pushLoopNode enterNode exitNode
  body <- mBody
  popLoopNode

  p  <- combineEnterExit init final
  p' <- combineEnterExit p step

  let setupExit = exit p' -- either the exit of final or step (b/c step is optional)


  cur_cfg %= addEdge enterNode (enter init)
  cur_cfg %= addEdge setupExit (enter body)
  cur_cfg %= addEdge setupExit exitNode

  cur_cfg %= addEdge (exit body) (enter body) -- there isn't really a separate node for the comparison

  return $ EnterExitPair enterNode exitNode


instance ConstructCfg (Sum MLuaSig) LuaCfgState Stat where
  constructCfg (collapseFProd' -> (t :*: Break))        = HState $ constructCfgBreak t
  constructCfg (collapseFProd' -> (t :*: (While e b)))  = HState $ constructCfgWhile t (unHState e) (unHState b)
  constructCfg (collapseFProd' -> (t :*: (Repeat b e))) = HState $ constructCfgDoWhile t (unHState e) (unHState b)

  -- We can get away with using the While cfg-generator for for-each's.
  constructCfg (collapseFProd' -> (t :*: (ForIn _ e b))) = HState $ constructCfgWhile t (unHState e) (unHState b)
  constructCfg            t@(remA -> Goto (nam :*: _))   = HState $ constructCfgGoto  (ffst $ collapseFProd' t) (nameString nam)
  constructCfg           t@(remA -> Label (nam :*: _))   = HState $ constructCfgLabel (ffst $ collapseFProd' t) (nameString nam)

  constructCfg (collapseFProd' -> (t :*: (ForRange _ init final optStep body))) = HState $ constructCfgLuaForRange  t (unHState init) (unHState final) (unHState optStep) (unHState body)
  constructCfg (collapseFProd' -> (t :*: (If clauses optElse)))                 = HState $ constructCfgIfElseIfElse t (extractClauses clauses) (extractEEPMaybe $ unHState optElse)
  constructCfg t = constructCfgDefault t

instance ConstructCfg (Sum MLuaSig) LuaCfgState P.Block where
  constructCfg p@(collapseFProd' -> (t :*: _)) = case project' t of
    Just (P.Block xs r) -> case (extractF xs, project' r) of
      ([], Just (LuaBlockEnd e)) -> constructCfgGeneric p -- FIXME: Doesn't properly handle returns, but I think the TACer won't notice
      _  -> constructCfgDefault p

instance CfgInitState (Sum MLuaSig) where
  cfgInitState _ = LuaCfgState emptyCfg (unsafeMkCSLabelGen ()) emptyLoopStack emptyLabelMap
