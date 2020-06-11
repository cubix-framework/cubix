{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fcontext-stack=300     #-}

module Cubix.Language.JavaScript.Cfg.Test where

import           Control.Lens hiding ( para )
import           Control.Monad ( when )
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Proxy
import           Data.Typeable
import qualified Data.Set as Set
import           Hedgehog
import qualified Hedgehog.Internal.Property as H


import           Cubix.Language.Info
import           Cubix.Language.JavaScript.Parametric.Common as JSCommon
import qualified Cubix.Language.JavaScript.Parametric.Full as JSFull
import           Cubix.Language.Parametric.Semantics.Cfg hiding ( enter, exit )
import           Cubix.ParsePretty
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, (:*:), HFix, (:->), NatM, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), inj', inject', EqHF, remA, (:-<:), proj, Cxt (..), inj, hfoldMap, K (..), cata, hfold )
import           Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )

import           Cubix.Language.Parametric.Cfg.Test
import qualified Cubix.Language.Parametric.Syntax as S

unit_js_cfg :: FilePath -> Property
unit_js_cfg fp =
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile fp
    (tLab, cfg) <- makeJavascriptEnv t
    runReaderT (assertCfgWellFormedness tLab) cfg
    pure ()

makeJavascriptEnv :: (MonadIO m) => MJSTerm JSASTL -> m (MJSTermLab JSASTL, Cfg MJSSig)
makeJavascriptEnv t = do
  gen <- mkCSLabelGen
  let tLab = labelProg gen t
      cfg = makeCfg tLab
  pure (tLab, cfg)

instance {-# OVERLAPPABLE #-} AssertCfgWellFormed MJSSig a where
  assertCfgWellFormed t = pure ()

instance AssertCfgWellFormed MJSSig JSExpression where
  assertCfgWellFormed t@(JSExpressionTernary test _ succ _ fail :&: _) = do
    assertCfgCondOp (inject' t) test succ fail
  assertCfgWellFormed t@(JSExpressionBinary e1 op e2 :&: _) = do
    case extractOp op of
      JSBinOpAnd _ -> assertCfgShortCircuit (inject' t) e1 e2
      JSBinOpOr _  -> assertCfgShortCircuit (inject' t) e1 e2
      _   -> pure ()

    where extractOp :: MJSTermLab JSBinOpL -> JSBinOp MJSTerm JSBinOpL
          extractOp (stripA -> project -> Just bp) = bp

  assertCfgWellFormed t = assertCfgWellFormedDefault t

instance AssertCfgWellFormed MJSSig JSStatement where
  assertCfgWellFormed t@(JSDoWhile _ body _ _ cond _ _ :&: _) =
    assertCfgDoWhile (inject' t) (extractBlock body) cond
  assertCfgWellFormed t@(JSIf _ _ cond _ thn :&: _) =
    assertCfgIf (inject' t) cond (extractBlock thn)
  assertCfgWellFormed t@(JSIfElse _ _ cond _ thn _ els :&: _) =
    assertCfgIfElse (inject' t) cond (extractBlock thn) (extractBlock els)
  assertCfgWellFormed t@(JSReturn _ e _ :&: _) =
    assertCfgReturn (inject' t) (S.extractF e)
  assertCfgWellFormed t@(JSThrow _ e _ :&: _) =
    assertCfgReturn (inject' t) (Just e)
  assertCfgWellFormed t@(JSTry _ block catchs finally :&: _) =
    assertCfgTry (inject' t) (extractJSBlock block) (map extractJSCatch $ S.extractF catchs) (extractJSFinally finally)
  assertCfgWellFormed t@(JSWhile _ _ e _ b :&: _) =
    assertCfgWhile (inject' t) e (extractBlock b)
  assertCfgWellFormed t@(JSSwitch _ _ exp _ _ cases _ _ :&: _) =
    error "TODO"
  assertCfgWellFormed t@(JSBreak _ (stripA -> JSIdent' targ) _ :&: _) = error "TODO"
  assertCfgWellFormed t@(JSBreak _ _ _ :&: _) =
    assertCfgBreak (inject' t)
  assertCfgWellFormed t@(JSContinue _ (stripA -> JSIdent' targ) _ :&: _) = error "TODO"
  assertCfgWellFormed t@(JSContinue _ _ _ :&: _) =
    assertCfgContinue (inject' t)
  assertCfgWellFormed t = assertCfgWellFormedDefault t

instance AssertCfgWellFormed MJSSig JSCommon.JSFor where
  assertCfgWellFormed t@(JSCommon.JSFor init cond step body :&: _) =
    assertCfgFor (inject' t) (toMaybe $ S.extractF init) (toMaybe $ S.extractF cond) (toMaybe $ S.extractF step) (extractBlock body)
      where toMaybe [] = Nothing
            toMaybe [x] = Just x
            toMaybe xs = error $ "Panic: unexpected list: " ++ show xs

  assertCfgWellFormed t@(JSCommon.JSForVar init cond step body :&: _) = do
    error "TODO" -- assertCfgWhile (inject' t) e (extractBlock body)
  assertCfgWellFormed t@(JSCommon.JSForIn _ _ e body :&: _) = do
    assertCfgWhile (inject' t) e (extractBlock body)
  assertCfgWellFormed t@(JSCommon.JSForVarIn _ _ e body :&: _) = do
    assertCfgWhile (inject' t) e (extractBlock body)

assertCfgShortCircuit ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs l -> TermLab fs e1 -> TermLab fs e2 -> m ()
assertCfgShortCircuit t e1 e2 = do
  (enSExp, exSExp) <- getEnterExitPair t
  (enE1, exE1) <- getEnterExitPair e1
  (enE2, exE2) <- getEnterExitPair e2

  assertEdges t [ (enSExp, enE1)
                , (exE1, exSExp)
                , (exE1, enE2)
                , (exE2, exSExp)
                ]
                [ enSExp, exSExp, enE1
                , exE1, enE2, exE2
                ]

assertCfgCondOp ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs e -> TermLab fs e -> TermLab fs e -> TermLab fs e -> m ()
assertCfgCondOp t test succ fail = do
  (enTop, exTop) <- getEnterExitPair t
  (enTest, exTest) <- getEnterExitPair test
  (enSucc, exSucc) <- getEnterExitPair succ
  (enFail, exFail) <- getEnterExitPair fail

  assertEdges t [(enTop, enTest), (exTest, enSucc), (exTest, enFail), (exSucc, exTop), (exFail, exTop)]
                [enTop, exTop, enTest, exTest, enSucc, exSucc, enFail, exFail]

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `break`.
--       * that outgoing edge is to a node which has an `ExitNode` type.
--       * that outgoing edge is to a node which is switch or loop-like (is one of `Do while/For*/While`)
--         (TODO: assert that it is the *nearest* such loop-like AST)
--       * there are no incoming nodes in exit node of `break`.
assertCfgBreak ::
  ( MonadTest m
  , MonadReader (Cfg MJSSig) m
  ) => TermLab MJSSig a -> m ()
assertCfgBreak b = do
  (enBreak, exBreak) <- getEnterExitPair b
  let jmpNodeLabs = enBreak ^. cfg_node_succs
  assert (length jmpNodeLabs == 1)

  let enJmpLab = head (Set.toList jmpNodeLabs)
  menJump <- preview (cur_cfg.cfg_nodes.(ix enJmpLab))
  enJmp <- assertJust "Cfg label lookup: " menJump
  assert (checkNodeType enJmp)
  assert (length (exBreak ^. cfg_node_prevs) == 0)

    where
      checkNodeType :: CfgNode MJSSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == ExitNode                    -- &&
        -- TODO: implement switch check; loopLike check
        -- (\(E t) -> isLoopLikeNode t) (node ^. cfg_node_term)

assertCfgWellFormedDefault :: (MonadTest m, MonadReader (Cfg MJSSig) m, f :-<: MJSSig) => (f :&: Label) MJSTermLab l -> m ()
assertCfgWellFormedDefault p = pure ()

assertCfgDoWhile ::
  ( MonadTest m
  , MonadReader (Cfg gs) m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  ) => TermLab gs l -> E (TermLab gs) -> TermLab gs j -> m ()
assertCfgDoWhile t (E b) e = do
  (enDoWhile, exDoWhile) <- getEnterExitPair t
  loDoWhile <- getLoopEntry t

  (enBody, exBody) <- getEnterExitPair b
  (enExp, exExp) <- getEnterExitPair e

  assertEdges t [ (enDoWhile, enBody)
                , (exExp, enBody)
                , (exExp, exDoWhile)
                , (exBody, loDoWhile)
                , (loDoWhile, enExp)
                ]
                [ enDoWhile, exDoWhile, loDoWhile
                , enBody, exBody, enExp, exExp
                ]

assertCfgIfElse ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs a -> TermLab fs e -> E (TermLab fs) -> E (TermLab fs) -> m ()
assertCfgIfElse t cond (E thn) (E els) = do
  (enIf, exIf) <- getEnterExitPair t
  (enCond, exCond) <- getEnterExitPair cond
  (enThn, exThn) <- getEnterExitPair thn
  (enEls, exEls) <- getEnterExitPair els
  midNode <- getIEP 0 t
  assertEdges t [ (enIf, midNode), (midNode, enCond), (exCond, enThn), (exThn, exIf), (exCond, enEls), (exEls, exIf)]
                [enIf, exIf, midNode, enCond, exCond, enThn, exThn, enEls, exEls]

assertCfgIf ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs a -> TermLab fs e -> E (TermLab fs) -> m ()
assertCfgIf t cond (E thn) = do
  (enIf, exIf) <- getEnterExitPair t
  (enCond, exCond) <- getEnterExitPair cond
  (enThn, exThn) <- getEnterExitPair thn
  midNode <- getIEP 0 t
  assertEdges t [ (enIf, midNode), (midNode, enCond), (exCond, enThn), (exCond, exIf), (exThn, exIf) ]
                [enIf, exIf, midNode, enCond, exCond, enThn, exThn]

-- NOTE: Asserts that
--        * If there is no expression, then return's entry and exit node are *not* connected.
--        * If there is an expression, then return's entry is
--          connected to expression's entry, and expression's exit
--          is not connected to return's exit, and return's entry
--          is not connected to return's exit.
assertCfgReturn ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs l -> Maybe (TermLab fs e) -> m ()
assertCfgReturn t me = do
  (enReturn, exReturn) <- getEnterExitPair t
  assertNoEdge t enReturn exReturn
  case me of
    Nothing -> pure ()
    Just exp -> do
      (enExp, exExp) <- getEnterExitPair exp
      assertEdge t enReturn enExp
      assertNoEdge t exExp exReturn

assertCfgWhile ::
  forall fs a e b m.
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs a -> TermLab fs e -> E (TermLab fs) -> m ()
assertCfgWhile t e (E b) = do
  (enWhile, exWhile) <- getEnterExitPair t
  loWhile <- getLoopEntry t
  (enCond, exCond) <- getEnterExitPair e
  (enBody, exBody) <- getEnterExitPair b

  assertEdges t ([ (enWhile, loWhile)
                 , (loWhile, enCond)
                 , (exBody, loWhile)
                 , (exCond, enBody)
                 , (exCond, exWhile)
                 ])
                [ enWhile, exWhile, loWhile
                , enCond, exCond, enBody, exBody
                ]

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `continue`.
--       * that outgoing edge is to a node which has an `LoopEntryNode` type.
--       * that outgoing edge is to a node which is loop-like (is one of `Do while/For*/While`)
--         (TODO: assert that it is the *nearest* such loop-like AST)
--       * there are no incoming nodes in exit node of `continue`.
assertCfgContinue ::
  ( MonadTest m
  , MonadReader (Cfg MJSSig) m
  ) => TermLab MJSSig a -> m ()
assertCfgContinue c = do
  (enContinue, exContinue) <- getEnterExitPair c
  let jmpNodeLabs = enContinue ^. cfg_node_succs
  assert (length jmpNodeLabs == 1)

  let enJmpLab = head (Set.toList jmpNodeLabs)
  menJump <- preview (cur_cfg.cfg_nodes.(ix enJmpLab))
  enJmp <- assertJust "Cfg label lookup: " menJump
  assert (checkNodeType enJmp)
  assert (length (exContinue ^. cfg_node_prevs) == 0)

    where
      checkNodeType :: CfgNode MJSSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == LoopEntryNode                -- &&
        -- TODO: add looplike check
        -- (\(E t) -> isLoopLikeNode t) (node ^. cfg_node_term)

assertCfgFor ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs l -> Maybe (TermLab fs i) -> Maybe (TermLab fs e) -> Maybe (TermLab fs e) -> E (TermLab fs) -> m ()
assertCfgFor t mInit mCond mStep (E body) = do
  eepFor <- getEnterExitPair t
  loFor <- getLoopEntry t
  eepBody <- getEnterExitPair body
  mEEPInit <- getEnterExitPairMaybe mInit
  mEEPCond <- getEnterExitPairMaybe mCond
  mEEPStep <- getEnterExitPairMaybe mStep

  let edges =
        let (e0, vs0) = mJoinEdgesR [] (identEdge $ enter eepFor) mEEPInit
            (e1, vs1) = mJoinEdgesR vs0 e0 mEEPCond
            (_ , vs2) = joinEdges   vs1 e1 eepBody

            (e3, vs3) = joinEdges   vs2 eepBody (identEdge loFor)
            (e4, vs4) = mJoinEdgesR vs3 e3 mEEPStep
            (e5, vs5) = mJoinEdgesR vs4 e4 mEEPCond
            (_,  vs6) = joinEdges  vs5 e5 eepBody
            (_,  vs7) = mJoinEdgesL vs6 mEEPCond (identEdge $ exit eepFor)

        in  vs7
      nodes =
        let ns = catMaybes [mEEPInit, mEEPCond, mEEPStep]
        in loFor : map enter ns ++ map exit ns

  assertEdges t (nub edges) nodes

assertCfgTry ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs
  ) => TermLab fs l -> E (TermLab fs) -> [(E (TermLab fs), E (TermLab fs))] -> Maybe (E (TermLab fs)) -> m ()
assertCfgTry t (E blk) catchs finally = do
  assertCfgIsSuspended t blk
  mapM_ (uncurryE (assertCfgIsSuspended' t)) catchs
  mapM_ (\(E t0) -> (assertCfgIsSuspended t) t0) finally

  where
    uncurryE :: (forall l1 l2. TermLab fs l1 -> TermLab fs l2 -> m ()) -> (E (TermLab fs), E (TermLab fs)) -> m ()
    uncurryE f (E t1, E t2) = f t1 t2

assertCfgSwitch = undefined

extractBlock :: TermLab MJSSig JSStatementL -> E (TermLab MJSSig)
extractBlock t@(project' -> Just (JSStatementBlock _ stmts _ _ :&: _)) =
  case S.extractF stmts of
    [] -> E t
    _  -> E stmts
extractBlock t                                                       = E t

extractJSBlock :: TermLab MJSSig JSBlockL -> E (TermLab MJSSig)
extractJSBlock t@(project' -> Just (JSBlock _ stmts _ :&: _)) = E stmts

extractJSCatch :: TermLab MJSSig JSTryCatchL -> (E (TermLab MJSSig), E (TermLab MJSSig))
extractJSCatch t@(project' -> Just (JSCatch _ _ e _ b :&: _)) = (E e, extractJSBlock b)
extractJSCatch t@(project' -> Just (JSCatchIf _ _ e _ cond _ b :&: _)) = (E cond, extractJSBlock b)

extractJSFinally :: TermLab MJSSig JSTryFinallyL -> Maybe (E (TermLab MJSSig))
extractJSFinally t@(project' -> Just (JSFinally _ b :&: _)) = Just (extractJSBlock b)
extractJSFinally t@(project' -> Just (JSNoFinally :&: _)) = Nothing

