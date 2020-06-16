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

module Cubix.Language.C.Cfg.Test where
{-
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
import           Cubix.Language.C.Parametric.Common as CCommon
import qualified Cubix.Language.C.Parametric.Full as CFull
import           Cubix.Language.Parametric.Semantics.Cfg hiding ( enter, exit )
import           Cubix.ParsePretty
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, (:*:), HFix, (:->), NatM, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), inj', inject', EqHF, remA, (:-<:), proj, Cxt (..), inj, hfoldMap, K (..), cata, hfold )
import           Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )

import           Cubix.Language.Parametric.Cfg.Test
import qualified Cubix.Language.Parametric.Syntax as S
import           Cubix.Language.Parametric.InjF ( projF )

unit_c_cfg :: FilePath -> Property
unit_c_cfg fp =
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile fp
    (tLab, cfg) <- makeCEnv t
    runReaderT (assertCfgWellFormedness tLab) cfg
    pure ()

makeCEnv :: (MonadIO m) => MCTerm CTranslationUnitL -> m (MCTermLab CTranslationUnitL, Cfg MCSig)
makeCEnv t = do
  gen <- mkCSLabelGen
  let tLab = labelProg gen t
      cfg = makeCfg tLab
  pure (tLab, cfg)

instance {-# OVERLAPPABLE #-} AssertCfgWellFormed MCSig a where
  assertCfgWellFormed t = pure ()

instance AssertCfgWellFormed MCSig CExpression where
  assertCfgWellFormed t@(CCond test succ fail _ :&: _) = do
    assertCfgCondOp (inject' t) test (S.extractF succ) fail
  assertCfgWellFormed t@(CBinary op e1 e2 _ :&: _) = do
    case extractOp op of
      CLndOp -> assertCfgShortCircuit (inject' t) e1 e2
      CLndOp  -> assertCfgShortCircuit (inject' t) e1 e2
      _   -> pure ()

    where extractOp :: MCTermLab CBinaryOpL -> CBinaryOp MCTerm CBinaryOpL
          extractOp (stripA -> project -> Just bp) = bp

  assertCfgWellFormed t = assertCfgWellFormedDefault t

instance AssertCfgWellFormed MCSig CStatement where
  assertCfgWellFormed t@(remA -> CLabel _ stat _ _) =
    assertCfgLabel (inject' t) (extractBlock stat)
  assertCfgWellFormed t@(CIf cond thn mels _ :&: _) =
    case S.extractF mels of
      Just els -> assertCfgIfElse (inject' t) cond (extractBlock thn) (extractBlock els)
      Nothing  -> assertCfgIf (inject' t) cond (extractBlock thn)
  assertCfgWellFormed t@(remA -> CWhile e b False _) =
    assertCfgWhile (inject' t) e (extractBlock b)
  assertCfgWellFormed t@(remA -> CWhile e b True _) =
    assertCfgDoWhile (inject' t) (extractBlock b) e
  assertCfgWellFormed t@(remA -> CGoto n _) =    
    assertCfgGoto (inject' t) (nameString n)
  assertCfgWellFormed t@(remA -> CGotoPtr e _) =    
    assertCfgReturn (inject' t) (Just e)
  assertCfgWellFormed t@(remA -> CCont _) =
    assertCfgContinue (inject' t)
  assertCfgWellFormed t@(remA -> CBreak _) =
    assertCfgBreak (inject' t)
  assertCfgWellFormed t@(remA -> CReturn e _) =
    assertCfgReturn (inject' t) (S.extractF e)
  assertCfgWellFormed t@(remA -> CFor init cond step body _) =
    assertCfgFor (inject' t) init0 (S.extractF cond) (S.extractF step) (extractBlock body)
    where init0 = case S.extractF2 init of
            Left t0 -> S.extractF t0
            -- TODO: complete this
            Right _d -> Nothing
  assertCfgWellFormed t@(remA -> CSwitch exp body _) =
    undefined
  assertCfgWellFormed t = assertCfgWellFormedDefault t

assertCfgLabel ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs  
  ) => TermLab fs a -> E (TermLab fs) -> m ()
assertCfgLabel t (E body) = do
  (enLabel, exLabel) <- getEnterExitPair t
  (enBody, exBody) <- getEnterExitPair body
  assertEdges t [(enLabel, exLabel), (exLabel, enBody)]
                [enLabel, exLabel, enBody, exBody]

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
  mEEPInit <- traverse getEnterExitPair mInit
  mEEPCond <- traverse getEnterExitPair mCond
  mEEPStep <- traverse getEnterExitPair mStep

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
        let ns = catMaybes [mEEPCond, mEEPStep, mEEPInit] ++ [ eepFor, eepBody ]
        in loFor : map enter ns ++ map exit ns

  assertEdges t (nub edges) nodes

{-
-- assertCfgSwitch = error "TODO"
isLoopLikeNode :: TermLab MCSig l -> Bool
isLoopLikeNode (project' -> Just (While {} :&: _))= True
isLoopLikeNode (project' -> Just (Do {} :&: _)) = True
isLoopLikeNode (project' -> Just (EnhancedFor {} :&: _)) = True
isLoopLikeNode (project' -> Just (BasicFor {} :&: _)) = True
isLoopLikeNode _ = False
-}

assertCfgCondOp ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs e -> TermLab fs e -> Maybe (TermLab fs e) -> TermLab fs e -> m ()
assertCfgCondOp t test succ fail = do
  (enTop, exTop) <- getEnterExitPair t
  (enTest, exTest) <- getEnterExitPair test
  mEEPsucc <- getEnterExitPairMaybe succ
  (enFail, exFail) <- getEnterExitPair fail

  case mEEPsucc of
    Just (enSucc, exSucc) -> do
      assertEdges t [(enTop, enTest), (exTest, enSucc), (exTest, enFail), (exSucc, exTop), (exFail, exTop)]
                    [enTop, exTop, enTest, exTest, enSucc, exSucc, enFail, exFail]
    Nothing -> do
      assertEdges t [(enTop, enTest), (exTest, exTop), (exTest, enFail), (exFail, exTop)]
                    [enTop, exTop, enTest, exTest, enFail, exFail]

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

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `continue`.
--       * that outgoing edge is to a node which has an `LoopEntryNode` type.
--       * that outgoing edge is to a node which is loop-like (is one of `Do while/EnhancedFor/BasicFor/ While`)
--         (TODO: assert that it is the *nearest* such loop-like AST)
--       * there are no incoming nodes in exit node of `continue`.
assertCfgContinue ::
  ( MonadTest m
  , MonadReader (Cfg MCSig) m
  ) => TermLab MCSig a -> m ()
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
      checkNodeType :: CfgNode MCSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == LoopEntryNode                -- &&
        -- TODO:
        -- (\(E t) -> isLoopLikeNode t) (node ^. cfg_node_term)

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

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `break`.
--       * that outgoing edge is to a node which has an `ExitNode` type.
--       * that outgoing edge is to a node which is switch or loop-like (is one of `Do while/EnhancedFor/BasicFor/While`)
--         (TODO: assert that it is the *nearest* such loop-like AST)
--       * there are no incoming nodes in exit node of `break`.
assertCfgBreak ::
  ( MonadTest m
  , MonadReader (Cfg MCSig) m
  ) => TermLab MCSig a -> m ()
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
      checkNodeType :: CfgNode MCSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == ExitNode                    -- &&
        -- TODO: implement switch check
        -- (\(E t) -> isLoopLikeNode t {- || isSwitchNode t-}) (node ^. cfg_node_term)

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `goto`
--       * that outgoing edge is to a node which has is a `Label` with right label name
--       * there are no incoming nodes in exit node of `goto`.
assertCfgGoto ::
  forall a m.
  ( MonadTest m
  , MonadReader (Cfg MCSig) m
  ) => TermLab MCSig a -> String -> m ()
assertCfgGoto t labName = do
  (enGoto, exGoto) <- getEnterExitPair t
  let jmpNodeLabs = enGoto ^. cfg_node_succs
  assert (length jmpNodeLabs == 1)

  let enJmpLab = head (Set.toList jmpNodeLabs)
  menJump <- preview (cur_cfg.cfg_nodes.(ix enJmpLab))
  enJmp <- assertJust "Cfg label lookup: " menJump
  
  assert (checkLab (enJmp ^.  cfg_node_term))
  assert (length (exGoto ^. cfg_node_prevs) == 0)

  where
    checkLab ::  E (TermLab MCSig) -> Bool
    checkLab (E (project' -> Just (CLabel (nameString -> n) _ _ _ :&: _)))
          | labName == n = True
          | otherwise = False
    checkLab _ = False

assertCfgWellFormedDefault :: (MonadTest m, MonadReader (Cfg MCSig) m, f :-<: MCSig) => (f :&: Label) MCTermLab l -> m ()
assertCfgWellFormedDefault p = pure ()

extractBlock :: TermLab MCSig CStatementL -> E (TermLab MCSig)
extractBlock (project' -> Just (CLabeledBlock _ blk :&: _)) =
  case project' blk of
    Just (S.Block items _ :&: _) -> E items
extractBlock t = E t

nameString :: MCTermLab CFull.IdentL -> String
nameString (stripA -> projF -> Just (Ident' n)) = n
-}
