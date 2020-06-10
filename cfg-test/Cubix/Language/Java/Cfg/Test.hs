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

module Cubix.Language.Java.Cfg.Test where

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
import           Cubix.Language.Java.Parametric.Common as JCommon
import qualified Cubix.Language.Java.Parametric.Full as JFull
import           Cubix.Language.Parametric.Semantics.Cfg hiding ( enter, exit )
import           Cubix.ParsePretty
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, (:*:), HFix, (:->), NatM, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), inj', inject', EqHF, remA, (:-<:), proj, Cxt (..), inj, hfoldMap, K (..), cata, hfold )
import           Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )

import           Cubix.Language.Parametric.Cfg.Test
import qualified Cubix.Language.Parametric.Syntax as S

unit_java_cfg :: FilePath -> Property
unit_java_cfg fp =
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile fp
    (tLab, cfg) <- makeJavaEnv t
    runReaderT (assertCfgWellFormedness tLab) cfg
    pure ()

makeJavaEnv :: (MonadIO m) => MJavaTerm CompilationUnitL -> m (MJavaTermLab CompilationUnitL, Cfg MJavaSig)
makeJavaEnv t = do
  gen <- mkCSLabelGen
  let tLab = labelProg gen t
      cfg = makeCfg tLab
  pure (tLab, cfg)

instance {-# OVERLAPPABLE #-} AssertCfgWellFormed MJavaSig a where
  assertCfgWellFormed t = pure ()

instance AssertCfgWellFormed MJavaSig Exp where
  assertCfgWellFormed t@(Cond test succ fail :&: _) = do
    assertCfgCondOp (inject' t) test succ fail
  assertCfgWellFormed t@(BinOp e1 op e2 :&: _) = do
    case extractOp op of
      And -> assertCfgShortCircuit (inject' t) e1 e2
      Or  -> assertCfgShortCircuit (inject' t) e1 e2
      _   -> pure ()

    where extractOp :: MJavaTermLab OpL -> Op MJavaTerm OpL
          extractOp (stripA -> project -> Just bp) = bp

  assertCfgWellFormed t = assertCfgWellFormedDefault t

instance AssertCfgWellFormed MJavaSig Stmt where
  assertCfgWellFormed t@(StmtBlock {} :&: _) = pure ()
  assertCfgWellFormed t@(IfThen cond thn :&: _) =
    assertCfgIf (inject' t) cond (extractBlock thn)
  assertCfgWellFormed t@(IfThenElse cond thn els :&: _) =
    assertCfgIfElse (inject' t) cond (extractBlock thn) (extractBlock els)
  assertCfgWellFormed t@(While e b :&: _) =
    assertCfgWhile (inject' t) e (extractBlock b)
  assertCfgWellFormed t@(BasicFor init cond step b :&: _) =
    assertCfgFor (inject' t)
                 (extractList . extractForInits <$> extractMaybe init)
                 (extractMaybe cond)
                 (extractList <$> extractMaybe step)
                 (extractBlock b)
  assertCfgWellFormed t@(EnhancedFor _ _ _ e b :&: _) =
    assertCfgEnhancedFor (inject' t) e (extractBlock b)
  assertCfgWellFormed t@(Switch exp switchBlocks :&: _) =
    error "TODO"
  assertCfgWellFormed t@(Do b e :&: _) =
    assertCfgDoWhile (inject' t) (extractBlock b) e
  assertCfgWellFormed t@(Break (stripA -> Nothing') :&: _) =
    assertCfgBreak (inject' t) 
  assertCfgWellFormed t@(Continue (stripA -> Nothing') :&: _) =
    assertCfgContinue (inject' t)
  assertCfgWellFormed t@(Return e :&: _) =
    assertCfgReturn (inject' t) (extractMaybe e)
  assertCfgWellFormed t@(Throw e :&: _) =
    assertCfgReturn (inject' t) (Just e)
  assertCfgWellFormed t@(Try block catchs finally :&: _) =
    assertCfgTry (inject' t)
                 (extractBlockFromBlock block)
                 (extractBlockFromCatch <$> extractList catchs)
                 (extractBlockFromBlock <$> extractMaybe finally)
  assertCfgWellFormed t = assertCfgWellFormedDefault t

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
  , Exp :-<: fs
  , Literal :-<: fs  
  ) => TermLab fs a -> TermLab fs e -> E (TermLab fs) -> m ()
assertCfgWhile t e (E b) = do
  (enWhile, exWhile) <- getEnterExitPair t
  loWhile <- getLoopEntry t
  (enCond, exCond) <- getEnterExitPair e
  (enBody, exBody) <- getEnterExitPair b

  assertEdges t ([ (enWhile, loWhile)
                 , (loWhile, enCond)
                 , (exBody, loWhile)
                 ] ++
                 staticLoopCheck enCond exCond enBody exWhile
                )

                [ enWhile, exWhile, loWhile
                , enCond, exCond, enBody, exBody
                ]

    where staticLoopCheck enCond exCond enBody exWhile =
            case evaluateExp e of
              Just True -> [(exCond, enBody)]
              Just False -> [(exCond, exWhile)]
              Nothing -> [(exCond, enBody), (exCond, exWhile)] 

          evaluateExp :: (Exp :-<: fs, Literal :-<: fs) => TermLab fs e -> Maybe Bool
          evaluateExp (stripA -> project -> Just t@(Lit lit)) =
            case project lit of
              Just (Boolean True) -> Just True
              Just (Boolean False) -> Just False
              _ -> Nothing
          evaluateExp _ = Nothing

assertCfgEnhancedFor ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs  
  ) => TermLab fs a -> TermLab fs e -> E (TermLab fs) -> m ()
assertCfgEnhancedFor t e (E b) = do
  (enEnFor, exEnFor) <- getEnterExitPair t
  loEnFor <- getLoopEntry t
  (enExp, exExp) <- getEnterExitPair e
  (enBody, exBody) <- getEnterExitPair b

  assertEdges t ([ (enEnFor, loEnFor)
                 , (loEnFor, enExp)
                 , (exBody, loEnFor)
                 , (exExp, enBody)
                 , (exExp, exEnFor)
                 ]                 
                )
                [ enEnFor, exEnFor, loEnFor
                , enExp, exExp, enBody, exBody
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

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `break`.
--       * that outgoing edge is to a node which has an `ExitNode` type.
--       * that outgoing edge is to a node which is switch or loop-like (is one of `Do while/EnhancedFor/BasicFor/While`)
--         (TODO: assert that it is the *nearest* such loop-like AST)
--       * there are no incoming nodes in exit node of `break`.
assertCfgBreak ::
  ( MonadTest m
  , MonadReader (Cfg MJavaSig) m
  ) => TermLab MJavaSig a -> m ()
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
      checkNodeType :: CfgNode MJavaSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == ExitNode                    &&
        -- TODO: implement switch check
        (\(E t) -> isLoopLikeNode t {- || isSwitchNode t-}) (node ^. cfg_node_term)

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `continue`.
--       * that outgoing edge is to a node which has an `LoopEntryNode` type.
--       * that outgoing edge is to a node which is loop-like (is one of `Do while/EnhancedFor/BasicFor/ While`)
--         (TODO: assert that it is the *nearest* such loop-like AST)
--       * there are no incoming nodes in exit node of `continue`.
assertCfgContinue ::
  ( MonadTest m
  , MonadReader (Cfg MJavaSig) m
  ) => TermLab MJavaSig a -> m ()
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
      checkNodeType :: CfgNode MJavaSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == LoopEntryNode                &&
        (\(E t) -> isLoopLikeNode t) (node ^. cfg_node_term)

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

assertCfgFor ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs  
  ) => TermLab fs l -> Maybe [TermLab fs i] -> Maybe (TermLab fs e) -> Maybe [TermLab fs e] -> E (TermLab fs) -> m ()
assertCfgFor t mInit mCond mStep (E body) = do
  eepFor <- getEnterExitPair t
  loFor <- getLoopEntry t
  eepBody <- getEnterExitPair body
  mEEPInits <- maybe (pure Nothing) (fmap Just . mapM getEnterExitPair) mInit
  mEEPCond <- maybe (pure Nothing) (fmap Just . getEnterExitPair) mCond
  mEEPSteps <- maybe (pure Nothing) (fmap Just . mapM getEnterExitPair) mStep

  let edges =
        let (e0, vs0) = mJoinEdgesR [] (identEdge $ enter eepFor) minitOutEdge
            (e1, vs1) = mJoinEdgesR vs0 e0 mEEPCond
            (_ , vs2) = joinEdges   vs1 e1 eepBody

            (e3, vs3) = joinEdges   vs2 eepBody (identEdge loFor)
            (e4, vs4) = mJoinEdgesR vs3 e3 mstepOutEdge
            (e5, vs5) = mJoinEdgesR vs4 e4 mEEPCond
            (_,  vs6) = joinEdges  vs5 e5 eepBody
            
            (_,  vs7) = mJoinEdgesL vs6 mEEPCond (identEdge $ exit eepFor)

            (mstepOutEdge, stepInEdges) = go mEEPSteps
              where go Nothing = (Nothing, [])
                    go (Just []) = (Nothing, [])
                    go (Just xs) = (Just (enter (head xs), exit (last xs)), xs)

            (minitOutEdge, initInEdges) = go mEEPInits
              where go Nothing = (Nothing, [])
                    go (Just []) = (Nothing, [])
                    go (Just xs) = (Just (enter (head xs), exit (last xs)), xs)
  
        in  vs7 ++ initInEdges ++ stepInEdges
      nodes =
        let ns = catMaybes [mEEPCond] ++ [ eepFor, eepBody ] ++ maybe [] id mEEPInits ++ maybe [] id mEEPSteps
        in loFor : map enter ns ++ map exit ns

  assertEdges t (nub edges) nodes

-- NOTE: Asserts that
--        * try block, catch(s) block and finally blocks are not connected
assertCfgTry ::
  ( MonadTest m
  , MonadReader (Cfg MJavaSig) m
  ) => TermLab MJavaSig t -> E (TermLab MJavaSig) -> [E (TermLab MJavaSig)] -> Maybe (E (TermLab MJavaSig)) -> m ()
assertCfgTry t (E block) catchs finally = do
  (enTry, exTry) <- getEnterExitPair t
  (enTryBlk, exTryBlk) <- getEnterExitPair block
  catchEEPs <- mapM (\(E a) -> getEnterExitPair a) catchs
  mfinallyEEP <- mapM (\(E a) -> getEnterExitPair a) finally

  assertNoEdge t enTry enTryBlk
  
  case null catchEEPs of
    True -> do
      case mfinallyEEP of
        Nothing -> pure ()
        Just finallyEEP -> assertNoEdge t exTryBlk (fst $ finallyEEP)
    False -> do     
      assertNoEdge t exTryBlk (fst $ head catchEEPs)
      case mfinallyEEP of
        Nothing -> pure ()
        Just finallyEEP -> assertNoEdge t (snd $ last catchEEPs) (fst $ finallyEEP)
  
  pure ()

-- assertCfgSwitch = error "TODO"

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

isLoopLikeNode :: TermLab MJavaSig l -> Bool
isLoopLikeNode (project' -> Just (While {} :&: _))= True
isLoopLikeNode (project' -> Just (Do {} :&: _)) = True
isLoopLikeNode (project' -> Just (EnhancedFor {} :&: _)) = True
isLoopLikeNode (project' -> Just (BasicFor {} :&: _)) = True
isLoopLikeNode _ = False

assertCfgWellFormedDefault :: (MonadTest m, MonadReader (Cfg MJavaSig) m, f :-<: MJavaSig) => (f :&: Label) MJavaTermLab l -> m ()
assertCfgWellFormedDefault p = pure ()

extractMaybe :: TermLab MJavaSig (Maybe l) -> Maybe (TermLab MJavaSig l)
extractMaybe (project' -> Just (JustF t :&: _)) = Just t
extractMaybe (project' -> Just (NothingF :&: _)) = Nothing

extractList :: TermLab MJavaSig [l] -> [TermLab MJavaSig l]
extractList (project' -> Just (ConsF x xs :&: _)) = x : extractList xs
extractList (project' -> Just (NilF :&: _)) = []

extractBlock :: TermLab MJavaSig StmtL -> E (TermLab MJavaSig)
extractBlock (project' -> Just (StmtBlock blkIsBlk :&: _)) = extractBlockFromBlock blkIsBlk
extractBlock t = E t

extractBlockFromBlock :: TermLab MJavaSig JFull.BlockL -> E (TermLab MJavaSig)
extractBlockFromBlock (project' -> Just (BlockIsBlock blk :&: _)) = fromJust $ do
  (S.Block blkItems _ :&: _) <- project' blk
  pure (E blkItems)


-- TODO: handle LocalVars case
extractForInits :: TermLab MJavaSig ForInitL -> TermLab MJavaSig [ExpL]
extractForInits (project' -> Just (JFull.ForInitExps exps :&: _)) = exps
extractForInits (project' -> Just (JFull.ForLocalVars _ _ decls :&: _)) = undefined -- decls
                 
extractBlockFromCatch :: TermLab MJavaSig CatchL -> E (TermLab MJavaSig)
extractBlockFromCatch (project' -> Just (Catch _ blkIsBlk :&: _)) = fromJust $ do
  (BlockIsBlock blk :&: _) <- project' blkIsBlk
  (S.Block blkItems _ :&: _) <- project' blk
  pure (E blkItems)
