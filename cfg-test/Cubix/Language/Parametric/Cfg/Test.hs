{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Cubix.Language.Parametric.Cfg.Test where

import           Control.Lens hiding ( para, children )
import           Data.Maybe ( catMaybes )
import           Control.Monad ( forM_, when )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Reader (MonadReader (..), ReaderT )
import           Data.Foldable ( traverse_, foldlM )
import qualified Data.Map as Map
import           Data.Proxy
import qualified Data.Set as Set
import           Hedgehog
import qualified Hedgehog.Internal.Property as H

import           Cubix.Language.Info
import           Cubix.Language.Parametric.Semantics.Cfg hiding ( enter, exit )
import           Cubix.Language.Parametric.Semantics.SemanticProperties ( NodeEvaluationPoint (..) )
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, HFix, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), K(..), HFoldable (..), (:-<:), Mem, subterms, unTerm, EqHF, Cxt (..), proj, inject', remA )
import           Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )

assertCfgWellFormedness :: (All ShowHF fs, All HFoldable fs, All HFunctor fs, MonadTest m, MonadReader (Cfg fs) m, All (AssertCfgWellFormed fs) fs) => TermLab fs l -> m ()
assertCfgWellFormedness t = do
  mapM_ (\(E t0) -> go t0) $ subterms t

  where go t0 = do
          cfg0 <- ask
          let cfgLab = getCfgLabs cfg0 t0
          if length cfgLab > 0 then do
            assertCfgWellFormed (unTerm t0)
          else
            pure ()

getCfgLabs :: forall fs l. Cfg fs -> TermLab fs l -> Map.Map CfgNodeType Label
getCfgLabs cfg t = case Map.lookup astLab (cfg ^. cfg_ast_nodes) of
                    Nothing -> Map.empty
                    Just m -> m
  where
    astLab = getAnn t

class AssertCfgWellFormed fs f where
  assertCfgWellFormed :: (MonadTest m, MonadReader (Cfg fs) m) => (f :&: Label) (TermLab fs) l -> m ()

  default assertCfgWellFormed :: (MonadTest m, MonadReader (Cfg fs) m, Mem f fs, All ShowHF fs, All HFunctor fs) => (f :&: Label) (TermLab fs) l -> m ()
  assertCfgWellFormed = assertCfgNoCfgNode . inject'

instance (All (AssertCfgWellFormed gs) fs) => AssertCfgWellFormed gs (Sum fs) where
  assertCfgWellFormed = caseCxt' (Proxy @(AssertCfgWellFormed gs)) assertCfgWellFormed

-- NOTE: Asserts that a CFG node is not created for this AST node.
assertCfgNoCfgNode ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  ) => TermLab fs l -> m ()
assertCfgNoCfgNode t = do
  mnodeLab <- preview (cur_cfg.cfg_ast_nodes.(ix astLab))
  case mnodeLab of
    Nothing -> pure ()
    Just l  -> H.failWith Nothing (msg l)

    where astLab = getAnn t
          msg l = "unexpected CfgNode created for AST: " ++
                  show t ++ "\n CfgNode is: " ++ show l

-- NOTE: Actually has to list the immedeate intermediate edges
--       and ensure that they are connected.
assertCfgIsSuspended' ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs  
  ) => TermLab fs l -> TermLab fs l1 -> TermLab fs l2 -> m ()
assertCfgIsSuspended' t t1 t2 = do
  en <- getEnterNode t t1
  ex <- getExitNode t t2
  (en ^.cfg_node_prevs) === Set.empty
  (ex ^.cfg_node_succs) === Set.empty  

assertCfgIsSuspended ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs  
  ) => TermLab fs l -> TermLab fs s -> m ()
assertCfgIsSuspended t t0 = assertCfgIsSuspended' t t0 t0

-- NOTE: Asserts that subterms are connected left to right
-- and enter of term is connected to leftmost subterm
-- and rightmost subterm is connected to exit of term 
assertCfgIsGeneric ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs l -> [E (TermLab fs)] -> m ()
assertCfgIsGeneric t subs = do
  -- assertCfgSubterms t subs
  (gEn, gEx) <- getEnterExitPair t t
  subsEEP <- mapM (getEnterExitPairE t) subs
  case subsEEP of
    [] -> assertEdges t [(gEn, gEx)] [gEn, gEx]
    _  -> do
      let edges = [ (gEn, (fst (head subsEEP)))
                  , ((snd (last subsEEP)), gEx)
                  ] ++ zipWith (\p n -> (snd p, fst n)) subsEEP (tail subsEEP)
          nodes = [gEn, gEx] ++ map fst subsEEP ++ map snd subsEEP
      assertEdges t edges nodes

assertCfgSubterms ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs l -> [E (TermLab fs)] -> m ()
assertCfgSubterms t subs = do
  subsEEP <- mapM (getEnterExitPairE t) subs  
  forM_ (zip subsEEP (tail subsEEP)) $
    \(prev, next) -> assertEdge t (snd prev) (fst next)  

getEnterExitPairE ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => TermLab gs a -> E (TermLab gs) -> m (CfgNode gs, CfgNode gs)
getEnterExitPairE t (E t0) = getEnterExitPair t t0

getEnterExitPair ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => TermLab gs a -> TermLab gs l -> m (CfgNode gs, CfgNode gs)
getEnterExitPair t t0 = do
  (,) <$> getEnterNode t t0 <*> getExitNode t t0

getEnterNodeE ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => TermLab gs a -> E (TermLab gs) -> m (CfgNode gs)
getEnterNodeE t (E t0) = getEnterNode t t0

getEnterNode ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => TermLab gs a -> TermLab gs l -> m (CfgNode gs)
getEnterNode = getNodeFromAstLabel EnterNode

getExitNodeE ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => TermLab gs a -> E (TermLab gs) -> m (CfgNode gs)
getExitNodeE t (E t0) = getExitNode t t0

getExitNode ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => TermLab gs a -> TermLab gs l -> m (CfgNode gs)
getExitNode = getNodeFromAstLabel ExitNode

getLoopEntry ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => TermLab gs a -> TermLab gs l -> m (CfgNode gs)
getLoopEntry = getNodeFromAstLabel LoopEntryNode

getIEP ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => Int -> TermLab gs a -> TermLab gs l -> m (CfgNode gs)
getIEP i = getNodeFromAstLabel (evalPointToNodeType (BeforeIntermediateEvalPoint i)) 

getNodeFromAstLabel ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs
  ) => CfgNodeType -> TermLab fs a -> TermLab fs l -> m (CfgNode fs)
getNodeFromAstLabel nodeType t t0 =
  getCfgNodeLabel t t0 nodeType >>= getCfgNode t t0

getCfgNodeLabel :: (MonadTest m, MonadReader (Cfg fs) m, All ShowHF fs, All HFunctor fs) => TermLab fs a -> TermLab fs l -> CfgNodeType -> m Label
getCfgNodeLabel t t0 nodeType = do
  mnodeLab <- preview (cur_cfg.cfg_ast_nodes.(ix astLab).(ix nodeType))
  case mnodeLab of
    Just lab -> pure lab
    Nothing -> H.failWith Nothing msg

  where astLab = getAnn t0
        msg = "Cannot find label for AST: \n" ++ show t0 ++
              "\nfor nodetype: " ++ show nodeType ++
              "\n while checking cfg for term: \n" ++ show t

getCfgNode :: (MonadTest m, MonadReader (Cfg fs) m, All ShowHF fs, All HFunctor fs) => TermLab fs a -> TermLab fs l -> Label -> m (CfgNode fs)
getCfgNode t t0 nodeLab = do
  mcfgNode <- preview (cur_cfg.cfg_nodes.(ix nodeLab))
  case mcfgNode of
    Just cfgNode -> pure cfgNode
    Nothing -> H.failWith Nothing msg

  where msg = "Cannot find CfgNode with Label: " ++ show nodeLab ++
              " for AST: \n" ++ show t0 ++
              "\n while checking cfg for term: \n" ++ show t

assertJust :: (MonadTest m) => String -> Maybe a -> m a
assertJust s = evalEither . maybe (Left s) Right

assertEdges ::
  ( MonadTest m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  , MonadReader (Cfg gs) m
  ) => TermLab gs l -> [(CfgNode gs, CfgNode gs)] -> [CfgNode gs] -> m ()
assertEdges t vs as = do
  mrelNodes <- preview (cur_cfg.cfg_ast_nodes.(ix astLab).(to Map.elems))
  mapM_ (\pair -> case pair `elem` vs of
            True -> uncurry (assertEdge t) pair
            False -> do
              isRel <- isRelevantEdge mrelNodes pair
              when isRel $ 
                uncurry (assertNoEdge t) pair
        ) nodes

  where nodes =
          [ (x, y) | x <- as, y <- as
                   , x /= y
                   ]
        isRelevantEdge (Just ns) (x, y) = pure $ 
          x ^. cfg_node_lab `elem` ns ||
          y ^. cfg_node_lab `elem` ns
        isRelevantEdge Nothing _ =
          failure
        astLab = getAnn t

assertEdge ::
  ( MonadTest m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  ) => TermLab gs l -> CfgNode gs -> CfgNode gs -> m ()
assertEdge t from to = do
  assertMembership t from to "Incoming" (to ^. cfg_node_prevs)
  assertMembership t to from "Outgoing" (from ^. cfg_node_succs)  

assertNoEdge ::
  ( MonadTest m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  ) => TermLab gs l -> CfgNode gs -> CfgNode gs -> m ()
assertNoEdge t from to = do
  assertNonMembership t from to "Incoming" (to ^. cfg_node_prevs)
  assertNonMembership t to from "Outgoing" (from ^. cfg_node_succs)    

project' :: (f :-<: fs) => TermLab fs l -> Maybe ((f :&: Label) (TermLab fs) l)
project' (Term (s :&: l)) = fmap (:&: l) (proj s)

assertMembership ::
  ( MonadTest m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  ) => TermLab gs l -> CfgNode gs -> CfgNode gs -> String -> Set.Set Label -> m ()
assertMembership t n1 n2  edgeType ls =
  case Set.member (n1 ^. cfg_node_lab) ls of
    True -> pure ()
    False -> H.failWith Nothing msg

    where msg =
            "Node: " ++ show n1 ++ "\n does not exist in " ++ edgeType ++
            " edges of cfg node: \n" ++ show n2 ++
            "\n while checking cfg for term: \n" ++ show t

assertNonMembership ::
  ( MonadTest m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  ) => TermLab gs l -> CfgNode gs -> CfgNode gs -> String -> Set.Set Label -> m ()
assertNonMembership t n1 n2 edgeType ls =
  case Set.member (n1 ^. cfg_node_lab) ls of
    False -> pure ()
    True -> H.failWith Nothing msg

    where msg =
            "Node: " ++ show n1 ++ "\n does exist in " ++ edgeType ++
            " edges of cfg node: \n" ++ show n2 ++
            "\n while checking cfg for term: \n" ++ show t

type Edge a = (a, a)
type Edges a = [Edge a]

joinEdges :: Edges a -> Edge a -> Edge a -> (Edge a, Edges a)
joinEdges vs e1 e2 = (bound, innerEdge : vs)
  where enter0 = fst
        exit0  = snd
        innerEdge = (exit e1, enter e2)
        bound = (enter e1, exit e2)
        
mJoinEdgesR :: Edges a -> Edge a -> Maybe (Edge a) -> (Edge a, Edges a)
mJoinEdgesR vs e1 (Just e2) = joinEdges vs e1 e2
mJoinEdgesR vs e1 Nothing   = (e1, vs)

mJoinEdgesL :: Edges a -> Maybe (Edge a) -> (Edge a) -> (Edge a, Edges a)
mJoinEdgesL vs (Just e1) e2 = joinEdges vs e1 e2
mJoinEdgesL vs  Nothing  e2 = (e2, vs)

enter :: Edge a -> a
enter = fst

exit :: Edge a -> a
exit = snd

identEdge :: a -> Edge a
identEdge a = (a, a)

children :: (All HFoldable gs, All HFunctor gs) => TermLab gs l -> [E (TermLab gs)]
children = reverse . hfoldl (\vs t0 -> E t0 : vs) [] . unTerm

-- NOTE: Given a term, generically finds the CFG nodes corresponding to the given term's
--       immedeate children, and asserts that child EEPs are connected
--       and that the given term is connected to the children.
assertCfgIsGenericAuto ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs
  ) => TermLab fs l -> m ()
assertCfgIsGenericAuto t = do
  (en, ex) <- getEnterExitPair t t
  subsEEP <- subtermsCfg t
  let edges = if null subsEEP
        then [(en, ex)]
        else (en, fst (head subsEEP)) :
              (snd (last subsEEP), ex) :
              (zipWith (\p n -> (snd p, fst n)) subsEEP (tail subsEEP))
      nodes = [en, ex] ++ map fst subsEEP ++ map snd subsEEP
  assertEdges t edges nodes
                  
assertCfgSubtermsAuto ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs
  ) => TermLab fs l -> m ()
assertCfgSubtermsAuto t = do
  subsEEP <- subtermsCfg t
  assertEdges t (zipWith (\p n -> (snd p, fst n)) subsEEP (tail subsEEP))
                (map fst subsEEP ++ map snd subsEEP)

subtermsCfg ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs
  ) => TermLab fs l -> m [(CfgNode fs, CfgNode fs)]
subtermsCfg =
  fmap catMaybes . mapM go . children

  where go :: (MonadTest m, All HFoldable fs, All HFunctor fs, MonadReader (Cfg fs) m) => E (TermLab fs) -> m (Maybe (CfgNode fs, CfgNode fs))
        go t0 = do
          -- For each piece of subterm, we get "leftmost"
          -- and "rightmost" CFG node
          l <- go0 children EnterNode t0
          r <- go0 (reverse . children) ExitNode t0
          pure ((,) <$> l <*> r)

        go0 :: (MonadTest m, All HFoldable fs, All HFunctor fs, MonadReader (Cfg fs) m) => (forall l. TermLab fs l -> [E (TermLab fs)]) -> CfgNodeType -> E (TermLab fs) -> m (Maybe (CfgNode fs))
        go0 children0 nodeType (E t0) = do
          let astLab = getAnn t0
          mnodeLab <- preview (cur_cfg.cfg_ast_nodes.(ix astLab).(ix nodeType))
          mcfgNode <- maybe (pure Nothing) (\nodeLab -> preview (cur_cfg.cfg_nodes.(ix nodeLab))) mnodeLab
          let cs = children0 t0           
          foldlM (\acc a -> maybe (go0 children0 nodeType a) (pure . Just) acc) mcfgNode cs
        
          
  
