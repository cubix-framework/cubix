{-# LANGUAGE DataKinds                  #-}
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

import           Control.Lens hiding ( para )
import           Control.Monad.Reader (MonadReader (..), ReaderT )
import           Data.Foldable (traverse_)
import qualified Data.Map as Map
import           Data.Proxy
import qualified Data.Set as Set
import           Hedgehog
import qualified Hedgehog.Internal.Property as H

import           Cubix.Language.Info
import           Cubix.Language.Parametric.Semantics.Cfg hiding ( enter, exit )
import           Cubix.Language.Parametric.Semantics.SemanticProperties ( NodeEvaluationPoint (..) )
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, HFix, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), K(..), HFoldable (..), (:-<:), Mem, subterms, unTerm, EqHF, Cxt (..), proj )
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

instance (All (AssertCfgWellFormed gs) fs) => AssertCfgWellFormed gs (Sum fs) where
  assertCfgWellFormed = caseCxt' (Proxy @(AssertCfgWellFormed gs)) assertCfgWellFormed

assertCfgIsSuspended' ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs  
  ) => TermLab fs l -> TermLab fs l1 -> TermLab fs l2 -> m ()
assertCfgIsSuspended' t t1 t2 = do
  en <- getEnterNode t1
  ex <- getExitNode t2
  (en ^.cfg_node_prevs) === Set.empty
  (ex ^.cfg_node_succs) === Set.empty  

assertCfgIsSuspended ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs  
  ) => TermLab fs l -> TermLab fs s -> m ()
assertCfgIsSuspended t t0 = assertCfgIsSuspended' t t0 t0
  
getEnterExitPairMaybe ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => Maybe (TermLab gs l) -> m (Maybe (CfgNode gs, CfgNode gs))
getEnterExitPairMaybe = traverse getEnterExitPair

getEnterExitPairE ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => E (TermLab gs) -> m (CfgNode gs, CfgNode gs)
getEnterExitPairE (E t) = getEnterExitPair t

getEnterExitPair ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => TermLab gs l -> m (CfgNode gs, CfgNode gs)
getEnterExitPair t = do
  (,) <$> getEnterNode t <*> getExitNode t

getEnterNodeE ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => E (TermLab gs) -> m (CfgNode gs)
getEnterNodeE (E t) = getEnterNode t

getEnterNode ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => TermLab gs l -> m (CfgNode gs)
getEnterNode = getNodeFromAstLabel EnterNode

getExitNodeE ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => E (TermLab gs) -> m (CfgNode gs)
getExitNodeE (E t) = getExitNode t

getExitNode ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => TermLab gs l -> m (CfgNode gs)
getExitNode = getNodeFromAstLabel ExitNode

getLoopEntry ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => TermLab gs l -> m (CfgNode gs)
getLoopEntry = getNodeFromAstLabel LoopEntryNode

getIEP ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  , All ShowHF gs
  , All HFunctor gs  
  ) => Int -> TermLab gs l -> m (CfgNode gs)
getIEP i = getNodeFromAstLabel (evalPointToNodeType (BeforeIntermediateEvalPoint i)) 

getNodeFromAstLabel ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs
  ) => CfgNodeType -> TermLab fs a -> m (CfgNode fs)
getNodeFromAstLabel nodeType t =
  getCfgNodeLabel t nodeType >>= getCfgNode t

getCfgNodeLabel :: (MonadTest m, MonadReader (Cfg fs) m, All ShowHF fs, All HFunctor fs) => TermLab fs a -> CfgNodeType -> m Label
getCfgNodeLabel t nodeType = do
  mnodeLab <- preview (cur_cfg.cfg_ast_nodes.(ix astLab).(ix nodeType))
  case mnodeLab of
    Just lab -> pure lab
    Nothing -> H.failWith Nothing msg

  where astLab = getAnn t
        msg = "Cannot find label for AST: \n" ++ show t ++
              "\nfor nodetype: " ++ show nodeType

getCfgNode :: (MonadTest m, MonadReader (Cfg fs) m, All ShowHF fs, All HFunctor fs) => TermLab fs a -> Label -> m (CfgNode fs)
getCfgNode t nodeLab = do
  mcfgNode <- preview (cur_cfg.cfg_nodes.(ix nodeLab))
  case mcfgNode of
    Just cfgNode -> pure cfgNode
    Nothing -> H.failWith Nothing msg

  where astLab = getAnn t
        msg = "Cannot find CfgNode with Label: " ++ show nodeLab ++
              " for AST: \n" ++ show t

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
  nodes <- mkNodes mrelNodes
  mapM_ (\pair -> case pair `elem` vs of
            True -> uncurry (assertEdge t) pair
            False -> uncurry (assertNoEdge t) pair
        ) nodes

  where mkNodes (Just relNodes) =
          pure $
          [ (x, y) | x <- as, y <- as
                   , x /= y
                   , x ^. cfg_node_lab `elem` relNodes || y ^. cfg_node_lab `elem` relNodes
                   ]
        mkNodes Nothing =
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
