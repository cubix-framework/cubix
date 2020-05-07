{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Language.Parametric.Semantics.Cfg.Graph (
    Cfg

  , CfgNodeType
  , pattern EnterNode
  , pattern LoopEntryNode
  , pattern ExitNode
  , isEnterNode
  , evalPointToNodeType
  , nodeTypeToEvalPoint

  , HasCurCfg(..)
  , CfgNode
  , cfg_node_prevs
  , cfg_node_succs
  , cfg_node_lab
  , cfg_node_type
  , cfg_node_term

  , emptyCfg
  , cfgNodes
  , addCfgNode
  , nodeForLab
  , addEdge
  , addEdgeLab
  , safeLookupCfg
  , lookupCfg
  , cfgNodeForTerm

  , contractNode

  , satisfyingPredBoundary
  , satisfyingSuccBoundary
  , satisfyingStrictPredBoundary
  , satisfyingStrictSuccBoundary

  , prettyCfg
  , debugCfg

  , isStartNode
  , startsBasicBlock

  ------- PRIVATE ---------

  , addCfgNodeWithLabel
  , mapCfgNode
 ) where

import Control.DeepSeq ( NFData )
import Control.Monad ( mzero )
import Control.Monad.List ( ListT(..) )
import Control.Monad.State ( MonadState )
import Control.Monad.Trans ( lift )

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromJust, fromMaybe, isNothing )
import Data.Proxy ( Proxy(..) )

import Data.Set ( Set )
import qualified Data.Set as Set

import GHC.Generics ( Generic )

import Control.Lens ( (^.), (%~), (%=), (&), (?=), (&), (%~), at, ix, use, makeClassy, makeClassyFor )

import Data.Comp.Multi ( K(..), E(..), appSigFun, subterms, HFoldable, HFunctor(..), ShowHF, runE, Sum, EqHF, OrdHF, AnnHFix, HFix )
import Data.Comp.Multi.Derive ( KShow(..) )

import Cubix.Language.Info
import Cubix.Language.Parametric.Semantics.SemanticProperties

import Cubix.Sin.Compdata.Annotation ( getAnn, propAnnSigFun )

--------------------------------------------------------------------------------------

instance KShow f => Show (E f) where
  show (E x) = unK $ kshow x

--------------------------------------------------------------------------------------

newtype CfgNodeType = CfgNodeType NodeEvaluationPoint
  deriving ( Eq, Ord, Show, Generic, NFData )

pattern EnterNode :: CfgNodeType
pattern EnterNode = CfgNodeType EnterEvalPoint

pattern LoopEntryNode :: CfgNodeType
pattern LoopEntryNode = CfgNodeType LoopEntryPoint

pattern ExitNode :: CfgNodeType
pattern ExitNode = CfgNodeType ExitEvalPoint

isEnterNode :: CfgNodeType -> Bool
isEnterNode EnterNode = True
isEnterNode _         = False

evalPointToNodeType :: NodeEvaluationPoint -> CfgNodeType
evalPointToNodeType = CfgNodeType

nodeTypeToEvalPoint :: CfgNodeType -> NodeEvaluationPoint
nodeTypeToEvalPoint (CfgNodeType p) = p

data CfgNode f = CfgNode { _cfg_node_prevs :: Set Label
                         , _cfg_node_succs :: Set Label
                         , _cfg_node_lab   :: Label
                         , _cfg_node_type  :: CfgNodeType
                         , _cfg_node_term  :: E (HFixLab f)
                         }
  deriving ( Show, Eq, Ord, Generic )

data Cfg f = Cfg {
                   _cfg_nodes     :: Map Label (CfgNode f)
                 , _cfg_ast_nodes :: Map Label (Map CfgNodeType Label)
                 }
  deriving ( Show, Eq, Ord, Generic )

makeClassyFor "HasCurCfg" "cur_cfg" [("_cfg_nodes", "cfg_nodes"), ("_cfg_ast_nodes", "cfg_ast_nodes")] ''Cfg

makeClassy ''CfgNode

mapCfgNode :: (HFunctor f) => (forall e i. f e i -> g e i) -> (CfgNode f -> CfgNode g)
mapCfgNode f n = CfgNode { _cfg_node_prevs = n ^. cfg_node_prevs
                         , _cfg_node_succs = n ^. cfg_node_succs
                         , _cfg_node_lab   = n ^. cfg_node_lab
                         , _cfg_node_type  = n ^. cfg_node_type
                         , _cfg_node_term  = (n ^. cfg_node_term) & (\(E x) -> E (appSigFun (propAnnSigFun f) x))
                         }

emptyCfg :: Cfg f
emptyCfg = Cfg Map.empty Map.empty

cfgNodes :: Cfg f -> [CfgNode f]
cfgNodes cfg = map snd $ Map.toList (cfg ^. cfg_nodes)

addCfgNodeWithLabel :: (HasCurCfg s f, MonadState s m) => HFixLab f l -> Label -> CfgNodeType -> m (CfgNode f)
addCfgNodeWithLabel t l typ = do
  let node = CfgNode { _cfg_node_prevs = Set.empty
                     , _cfg_node_succs = Set.empty
                     , _cfg_node_lab   = l
                     , _cfg_node_type  = typ
                     , _cfg_node_term  = E t
                     }

  let astLab = getAnn t
  cur_cfg.cfg_nodes.(at l) ?= node

  -- ensure map exists
  cur_cfg.cfg_ast_nodes %= \m -> case Map.lookup astLab m of
                                   Nothing -> Map.insert astLab Map.empty m
                                   Just  _ -> m

  cur_cfg.cfg_ast_nodes.(ix astLab) %= Map.insert typ l

  return node

addCfgNode :: (HasCurCfg s f, HasLabelGen s, MonadState s m) => HFixLab f l -> CfgNodeType -> m (CfgNode f)
addCfgNode t typ = do
  l <- nextLabel
  addCfgNodeWithLabel t l typ

nodeForLab :: (HasCurCfg s f, MonadState s m) => Label -> m (Maybe (CfgNode f))
nodeForLab l = Map.lookup l <$> use (cur_cfg.cfg_nodes)

addEdge :: CfgNode f -> CfgNode f -> Cfg f -> Cfg f
addEdge from to cfg = cfg''
  where
    fl = from ^. cfg_node_lab
    tl = to   ^. cfg_node_lab

    cfg'  = cfg  & (cfg_nodes.(at fl).traverse.cfg_node_succs) %~ (Set.insert tl)
    cfg'' = cfg' & (cfg_nodes.(at tl).traverse.cfg_node_prevs) %~ (Set.insert fl)

addEdgeLab :: forall f. Proxy f -> Label -> Label -> Cfg f -> Cfg f
addEdgeLab _ l1 l2 cfg = fromMaybe cfg cfg'
  where
    cfg' :: Maybe (Cfg f)
    cfg' = do
      n1 <- safeLookupCfg cfg l1
      n2 <- safeLookupCfg cfg l2
      return $ addEdge n1 n2 cfg

removeEdgeLab :: Label -> Label -> Cfg f -> Cfg f
removeEdgeLab l1 l2 cfg = cfg''
  where
    cfg'  = cfg  & (cfg_nodes.(at l1).traverse.cfg_node_succs) %~ (Set.delete l2)
    cfg'' = cfg' & (cfg_nodes.(at l2).traverse.cfg_node_prevs) %~ (Set.delete l1)

safeLookupCfg :: Cfg f -> Label -> Maybe (CfgNode f)
safeLookupCfg cfg l = Map.lookup l (cfg ^. cfg_nodes)

lookupCfg :: Cfg f -> Label -> CfgNode f
lookupCfg cfg l = case safeLookupCfg cfg l of
  Just n  -> n
  Nothing -> error $ "Label not found in CFG: " ++ show l

cfgNodeForTerm :: Cfg f -> CfgNodeType -> HFixLab f l -> Maybe (CfgNode f)
cfgNodeForTerm cfg typ t = do
  nodeMap <- Map.lookup (getAnn t) (cfg ^. cfg_ast_nodes)
  cfgLab <- Map.lookup typ nodeMap
  safeLookupCfg cfg cfgLab

removeNode :: CfgNode f -> Cfg f -> Cfg f
removeNode n g = g & removePredEdges
                   & removeSuccEdges
                   & (cfg_nodes     %~ Map.delete lab)
                   & (cfg_ast_nodes.(ix termLab) %~ Map.delete nodeType)
  where
    lab = n ^. cfg_node_lab
    termLab = runE getAnn (n ^. cfg_node_term)
    nodeType = n ^. cfg_node_type

    removePredEdges gr = foldr (\p -> removeEdgeLab p lab) gr (Set.toList (n ^. cfg_node_prevs))
    removeSuccEdges gr = foldr (\s -> removeEdgeLab lab s) gr (Set.toList (n ^. cfg_node_succs))

-- TODO: Find out what this is actually called; "vertex contraction" is something else
contractNode :: Label -> Cfg f -> Cfg f
contractNode l g = removeNode n $
                   foldr add g [(x, y) | x <- (Set.toList (n ^. cfg_node_prevs))
                                       , y <- (Set.toList (n ^. cfg_node_succs))]
  where
    n = lookupCfg g l
    add (x, y) gr = addEdge (lookupCfg gr x) (lookupCfg gr y) gr

satisfyingBoundary :: Set Label -> (CfgNode f -> Set Label) -> (CfgNode f -> Bool) -> Cfg f -> CfgNode f -> ListT Maybe (CfgNode f)
satisfyingBoundary seen succ pred cfg node =
  if Set.member (node ^. cfg_node_lab) seen then
    mzero
  else if pred node then
    return node
  else
    let labs = Set.toList $ succ node in
    if labs == [] then
      lift Nothing
    else do
      nextLab <- ListT (Just labs)
      satisfyingBoundary (Set.insert (node ^. cfg_node_lab) seen) succ pred cfg (lookupCfg cfg nextLab)


satisfyingPredBoundary :: (CfgNode f -> Bool) -> Cfg f -> CfgNode f -> Maybe [CfgNode f]
satisfyingPredBoundary pred cfg node = runListT $ satisfyingBoundary Set.empty (^. cfg_node_prevs) pred cfg node

satisfyingSuccBoundary :: (CfgNode f -> Bool) -> Cfg f -> CfgNode f -> Maybe [CfgNode f]
satisfyingSuccBoundary pred cfg node = runListT $ satisfyingBoundary Set.empty (^. cfg_node_succs) pred cfg node

satisfyingStrictPredBoundary :: (CfgNode f -> Bool) -> Cfg f -> CfgNode f -> Maybe [CfgNode f]
satisfyingStrictPredBoundary pred cfg node = satisfyingPredBoundary pred' cfg node
  where
    pred' n = ((n ^. cfg_node_lab) /= (node ^. cfg_node_lab)) && pred n

satisfyingStrictSuccBoundary :: (CfgNode f -> Bool) -> Cfg f -> CfgNode f -> Maybe [CfgNode f]
satisfyingStrictSuccBoundary pred cfg node = satisfyingSuccBoundary pred' cfg node
  where
    pred' n = ((n ^. cfg_node_lab) /= (node ^. cfg_node_lab)) && pred n

enterNodePreds :: Cfg f -> CfgNode f -> Maybe [CfgNode f]
enterNodePreds cfg n = satisfyingStrictPredBoundary (isEnterNode . (^. cfg_node_type)) cfg n

enterNodeSuccs :: Cfg f -> CfgNode f -> Maybe [CfgNode f]
enterNodeSuccs cfg n = satisfyingStrictSuccBoundary (isEnterNode . (^. cfg_node_type)) cfg n

--------------------------------------------------------------------------------------

prettyCfg :: Cfg f -> String
prettyCfg cfg = concatMap nodeEdges nodes
  where
    nodes = map snd $ Map.toList (cfg ^. cfg_nodes)

    nodeEdges :: CfgNode f -> String
    nodeEdges n = concatMap (pEdge (n ^. cfg_node_lab)) (n ^. cfg_node_succs)
                ++ pInterestingDegree n

    pEdge :: Label -> Label -> String
    pEdge x y = "Edge: " ++ ppLabel x ++ " -> " ++ ppLabel y ++ "\n"

    pInterestingDegree n |
          (Set.size  (n ^. cfg_node_succs) /= 1)
       || (Set.size  (n ^. cfg_node_prevs) /= 1)
       = "Node " ++ ppLabel (n ^. cfg_node_lab) ++ " has interesting degree\n"
    pInterestingDegree _ = ""


getCfgLab :: forall f l. Cfg f -> HFixLab f l -> [Label]
getCfgLab cfg t = case Map.lookup astLab (cfg ^. cfg_ast_nodes) of
                    Nothing -> []
                    Just m -> map snd $ Map.toList m
  where
    astLab = getAnn t

putSubtree :: (ShowHF f, HFoldable f) => HFixLab f l -> Cfg f ->  IO ()
putSubtree t cfg = do
 let cfgLab = getCfgLab cfg t
 if length cfgLab > 0 then do
   putStrLn ""
   putStrLn $ (show $ getAnn t) ++ "(cfg: " ++ show cfgLab ++ ")"
   putStrLn $ show t
  else
   return ()

debugCfg :: (ShowHF f, HFoldable f) => HFixLab f l -> Cfg f -> IO ()
debugCfg t cfg = do
  putStrLn $ prettyCfg cfg
  mapM (\(E t) -> putSubtree t cfg) $ subterms t
  putStrLn "\nBasic blocks: "
  print $ map (^. cfg_node_lab) $ filter (startsBasicBlock cfg) $ cfgNodes cfg

--------------------------------------------------------------------------------------

isStartNode :: Cfg f -> CfgNode f -> Bool
isStartNode cfg n = isNothing maybePrecs || numPrecs == 0
  where
    maybePrecs = enterNodePreds cfg n
    numPrecs = length $ fromJust maybePrecs


startsBasicBlock :: Cfg f -> CfgNode f -> Bool
startsBasicBlock cfg n = (isEnterNode (n ^. cfg_node_type)) && (isStartNode cfg n || isJoinNode || predIsFork)
  where
    maybePrecs = enterNodePreds cfg n

    numPrecs = length $ fromJust maybePrecs
    isJoinNode  = numPrecs > 1

    uniquePred = head $ concat maybePrecs
    predSuccs = enterNodeSuccs cfg uniquePred
    predIsFork = case predSuccs of
                   Nothing -> True
                   Just l  -> length l > 1
