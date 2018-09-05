{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cubix.Language.Parametric.Semantics.Cfg.CfgDot (
    renderCfgDot
  ) where

import qualified Data.Graph.Inductive.Graph as Fgl
import qualified Data.Graph.Inductive.PatriciaTree as Fgl
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Lens ((^.))

import qualified Language.Dot.Syntax as Dot

import Cubix.Language.Info
import Cubix.Language.Parametric.Semantics.Cfg.CfgConstruction
import Cubix.Language.Parametric.Semantics.Cfg.Graph


renderCfgDot :: forall f l. (CfgBuilder f) => TermLab f l -> Dot.Graph
renderCfgDot t = Dot.Graph Dot.StrictGraph Dot.DirectedGraph Nothing (nodeStmts ++ edgeStmts)
  where
    cfg = simplify $ cfgToFgl $ makeCfg t
    nodeStmts = map renderNode $ Fgl.labNodes cfg
    edgeStmts = map renderEdge $ Fgl.labEdges cfg

renderNode :: Fgl.LNode Label -> Dot.Statement
renderNode (nId, _) = Dot.NodeStatement (renderId nId) []

renderEdge :: Fgl.LEdge () -> Dot.Statement
renderEdge (a, b, _) =
    Dot.EdgeStatement [ea, eb] []
  where
    ea = Dot.ENodeId Dot.NoEdge (renderId a)
    eb = Dot.ENodeId Dot.DirectedEdge (renderId b)

renderId :: Int -> Dot.NodeId
renderId nId = Dot.NodeId (Dot.NameId $ show nId) Nothing

cfgToFgl :: Cfg f -> Fgl.Gr Label ()
cfgToFgl cfg = Fgl.mkGraph nodes edges
  where
    (nodeLabs, cfgNodes) = unzip $ Map.toList (cfg ^. cfg_nodes)
    nodes :: [Fgl.LNode Label]
    nodes = zip [1..] nodeLabs
    nodeMap = Map.fromList [(lab, id) | (id, lab) <- nodes]
    edges :: [Fgl.LEdge ()]
    edges = concatMap
      (\ n -> [ Fgl.toLEdge (nodeMap Map.! (n ^. cfg_node_lab), nodeMap Map.! t) ()
              | t <- Set.toList (n ^. cfg_node_succs) ]
      )
      cfgNodes

simplify :: (Fgl.DynGraph gr) => gr a () -> gr a ()
simplify gr =
  case simplifyOnce gr of
    Just gr' -> simplify gr'
    Nothing -> gr

simplifyOnce :: (Fgl.DynGraph gr) => gr a () -> Maybe (gr a ())
simplifyOnce gr =
    case nodeLikeEdges of
      ((s, t) : _) ->
        Just $ Fgl.delNode t $ Fgl.insEdges [Fgl.toLEdge (s, u) () | u <- Fgl.suc gr t] gr
      [] ->
        Nothing
  where
    nodeLikeEdges = filter (\ (s, t) -> Fgl.outdeg gr s == 1 && Fgl.indeg gr t == 1) (Fgl.edges gr)
