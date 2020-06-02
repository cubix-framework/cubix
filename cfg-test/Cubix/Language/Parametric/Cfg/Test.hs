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


import           Cubix.Language.Info
import           Cubix.Language.Parametric.Semantics.Cfg
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, HFix, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), K(..), HFoldable (..), (:-<:), Mem, subterms, unTerm, EqHF )
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

instance {-# OVERLAPPABLE #-} AssertCfgWellFormed gs f where
  assertCfgWellFormed = assertCfgWellFormedDefault

assertCfgWellFormedDefault :: (MonadTest m, MonadReader (Cfg fs) m) => (f :&: Label) (TermLab fs) l -> m ()
assertCfgWellFormedDefault _ = pure ()

getEnterExitPair ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => TermLab gs l -> m (CfgNode gs, CfgNode gs)
getEnterExitPair t = do
  (,) <$> getEnterNode t <*> getExitNode t

getEnterNode ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => TermLab gs l -> m (CfgNode gs)
getEnterNode = getNodeFromAstLabel EnterNode . getAnn

getExitNode ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => TermLab gs l -> m (CfgNode gs)
getExitNode = getNodeFromAstLabel ExitNode . getAnn

getLoopEntry ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => TermLab gs l -> m (CfgNode gs)
getLoopEntry = getNodeFromAstLabel LoopEntryNode . getAnn

getNodeFromAstLabel ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => CfgNodeType -> Label -> m (CfgNode gs)
getNodeFromAstLabel nodeType astLab = do
  mnodeLab <- preview (cur_cfg.cfg_ast_nodes.(ix astLab).(ix nodeType))
  nodeLab  <- assertJust "Ast label lookup: " mnodeLab
  mcfgNode <- preview (cur_cfg.cfg_nodes.(ix nodeLab))
  assertJust "Cfg label lookup: " mcfgNode

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
  ) => TermLab gs l -> CfgNode gs -> CfgNode gs -> m ()
assertEdge _t from to = do
  let fl = from ^. cfg_node_lab
      tl = to   ^. cfg_node_lab

  assert (Set.member tl (from ^. cfg_node_succs))
  assert (Set.member fl (to ^. cfg_node_prevs))
  pure ()

assertNoEdge ::
  ( MonadTest m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  ) => TermLab gs l -> CfgNode gs -> CfgNode gs -> m ()
assertNoEdge _t from to = do
  let fl = from ^. cfg_node_lab
      tl = to   ^. cfg_node_lab

  assert (not (Set.member tl (from ^. cfg_node_succs)))
  assert (not (Set.member fl (to ^. cfg_node_prevs)))
  pure ()

