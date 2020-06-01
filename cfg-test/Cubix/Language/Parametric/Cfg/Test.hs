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
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, HFix, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), K(..), HFoldable (..), (:-<:), Mem, subterms, unTerm )
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

assertCfgBreak :: (MonadTest m) => TermLab fs a -> m ()
assertCfgBreak _ = pure ()

assertCfgShortCircuit ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  ) => TermLab fs l -> TermLab fs e1 -> TermLab fs e2 -> m ()
assertCfgShortCircuit t e1 e2 = do
  enSExp <- getEnterNodeFromAstNode t
  exSExp <- getExitNodeFromAstNode t

  enE1 <- getEnterNodeFromAstNode e1
  enE2 <- getEnterNodeFromAstNode e2

  exE1 <- getExitNodeFromAstNode e1
  exE2 <- getExitNodeFromAstNode e2

  assertEdge t enSExp enE1
  assertEdge t exE1 enE2
  assertEdge t exE1 exSExp
  assertEdge t exE2 exSExp

assertCfgWhile ::
  ( MonadTest m
  , MonadReader (Cfg gs) m
  , All ShowHF gs
  , All HFunctor gs
  ) => TermLab gs l -> TermLab gs i -> TermLab gs j -> m ()
assertCfgWhile t e b = do
  enWhile <- getEnterNodeFromAstNode t
  loWhile <- getLoopEntryNodeFromAstNode t
  exWhile <- getExitNodeFromAstNode t

  enExp <- getEnterNodeFromAstNode e
  exExp <- getExitNodeFromAstNode e

  enBody <- getEnterNodeFromAstNode b
  exBody <- getExitNodeFromAstNode b
  
  assertEdge t enWhile loWhile
  assertEdge t loWhile enExp
  assertEdge t exExp enBody
  assertEdge t exExp exWhile
  assertEdge t exBody loWhile
  pure ()

assertCfgDoWhile ::
  ( MonadTest m
  , MonadReader (Cfg gs) m
  , All ShowHF gs
  , All HFunctor gs
  ) => TermLab gs l -> TermLab gs i -> TermLab gs j -> m ()
assertCfgDoWhile t b e = do
  footnoteShow t
  enDoWhile <- getEnterNodeFromAstNode t
  loDoWhile <- getLoopEntryNodeFromAstNode t
  exDoWhile <- getExitNodeFromAstNode t

  enBody <- getEnterNodeFromAstNode b
  exBody <- getExitNodeFromAstNode b
  
  enExp <- getEnterNodeFromAstNode e
  exExp <- getExitNodeFromAstNode e

  assertEdge t enDoWhile enBody
  assertEdge t exExp enBody
  assertEdge t exExp exDoWhile  
  assertEdge t exBody loDoWhile
  assertEdge t loDoWhile enExp
  pure ()
  
{-
newtype AstLoopStack = AstLoopStack {
  _loop_stack :: [Label]
  }
  deriving ( Eq, Ord, Show )

makeClassy ''AstLoopStack

emptyAstLoopStack :: AstLoopStack
emptyAstLoopStack = AstLoopStack []

class AssertCfgWellFormed gs r f where
  assertCfgWellFormed :: (MonadTest m) => PreRAlg (f :&: Label) (Sum gs :&: Label) (HEnvM r m)

instance {-# OVERLAPPING #-} (All (AssertCfgWellFormed gs e) fs) => AssertCfgWellFormed gs e (Sum fs) where
  assertCfgWellFormed = caseCxt' (Proxy @(AssertCfgWellFormed gs e)) assertCfgWellFormed

instance {-# OVERLAPPABLE #-} (HFoldable a, Mem a gs) => AssertCfgWellFormed gs e a where
  assertCfgWellFormed = assertCfgWellFormedDefault

assertCfgWellFormedDefault :: (MonadTest m, HFoldable f, f :-<: gs) => PreRAlg (f :&: Label) (Sum gs :&: Label) (HEnvM r m)
assertCfgWellFormedDefault p@(collapseFProd' -> (t :*: subTests)) = HEnvM $ 
  hfoldr (\a acc -> acc >> assertCfgSubcomponent a) (pure ()) subTests
  
assertCfgBreak ::
  forall gs s m l.
  ( MonadTest m
  , HasCurCfg s gs
  , MonadReader s m
  , HasAstLoopStack s
  , All ShowHF gs
  , All HFunctor gs
  ) => TermLab gs l -> m ()
assertCfgBreak t = do

  enBreak <- getEnterNodeFromAstNode t

  mjmpAstLabel <- preview (loop_stack._head)
  jmpAstLabel <- assertJust "Jump to AST LABEL" mjmpAstLabel

  enJmp <- getExitNodeFromAstLabel @gs jmpAstLabel
  assertEdge t enBreak enJmp

  pure ()

assertCfgGoto ::
  ( MonadTest m
  , HasCurCfg s gs
  , MonadReader s m
  , HasAstLoopStack s
  , All ShowHF gs
  , All HFunctor gs
  ) => TermLab gs i -> String -> m ()
assertCfgGoto t _n = do
  _enGoto <- getEnterNodeFromAstNode t
  _exGoto <- getEnterNodeFromAstNode t
  assert False

assertCfgLabel ::
  ( MonadTest m
  , HasCurCfg s gs
  , MonadReader s m
  , HasAstLoopStack s
  , All ShowHF gs
  , All HFunctor gs
  ) => TermLab gs i -> String -> m ()
assertCfgLabel t _n = do
  enLabel <- getEnterNodeFromAstNode t
  exLabel <- getEnterNodeFromAstNode t
  assertEdge t enLabel exLabel

newtype HEnvM r m l = HEnvM { runHEnvM :: ReaderT r m () }

assertCfgSubcomponent :: HEnvM r m l -> ReaderT r m ()
assertCfgSubcomponent = runHEnvM
-}
  
getEnterNodeFromAstNode ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => TermLab gs l -> m (CfgNode gs)
getEnterNodeFromAstNode = getEnterNodeFromAstLabel . getAnn

getExitNodeFromAstNode ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => TermLab gs l -> m (CfgNode gs)
getExitNodeFromAstNode = getExitNodeFromAstLabel . getAnn

getLoopEntryNodeFromAstNode ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => TermLab gs l -> m (CfgNode gs)
getLoopEntryNodeFromAstNode = getLoopEntryNodeFromAstLabel . getAnn

getEnterNodeFromAstLabel ::
  forall gs s m.
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => Label -> m (CfgNode gs)
getEnterNodeFromAstLabel = getNodeFromAstLabel EnterNode

getExitNodeFromAstLabel ::
  forall gs s m.  
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => Label -> m (CfgNode gs)
getExitNodeFromAstLabel = getNodeFromAstLabel ExitNode

getLoopEntryNodeFromAstLabel ::
  forall gs s m.  
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => Label -> m (CfgNode gs)
getLoopEntryNodeFromAstLabel = getNodeFromAstLabel LoopEntryNode

getNodeFromAstLabel ::
  ( MonadReader (Cfg gs) m
  , MonadTest m
  ) => CfgNodeType -> Label -> m (CfgNode gs)
getNodeFromAstLabel nodeType astLab = do
  debug <- preview (cur_cfg.cfg_ast_nodes)
  mnodeLab <- preview (cur_cfg.cfg_ast_nodes.(ix astLab).(ix nodeType))
  let astMsg = "Label is: " ++ show astLab ++ " ( " ++ show nodeType ++ " ) :" ++ show debug
  nodeLab  <- assertJust ("Ast label lookup: " ++ astMsg) mnodeLab
  mcfgNode <- preview (cur_cfg.cfg_nodes.(ix nodeLab))
  let cfgMsg = "Label is: " ++ show nodeLab
  assertJust ("Cfg label lookup: " ++ show cfgMsg) mcfgNode

assertJust :: (MonadTest m) => String -> Maybe a -> m a
assertJust s = evalEither . maybe (Left $ "AssertJust: " ++ s) Right

assertEdge ::
  ( MonadTest m
  , All ShowHF gs
  , All HFunctor gs
  ) => TermLab gs l -> CfgNode gs -> CfgNode gs -> m ()
assertEdge t from to = do
  let fl = from ^. cfg_node_lab
      tl = to   ^. cfg_node_lab
  footnote ("Term is: " ++ show t)
  footnote ("From node is: " ++ show from)
  footnote ("To node is: " ++ show to)

  assert (Set.member tl (from ^. cfg_node_succs))
  assert (Set.member fl (to ^. cfg_node_prevs))
  pure ()

{-
withLoopAst ::
  ( MonadReader r m
  , HasAstLoopStack r
  ) => TermLab fs l -> m () -> m ()
withLoopAst t = local (\a -> a & loop_stack %~ (getAnn t :))

-}

