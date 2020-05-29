{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Cubix.Language.Lua.Cfg.Test where

import           Control.Lens hiding (para)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import qualified Data.Map as Map
import           Data.Proxy
import qualified Data.Set as Set
import           Hedgehog


import           Cubix.Language.Info
import           Cubix.Language.Lua.Parametric.Common as LCommon
import           Cubix.Language.Parametric.Semantics.Cfg
import           Cubix.ParsePretty
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', paraM, (:*:), HFix, (:->), NatM, ffst, hfmap, ShowHF, HFunctor )
import           Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )


unit_lua_cfg :: FilePath -> Property
unit_lua_cfg fp =
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile fp
    (tLab, cfg) <- makeLuaCfg t
    runReaderT (paraM assertCfgWellFormed tLab) cfg
    pure ()

type PreRAlgM m f g a = NatM m (f (HFix g :*: a)) a

newtype Unit (fs :: [(* -> *) -> * -> *]) l = Unit { getUnit :: () }

unit :: Unit fs l
unit = Unit ()

class AssertCfgWellFormed gs f where
  assertCfgWellFormed :: (MonadReader (Cfg gs) m, MonadTest m) => PreRAlgM m (f :&: Label) (Sum gs :&: Label) (Unit gs)

instance {-# OVERLAPPING #-} (All (AssertCfgWellFormed gs) fs) => AssertCfgWellFormed gs (Sum fs) where
  assertCfgWellFormed = caseCxt' (Proxy @(AssertCfgWellFormed gs)) assertCfgWellFormed

instance {-# OVERLAPPABLE #-} AssertCfgWellFormed gs f where
  assertCfgWellFormed _ = pure unit

instance AssertCfgWellFormed MLuaSig Exp where
  assertCfgWellFormed ((hfmap ffst -> Binop op el er) :&: lab) = do
    case extractOp op of
      And -> assertLuaCfgAnd lab el er
      Or  -> assertLuaCfgOr lab el er
      _   -> pure unit

    where extractOp :: MLuaTermLab BinopL -> Binop MLuaTerm BinopL
          extractOp (stripA -> project -> Just bp) = bp

  assertCfgWellFormed _ = pure unit

assertLuaCfgAnd ::
  ( MonadTest m
  , MonadReader (Cfg MLuaSig) m
  ) => Label -> MLuaTermLab ExpL -> MLuaTermLab ExpL -> m (Unit MLuaSig l)
assertLuaCfgAnd = assertLuaCfgShortCircuit

assertLuaCfgOr ::
  ( MonadTest m
  , MonadReader (Cfg MLuaSig) m
  ) => Label -> MLuaTermLab ExpL -> MLuaTermLab ExpL -> m (Unit MLuaSig l)
assertLuaCfgOr = assertLuaCfgShortCircuit

assertLuaCfgShortCircuit ::
  ( MonadTest m
  , MonadReader (Cfg MLuaSig) m
  ) => Label -> MLuaTermLab ExpL -> MLuaTermLab ExpL -> m (Unit MLuaSig l)
assertLuaCfgShortCircuit andLab el er = do
  enAnd <- getEnterNodeFromAstLabel @MLuaSig andLab
  exAnd <- getExitNodeFromAstLabel @MLuaSig andLab

  enEl <- getEnterNodeFromAstNode el
  exEl <- getExitNodeFromAstNode el

  enEr <- getEnterNodeFromAstNode er
  exEr <- getExitNodeFromAstNode er

  assertEdge enAnd enEl
  assertEdge exEl enEr
  assertEdge exEl exAnd
  assertEdge exEr exAnd

  pure unit

getEnterNodeFromAstNode ::
  ( MonadReader (Cfg gs) m
  , HasCurCfg (Cfg gs) gs
  , MonadTest m
  ) => TermLab gs l -> m (CfgNode gs)
getEnterNodeFromAstNode = getEnterNodeFromAstLabel . getAnn

getExitNodeFromAstNode ::
  ( MonadReader (Cfg gs) m
  , HasCurCfg (Cfg gs) gs
  , MonadTest m
  ) => TermLab gs l -> m (CfgNode gs)
getExitNodeFromAstNode = getExitNodeFromAstLabel . getAnn

getEnterNodeFromAstLabel ::
  forall gs m.
  ( MonadReader (Cfg gs) m
  , HasCurCfg (Cfg gs) gs
  , MonadTest m
  ) => Label -> m (CfgNode gs)
getEnterNodeFromAstLabel = getNodeFromAstLabel EnterNode

getExitNodeFromAstLabel ::
  forall gs m.
  ( MonadReader (Cfg gs) m
  , HasCurCfg (Cfg gs) gs
  , MonadTest m
  ) => Label -> m (CfgNode gs)
getExitNodeFromAstLabel = getNodeFromAstLabel ExitNode

getNodeFromAstLabel ::
  forall gs m.
  ( MonadReader (Cfg gs) m
  , HasCurCfg (Cfg gs) gs
  , MonadTest m
  ) => CfgNodeType -> Label -> m (CfgNode gs)
getNodeFromAstLabel nodeType astLab = do
  mnodeLab <- preview (cur_cfg.cfg_ast_nodes.(ix astLab).(ix nodeType))
  nodeLab  <- assertJust mnodeLab
  mcfgNode <- preview (cur_cfg.cfg_nodes.(ix nodeLab))
  assertJust mcfgNode

assertJust :: (MonadTest m) => Maybe a -> m a
assertJust = evalEither . maybe (Left "AssertJust") Right

assertEdge ::
  ( MonadTest m
  , All ShowHF gs
  , All HFunctor gs
  ) => CfgNode gs -> CfgNode gs -> m (Unit gs l)
assertEdge from to = do
  let fl = from ^. cfg_node_lab
      tl = to   ^. cfg_node_lab
  footnote ("From node is: " ++ show from)
  footnote ("To node is: " ++ show to)

  assert (Set.member tl (from ^. cfg_node_succs))
  assert (Set.member fl (to ^. cfg_node_prevs))
  pure unit

makeLuaCfg :: (MonadIO m) => MLuaTerm LBlockL -> m (MLuaTermLab LBlockL, Cfg MLuaSig)
makeLuaCfg t = do
  gen <- mkCSLabelGen
  let tLab = labelProg gen t
      cfg = makeCfg tLab
  pure (tLab, cfg)

