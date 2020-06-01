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

module Cubix.Language.Lua.Cfg.Test where

import           Control.Lens hiding ( para )
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
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, (:*:), HFix, (:->), NatM, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), inj', Cxt (..), inject' )
import           Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )

import           Cubix.Language.Parametric.Cfg.Test

unit_lua_cfg :: FilePath -> Property
unit_lua_cfg fp =
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile fp
    (tLab, cfg) <- makeLuaEnv t
    runReaderT (assertCfgWellFormedness tLab) cfg
    pure ()

makeLuaEnv :: (MonadIO m) => MLuaTerm LBlockL -> m (MLuaTermLab LBlockL, Cfg MLuaSig)
makeLuaEnv t = do
  gen <- mkCSLabelGen
  let tLab = labelProg gen t
      cfg = makeCfg tLab
  pure (tLab, cfg)

instance AssertCfgWellFormed MLuaSig Stat where
  assertCfgWellFormed t@(Break :&: _) =
    assertCfgBreak (inject' t)
-- NOTE: The tests fail now for Repeat
-- This is because the labels for AST (for repeat; we look up labels of `e` and `b`)
-- is looked up in cfg_ast_nodes. But lets say b is a Block, and thus AST formed is like
-- Repeat (BlockIsBlock (Block (ConsF  .... )))
-- But since BlockIsBlock is not in ComputationSort, no entry is created for
-- BlockIsBlock in cfg_ast_nodes.
-- This means, we cannot rely on AST labels to lookup the corresponding CFG node.
-- Same for While / ForIn ..
-- If not via AST labels, then how do we figure out what is the "sort" of connected CFG node?
  assertCfgWellFormed t@(Repeat b e :&: _) =    
    assertCfgDoWhile (inject' t) b e
  assertCfgWellFormed t@(While e b :&: _) =
    assertCfgWhile (inject' t) e b
  assertCfgWellFormed t@(ForIn _ e b :&: _) = do
    assertCfgWhile (inject' t) e b
  -- assertCfgWellFormed t@(Goto (n :*: _) :&: _) = HEnvM $ do
  --  assertCfgGoto t (nameString n)
   
  assertCfgWellFormed t = assertCfgWellFormedDefault t

instance AssertCfgWellFormed MLuaSig Exp where
  assertCfgWellFormed t@(Binop op e1 e2 :&: _) = do
    case extractOp op of
      And -> assertCfgShortCircuit (Term $ inj' t) e1 e2
      Or  -> assertCfgShortCircuit (Term $ inj' t) e1 e2
      _   -> pure ()

    where extractOp :: MLuaTermLab BinopL -> Binop MLuaTerm BinopL
          extractOp (stripA -> project -> Just bp) = bp

  assertCfgWellFormed t = assertCfgWellFormedDefault t

