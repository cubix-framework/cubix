{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Cubix.Language.Parametric.ProgInfo (
    ProgInfo
  , HasProgInfo(..)

  , makeProgInfo
  , cfgNodePath
  , labToPath
  ) where

import Control.Monad.State ( MonadState )
import Control.Lens ( makeClassy, (^.), use )
import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Comp.Multi ( runE)

import Cubix.Language.Info
import Cubix.Language.Parametric.Path
import Cubix.Language.Parametric.Semantics.Cfg

import Cubix.Sin.Compdata.Annotation ( getAnn )


data ProgInfo f = ProgInfo { _proginf_cfg :: Cfg f
                           , _proginf_paths :: Map Label Path
                           }

makeClassy ''ProgInfo

makeProgInfo :: (CfgBuilder f) => TermLab f l -> ProgInfo f
makeProgInfo t = ProgInfo (makeCfg t) (getPaths t)

cfgNodePath :: ProgInfo f -> CfgNode f -> Maybe Path
cfgNodePath progInf n = Map.lookup termLab (progInf ^. proginf_paths)
  where
    termLab = runE getAnn (n ^. cfg_node_term)



labToPath :: Label -> ProgInfo f -> Path
labToPath l progInf = let paths = progInf ^. proginf_paths in
                      case Map.lookup l paths of
                        Just p  -> p
                        Nothing -> error $ "No path for label: " ++ show l