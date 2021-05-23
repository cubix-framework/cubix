{-# LANGUAGE TemplateHaskell          #-}

module Cubix.Language.Parametric.ProgInfo (
    -- * Type
    ProgInfo
  , HasProgInfo(..)

    -- * Construction
  , makeProgInfo

    -- * Accessors
  , cfgNodePath
  , labToPath
  , termToPath

    -- * Utilities involving multiple components

  , containingCfgNode
  , withContainingCfgNode
  ) where

import Control.Lens ( makeClassy, (^.) )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( isJust )

import Data.Comp.Multi ( runE, E(..), TreeLike )

import Cubix.Language.Info
import Cubix.Language.Parametric.Path
import Cubix.Language.Parametric.Semantics.Cfg

import Cubix.Sin.Compdata.Annotation ( getAnn )

-----------------------------------------------------------------------
------------------ Datatype, construction access ----------------------
-----------------------------------------------------------------------

data ProgInfo fs = ProgInfo { _proginf_program :: E (TermLab fs)
                            , _proginf_cfg     :: Cfg fs
                            , _proginf_paths   :: Map Label Path
                            }

makeClassy ''ProgInfo

makeProgInfo :: (CfgBuilder fs, TreeLike fs) => TermLab fs l -> ProgInfo fs
makeProgInfo t = ProgInfo (E t) (makeCfg t) (getPaths t)

cfgNodePath :: ProgInfo fs -> CfgNode fs -> Maybe Path
cfgNodePath progInf n = Map.lookup termLab (progInf ^. proginf_paths)
  where
    termLab = runE getAnn (n ^. cfg_node_term)

-- TODO: Switch arg order
labToPath :: Label -> ProgInfo fs -> Path
labToPath l progInf = let paths = progInf ^. proginf_paths in
                      case Map.lookup l paths of
                        Just p  -> p
                        Nothing -> error $ "No path for label: " ++ show l

termToPath :: ProgInfo fs -> TermLab fs l -> Path
termToPath progInf t = labToPath (getAnn t) progInf

-----------------------------------------------------------------------
------------- Utilities involving multiple components -----------------
-----------------------------------------------------------------------

-- TODO: This needs a name that reflects that it returns the AST node, not the CFG node
--       Of course, it's better to refactor the CFG inserter to take a CFG node,
--       and then change this to indeed return a node
-- | Let @prog@ be the overall program, @t@ be a subterm, and @progInfo@ the `ProgInfo` for @prog@.
-- Then @containingCfgNode progInf t@ returns the smallest ancestor
-- of @t@ which has a corresponding node in the CFG, or `Nothing` if no such
-- node exists.
--
-- This is useful for writing code which deals with generic nodes such as @Ident@
-- which are contained in computation nodes, but are not themselves computation nodes.
containingCfgNode :: forall fs l. (TreeLike fs)
                  => ProgInfo fs
                  -> TermLab fs l
                  -> Maybe (E (TermLab fs))
containingCfgNode progInf t = runE (\prog -> searchParent inCfg prog (termToPath progInf t))
                                   (progInf ^. proginf_program)
  where
    cfg = progInf ^. proginf_cfg

    inCfg :: TermLab fs i -> Bool
    inCfg = isJust . cfgNodeForTerm cfg EnterNode

-- | See documentation of `containingCfgNode`. Runs the passed function
-- on the result of `containingCfgNode`, if the result is not `Nothing`.
withContainingCfgNode :: (TreeLike fs, Applicative m)
                      => ProgInfo fs
                      -> TermLab fs l
                      -> (forall i. TermLab fs i -> m ())
                      -> m ()
withContainingCfgNode progInf t f = case containingCfgNode progInf t of
                                      Just (E x) -> f x
                                      Nothing    -> pure ()