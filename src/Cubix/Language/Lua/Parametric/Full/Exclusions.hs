{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE TemplateHaskell #-}

module Cubix.Language.Lua.Parametric.Full.Exclusions (
    luaExcludedNames
  , excludedNamesSet
  ) where

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Comp.Trans ( standardExcludedNames )

import qualified Language.Haskell.TH.Syntax as TH
import Language.Lua.Annotated.Syntax

-- | TAG_LUA_FUNBODY_EXCEPTION
-- | We handle FunDef specially because it can have two annotations
luaExcludedNames :: [TH.Name]
luaExcludedNames = [''FunBody]

excludedNamesSet :: Set TH.Name
excludedNamesSet = Set.union standardExcludedNames $ Set.fromList luaExcludedNames


