{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Cubix.Language.Lua.Parametric.Common
-- Copyright   :  (c) 2016-2020 James Koppel
-- License     :  BSD3
--
--
-- Language definition is auto-generated from that of @language-lua@, and modified
-- to replace some language-specific nodes with generic nodes.
-- See https://hackage.haskell.org/package/language-lua-0.11.0.1/docs/Language-Lua-Annotated-Syntax.html
--
--------------------------------------------------------------------------------


module Cubix.Language.Lua.Parametric.Common (
    module Semantics
  , module Types
  , module Trans
  , module F
  , module Para

  , LBlock
  , LBlockL
  ) where

import Cubix.Language.Lua.Parametric.Common.Cfg () -- instances only
import Cubix.Language.Lua.Parametric.Common.Semantics as Semantics
import Cubix.Language.Lua.Parametric.Common.Types as Types
import Cubix.Language.Lua.Parametric.Common.Trans as Trans
import Cubix.Language.Lua.Parametric.Full as F hiding ( translate, untranslate, Name, iName, NameL
                                                       , Block, iBlock, jBlock, BlockL, Assign, iAssign, jAssign  )
import Cubix.Language.Parametric.Syntax as Para


------------------------------------------------------------------------------------------

-- Need to export both Lua and general blocks
import qualified Cubix.Language.Lua.Parametric.Full as Full

type LBlock  = Full.Block
type LBlockL = Full.BlockL