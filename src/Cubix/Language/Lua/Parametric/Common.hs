{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
                                                       , Block, iBlock, BlockL, Assign, iAssign  )
import Cubix.Language.Parametric.Syntax as Para


------------------------------------------------------------------------------------------

-- Need to export both Lua and general blocks
import qualified Cubix.Language.Lua.Parametric.Full as Full

type LBlock  = Full.Block
type LBlockL = Full.BlockL