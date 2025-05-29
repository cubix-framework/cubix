{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Cubix.Language.Solidity.IPS
-- Copyright   :  (c) 2016-2023 James Koppel
-- License     :  BSD3
--
--
-- Language definition is auto-generated from that of the @solidity@ package, and modified
-- to replace some language-specific nodes with generic nodes.
-- See https://github.com/jkoppel/solidity-hs/blob/main/src/Types.hs
--
--------------------------------------------------------------------------------

module Cubix.Language.Solidity.IPS (
    module Types
  , module Trans
  , module M

  , module Para
  ) where

import Cubix.Language.Solidity.IPS.Types as Types
import Cubix.Language.Solidity.IPS.Trans as Trans
import Cubix.Language.Solidity.Modularized as M hiding ( translate, untranslate, Assign, iAssign, jAssign, pattern Assign' )
import Cubix.Language.Parametric.Syntax as Para hiding ( FunctionCall, iFunctionCall, jFunctionCall, pattern FunctionCall' )
