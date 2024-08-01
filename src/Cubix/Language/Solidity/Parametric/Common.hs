{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE CPP #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Cubix.Language.Solidity.Parametric.Common
-- Copyright   :  (c) 2016-2023 James Koppel
-- License     :  BSD3
--
--
-- Language definition is auto-generated from that of the @solidity@ package, and modified
-- to replace some language-specific nodes with generic nodes.
-- See https://github.com/jkoppel/solidity-hs/blob/main/src/Types.hs
--
--------------------------------------------------------------------------------

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Solidity.Parametric.Common () where
#else

module Cubix.Language.Solidity.Parametric.Common (
    module Types
  , module Trans
  , module F

  , module Para
  ) where

import Cubix.Language.Solidity.Parametric.Common.Types as Types
import Cubix.Language.Solidity.Parametric.Common.Trans as Trans
import Cubix.Language.Solidity.Parametric.Full as F hiding ( translate, untranslate )
import Cubix.Language.Parametric.Syntax as Para hiding ( FunctionCall, iFunctionCall, jFunctionCall, pattern FunctionCall', Assign, iAssign, jAssign, pattern Assign' )
#endif