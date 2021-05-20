{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Cubix.Language.C.Parametric.Common
-- Copyright   :  (c) 2016-2020 James Koppel
-- License     :  BSD3
--
--
-- Language definition is auto-generated from that of @language-c@, and modified
-- to replace some language-specific nodes with generic nodes.
-- See https://hackage.haskell.org/package/language-c-0.5.0/docs/Language-C-Syntax-AST.html
--
--------------------------------------------------------------------------------

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.C.Parametric.Common () where
#else

module Cubix.Language.C.Parametric.Common (
    module Semantics
  , module Types
  , module Trans
  , module F
  , module Para
  ) where

import Cubix.Language.C.Parametric.Common.Cfg ()  -- instances only
import Cubix.Language.C.Parametric.Common.Semantics as Semantics
import Cubix.Language.C.Parametric.Common.Types as Types
import Cubix.Language.C.Parametric.Common.Trans as Trans
import Cubix.Language.C.Parametric.Full as F hiding ( translate, untranslate, iIdent, Ident, IdentL, CFor, iCFor )
import Cubix.Language.Parametric.Syntax as Para hiding ( Block, BlockL, iBlock, iAssign )
#endif