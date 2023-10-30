{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE CPP #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Cubix.Language.Java.Parametric.Common
-- Copyright   :  (c) 2016-2020 James Koppel
-- License     :  BSD3
--
--
-- Language definition is auto-generated from that of @language-java@, and modified
-- to replace some language-specific nodes with generic nodes.
-- See https://hackage.haskell.org/package/language-java-0.2.8/docs/Language-Java-Syntax.html
--
--------------------------------------------------------------------------------

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Java.Parametric.Common () where
#else

module Cubix.Language.Java.Parametric.Common (
    module Semantics
  , module Types
  , module Trans
  , module F
  , module Para
  ) where

import Cubix.Language.Java.Parametric.Common.Cfg () -- instances only
import Cubix.Language.Java.Parametric.Common.Semantics as Semantics
import Cubix.Language.Java.Parametric.Common.Types as Types
import Cubix.Language.Java.Parametric.Common.Trans as Trans
import Cubix.Language.Java.Parametric.Full as F hiding ( translate, untranslate, Ident, iIdent, IdentL, Block, iBlock, BlockL, LhsL )
import Cubix.Language.Parametric.Syntax as Para hiding ( Block, BlockL, iBlock, LhsL, AssignOpL, Assign, iAssign )
#endif