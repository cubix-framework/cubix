{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}
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