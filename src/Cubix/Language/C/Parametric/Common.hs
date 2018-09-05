{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}

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
import Cubix.Language.C.Parametric.Full as F hiding ( translate, untranslate, iIdent, Ident, IdentL )
import Cubix.Language.Parametric.Syntax as Para hiding ( Block, BlockL, iBlock, iAssign )
#endif