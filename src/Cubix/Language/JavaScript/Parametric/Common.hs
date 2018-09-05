{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}
#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.JavaScript.Parametric.Common () where
#else

module Cubix.Language.JavaScript.Parametric.Common (
    module Semantics
  , module Types
  , module Trans
  , module F
  , module VarDecl
  ) where

import Cubix.Language.JavaScript.Parametric.Common.Cfg ()
import Cubix.Language.JavaScript.Parametric.Common.Semantics as Semantics
import Cubix.Language.JavaScript.Parametric.Common.Types as Types
import Cubix.Language.JavaScript.Parametric.Common.Trans as Trans
import Cubix.Language.JavaScript.Parametric.Full as F
        hiding ( translate, untranslate, JSIdent, iJSIdentName, iJSIdentNone
               , JSVarInitializer, iJSVarInit, iJSVarInitNone
               , JSFor, iJSFor, JSForIn, iJSForIn, JSForVar, iJSForVar, JSForVarIn, iJSForVarIn
               )
import Cubix.Language.Parametric.Syntax.VarDecl as VarDecl

#endif