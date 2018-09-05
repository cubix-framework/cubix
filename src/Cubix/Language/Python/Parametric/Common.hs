{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}
#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Python.Parametric.Common () where
#else

module Cubix.Language.Python.Parametric.Common (
    module Semantics
  , module Types
  , module Trans
  , module F
  , module Para

  , PAssignOp
  , PAssignOpL
  ) where

import Cubix.Language.Python.Parametric.Common.Cfg () -- instances only
import Cubix.Language.Python.Parametric.Common.Semantics as Semantics
import Cubix.Language.Python.Parametric.Common.Types as Types
import Cubix.Language.Python.Parametric.Common.Trans as Trans
import Cubix.Language.Python.Parametric.Full as F hiding ( translate, untranslate, Ident, iIdent, IdentL, AssignOp, AssignOpL, With, iWith )
-- FIXME: Incomplete import list. Probably easier to just hide the Python things that clash
import Cubix.Language.Parametric.Syntax as Para ( Assign, AssignOpL, AssignOpEquals
                                                , LhsL, RhsL, EmptyBlockEnd, Block, BlockL, BlockItemL, BlockEndL
                                                , FunctionCall, FunctionCallL, FunctionCallAttrsL
                                                , EmptyFunctionCallAttrs, FunctionIdent, FunctionExpL, FunctionArgumentsL
                                                , FunctionArgumentList, FunctionArgumentL, PositionalArgument, ReceiverArg
                                                , FunctionDef, FunctionDefL, PositionalParameter
                                                )

------------------------------------------------------------------------------------------

import qualified Cubix.Language.Python.Parametric.Full as Full

type PAssignOp  = Full.AssignOp
type PAssignOpL = Full.AssignOpL

#endif