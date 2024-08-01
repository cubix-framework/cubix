
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Language.Parametric.Syntax.Function (
  -- Function calls
    FunctionCallAttrsL
  , FunctionExpL
  , FunctionArgumentsL
  , FunctionCallL
  , FunctionCall(..)
  , EmptyFunctionCallAttrs(..)
  , FunctionIdent(..)

  , FunctionArgumentL
  , FunctionArgumentList(..)
  , ReceiverL
  , ReceiverArg(..)
  , PositionalArgExpL
  , PositionalArgument(..)

  , pattern FunctionCall'
  ,        iFunctionCall
  ,        jFunctionCall
  , pattern EmptyFunctionCallAttrs'
  ,        iEmptyFunctionCallAttrs
  ,        jEmptyFunctionCallAttrs
  , pattern FunctionIdent'
  ,        iFunctionIdent
  ,        jFunctionIdent
  , pattern FunctionArgumentList'
  ,        iFunctionArgumentList
  ,        jFunctionArgumentList
  , pattern ReceiverArg'
  ,        iReceiverArg
  ,        jReceiverArg
  , pattern PositionalArgument'
  ,        iPositionalArgument
  ,        jPositionalArgument

  -- Function decls

  , FunctionDeclAttrsL
  , FunctionParameterDeclL
  , FunctionParameterDeclAttrsL
  , FunctionDeclL
  , FunctionDecl(..)
  , EmptyFunctionDeclAttrs(..)
  , SelfParameterDecl(..)
  , PositionalParameterDeclOptionalIdent(..)
  , PositionalParameterDeclWithIdent(..)

  , pattern FunctionDecl'
  ,        iFunctionDecl
  ,        jFunctionDecl
  , pattern EmptyFunctionDeclAttrs'
  ,        iEmptyFunctionDeclAttrs
  ,        jEmptyFunctionDeclAttrs
  , pattern SelfParameterDecl'
  ,        iSelfParameterDecl
  ,        jSelfParameterDecl
  , pattern PositionalParameterDeclOptionalIdent'
  ,        iPositionalParameterDeclOptionalIdent
  ,        jPositionalParameterDeclOptionalIdent
  , pattern PositionalParameterDeclWithIdent'
  ,        iPositionalParameterDeclWithIdent
  ,        jPositionalParameterDeclWithIdent

  -- Function defns

  , FunctionDefAttrsL
  , FunctionParameterL
  , FunctionBodyL
  , FunctionDefL
  , FunctionDef(..)
  , EmptyFunctionDefAttrs(..)
  , SelfParameter(..)
  , ParameterAttrsL
  , PositionalParameter(..)
  , EmptyParameterAttrs(..)

  , pattern FunctionDef'
  ,        iFunctionDef
  ,        jFunctionDef
  , pattern EmptyFunctionDefAttrs'
  ,        iEmptyFunctionDefAttrs
  ,        jEmptyFunctionDefAttrs
  , pattern SelfParameter'
  ,        iSelfParameter
  ,        jSelfParameter
  , pattern PositionalParameter'
  ,        iPositionalParameter
  ,        jPositionalParameter
  , pattern EmptyParameterAttrs'
  ,        iEmptyParameterAttrs
  ,        jEmptyParameterAttrs

  ) where

import Data.Comp.Multi ( Node )

import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax.VarDecl

---------------------------------------------------------------------------------
---------------------------              Function Calls         -----------------
---------------------------------------------------------------------------------

data FunctionCallAttrsL
data FunctionExpL
data FunctionArgumentsL


-- | Upon evaluating a FunctionCall node, the following occurs:
--  1) The FunctionExp is evaluated.
--  2) The function call attrs are evaluated.
--  3) The arguments are evaluated, in unspecified order. (Remember, we model C, which has unspecified order)
--  4) A function is looked up based on the function exp and the arguments, and is then invoked with the given arguments.
--
--  1 and 2 happpen in unspecified order. 1 and 2 happen before 3, which happens before 4.
--
--  All positional arguments in the invocation and the function definition are matched up sequentially
--  If argument types do not line up (e.g.: pass a positional arg at a receiver position), this is undefined;
--  said positional argument is ignored in the above computation
data FunctionCallL
data FunctionCall e l where
  FunctionCall :: e FunctionCallAttrsL -> e FunctionExpL -> e FunctionArgumentsL -> FunctionCall e FunctionCallL

deriveAll [''FunctionCall]

data EmptyFunctionCallAttrs :: Node where
  EmptyFunctionCallAttrs :: EmptyFunctionCallAttrs e FunctionCallAttrsL

deriveAll [''EmptyFunctionCallAttrs]

data FunctionIdent e l where
  FunctionIdent :: e IdentL -> FunctionIdent e FunctionExpL

deriveAll [''FunctionIdent]

-- | Additional non-syntactic assumptions:
-- * At most one argument is receiver, and
--     it's at the front of the list if exists.
-- * Positional and receiver args are before other args
data FunctionArgumentL
data FunctionArgumentList :: Node where
  FunctionArgumentList :: e [FunctionArgumentL] -> FunctionArgumentList e FunctionArgumentsL

deriveAll [''FunctionArgumentList]

data ReceiverL
data ReceiverArg e l where
  ReceiverArg :: e ReceiverL -> ReceiverArg e FunctionArgumentL

deriveAll [''ReceiverArg]

data PositionalArgExpL
data PositionalArgument :: Node where
  PositionalArgument :: e PositionalArgExpL -> PositionalArgument e FunctionArgumentL

deriveAll [''PositionalArgument]

---------------------------------------------------------------------------------
---------------------------          Function Decls             -----------------
---------------------------------------------------------------------------------

data FunctionDeclAttrsL
data FunctionParameterDeclL

data FunctionDeclL
data FunctionDecl e l where
  FunctionDecl :: e FunctionDeclAttrsL -> e IdentL -> e [FunctionParameterDeclL] -> FunctionDecl e FunctionDeclL

deriveAll [''FunctionDecl]

data EmptyFunctionDeclAttrs :: Node where
  EmptyFunctionDeclAttrs :: EmptyFunctionDeclAttrs e FunctionDeclAttrsL

deriveAll [''EmptyFunctionDeclAttrs]

data SelfParameterDecl :: Node where
  SelfParameterDecl :: SelfParameterDecl e FunctionParameterDeclL

deriveAll [''SelfParameterDecl]

data FunctionParameterDeclAttrsL

data PositionalParameterDeclOptionalIdent e l where
  PositionalParameterDeclOptionalIdent :: e FunctionParameterDeclAttrsL -> e (Maybe IdentL) -> PositionalParameterDeclOptionalIdent e FunctionParameterDeclL

deriveAll [''PositionalParameterDeclOptionalIdent]

data PositionalParameterDeclWithIdent e l where
  PositionalParameterDeclWithIdent :: e FunctionParameterDeclAttrsL -> e IdentL -> PositionalParameterDeclWithIdent e FunctionParameterDeclL

deriveAll [''PositionalParameterDeclWithIdent]


---------------------------------------------------------------------------------
---------------------------          Function Defns             -----------------
---------------------------------------------------------------------------------

data FunctionDefAttrsL
data FunctionParameterL
data FunctionBodyL
data FunctionDefL
data FunctionDef e l where
  FunctionDef :: e FunctionDefAttrsL -> e IdentL -> e [FunctionParameterL] -> e FunctionBodyL -> FunctionDef e FunctionDefL

deriveAll [''FunctionDef]

data EmptyFunctionDefAttrs :: Node where
  EmptyFunctionDefAttrs :: EmptyFunctionDefAttrs e FunctionDefAttrsL

deriveAll [''EmptyFunctionDefAttrs]

data SelfParameter :: Node where
  SelfParameter :: SelfParameter e FunctionParameterL

deriveAll [''SelfParameter]

data ParameterAttrsL
data PositionalParameter e l where
  PositionalParameter :: e ParameterAttrsL -> e IdentL -> PositionalParameter e FunctionParameterL

deriveAll [''PositionalParameter]

data EmptyParameterAttrs :: Node where
  EmptyParameterAttrs :: EmptyParameterAttrs e ParameterAttrsL

deriveAll [''EmptyParameterAttrs]
