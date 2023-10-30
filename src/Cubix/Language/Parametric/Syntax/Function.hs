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
  , pattern EmptyFunctionCallAttrs'
  ,        iEmptyFunctionCallAttrs
  , pattern FunctionIdent'
  ,        iFunctionIdent
  , pattern FunctionArgumentList'
  ,        iFunctionArgumentList
  , pattern ReceiverArg'
  ,        iReceiverArg
  , pattern PositionalArgument'
  ,        iPositionalArgument

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
  , pattern EmptyFunctionDeclAttrs'
  ,        iEmptyFunctionDeclAttrs
  , pattern SelfParameterDecl'
  ,        iSelfParameterDecl
  , pattern PositionalParameterDeclOptionalIdent'
  ,        iPositionalParameterDeclOptionalIdent
  , pattern PositionalParameterDeclWithIdent'
  ,        iPositionalParameterDeclWithIdent

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
  , pattern EmptyFunctionDefAttrs'
  ,        iEmptyFunctionDefAttrs
  , pattern EmptyParameterAttrs
  ,        iEmptyParameterAttrs
  , pattern SelfParameter'
  ,        iSelfParameter
  , pattern PositionalParameter'
  ,        iPositionalParameter
  , pattern EmptyParameterAttrs'
  ,        iEmptyParameterAttrs

  ) where

import Data.Comp.Multi (Node, CxtS,project, (:-<:), HFunctor, All )

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

pattern FunctionCall' ::
  ( FunctionCall :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a FunctionCallAttrsL
  -> CxtS h fs a FunctionExpL
  -> CxtS h fs a FunctionArgumentsL
  -> CxtS h fs a FunctionCallL
pattern FunctionCall' a e args <- (project -> Just (FunctionCall a e args)) where
  FunctionCall' a e args = iFunctionCall a e args

data EmptyFunctionCallAttrs :: Node where
  EmptyFunctionCallAttrs :: EmptyFunctionCallAttrs e FunctionCallAttrsL

deriveAll [''EmptyFunctionCallAttrs]

pattern EmptyFunctionCallAttrs' :: (EmptyFunctionCallAttrs :-<: fs, All HFunctor fs) => CxtS h fs a FunctionCallAttrsL
pattern EmptyFunctionCallAttrs' <- (project -> Just EmptyFunctionCallAttrs) where
  EmptyFunctionCallAttrs' = iEmptyFunctionCallAttrs

data FunctionIdent e l where
  FunctionIdent :: e IdentL -> FunctionIdent e FunctionExpL

deriveAll [''FunctionIdent]

pattern FunctionIdent' :: (FunctionIdent :-<: fs, All HFunctor fs) => CxtS h fs a IdentL -> CxtS h fs a FunctionExpL
pattern FunctionIdent' n <- (project -> Just (FunctionIdent n)) where
  FunctionIdent' n = iFunctionIdent n

-- | Additional non-syntactic assumptions:
-- * At most one argument is receiver, and
--     it's at the front of the list if exists.
-- * Positional and receiver args are before other args
data FunctionArgumentL
data FunctionArgumentList :: Node where
  FunctionArgumentList :: e [FunctionArgumentL] -> FunctionArgumentList e FunctionArgumentsL

deriveAll [''FunctionArgumentList]

pattern FunctionArgumentList' :: (FunctionArgumentList :-<: fs, All HFunctor fs) => CxtS h fs a [FunctionArgumentL] -> CxtS h fs a FunctionArgumentsL
pattern FunctionArgumentList' args <- (project -> Just (FunctionArgumentList args)) where
  FunctionArgumentList' args = iFunctionArgumentList args

data ReceiverL
data ReceiverArg e l where
  ReceiverArg :: e ReceiverL -> ReceiverArg e FunctionArgumentL

deriveAll [''ReceiverArg]

pattern ReceiverArg' :: (ReceiverArg :-<: fs, All HFunctor fs) => CxtS h fs a ReceiverL -> CxtS h fs a FunctionArgumentL
pattern ReceiverArg' x <- (project -> Just (ReceiverArg x)) where
  ReceiverArg' x = iReceiverArg x

data PositionalArgExpL
data PositionalArgument :: Node where
  PositionalArgument :: e PositionalArgExpL -> PositionalArgument e FunctionArgumentL

deriveAll [''PositionalArgument]

pattern PositionalArgument' :: (PositionalArgument :-<: fs, All HFunctor fs) => CxtS h fs a PositionalArgExpL -> CxtS h fs a FunctionArgumentL
pattern PositionalArgument' e <- (project -> Just (PositionalArgument e)) where
  PositionalArgument' e = iPositionalArgument e


---------------------------------------------------------------------------------
---------------------------          Function Decls             -----------------
---------------------------------------------------------------------------------

data FunctionDeclAttrsL
data FunctionParameterDeclL

data FunctionDeclL
data FunctionDecl e l where
  FunctionDecl :: e FunctionDeclAttrsL -> e IdentL -> e [FunctionParameterDeclL] -> FunctionDecl e FunctionDeclL

deriveAll [''FunctionDecl]

pattern FunctionDecl' :: (FunctionDecl :-<: fs, All HFunctor fs) => CxtS h fs a FunctionDeclAttrsL -> CxtS h fs a IdentL -> CxtS h fs a [FunctionParameterDeclL] -> CxtS h fs a FunctionDeclL
pattern FunctionDecl' a n ps <- (project -> (Just (FunctionDecl a n ps))) where
  FunctionDecl' a n ps = iFunctionDecl a n ps

data EmptyFunctionDeclAttrs :: Node where
  EmptyFunctionDeclAttrs :: EmptyFunctionDeclAttrs e FunctionDeclAttrsL

deriveAll [''EmptyFunctionDeclAttrs]

pattern EmptyFunctionDeclAttrs' :: (EmptyFunctionDeclAttrs :-<: fs, All HFunctor fs) => CxtS h fs a FunctionDeclAttrsL
pattern EmptyFunctionDeclAttrs' <- (project -> Just EmptyFunctionDeclAttrs) where
  EmptyFunctionDeclAttrs' = iEmptyFunctionDeclAttrs

data SelfParameterDecl :: Node where
  SelfParameterDecl :: SelfParameterDecl e FunctionParameterDeclL

deriveAll [''SelfParameterDecl]

pattern SelfParameterDecl' :: (SelfParameterDecl :-<: fs, All HFunctor fs) => CxtS h fs a FunctionParameterDeclL
pattern SelfParameterDecl' <- (project -> Just SelfParameterDecl) where
  SelfParameterDecl' = iSelfParameterDecl

data FunctionParameterDeclAttrsL

data PositionalParameterDeclOptionalIdent e l where
  PositionalParameterDeclOptionalIdent :: e FunctionParameterDeclAttrsL -> e (Maybe IdentL) -> PositionalParameterDeclOptionalIdent e FunctionParameterDeclL

deriveAll [''PositionalParameterDeclOptionalIdent]

pattern PositionalParameterDeclOptionalIdent' ::
  ( PositionalParameterDeclOptionalIdent :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a FunctionParameterDeclAttrsL
  -> CxtS h fs a (Maybe IdentL)
  -> CxtS h fs a FunctionParameterDeclL
pattern PositionalParameterDeclOptionalIdent' a n <- (project -> Just (PositionalParameterDeclOptionalIdent a n)) where
  PositionalParameterDeclOptionalIdent' a n = iPositionalParameterDeclOptionalIdent a n

data PositionalParameterDeclWithIdent e l where
  PositionalParameterDeclWithIdent :: e FunctionParameterDeclAttrsL -> e IdentL -> PositionalParameterDeclWithIdent e FunctionParameterDeclL

deriveAll [''PositionalParameterDeclWithIdent]

pattern PositionalParameterDeclWithIdent' ::
  ( PositionalParameterDeclWithIdent :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a FunctionParameterDeclAttrsL
  -> CxtS h fs a IdentL
  -> CxtS h fs a FunctionParameterDeclL
pattern PositionalParameterDeclWithIdent' a n <- (project -> Just (PositionalParameterDeclWithIdent a n)) where
  PositionalParameterDeclWithIdent' a n = iPositionalParameterDeclWithIdent a n


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

pattern FunctionDef' ::
  ( FunctionDef :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a FunctionDefAttrsL
  -> CxtS h fs a IdentL
  -> CxtS h fs a [FunctionParameterL]
  -> CxtS h fs a FunctionBodyL
  -> CxtS h fs a FunctionDefL
pattern FunctionDef' attrs i args body <- (project -> Just (FunctionDef attrs i args body)) where
  FunctionDef' attrs i args body = iFunctionDef attrs i args body

data EmptyFunctionDefAttrs :: Node where
  EmptyFunctionDefAttrs :: EmptyFunctionDefAttrs e FunctionDefAttrsL

deriveAll [''EmptyFunctionDefAttrs]

pattern EmptyFunctionDefAttrs' :: (EmptyFunctionDefAttrs :-<: fs, All HFunctor fs) => CxtS h fs a FunctionDefAttrsL
pattern EmptyFunctionDefAttrs' <- (project -> Just EmptyFunctionDefAttrs) where
  EmptyFunctionDefAttrs' = iEmptyFunctionDefAttrs

data SelfParameter :: Node where
  SelfParameter :: SelfParameter e FunctionParameterL

deriveAll [''SelfParameter]

pattern SelfParameter' :: (SelfParameter :-<: fs, All HFunctor fs) => CxtS h fs a FunctionParameterL
pattern SelfParameter' <- (project -> Just SelfParameter) where
  SelfParameter' = iSelfParameter

data ParameterAttrsL
data PositionalParameter e l where
  PositionalParameter :: e ParameterAttrsL -> e IdentL -> PositionalParameter e FunctionParameterL

deriveAll [''PositionalParameter]

pattern PositionalParameter' ::
  ( PositionalParameter :-<: fs, All HFunctor fs) => CxtS h fs a ParameterAttrsL -> CxtS h fs a IdentL -> CxtS h fs a FunctionParameterL
pattern PositionalParameter' attrs i <- (project -> Just (PositionalParameter attrs i)) where
  PositionalParameter' attrs i = iPositionalParameter attrs i

data EmptyParameterAttrs :: Node where
  EmptyParameterAttrs :: EmptyParameterAttrs e ParameterAttrsL

deriveAll [''EmptyParameterAttrs]

pattern EmptyParameterAttrs' :: (EmptyParameterAttrs :-<: fs, All HFunctor fs) => CxtS h fs a ParameterAttrsL
pattern EmptyParameterAttrs' <- (project -> Just EmptyParameterAttrs) where
  EmptyParameterAttrs' = iEmptyParameterAttrs
