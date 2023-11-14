
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
  , pattern EmptyParameterAttrs
  ,        iEmptyParameterAttrs
  ,        jEmptyParameterAttrs
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

import Data.Comp.Multi (Node, Cxt, project, (:<:) )

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
  ( FunctionCall :<: f
  ) => Cxt h f a FunctionCallAttrsL
  -> Cxt h f a FunctionExpL
  -> Cxt h f a FunctionArgumentsL
  -> Cxt h f a FunctionCallL
pattern FunctionCall' a e args <- (project -> Just (FunctionCall a e args)) where
  FunctionCall' a e args = jFunctionCall a e args

data EmptyFunctionCallAttrs :: Node where
  EmptyFunctionCallAttrs :: EmptyFunctionCallAttrs e FunctionCallAttrsL

deriveAll [''EmptyFunctionCallAttrs]

pattern EmptyFunctionCallAttrs' :: (EmptyFunctionCallAttrs :<: f) => Cxt h f a FunctionCallAttrsL
pattern EmptyFunctionCallAttrs' <- (project -> Just EmptyFunctionCallAttrs) where
  EmptyFunctionCallAttrs' = jEmptyFunctionCallAttrs

data FunctionIdent e l where
  FunctionIdent :: e IdentL -> FunctionIdent e FunctionExpL

deriveAll [''FunctionIdent]

pattern FunctionIdent' :: (FunctionIdent :<: f) => Cxt h f a IdentL -> Cxt h f a FunctionExpL
pattern FunctionIdent' n <- (project -> Just (FunctionIdent n)) where
  FunctionIdent' n = jFunctionIdent n

-- | Additional non-syntactic assumptions:
-- * At most one argument is receiver, and
--     it's at the front of the list if exists.
-- * Positional and receiver args are before other args
data FunctionArgumentL
data FunctionArgumentList :: Node where
  FunctionArgumentList :: e [FunctionArgumentL] -> FunctionArgumentList e FunctionArgumentsL

deriveAll [''FunctionArgumentList]

pattern FunctionArgumentList' :: (FunctionArgumentList :<: f) => Cxt h f a [FunctionArgumentL] -> Cxt h f a FunctionArgumentsL
pattern FunctionArgumentList' args <- (project -> Just (FunctionArgumentList args)) where
  FunctionArgumentList' args = jFunctionArgumentList args

data ReceiverL
data ReceiverArg e l where
  ReceiverArg :: e ReceiverL -> ReceiverArg e FunctionArgumentL

deriveAll [''ReceiverArg]

pattern ReceiverArg' :: (ReceiverArg :<: f) => Cxt h f a ReceiverL -> Cxt h f a FunctionArgumentL
pattern ReceiverArg' x <- (project -> Just (ReceiverArg x)) where
  ReceiverArg' x = jReceiverArg x

data PositionalArgExpL
data PositionalArgument :: Node where
  PositionalArgument :: e PositionalArgExpL -> PositionalArgument e FunctionArgumentL

deriveAll [''PositionalArgument]

pattern PositionalArgument' :: (PositionalArgument :<: f) => Cxt h f a PositionalArgExpL -> Cxt h f a FunctionArgumentL
pattern PositionalArgument' e <- (project -> Just (PositionalArgument e)) where
  PositionalArgument' e = jPositionalArgument e


---------------------------------------------------------------------------------
---------------------------          Function Decls             -----------------
---------------------------------------------------------------------------------

data FunctionDeclAttrsL
data FunctionParameterDeclL

data FunctionDeclL
data FunctionDecl e l where
  FunctionDecl :: e FunctionDeclAttrsL -> e IdentL -> e [FunctionParameterDeclL] -> FunctionDecl e FunctionDeclL

deriveAll [''FunctionDecl]

pattern FunctionDecl' :: (FunctionDecl :<: f) => Cxt h f a FunctionDeclAttrsL -> Cxt h f a IdentL -> Cxt h f a [FunctionParameterDeclL] -> Cxt h f a FunctionDeclL
pattern FunctionDecl' a n ps <- (project -> (Just (FunctionDecl a n ps))) where
  FunctionDecl' a n ps = jFunctionDecl a n ps

data EmptyFunctionDeclAttrs :: Node where
  EmptyFunctionDeclAttrs :: EmptyFunctionDeclAttrs e FunctionDeclAttrsL

deriveAll [''EmptyFunctionDeclAttrs]

pattern EmptyFunctionDeclAttrs' :: (EmptyFunctionDeclAttrs :<: f) => Cxt h f a FunctionDeclAttrsL
pattern EmptyFunctionDeclAttrs' <- (project -> Just EmptyFunctionDeclAttrs) where
  EmptyFunctionDeclAttrs' = jEmptyFunctionDeclAttrs

data SelfParameterDecl :: Node where
  SelfParameterDecl :: SelfParameterDecl e FunctionParameterDeclL

deriveAll [''SelfParameterDecl]

pattern SelfParameterDecl' :: (SelfParameterDecl :<: f) => Cxt h f a FunctionParameterDeclL
pattern SelfParameterDecl' <- (project -> Just SelfParameterDecl) where
  SelfParameterDecl' = jSelfParameterDecl

data FunctionParameterDeclAttrsL

data PositionalParameterDeclOptionalIdent e l where
  PositionalParameterDeclOptionalIdent :: e FunctionParameterDeclAttrsL -> e (Maybe IdentL) -> PositionalParameterDeclOptionalIdent e FunctionParameterDeclL

deriveAll [''PositionalParameterDeclOptionalIdent]

pattern PositionalParameterDeclOptionalIdent' ::
  ( PositionalParameterDeclOptionalIdent :<: f
  ) => Cxt h f a FunctionParameterDeclAttrsL
  -> Cxt h f a (Maybe IdentL)
  -> Cxt h f a FunctionParameterDeclL
pattern PositionalParameterDeclOptionalIdent' a n <- (project -> Just (PositionalParameterDeclOptionalIdent a n)) where
  PositionalParameterDeclOptionalIdent' a n = jPositionalParameterDeclOptionalIdent a n

data PositionalParameterDeclWithIdent e l where
  PositionalParameterDeclWithIdent :: e FunctionParameterDeclAttrsL -> e IdentL -> PositionalParameterDeclWithIdent e FunctionParameterDeclL

deriveAll [''PositionalParameterDeclWithIdent]

pattern PositionalParameterDeclWithIdent' ::
  ( PositionalParameterDeclWithIdent :<: f
  ) => Cxt h f a FunctionParameterDeclAttrsL
  -> Cxt h f a IdentL
  -> Cxt h f a FunctionParameterDeclL
pattern PositionalParameterDeclWithIdent' a n <- (project -> Just (PositionalParameterDeclWithIdent a n)) where
  PositionalParameterDeclWithIdent' a n = jPositionalParameterDeclWithIdent a n


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
  ( FunctionDef :<: f
  ) => Cxt h f a FunctionDefAttrsL
  -> Cxt h f a IdentL
  -> Cxt h f a [FunctionParameterL]
  -> Cxt h f a FunctionBodyL
  -> Cxt h f a FunctionDefL
pattern FunctionDef' attrs i args body <- (project -> Just (FunctionDef attrs i args body)) where
  FunctionDef' attrs i args body = jFunctionDef attrs i args body

data EmptyFunctionDefAttrs :: Node where
  EmptyFunctionDefAttrs :: EmptyFunctionDefAttrs e FunctionDefAttrsL

deriveAll [''EmptyFunctionDefAttrs]

pattern EmptyFunctionDefAttrs' :: (EmptyFunctionDefAttrs :<: f) => Cxt h f a FunctionDefAttrsL
pattern EmptyFunctionDefAttrs' <- (project -> Just EmptyFunctionDefAttrs) where
  EmptyFunctionDefAttrs' = jEmptyFunctionDefAttrs

data SelfParameter :: Node where
  SelfParameter :: SelfParameter e FunctionParameterL

deriveAll [''SelfParameter]

pattern SelfParameter' :: (SelfParameter :<: f) => Cxt h f a FunctionParameterL
pattern SelfParameter' <- (project -> Just SelfParameter) where
  SelfParameter' = jSelfParameter

data ParameterAttrsL
data PositionalParameter e l where
  PositionalParameter :: e ParameterAttrsL -> e IdentL -> PositionalParameter e FunctionParameterL

deriveAll [''PositionalParameter]

pattern PositionalParameter' ::
  ( PositionalParameter :<: f) => Cxt h f a ParameterAttrsL -> Cxt h f a IdentL -> Cxt h f a FunctionParameterL
pattern PositionalParameter' attrs i <- (project -> Just (PositionalParameter attrs i)) where
  PositionalParameter' attrs i = jPositionalParameter attrs i

data EmptyParameterAttrs :: Node where
  EmptyParameterAttrs :: EmptyParameterAttrs e ParameterAttrsL

deriveAll [''EmptyParameterAttrs]

pattern EmptyParameterAttrs' :: (EmptyParameterAttrs :<: f) => Cxt h f a ParameterAttrsL
pattern EmptyParameterAttrs' <- (project -> Just EmptyParameterAttrs) where
  EmptyParameterAttrs' = jEmptyParameterAttrs
