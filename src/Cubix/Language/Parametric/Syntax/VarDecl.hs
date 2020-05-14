{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Only because can't make overlapping in TH
{-# LANGUAGE OverlappingInstances #-}
-- |

module Cubix.Language.Parametric.Syntax.VarDecl (
    IdentL
  , Ident(..)

  , MultiLocalVarDeclCommonAttrsL
  , LocalVarInitL
  , IsOptional(..)
  , OptLocalVarInitL
  , OptLocalVarInit(..)
  , LocalVarDeclAttrsL
  , TupleBinder(..)
  , IdentIsVarDeclBinder(..)
  , EmptyLocalVarDeclAttrs(..)
  , VarDeclBinderL
  , SingleLocalVarDeclL
  , SingleLocalVarDecl(..)
  , EmptyMultiLocalVarDeclCommonAttrs
  , MultiLocalVarDeclL
  , MultiLocalVarDecl(..)

  , AssignOpL
  , AssignOpEquals(..)
  , LhsL
  , RhsL
  , AssignL
  , Assign(..)

  , BlockItemL
  , BlockEndL
  , EmptyBlockEnd(..)
  , BlockL
  , Block(..)
  , EmptyBlockItem(..)


  , pattern Ident'
  ,        iIdent

  , pattern JustLocalVarInit'
  ,        iJustLocalVarInit
  , pattern NoLocalVarInit'
  ,        iNoLocalVarInit

  , pattern EmptyLocalVarDeclAttrs'
  ,        iEmptyLocalVarDeclAttrs

  , pattern TupleBinder'
  ,        iTupleBinder
  , pattern IdentIsVarDeclBinder'
  ,        iIdentIsVarDeclBinder
  , pattern SingleLocalVarDecl'
  ,        iSingleLocalVarDecl

  , pattern EmptyMultiLocalVarDeclCommonAttrs'
  ,        iEmptyMultiLocalVarDeclCommonAttrs

  , pattern MultiLocalVarDecl'
  ,        iMultiLocalVarDecl

  , pattern AssignOpEquals'
  ,        iAssignOpEquals
  , pattern Assign'
  ,        iAssign
  , pattern EmptyBlockEnd'
  ,        iEmptyBlockEnd
  , pattern Block'
  ,        iBlock
  , pattern EmptyBlockItem'
  ,        iEmptyBlockItem
  ) where

import Data.Comp.Multi ( Cxt, project, project', (:<:), HFunctor, (:-<:), All, CxtS, Sum )

import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF

data IdentL
data Ident (e :: * -> *) l where
  Ident :: String -> Ident e IdentL

deriveAll [''Ident]

pattern Ident' :: (Ident :-<: fs, All HFunctor fs) => String -> CxtS h fs a IdentL
pattern Ident' s <- (project -> (Just (Ident s))) where
  Ident' s = iIdent s

data MultiLocalVarDeclCommonAttrsL
data LocalVarInitL

data IsOptional = Optional | NotOptional


data OptLocalVarInitL

-- Machinery not yet ready for this kind of parameterization
--data OptLocalVarInit opt e l where
--  JustLocalVarInit :: e LocalVarInitL -> OptLocalVarInit opt e OptLocalVarInitL
--  NoLocalVarInit   :: OptLocalVarInit Optional e OptLocalVarInitL

data OptLocalVarInit e l where
  JustLocalVarInit :: e LocalVarInitL -> OptLocalVarInit e OptLocalVarInitL
  NoLocalVarInit   :: OptLocalVarInit e OptLocalVarInitL

deriveAll [''OptLocalVarInit]

pattern JustLocalVarInit' :: (OptLocalVarInit :-<: fs, All HFunctor fs) => CxtS h fs a LocalVarInitL -> CxtS h fs a OptLocalVarInitL
pattern JustLocalVarInit' x <- (project -> Just (JustLocalVarInit x)) where
  JustLocalVarInit' x = iJustLocalVarInit x

pattern NoLocalVarInit' :: (OptLocalVarInit :-<: fs, All HFunctor fs) => CxtS h fs a OptLocalVarInitL
pattern NoLocalVarInit' <- (project -> Just NoLocalVarInit) where
  NoLocalVarInit' = iNoLocalVarInit

data LocalVarDeclAttrsL

-- Needs better name because may need to distinguish part of multi-decl from a standalone decl
------ That's a really hypothetical thing. Why, Jimmy, why

data EmptyLocalVarDeclAttrs (e :: * -> *) l where
  EmptyLocalVarDeclAttrs :: EmptyLocalVarDeclAttrs e LocalVarDeclAttrsL

deriveAll [''EmptyLocalVarDeclAttrs]

pattern EmptyLocalVarDeclAttrs' :: (EmptyLocalVarDeclAttrs :-<: fs, All HFunctor fs) => CxtS h fs a LocalVarDeclAttrsL
pattern EmptyLocalVarDeclAttrs' <- (project -> Just EmptyLocalVarDeclAttrs) where
  EmptyLocalVarDeclAttrs' = iEmptyLocalVarDeclAttrs


data VarDeclBinderL
-- | Represents declaring a list of identifiers, where the list of identifiers
-- should be thought of a single entity being declared. Not to be used as the LHS of an assignment
--
-- This models the (x,y) = (1,2) pattern found in Haskell, or "local x,y" in Lua
data TupleBinder e l where
  TupleBinder :: e [IdentL] -> TupleBinder e VarDeclBinderL

deriveAll [''TupleBinder]

pattern TupleBinder' :: (TupleBinder :-<: fs, All HFunctor fs) => CxtS h fs a [IdentL] -> CxtS h fs a VarDeclBinderL
pattern TupleBinder' xs <- (project -> Just (TupleBinder xs)) where
  TupleBinder' xs = iTupleBinder xs


instance (TupleBinder :-<: fs, All HFunctor fs) => InjF fs [IdentL] VarDeclBinderL where
  injF = iTupleBinder

  projF' (project' -> Just (TupleBinder ns)) = Just ns
  projF' _                                   = Nothing

createSortInclusionType ''IdentL ''VarDeclBinderL
deriveAll [''IdentIsVarDeclBinder]
createSortInclusionInfer ''IdentL ''VarDeclBinderL

pattern IdentIsVarDeclBinder' :: (IdentIsVarDeclBinder :-<: fs, All HFunctor fs) => CxtS h fs a IdentL -> CxtS h fs a VarDeclBinderL
pattern IdentIsVarDeclBinder' n <- (project -> Just (IdentIsVarDeclBinder n)) where
  IdentIsVarDeclBinder' n = iIdentIsVarDeclBinder n


-- |
-- See MultiLocalVarDecl spec
--
-- If no LocalVarInit is present, then the declared variable is unitialized, and may not be referenced until another language
-- If a LocalVarInit is present, then it is executed immediately, before placing the variable in scope, and the result is stored
-- in the declared variable.
--
-- We assume this variable can only be referenced by something enclosed by the local block which contains
-- an identifier contained within the var binder, or at least by something assigned to some value derived from
-- such a reference.
--
-- This forbids a newly-declared variable from referring to itself (sorry Haskell)
-- This does not model C's "static" keyword
--
-- FIXME: So.....I wrote that variables declared without an initializer are unitialized, but that does not model Lua.
-- Luckily, I don't actually depend on this anywhere.
data SingleLocalVarDeclL
data SingleLocalVarDecl e l where
  SingleLocalVarDecl :: e LocalVarDeclAttrsL -> e VarDeclBinderL -> e OptLocalVarInitL -> SingleLocalVarDecl e SingleLocalVarDeclL

deriveAll [''SingleLocalVarDecl]

pattern SingleLocalVarDecl' ::
  ( SingleLocalVarDecl :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a LocalVarDeclAttrsL
  -> CxtS h fs a VarDeclBinderL
  -> CxtS h fs a OptLocalVarInitL
  -> CxtS h fs a SingleLocalVarDeclL
pattern SingleLocalVarDecl' x y z <- (project -> (Just (SingleLocalVarDecl x y z))) where
  SingleLocalVarDecl' x y z = iSingleLocalVarDecl x y z


data EmptyMultiLocalVarDeclCommonAttrs (e :: * -> *) l where
  EmptyMultiLocalVarDeclCommonAttrs :: EmptyMultiLocalVarDeclCommonAttrs e MultiLocalVarDeclCommonAttrsL

deriveAll [''EmptyMultiLocalVarDeclCommonAttrs]

pattern EmptyMultiLocalVarDeclCommonAttrs' :: (EmptyMultiLocalVarDeclCommonAttrs :-<: fs, All HFunctor fs) => CxtS h fs a MultiLocalVarDeclCommonAttrsL
pattern EmptyMultiLocalVarDeclCommonAttrs' <- (project -> Just EmptyMultiLocalVarDeclCommonAttrs) where
  EmptyMultiLocalVarDeclCommonAttrs' = iEmptyMultiLocalVarDeclCommonAttrs

-- | Informal spec:
--
-- There is a notion of variable scope. Executing a MultiLocalVarDecl statement places each local variable declared into scope.
-- Our intention is that MultiLocalVarDecl is only to be used for local declarations, although we have not yet found
-- a reason why it can't be used for top-level variable declarations as in C.
--
-- -- Multi local var decl
-- Each variable declaration has no effect save putting the variable into scope, and executing the initializer expression.
-- Initializer expressions, if present, are executed in left-to-right order. If the language has a notion of
-- an initialized variable, then all declarations for which an initializer is present will be considered initialized.
--
-- Executing a MultiLocalVarDecl will execute all contained SingleLocalVardecl
--
-- Note: C++ variable declarations cannot use this, because the declarations are effectful
-- FIXME: Does not really give a good account for space allocation (e.g.: references to local variables)
data MultiLocalVarDeclL
data MultiLocalVarDecl e l where
  MultiLocalVarDecl :: e MultiLocalVarDeclCommonAttrsL -> e [SingleLocalVarDeclL] -> MultiLocalVarDecl e MultiLocalVarDeclL


deriveAll [''MultiLocalVarDecl]

pattern MultiLocalVarDecl' ::
  ( MultiLocalVarDecl :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a MultiLocalVarDeclCommonAttrsL
  -> CxtS h fs a [SingleLocalVarDeclL]
  -> CxtS h fs a MultiLocalVarDeclL
pattern MultiLocalVarDecl' x y <- (project -> (Just (MultiLocalVarDecl x y))) where
  MultiLocalVarDecl' x y = iMultiLocalVarDecl x y


-- |
-- See spec for assign
--
-- We assume that AssignOpEquals has, as its associated f_op, the function f(x,y)=(typeof x)y
-- where (typeof x)y denotes a type conversion from y to the type of x. We leave it unspecified what
-- exactly that means
data AssignOpL
data AssignOpEquals (e :: * -> *) l where
  AssignOpEquals :: AssignOpEquals e AssignOpL

deriveAll [''AssignOpEquals]

pattern AssignOpEquals' :: (AssignOpEquals :-<: fs, All HFunctor fs) => CxtS h fs a AssignOpL
pattern AssignOpEquals' <- (project -> Just AssignOpEquals) where
  AssignOpEquals' = iAssignOpEquals

data LhsL
data RhsL

-- |
-- The Assign node must have semantics of the following form:
--
-- store(lhs',f_op(lhs', eval(rhs)))
--   where lhs' = evalToLoc(lhs)
--
-- where eval and evalToLoc are arbitrary evaluation function (potentially with side effects)
--
-- The lhs must be evaluated before the rhs.
--
--Note that C can contain arbitrary function calls in lvalues
--
-- If an assign may be used in an expression context, then the store takes effect before its return value, all effects
-- in eval(rhs) take effect before the return, and the return value is the value assigned to lhs
--
-- If the LHS is a variable, it will be considered initialized after execution of the Assign
--
-- FIXME: This spec should maybe contain something like "every var init can be a valid assign", because there
-- are restrictions on assignments, e.g.: C can assign between lvalues with partial overlap. This opens up a rabbit
-- hole of possible constraints we need to specify to avoid allowing transformations to create invalid programs
data AssignL
data Assign e l where
  Assign :: e LhsL -> e AssignOpL -> e RhsL -> Assign e AssignL

deriveAll [''Assign]

pattern Assign' :: (Assign :-<: fs, All HFunctor fs) => CxtS h fs a LhsL -> CxtS h fs a AssignOpL -> CxtS h fs a RhsL -> CxtS h fs a AssignL
pattern Assign' l o r <- (project -> Just (Assign l o r)) where
  Assign' l o r = iAssign l o r


data BlockItemL
data BlockEndL
data BlockL

data EmptyBlockEnd (e :: * -> *) l where
  EmptyBlockEnd :: EmptyBlockEnd e BlockEndL

deriveAll [''EmptyBlockEnd]

pattern EmptyBlockEnd' :: (EmptyBlockEnd :-<: fs, All HFunctor fs) => CxtS h fs a BlockEndL
pattern EmptyBlockEnd' <- (project -> Just EmptyBlockEnd) where
  EmptyBlockEnd' = iEmptyBlockEnd

-- | Block has the following semantics
-- * Any variable introduced by MultiLocalVarDecl may not be referenced outside of the items contained in this block.
-- * Beware that languages such as JavaScript have assignment constructs which do not obey this. We place
-- * no limitation on the visibilty of other kinds of declarations.
--
-- We are also using this to model function bodies in Python, though these
-- have the additional restriction of being nonempty
data Block e l where
  Block :: e [BlockItemL] -> e BlockEndL -> Block e BlockL

deriveAll [''Block]

pattern Block' :: (Block :-<: fs, All HFunctor fs) => CxtS h fs a [BlockItemL] -> CxtS h fs a BlockEndL -> CxtS h fs a BlockL
pattern Block' xs e <- (project -> Just (Block xs e)) where
  Block' xs e = iBlock xs e

-- | This is inserted at the end of blocks
-- so that there's a place to insert at the end of blocks,
-- and so that the sort of empty block-item lists can be correctly determined
--
-- This is a bit of a hack....but, it's actually kinda nice in some ways
data EmptyBlockItem (e :: * -> *) l where
  EmptyBlockItem :: EmptyBlockItem e BlockItemL

deriveAll [''EmptyBlockItem]

pattern EmptyBlockItem' :: (EmptyBlockItem :-<: fs, All HFunctor fs) => CxtS h fs a BlockItemL
pattern EmptyBlockItem' <- (project -> Just EmptyBlockItem) where
  EmptyBlockItem' = iEmptyBlockItem
