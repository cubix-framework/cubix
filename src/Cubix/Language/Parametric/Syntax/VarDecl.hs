{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |

module Cubix.Language.Parametric.Syntax.VarDecl (
    -- * Identifiers
    IdentL
  , Ident(..)

  -- ** Constructors and Patterns

  , pattern Ident'
  ,        iIdent
  ,        jIdent

    -- * Variable declarations
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

  -- ** Constructors and Patterns

  , pattern JustLocalVarInit'
  ,        iJustLocalVarInit
  ,        jJustLocalVarInit
  , pattern NoLocalVarInit'
  ,        iNoLocalVarInit
  ,        jNoLocalVarInit

  , pattern EmptyLocalVarDeclAttrs'
  ,        iEmptyLocalVarDeclAttrs
  ,        jEmptyLocalVarDeclAttrs

  , pattern TupleBinder'
  ,        iTupleBinder
  ,        jTupleBinder
  , pattern IdentIsVarDeclBinder'
  ,        iIdentIsVarDeclBinder
  ,        jIdentIsVarDeclBinder
  , pattern SingleLocalVarDecl'
  ,        iSingleLocalVarDecl
  ,        jSingleLocalVarDecl

  , pattern EmptyMultiLocalVarDeclCommonAttrs'
  ,        iEmptyMultiLocalVarDeclCommonAttrs
  ,        jEmptyMultiLocalVarDeclCommonAttrs

  , pattern MultiLocalVarDecl'
  ,        iMultiLocalVarDecl
  ,        jMultiLocalVarDecl

    -- * Assignment
  , AssignOpL
  , AssignOpEquals(..)
  , LhsL
  , RhsL
  , AssignL
  , Assign(..)

  -- ** Constructors and Patterns

  , pattern AssignOpEquals'
  ,        iAssignOpEquals
  ,        jAssignOpEquals
  , pattern Assign'
  ,        iAssign
  ,        jAssign

    -- * Blocks
  , BlockItemL
  , BlockEndL
  , EmptyBlockEnd(..)
  , BlockL
  , Block(..)
  , EmptyBlockItem(..)

  -- ** Constructors and Patterns

  , pattern EmptyBlockEnd'
  ,        iEmptyBlockEnd
  ,        jEmptyBlockEnd
  , pattern Block'
  ,        iBlock
  ,        jBlock
  , pattern EmptyBlockItem'
  ,        iEmptyBlockItem
  ,        jEmptyBlockItem
  ) where

import Data.Comp.Multi ( Node, project, project', HFunctor, (:-<:), (:<:), All, CxtS, Cxt)
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF

data IdentL
data Ident :: Node where
  Ident :: String -> Ident e IdentL

deriveAll [''Ident]

pattern Ident' :: (Ident :<: f) => String -> Cxt h f a IdentL
pattern Ident' s <- (project -> (Just (Ident s))) where
  Ident' s = jIdent s

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

pattern JustLocalVarInit' :: (OptLocalVarInit :<: f) => Cxt h f a LocalVarInitL -> Cxt h f a OptLocalVarInitL
pattern JustLocalVarInit' x <- (project -> Just (JustLocalVarInit x)) where
  JustLocalVarInit' x = jJustLocalVarInit x

pattern NoLocalVarInit' :: (OptLocalVarInit :<: f) => Cxt h f a OptLocalVarInitL
pattern NoLocalVarInit' <- (project -> Just NoLocalVarInit) where
  NoLocalVarInit' = jNoLocalVarInit

data LocalVarDeclAttrsL

-- Needs better name because may need to distinguish part of multi-decl from a standalone decl
------ That's a really hypothetical thing. Why, Jimmy, why

data EmptyLocalVarDeclAttrs :: Node where
  EmptyLocalVarDeclAttrs :: EmptyLocalVarDeclAttrs e LocalVarDeclAttrsL

deriveAll [''EmptyLocalVarDeclAttrs]

pattern EmptyLocalVarDeclAttrs' :: (EmptyLocalVarDeclAttrs :<: f) => Cxt h f a LocalVarDeclAttrsL
pattern EmptyLocalVarDeclAttrs' <- (project -> Just EmptyLocalVarDeclAttrs) where
  EmptyLocalVarDeclAttrs' = jEmptyLocalVarDeclAttrs


data VarDeclBinderL
-- | Represents declaring a list of identifiers, where the list of identifiers
-- should be thought of a single entity being declared. Not to be used as the LHS of an assignment
--
-- This models the (x,y) = (1,2) pattern found in Haskell, or "local x,y" in Lua
data TupleBinder e l where
  TupleBinder :: e [IdentL] -> TupleBinder e VarDeclBinderL

deriveAll [''TupleBinder]

pattern TupleBinder' :: (TupleBinder :<: f) => Cxt h f a [IdentL] -> Cxt h f a VarDeclBinderL
pattern TupleBinder' xs <- (project -> Just (TupleBinder xs)) where
  TupleBinder' xs = jTupleBinder xs


instance (TupleBinder :-<: fs, All HFunctor fs) => InjF fs [IdentL] VarDeclBinderL where
  injF = iTupleBinder

  projF' (project' -> Just (TupleBinder ns)) = Just ns
  projF' _                                   = Nothing

createSortInclusionType ''IdentL ''VarDeclBinderL
deriveAll [''IdentIsVarDeclBinder]
createSortInclusionInfer ''IdentL ''VarDeclBinderL

pattern IdentIsVarDeclBinder' :: (IdentIsVarDeclBinder :<: f) => Cxt h f a IdentL -> Cxt h f a VarDeclBinderL
pattern IdentIsVarDeclBinder' n <- (project -> Just (IdentIsVarDeclBinder n)) where
  IdentIsVarDeclBinder' n = jIdentIsVarDeclBinder n


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
  ( SingleLocalVarDecl :<: f
  ) => Cxt h f a LocalVarDeclAttrsL
  -> Cxt h f a VarDeclBinderL
  -> Cxt h f a OptLocalVarInitL
  -> Cxt h f a SingleLocalVarDeclL
pattern SingleLocalVarDecl' x y z <- (project -> (Just (SingleLocalVarDecl x y z))) where
  SingleLocalVarDecl' x y z = jSingleLocalVarDecl x y z


data EmptyMultiLocalVarDeclCommonAttrs :: Node where
  EmptyMultiLocalVarDeclCommonAttrs :: EmptyMultiLocalVarDeclCommonAttrs e MultiLocalVarDeclCommonAttrsL

deriveAll [''EmptyMultiLocalVarDeclCommonAttrs]

pattern EmptyMultiLocalVarDeclCommonAttrs' :: (EmptyMultiLocalVarDeclCommonAttrs :<: f) => Cxt h f a MultiLocalVarDeclCommonAttrsL
pattern EmptyMultiLocalVarDeclCommonAttrs' <- (project -> Just EmptyMultiLocalVarDeclCommonAttrs) where
  EmptyMultiLocalVarDeclCommonAttrs' = jEmptyMultiLocalVarDeclCommonAttrs

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
  ( MultiLocalVarDecl :<: f
  ) => Cxt h f a MultiLocalVarDeclCommonAttrsL
  -> Cxt h f a [SingleLocalVarDeclL]
  -> Cxt h f a MultiLocalVarDeclL
pattern MultiLocalVarDecl' x y <- (project -> (Just (MultiLocalVarDecl x y))) where
  MultiLocalVarDecl' x y = jMultiLocalVarDecl x y


-- |
-- See spec for assign
--
-- We assume that AssignOpEquals has, as its associated f_op, the function f(x,y)=(typeof x)y
-- where (typeof x)y denotes a type conversion from y to the type of x. We leave it unspecified what
-- exactly that means
data AssignOpL
data AssignOpEquals :: Node where
  AssignOpEquals :: AssignOpEquals e AssignOpL

deriveAll [''AssignOpEquals]

pattern AssignOpEquals' :: (AssignOpEquals :<: f) => Cxt h f a AssignOpL
pattern AssignOpEquals' <- (project -> Just AssignOpEquals) where
  AssignOpEquals' = jAssignOpEquals

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

pattern Assign' :: (Assign :<: f) => Cxt h f a LhsL -> Cxt h f a AssignOpL -> Cxt h f a RhsL -> Cxt h f a AssignL
pattern Assign' l o r <- (project -> Just (Assign l o r)) where
  Assign' l o r = jAssign l o r


data BlockItemL
data BlockEndL
data BlockL

data EmptyBlockEnd :: Node where
  EmptyBlockEnd :: EmptyBlockEnd e BlockEndL

deriveAll [''EmptyBlockEnd]

pattern EmptyBlockEnd' :: (EmptyBlockEnd :<: f) => Cxt h f a BlockEndL
pattern EmptyBlockEnd' <- (project -> Just EmptyBlockEnd) where
  EmptyBlockEnd' = jEmptyBlockEnd

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

pattern Block' :: (Block :<: f) => Cxt h f a [BlockItemL] -> Cxt h f a BlockEndL -> Cxt h f a BlockL
pattern Block' xs e <- (project -> Just (Block xs e)) where
  Block' xs e = jBlock xs e

-- | This is inserted at the end of blocks
-- so that there's a place to insert at the end of blocks,
-- and so that the sort of empty block-item lists can be correctly determined
--
-- This is a bit of a hack....but, it's actually kinda nice in some ways
data EmptyBlockItem :: Node where
  EmptyBlockItem :: EmptyBlockItem e BlockItemL

deriveAll [''EmptyBlockItem]

pattern EmptyBlockItem' :: (EmptyBlockItem :<: f) => Cxt h f a BlockItemL
pattern EmptyBlockItem' <- (project -> Just EmptyBlockItem) where
  EmptyBlockItem' = jEmptyBlockItem
