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
  , AssignOpAdd(..)
  , AssignOpSub(..)
  , AssignOpMul(..)
  , AssignOpDiv(..)
  , AssignOpMod(..)
  , AssignOpBitAnd(..)
  , AssignOpBitOr(..)
  , AssignOpBitXor(..)
  , AssignOpArithShr(..)
  , AssignOpLogicShr(..)
  , AssignOpShl(..)
  , LhsL
  , RhsL
  , AssignL
  , Assign(..)

  -- ** Constructors and Patterns

  , pattern AssignOpEquals'
  ,        iAssignOpEquals
  ,        jAssignOpEquals
  , pattern AssignOpAdd'
  ,        iAssignOpAdd
  ,        jAssignOpAdd
  , pattern AssignOpSub'
  ,        iAssignOpSub
  ,        jAssignOpSub
  , pattern AssignOpMul'
  ,        iAssignOpMul
  ,        jAssignOpMul
  , pattern AssignOpDiv'
  ,        iAssignOpDiv
  ,        jAssignOpDiv
  , pattern AssignOpMod'
  ,        iAssignOpMod
  ,        jAssignOpMod
  , pattern AssignOpBitAnd'
  ,        iAssignOpBitAnd
  ,        jAssignOpBitAnd
  , pattern AssignOpBitOr'
  ,        iAssignOpBitOr
  ,        jAssignOpBitOr
  , pattern AssignOpBitXor'
  ,        iAssignOpBitXor
  ,        jAssignOpBitXor
  , pattern AssignOpArithShr'
  ,        iAssignOpArithShr
  ,        jAssignOpArithShr
  , pattern AssignOpLogicShr'
  ,        iAssignOpLogicShr
  ,        jAssignOpLogicShr
  , pattern AssignOpShl'
  ,        iAssignOpShl
  ,        jAssignOpShl
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

import Data.Comp.Multi ( Node, project', HFunctor, (:-<:), All)
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF

data IdentL
data Ident :: Node where
  Ident :: String -> Ident e IdentL

deriveAll [''Ident]

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

data LocalVarDeclAttrsL

-- Needs better name because may need to distinguish part of multi-decl from a standalone decl
------ That's a really hypothetical thing. Why, Jimmy, why

data EmptyLocalVarDeclAttrs :: Node where
  EmptyLocalVarDeclAttrs :: EmptyLocalVarDeclAttrs e LocalVarDeclAttrsL

deriveAll [''EmptyLocalVarDeclAttrs]

data VarDeclBinderL
-- | Represents declaring a list of identifiers, where the list of identifiers
-- should be thought of a single entity being declared. Not to be used as the LHS of an assignment
--
-- This models the (x,y) = (1,2) pattern found in Haskell, or "local x,y" in Lua
data TupleBinder e l where
  TupleBinder :: e [IdentL] -> TupleBinder e VarDeclBinderL

deriveAll [''TupleBinder]


instance (TupleBinder :-<: fs, All HFunctor fs) => InjF fs [IdentL] VarDeclBinderL where
  injF = iTupleBinder

  projF' (project' -> Just (TupleBinder ns)) = Just ns
  projF' _                                   = Nothing

createSortInclusionType ''IdentL ''VarDeclBinderL
deriveAllButSortInjection [''IdentIsVarDeclBinder]
createSortInclusionInfer ''IdentL ''VarDeclBinderL


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


data EmptyMultiLocalVarDeclCommonAttrs :: Node where
  EmptyMultiLocalVarDeclCommonAttrs :: EmptyMultiLocalVarDeclCommonAttrs e MultiLocalVarDeclCommonAttrsL

deriveAll [''EmptyMultiLocalVarDeclCommonAttrs]

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


data AssignOpL

-- |
-- See spec for assign 'Assign'
--
-- We assume that 'AssignOpEquals' has, as its associated f_op, the function f(x,y)=(typeof x)y
-- where (typeof x)y denotes a type conversion from y to the type of x. We leave it unspecified what
-- exactly that means
data AssignOpEquals :: Node where
  AssignOpEquals :: AssignOpEquals e AssignOpL

-- |
-- See spec for 'Assign'.
--
-- The @f_op@ for 'AssignOpAdd' is the same as 'Add' semantics.
data AssignOpAdd :: Node where
  AssignOpAdd :: AssignOpAdd e AssignOpL

-- |
-- See spec for 'Assign'.
--
-- The @f_op@ for 'AssignOpSub' is the same as 'Sub' semantics.
data AssignOpSub :: Node where
  AssignOpSub :: AssignOpSub e AssignOpL

-- |
-- See spec for 'Assign'.
--
-- The @f_op@ for 'AssignOpMul' is the same as 'Mul' semantics.
data AssignOpMul :: Node where
  AssignOpMul :: AssignOpMul e AssignOpL

-- |
-- See spec for 'Assign'.
--
-- The @f_op@ for 'AssignOpDiv' is the same as 'Div' semantics.
data AssignOpDiv :: Node where
  AssignOpDiv :: AssignOpDiv e AssignOpL

-- |
-- See spec for 'Assign'.
--
-- The @f_op@ for 'AssignOpMod' is the same as 'Mod' semantics.
data AssignOpMod :: Node where
  AssignOpMod :: AssignOpMod e AssignOpL

-- |
-- See spec for 'Assign'.
--
-- The @f_op@ for 'AssignOpBitAnd' is the same as 'BitAnd' semantics.
data AssignOpBitAnd :: Node where
  AssignOpBitAnd :: AssignOpBitAnd e AssignOpL

-- |
-- See spec for 'Assign'.
--
-- The @f_op@ for 'AssignOpBitOr' is the same as 'BitOr' semantics.
data AssignOpBitOr :: Node where
  AssignOpBitOr :: AssignOpBitOr e AssignOpL

-- |
-- See spec for 'Assign'.
--
-- The @f_op@ for 'AssignOpBitXor' is the same as 'BitXor' semantics.
data AssignOpBitXor :: Node where
  AssignOpBitXor :: AssignOpBitXor e AssignOpL

-- |
-- See spec for 'Assign'.
--
-- The @f_op@ for 'AssignOpArithShr' is the same as 'ArithShr' semantics.
data AssignOpArithShr :: Node where
  AssignOpArithShr :: AssignOpArithShr e AssignOpL

-- |
-- See spec for 'Assign'.
--
-- The @f_op@ for 'AssignOpLogicShr' is the same as 'Shr' semantics.
data AssignOpLogicShr :: Node where
  AssignOpLogicShr :: AssignOpLogicShr e AssignOpL

-- |
-- See spec for 'Assign'.
--
-- The @f_op@ for 'AssignOpShl' is the same as 'Shl' semantics.
data AssignOpShl :: Node where
  AssignOpShl :: AssignOpShl e AssignOpL

deriveAll
  [ ''AssignOpEquals, ''AssignOpAdd, ''AssignOpSub, ''AssignOpMul, ''AssignOpDiv, ''AssignOpMod
  , ''AssignOpBitAnd, ''AssignOpBitOr, ''AssignOpBitXor
  , ''AssignOpArithShr, ''AssignOpLogicShr, ''AssignOpShl
  ]

data LhsL
data RhsL

data AssignL

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
data Assign e l where
  Assign :: e LhsL -> e AssignOpL -> e RhsL -> Assign e AssignL

deriveAll [''Assign]

data BlockItemL
data BlockEndL
data BlockL

data EmptyBlockEnd :: Node where
  EmptyBlockEnd :: EmptyBlockEnd e BlockEndL

deriveAll [''EmptyBlockEnd]

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

-- | This is inserted at the end of blocks
-- so that there's a place to insert at the end of blocks,
-- and so that the sort of empty block-item lists can be correctly determined
--
-- This is a bit of a hack....but, it's actually kinda nice in some ways
data EmptyBlockItem :: Node where
  EmptyBlockItem :: EmptyBlockItem e BlockItemL

deriveAll [''EmptyBlockItem]
