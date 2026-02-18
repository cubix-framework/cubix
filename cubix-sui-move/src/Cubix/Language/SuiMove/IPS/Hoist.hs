{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE UndecidableInstances #-}

-- | Hoist transformation support for Sui Move
--
-- This module provides the instances required for the hoist transformation
-- to work with Sui Move. The hoist transformation moves variable declarations
-- from nested blocks to the outermost enclosing block.
--
-- == Semantic Limitations
--
-- The hoist transformation may produce invalid Move code in certain cases
-- that are not caught by the existing shadowing checks:
--
-- * __Type inference failure__: @let x = vec.pop();@ becomes @let x; x = vec.pop();@
--   where the type of @x@ cannot be inferred without the initializer.
--
-- * __Complex binders__: Struct destructuring like @let MyStruct { f1, f2 } = expr;@
--   cannot be split into declaration and assignment.
--
-- * __Or-patterns__: @let (Ok(x) | Err(x)) = result;@ cannot be hoisted.
--
-- * __No-drop types__: Variables whose types lack the @drop@ ability cannot
--   remain uninitialized.
--
-- The 'BlockHoisting' instance attempts to block hoisting for complex binders.
module Cubix.Language.SuiMove.IPS.Hoist (
  -- * Re-exports for convenience
    module Cubix.Transformations.Hoist
  ) where

import Data.Constraint (Dict(..))
import Data.Text qualified as Text

import Cubix.Language.Parametric.InjF (InjF(..), projF)
import Cubix.Language.Parametric.Syntax
  ( BlockItemL, IdentL, LhsL, VarDeclBinderL
  , pattern Ident', pattern IdentIsVarDeclBinder', pattern SingleLocalVarDecl'
  , pattern TupleBinder', pattern Nothing', extractF
  )

import Cubix.Transformations.Hoist

import Cubix.Language.SuiMove.IPS.Types
import Cubix.Language.SuiMove.Modularized qualified as M

--------------------------------------------------------------------------------
-- StatSort type instance
--------------------------------------------------------------------------------

-- | Sui Move uses BlockItemL as its statement sort (like Lua, C, Java)
type instance StatSort MSuiMoveSig = BlockItemL

--------------------------------------------------------------------------------
-- SpecialHoistState type instance
--------------------------------------------------------------------------------

-- | Sui Move doesn't need special hoist state tracking
type instance SpecialHoistState MSuiMoveSig = ()

--------------------------------------------------------------------------------
-- VariableInsertionVariation instance
--------------------------------------------------------------------------------

-- | Sui Move uses single-declaration insertion (like Lua).
-- Each @let@ statement declares a single binding (though it may bind multiple
-- variables via destructuring).
instance (h MSuiMoveSig) => VariableInsertionVariation MSuiMoveSig g h where
  variableInsertionVariation _ _ _ = SingleDecInsertionVariation Dict

--------------------------------------------------------------------------------
-- VarInitToRhs instance
--------------------------------------------------------------------------------

-- | Convert a variable initializer to an RHS expression.
-- In Sui Move, initializers are just HiddenExpression, which can be directly
-- injected to RhsL.
instance VarInitToRhs MSuiMoveTerm where
  varInitToRhs _ _ _ init = case projF init of
    Just (expr :: MSuiMoveTerm M.HiddenExpressionL) -> injF expr
    Nothing -> error "varInitToRhs: unexpected initializer type"

--------------------------------------------------------------------------------
-- VarDeclBinderToLhs instance
--------------------------------------------------------------------------------

-- | Convert a variable binder to an assignment LHS.
--
-- This handles:
-- * Single identifiers: @let x = ...@ -> @x = ...@
-- * Tuple binders: @let (a, b) = ...@ -> @(a, b) = ...@
--
-- Complex binders (struct destructuring, or-patterns) cannot be converted
-- to LHS and will cause an error. These should be blocked by 'BlockHoisting'.
instance VarDeclBinderToLhs MSuiMoveTerm where
  -- Single identifier
  varDeclBinderToLhs (IdentIsVarDeclBinder' ident) = identToLhs ident

  -- Tuple of identifiers - not supported, should be blocked by BlockHoisting
  varDeclBinderToLhs (TupleBinder' _) =
    error "varDeclBinderToLhs: tuple binders not supported (should be blocked by BlockHoisting)"

  -- Complex binder preserved from original syntax (should be blocked by BlockHoisting)
  varDeclBinderToLhs (BinderIsVarDeclBinder' _) =
    error "varDeclBinderToLhs: cannot convert complex binder (struct/or-pattern) to LHS"

  varDeclBinderToLhs _ =
    error "varDeclBinderToLhs: unexpected binder type"

-- | Convert an identifier to an LHS expression
identToLhs :: MSuiMoveTerm IdentL -> MSuiMoveTerm LhsL
identToLhs ident = injF $ identToVarExpr ident

-- | Convert an identifier to a variable expression (HiddenUnaryExpression)
-- Creates a NameExpression with a simple ModuleAccess4 (identifier with no type args)
identToVarExpr :: MSuiMoveTerm IdentL -> MSuiMoveTerm M.HiddenUnaryExpressionL
identToVarExpr (Ident' name) =
  M.iHiddenUnaryExpression $
    M.iHiddenUnaryExpressionInternal0ExpressionTerm $
      M.iHiddenExpressionTermNameExpression $
        M.iNameExpression Nothing' $
          M.iModuleAccess4
            (M.iIdentifier (Text.pack name))
            Nothing'
identToVarExpr _ = error "identToVarExpr: expected Ident"

--------------------------------------------------------------------------------
-- BlockHoisting instance
--------------------------------------------------------------------------------

-- | Prevent hoisting in cases that would produce invalid Move code.
--
-- We block hoisting when the binder is a complex pattern (struct destructuring,
-- or-patterns) that cannot be converted to an assignment LHS.
instance BlockHoisting MSuiMoveSig where
  blockHoistSingle _ (SingleLocalVarDecl' _ binder _) = isComplexBinder binder
  blockHoistSingle _ _ = True  -- Unknown declaration type, block hoisting
  blockHoistMulti _ _ = False  -- Sui Move doesn't have multi-decl

-- | Check if a binder is too complex to hoist (tuple, struct destructuring, or-patterns)
isComplexBinder :: MSuiMoveTerm VarDeclBinderL -> Bool
isComplexBinder (IdentIsVarDeclBinder' _) = False
isComplexBinder (TupleBinder' _) = True  -- Tuple binders not supported for hoisting
isComplexBinder (BinderIsVarDeclBinder' bindList) = isComplexBindList bindList
isComplexBinder _ = True  -- Unknown binder type, be conservative

-- | Check if a BindList contains complex patterns
isComplexBindList :: MSuiMoveTerm M.BindListL -> Bool
isComplexBindList t = case t of
  -- Simple variable binding
  M.BindListBind' (M.HiddenBindBindInternal0' (M.HiddenBindInternal0VariableIdentifier' _)) -> False
  -- Mutable variable binding
  M.BindListBind' (M.HiddenBindBindInternal0' (M.HiddenBindInternal0MutBindVar' _)) -> False
  -- Tuple of simple bindings - check each element
  M.BindListCommaBindList' (M.CommaBindList' binds) ->
    any isComplexHiddenBind (extractF binds)
  -- Or-pattern - always complex
  M.BindListOrBindList' _ -> True
  -- Struct unpacking or other complex patterns
  _ -> True

-- | Check if a HiddenBind is complex
isComplexHiddenBind :: MSuiMoveTerm M.HiddenBindL -> Bool
isComplexHiddenBind (M.HiddenBindBindInternal0' (M.HiddenBindInternal0VariableIdentifier' _)) = False
isComplexHiddenBind (M.HiddenBindBindInternal0' (M.HiddenBindInternal0MutBindVar' _)) = False
isComplexHiddenBind _ = True  -- Struct unpacking, @ binding, etc.
