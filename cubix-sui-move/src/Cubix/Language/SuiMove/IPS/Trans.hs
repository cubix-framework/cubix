{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cubix.Language.SuiMove.IPS.Trans (
    translate
  , untranslate
  ) where

import Data.List( (\\) )
import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Data.Comp.Multi ( project, inject, unTerm, caseCxt, Sum, All, HFunctor(..), (:-<:) )

import Cubix.Language.SuiMove.IPS.Types
import Cubix.Language.SuiMove.Modularized qualified as F
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax
import Data.Typeable (Typeable)

------------------------------------------------------------------------------------
---------------- Forward translation: Modularized syntax to IPS  -------------------
------------------------------------------------------------------------------------

--------------------------------
------------- Top-level translate
--------------------------------

-- CODE_GUARD_START
-- Name: Top-level definition
-- Description: Entry point for translation
translate :: F.MoveTerm l -> MSuiMoveTerm l
translate = trans . unTerm @(Sum F.MoveSig)

translate' :: (InjF MSuiMoveSig l l') => F.MoveTerm l -> MSuiMoveTerm l'
translate' = injF . translate
-- CODE_GUARD_END

-- CODE_GUARD_START
-- Name: Trans class
-- Description: Standard interface for translation
class Trans f where
  trans :: f F.MoveTerm l -> MSuiMoveTerm l
-- CODE_GUARD_END

-- CODE_GUARD_START
-- Name: Default and standard cases
-- Description: Instances for standard cubix fragments
instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt @Trans trans

transDefault :: (HFunctor f, f :-<: MSuiMoveSig, f :-<: F.MoveSig) => f F.MoveTerm l -> MSuiMoveTerm l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MSuiMoveSig, f :-<: F.MoveSig) => Trans f where
  trans = transDefault
-- CODE_GUARD_END

---------------------------------
-------------- Per-fragment Instances
---------------------------------

-------- Identifiers

transIdent :: F.MoveTerm F.IdentifierL -> MSuiMoveTerm IdentL
transIdent (project -> Just (F.Identifier t)) = Ident' (Text.unpack t)

-- Clone of transIdent because type-safe pattern match
instance Trans F.Identifier where
  trans (F.Identifier n) = iIdent (Text.unpack n)

-------- Binary Expressions

-- Translate binary operators to parametric BinaryOpL when possible
transBinOp :: F.BinaryExpression F.MoveTerm l -> Maybe (MSuiMoveTerm BinaryOpL, F.MoveTerm F.HiddenExpressionL, F.MoveTerm F.HiddenExpressionL)
transBinOp (F.BinaryExpression2 a _ b) = Just (LogicOr', a, b)   -- ||
transBinOp (F.BinaryExpression3 a _ b) = Just (LogicAnd', a, b)  -- &&
transBinOp (F.BinaryExpression4 a _ b) = Just (Eq', a, b)        -- ==
transBinOp (F.BinaryExpression5 a _ b) = Just (Neq', a, b)       -- !=
transBinOp (F.BinaryExpression6 a _ b) = Just (Lt', a, b)        -- <
transBinOp (F.BinaryExpression7 a _ b) = Just (Gt', a, b)        -- >
transBinOp (F.BinaryExpression8 a _ b) = Just (Lte', a, b)       -- <=
transBinOp (F.BinaryExpression9 a _ b) = Just (Gte', a, b)       -- >=
transBinOp (F.BinaryExpression11 a _ b) = Just (BitOr', a, b)    -- |
transBinOp (F.BinaryExpression12 a _ b) = Just (BitXor', a, b)   -- ^
transBinOp (F.BinaryExpression13 a _ b) = Just (BitAnd', a, b)   -- &
transBinOp (F.BinaryExpression14 a _ b) = Just (Shl', a, b)      -- <<
transBinOp (F.BinaryExpression15 a _ b) = Just (LogicShr', a, b) -- >>
transBinOp (F.BinaryExpression16 a _ b) = Just (Add', a, b)      -- +
transBinOp (F.BinaryExpression17 a _ b) = Just (Sub', a, b)      -- -
transBinOp (F.BinaryExpression18 a _ b) = Just (Mul', a, b)      -- *
transBinOp (F.BinaryExpression19 a _ b) = Just (Div', a, b)      -- /
transBinOp (F.BinaryExpression20 a _ b) = Just (Mod', a, b)      -- %
transBinOp _ = Nothing

-- Translate HiddenExpression nodes, converting BinaryExpressions to parametric Binary
-- We use a helper function to safely extract ExpressionL from translated HiddenExpression
safeFromProjF :: MSuiMoveTerm F.HiddenExpressionL -> Maybe (MSuiMoveTerm ExpressionL)
safeFromProjF term = case project term of
  Just (ExpressionIsHiddenExpression expr) -> Just expr
  _ -> Nothing

instance Trans F.HiddenExpression where
  trans (F.HiddenExpressionBinaryExpression binExpr) = 
    case project binExpr of
      Just binExpr' -> case transBinOp binExpr' of
        Just (op, a, b) -> 
          let a' = translate a
              b' = translate b
          in case (safeFromProjF a', safeFromProjF b') of
            (Just aExpr, Just bExpr) ->
              let binary = iBinary op aExpr bExpr
              in inject $ ExpressionIsHiddenExpression binary
            _ -> transDefault (F.HiddenExpressionBinaryExpression binExpr)
        Nothing -> transDefault (F.HiddenExpressionBinaryExpression binExpr)
      Nothing -> transDefault (F.HiddenExpressionBinaryExpression binExpr)
  trans x = transDefault x

------------------------------------------------------------------------------------
---------------- Reverse translation: IPS to modularized syntax  -------------------
------------------------------------------------------------------------------------

--------------------------------
------------- Top-level untranslate
--------------------------------

-- CODE_GUARD_START
-- Name: Untrans class
-- Description: Contains top-level function on sigs
class Untrans f where
  untrans :: f MSuiMoveTerm l -> F.MoveTerm l
-- CODE_GUARD_END

------ Default and standard cases

-- CODE_GUARD_START
-- Name: Standard cases
-- Description: Instances for standard cubix fragments
instance {-# OVERLAPPING #-} (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt @Untrans untrans
-- CODE_GUARD_END

-- CODE_GUARD_START
-- Name: Error reporting function
-- Description: For cases when untrans should fail
untransError :: (HFunctor f, f :-<: MSuiMoveSig) => f MSuiMoveTerm l -> F.MoveTerm l
untransError t = error $ "Cannot untranslate root node: " ++ show (inject t)
-- CODE_GUARD_END

do ipsNames <- sumToNames ''MSuiMoveSig
   let targTs = map ConT $ (ipsNames \\ F.moveSigNames) \\ [''IdentIsIdentifier, ''ExpressionIsHiddenExpression]
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)

-- CODE_GUARD_START
-- Name: Default cases
-- Description: Instances fragments based on constraints
untransDefault :: (HFunctor f, f :-<: F.MoveSig) => f MSuiMoveTerm l -> F.MoveTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: F.MoveSig) => Untrans f where
  untrans = untransDefault
-- CODE_GUARD_END

-- CODE_GUARD_START
-- Name: Top-level definition
-- Description: Entry point for untranslation
untranslate :: MSuiMoveTerm l -> F.MoveTerm l
untranslate = untrans . unTerm

untranslate' :: InjF MSuiMoveSig l l' => MSuiMoveTerm l' -> F.MoveTerm l
untranslate' = untranslate . fromProjF
-- CODE_GUARD_END

--------------------------------
------------- Per-fragment instances
--------------------------------

-------- Identifiers

untransIdent :: MSuiMoveTerm IdentL -> F.MoveTerm F.IdentifierL
untransIdent (Ident' s) = F.iIdentifier (Text.pack s)

instance {-# OVERLAPPING #-} Untrans IdentIsIdentifier where
  untrans (IdentIsIdentifier n) = untransIdent n

-------- Expressions

-- Untrans instance for ExpressionIsHiddenExpression sort injection
instance {-# OVERLAPPING #-} Untrans ExpressionIsHiddenExpression where
  untrans (ExpressionIsHiddenExpression e) = untransExpression e

-------- Binary Expressions

-- Helper to untranslate expressions
untransExpression :: MSuiMoveTerm ExpressionL -> F.MoveTerm F.HiddenExpressionL
untransExpression (Binary' op a b) = untransBinary op a b
untransExpression e = 
  let hiddenExpr :: MSuiMoveTerm F.HiddenExpressionL
      hiddenExpr = injF e
  in untranslate hiddenExpr

untransBinary
  :: MSuiMoveTerm BinaryOpL
  -> MSuiMoveTerm ExpressionL
  -> MSuiMoveTerm ExpressionL
  -> F.MoveTerm F.HiddenExpressionL
untransBinary op a b =
  let a' = untransExpression a
      b' = untransExpression b
      -- Create the binary expression based on the operator
      binExpr = case op of
        LogicOr'   -> F.iBinaryExpression2 a' F.Or' b'
        LogicAnd'  -> F.iBinaryExpression3 a' F.And' b'
        Eq'        -> F.iBinaryExpression4 a' F.Eq' b'
        Neq'       -> F.iBinaryExpression5 a' F.Neq' b'
        Lt'        -> F.iBinaryExpression6 a' F.Lt' b'
        Gt'        -> F.iBinaryExpression7 a' F.Gt' b'
        Lte'       -> F.iBinaryExpression8 a' F.Le' b'
        Gte'       -> F.iBinaryExpression9 a' F.Ge' b'
        BitOr'     -> F.iBinaryExpression11 a' F.Bitor' b'
        BitXor'    -> F.iBinaryExpression12 a' F.Xor' b'
        BitAnd'    -> F.iBinaryExpression13 a' F.Bitand' b'
        Shl'       -> F.iBinaryExpression14 a' F.Shl' b'
        LogicShr'  -> F.iBinaryExpression15 a' F.Shr' b'
        Add'       -> F.iBinaryExpression16 a' F.Add' b'
        Sub'       -> F.iBinaryExpression17 a' F.Sub' b'
        Mul'       -> F.iBinaryExpression18 a' F.Mul' b'
        Div'       -> F.iBinaryExpression19 a' F.Div' b'
        Mod'       -> F.iBinaryExpression20 a' F.Mod' b'
  in F.iHiddenExpressionBinaryExpression binExpr
