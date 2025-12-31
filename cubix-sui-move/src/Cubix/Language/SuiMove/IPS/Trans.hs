{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cubix.Language.SuiMove.IPS.Trans (
    translate
  , untranslate
  ) where

import Data.Either ( partitionEithers )
import Data.List( (\\) )
import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Data.Comp.Multi ( project, project', inject, unTerm, caseCxt, Sum, All, HFunctor(..), (:-<:) )

import Cubix.Language.SuiMove.IPS.Types
import Cubix.Language.SuiMove.Modularized qualified as Modularized
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
translate :: Modularized.MoveTerm l -> MSuiMoveTerm l
translate = trans . unTerm @(Sum Modularized.MoveSig)

translate' :: (InjF MSuiMoveSig l l') => Modularized.MoveTerm l -> MSuiMoveTerm l'
translate' = injF . translate
-- CODE_GUARD_END

-- CODE_GUARD_START
-- Name: Trans class
-- Description: Standard interface for translation
class Trans f where
  trans :: f Modularized.MoveTerm l -> MSuiMoveTerm l
-- CODE_GUARD_END

-- CODE_GUARD_START
-- Name: Default and standard cases
-- Description: Instances for standard cubix fragments
instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt @Trans trans

transDefault :: (HFunctor f, f :-<: MSuiMoveSig, f :-<: Modularized.MoveSig) => f Modularized.MoveTerm l -> MSuiMoveTerm l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MSuiMoveSig, f :-<: Modularized.MoveSig) => Trans f where
  trans = transDefault
-- CODE_GUARD_END

---------------------------------
-------------- Per-fragment Instances
---------------------------------

-------- Identifiers
transIdent :: Modularized.MoveTerm Modularized.IdentifierL -> MSuiMoveTerm IdentL
transIdent (Modularized.Identifier' t) = Ident' (Text.unpack t)

-- Clone of transIdent because type-safe pattern match
instance Trans Modularized.Identifier where
  trans (Modularized.Identifier n) = iIdent (Text.unpack n)

-------- Binary Expressions

transBinaryExpr :: Modularized.MoveTerm Modularized.BinaryExpressionL -> MSuiMoveTerm ExpressionL
transBinaryExpr (Modularized.BinaryExpression1' lhs _ rhs) =
  Binary' LogicOr' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression2' lhs _ rhs) =
  Binary' LogicOr' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression3' lhs _ rhs) =
  Binary' LogicAnd' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression4' lhs _ rhs) =
  Binary' Eq' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression5' lhs _ rhs) =
  Binary' Neq' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression6' lhs _ rhs) =
  Binary' Lt' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression7' lhs _ rhs) =
  Binary' Gt' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression8' lhs _ rhs) =
  Binary' Lte' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression9' lhs _ rhs) =
  Binary' Gte' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression10' lhs _ rhs) =
  -- Range operator (..) - no direct equivalent in parametric syntax, keep as-is
  Modularized.iBinaryExpression10 (translate lhs) Modularized.iRange (translate rhs)
transBinaryExpr (Modularized.BinaryExpression11' lhs _ rhs) =
  Binary' BitOr' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression12' lhs _ rhs) =
  Binary' BitXor' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression13' lhs _ rhs) =
  Binary' BitAnd' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression14' lhs _ rhs) =
  Binary' Shl' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression15' lhs _ rhs) =
  Binary' ArithShr' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression16' lhs _ rhs) =
  Binary' Add' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression17' lhs _ rhs) =
  Binary' Sub' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression18' lhs _ rhs) =
  Binary' Mul' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression19' lhs _ rhs) =
  Binary' Div' (translate' lhs) (translate' rhs)
transBinaryExpr (Modularized.BinaryExpression20' lhs _ rhs) =
  Binary' Mod' (translate' lhs) (translate' rhs)

instance Trans Modularized.HiddenExpression where
  trans (Modularized.HiddenExpressionBinaryExpression be) = injF $ transBinaryExpr be
  trans x = transDefault x

-------- Block

transBlock :: Modularized.MoveTerm Modularized.BlockL -> MSuiMoveTerm BlockL
transBlock (Modularized.Block' useDecls blockItems maybeExpr) =
  let -- Extract, translate, and inject use declarations as BlockItems
      translatedUseDecls = map (iUseDeclarationIsBlockItem . translate') (extractF useDecls)
      -- Extract, translate, and inject block items as BlockItems
      translatedBlockItems = map (iBlockItemIsBlockItem . translate') (extractF blockItems)
      -- Combine all items
      allItems = insertF $ translatedUseDecls ++ translatedBlockItems
      -- Translate the optional final expression into BlockEnd
      blockEnd = iSuiMoveBlockEnd (mapF (injF . translate) maybeExpr)
  in Block' allItems blockEnd

instance Trans Modularized.Block where
  trans b@(Modularized.Block {}) = iBlockIsBlock (transBlock $ inject b)

transUnitExpression :: Modularized.MoveTerm Modularized.UnitExpressionL -> MSuiMoveTerm ()
transUnitExpression Modularized.UnitExpression' = UnitF'

instance Trans Modularized.UnitExpression where
  trans Modularized.UnitExpression = iUnitIsUnitExpression (transUnitExpression $ inject Modularized.UnitExpression)

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
  untrans :: f MSuiMoveTerm l -> Modularized.MoveTerm l
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
untransError :: (HFunctor f, f :-<: MSuiMoveSig) => f MSuiMoveTerm l -> Modularized.MoveTerm l
untransError t = error $ "Cannot untranslate root node: " ++ show (inject t)
-- CODE_GUARD_END

do ipsNames <- sumToNames ''MSuiMoveSig
   let targTs = map ConT $ (ipsNames \\ Modularized.moveSigNames) \\ [''IdentIsIdentifier, ''ExpressionIsHiddenExpression, ''BlockIsBlock, ''UnitIsUnitExpression]
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)

-- CODE_GUARD_START
-- Name: Default cases
-- Description: Instances fragments based on constraints
untransDefault :: (HFunctor f, f :-<: Modularized.MoveSig) => f MSuiMoveTerm l -> Modularized.MoveTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: Modularized.MoveSig) => Untrans f where
  untrans = untransDefault
-- CODE_GUARD_END

-- CODE_GUARD_START
-- Name: Top-level definition
-- Description: Entry point for untranslation
untranslate :: MSuiMoveTerm l -> Modularized.MoveTerm l
untranslate = untrans . unTerm

untranslate' :: InjF MSuiMoveSig l l' => MSuiMoveTerm l' -> Modularized.MoveTerm l
untranslate' = untranslate . fromProjF
-- CODE_GUARD_END

--------------------------------
------------- Per-fragment instances
--------------------------------

-------- Identifiers

untransIdent :: MSuiMoveTerm IdentL -> Modularized.MoveTerm Modularized.IdentifierL
untransIdent (Ident' s) = Modularized.iIdentifier (Text.pack s)

instance {-# OVERLAPPING #-} Untrans IdentIsIdentifier where
  untrans (IdentIsIdentifier n) = untransIdent n

-------- Binary Expressions

instance {-# OVERLAPPING #-} Untrans ExpressionIsHiddenExpression where
  untrans (ExpressionIsHiddenExpression (Binary' op lhs rhs)) =
    Modularized.iHiddenExpressionBinaryExpression $ untransBinaryOp op (fromProjF lhs) (fromProjF rhs)
  untrans (ExpressionIsHiddenExpression e) =
    error $ "Cannot untranslate ExpressionIsHiddenExpression for non-Binary expression: " ++ show e

untransBinaryOp :: MSuiMoveTerm BinaryOpL -> MSuiMoveTerm Modularized.HiddenExpressionL -> MSuiMoveTerm Modularized.HiddenExpressionL -> Modularized.MoveTerm Modularized.BinaryExpressionL
untransBinaryOp LogicOr' lhs rhs  = Modularized.iBinaryExpression2 (untranslate lhs) (inject Modularized.Or) (untranslate rhs)
untransBinaryOp LogicAnd' lhs rhs = Modularized.iBinaryExpression3 (untranslate lhs) (inject Modularized.And) (untranslate rhs)
untransBinaryOp Eq' lhs rhs       = Modularized.iBinaryExpression4 (untranslate lhs) (inject Modularized.Eq) (untranslate rhs)
untransBinaryOp Neq' lhs rhs      = Modularized.iBinaryExpression5 (untranslate lhs) (inject Modularized.Neq) (untranslate rhs)
untransBinaryOp Lt' lhs rhs       = Modularized.iBinaryExpression6 (untranslate lhs) (inject Modularized.Lt) (untranslate rhs)
untransBinaryOp Gt' lhs rhs       = Modularized.iBinaryExpression7 (untranslate lhs) (inject Modularized.Gt) (untranslate rhs)
untransBinaryOp Lte' lhs rhs      = Modularized.iBinaryExpression8 (untranslate lhs) (inject Modularized.Le) (untranslate rhs)
untransBinaryOp Gte' lhs rhs      = Modularized.iBinaryExpression9 (untranslate lhs) (inject Modularized.Ge) (untranslate rhs)
untransBinaryOp BitOr' lhs rhs    = Modularized.iBinaryExpression11 (untranslate lhs) (inject Modularized.Bitor) (untranslate rhs)
untransBinaryOp BitXor' lhs rhs   = Modularized.iBinaryExpression12 (untranslate lhs) (inject Modularized.Xor) (untranslate rhs)
untransBinaryOp BitAnd' lhs rhs   = Modularized.iBinaryExpression13 (untranslate lhs) (inject Modularized.Bitand) (untranslate rhs)
untransBinaryOp Shl' lhs rhs      = Modularized.iBinaryExpression14 (untranslate lhs) (inject Modularized.Shl) (untranslate rhs)
untransBinaryOp ArithShr' lhs rhs = Modularized.iBinaryExpression15 (untranslate lhs) (inject Modularized.Shr) (untranslate rhs)
untransBinaryOp Add' lhs rhs      = Modularized.iBinaryExpression16 (untranslate lhs) (inject Modularized.Add) (untranslate rhs)
untransBinaryOp Sub' lhs rhs      = Modularized.iBinaryExpression17 (untranslate lhs) (inject Modularized.Sub) (untranslate rhs)
untransBinaryOp Mul' lhs rhs      = Modularized.iBinaryExpression18 (untranslate lhs) (inject Modularized.Mul) (untranslate rhs)
untransBinaryOp Div' lhs rhs      = Modularized.iBinaryExpression19 (untranslate lhs) (inject Modularized.Div) (untranslate rhs)
untransBinaryOp Mod' lhs rhs      = Modularized.iBinaryExpression20 (untranslate lhs) (inject Modularized.Mod) (untranslate rhs)
untransBinaryOp _ _ _             = error "untransBinaryOp: unsupported operator"

-------- Block

untransBlock :: MSuiMoveTerm BlockL -> Modularized.MoveTerm Modularized.BlockL
untransBlock (Block' items (projF -> Just (SuiMoveBlockEnd' maybeExpr))) =
  let -- Extract all items and separate UseDeclarations from BlockItems
      allItems = extractF items
      (useDecls, blockItems) = partitionEithers $ map separateItem allItems
  in Modularized.iBlock
      (insertF useDecls)
      (insertF blockItems)
      (mapF untranslate maybeExpr)
  where
    separateItem :: MSuiMoveTerm BlockItemL -> Either (Modularized.MoveTerm Modularized.UseDeclarationL) (Modularized.MoveTerm Modularized.BlockItemL)
    separateItem (projF -> Just (UseDeclarationIsBlockItem' ud)) = Left (untranslate $ fromProjF ud)
    separateItem (projF -> Just (BlockItemIsBlockItem' bi)) = Right (untranslate $ fromProjF bi)
    separateItem item = error $ "untransBlock: unexpected BlockItem type: " ++ show item

instance {-# OVERLAPPING #-} Untrans BlockIsBlock where
  untrans (BlockIsBlock b) = untransBlock b

untransUnitExpression :: MSuiMoveTerm () -> Modularized.MoveTerm Modularized.UnitExpressionL
untransUnitExpression UnitF' = Modularized.UnitExpression'

instance {-# OVERLAPPING #-} Untrans UnitIsUnitExpression where
  untrans (UnitIsUnitExpression u) = untransUnitExpression u
