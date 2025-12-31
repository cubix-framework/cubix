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

transBinaryExpr :: F.MoveTerm F.BinaryExpressionL -> Maybe (MSuiMoveTerm F.HiddenExpressionL)
transBinaryExpr (F.BinaryExpression1' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' LogicOr' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression2' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' LogicOr' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression3' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' LogicAnd' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression4' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Eq' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression5' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Neq' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression6' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Lt' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression7' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Gt' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression8' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Lte' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression9' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Gte' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression10' lhs _ rhs) =
  -- Range operator (..) - no direct equivalent in parametric syntax, keep as-is
  Just $ F.iHiddenExpressionBinaryExpression $ F.iBinaryExpression10 (translate lhs) (inject F.Range) (translate rhs)
transBinaryExpr (F.BinaryExpression11' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' BitOr' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression12' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' BitXor' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression13' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' BitAnd' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression14' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Shl' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression15' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' ArithShr' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression16' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Add' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression17' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Sub' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression18' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Mul' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression19' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Div' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr (F.BinaryExpression20' lhs _ rhs) =
  Just $ iExpressionIsHiddenExpression $ Binary' Mod' (injF $ translate lhs) (injF $ translate rhs)
transBinaryExpr _ = Nothing

instance Trans F.HiddenExpression where
  trans he@(F.HiddenExpressionBinaryExpression be) =
    case transBinaryExpr be of
      Just result -> result
      Nothing -> transDefault he
  trans x = transDefault x

-------- Block

transBlock :: F.MoveTerm F.BlockL -> MSuiMoveTerm BlockL
transBlock (F.Block' useDecls blockItems maybeExpr) =
  let -- Extract, translate, and inject use declarations as BlockItems
      translatedUseDecls = map (\ud -> iUseDeclarationIsBlockItem (injF $ translate ud)) (extractF useDecls)
      -- Extract, translate, and inject block items as BlockItems
      translatedBlockItems = map (\bi -> iBlockItemIsBlockItem (injF $ translate bi)) (extractF blockItems)
      -- Combine all items
      allItems = insertF $ translatedUseDecls ++ translatedBlockItems
      -- Translate the optional final expression into BlockEnd
      blockEnd = iSuiMoveBlockEnd (mapF (injF . translate) maybeExpr)
  in Block' allItems blockEnd

instance Trans F.Block where
  trans b@(F.Block _ _ _) = iBlockIsBlock (transBlock $ inject b)

transUnitExpression :: F.MoveTerm F.UnitExpressionL -> MSuiMoveTerm ()
transUnitExpression F.UnitExpression' = UnitF'

instance Trans F.UnitExpression where
  trans F.UnitExpression = iUnitIsUnitExpression (transUnitExpression $ inject F.UnitExpression)

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
   let targTs = map ConT $ (ipsNames \\ F.moveSigNames) \\ [''IdentIsIdentifier, ''ExpressionIsHiddenExpression, ''BlockIsBlock, ''UnitIsUnitExpression]
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

-------- Binary Expressions

instance {-# OVERLAPPING #-} Untrans ExpressionIsHiddenExpression where
  untrans (ExpressionIsHiddenExpression (Binary' op lhs rhs)) =
    F.iHiddenExpressionBinaryExpression $ untransBinaryOp op (fromProjF lhs) (fromProjF rhs)
  untrans (ExpressionIsHiddenExpression e) =
    error $ "Cannot untranslate ExpressionIsHiddenExpression for non-Binary expression: " ++ show e

untransBinaryOp :: MSuiMoveTerm BinaryOpL -> MSuiMoveTerm F.HiddenExpressionL -> MSuiMoveTerm F.HiddenExpressionL -> F.MoveTerm F.BinaryExpressionL
untransBinaryOp LogicOr' lhs rhs  = F.iBinaryExpression2 (untranslate lhs) (inject F.Or) (untranslate rhs)
untransBinaryOp LogicAnd' lhs rhs = F.iBinaryExpression3 (untranslate lhs) (inject F.And) (untranslate rhs)
untransBinaryOp Eq' lhs rhs       = F.iBinaryExpression4 (untranslate lhs) (inject F.Eq) (untranslate rhs)
untransBinaryOp Neq' lhs rhs      = F.iBinaryExpression5 (untranslate lhs) (inject F.Neq) (untranslate rhs)
untransBinaryOp Lt' lhs rhs       = F.iBinaryExpression6 (untranslate lhs) (inject F.Lt) (untranslate rhs)
untransBinaryOp Gt' lhs rhs       = F.iBinaryExpression7 (untranslate lhs) (inject F.Gt) (untranslate rhs)
untransBinaryOp Lte' lhs rhs      = F.iBinaryExpression8 (untranslate lhs) (inject F.Le) (untranslate rhs)
untransBinaryOp Gte' lhs rhs      = F.iBinaryExpression9 (untranslate lhs) (inject F.Ge) (untranslate rhs)
untransBinaryOp BitOr' lhs rhs    = F.iBinaryExpression11 (untranslate lhs) (inject F.Bitor) (untranslate rhs)
untransBinaryOp BitXor' lhs rhs   = F.iBinaryExpression12 (untranslate lhs) (inject F.Xor) (untranslate rhs)
untransBinaryOp BitAnd' lhs rhs   = F.iBinaryExpression13 (untranslate lhs) (inject F.Bitand) (untranslate rhs)
untransBinaryOp Shl' lhs rhs      = F.iBinaryExpression14 (untranslate lhs) (inject F.Shl) (untranslate rhs)
untransBinaryOp ArithShr' lhs rhs = F.iBinaryExpression15 (untranslate lhs) (inject F.Shr) (untranslate rhs)
untransBinaryOp Add' lhs rhs      = F.iBinaryExpression16 (untranslate lhs) (inject F.Add) (untranslate rhs)
untransBinaryOp Sub' lhs rhs      = F.iBinaryExpression17 (untranslate lhs) (inject F.Sub) (untranslate rhs)
untransBinaryOp Mul' lhs rhs      = F.iBinaryExpression18 (untranslate lhs) (inject F.Mul) (untranslate rhs)
untransBinaryOp Div' lhs rhs      = F.iBinaryExpression19 (untranslate lhs) (inject F.Div) (untranslate rhs)
untransBinaryOp Mod' lhs rhs      = F.iBinaryExpression20 (untranslate lhs) (inject F.Mod) (untranslate rhs)
untransBinaryOp _ _ _             = error "untransBinaryOp: unsupported operator"

-------- Block

untransBlock :: MSuiMoveTerm BlockL -> F.MoveTerm F.BlockL
untransBlock (Block' items (projF -> Just (SuiMoveBlockEnd' maybeExpr))) =
  let -- Extract all items and separate UseDeclarations from BlockItems
      allItems = extractF items
      (useDecls, blockItems) = partitionEithers $ map separateItem allItems
  in F.iBlock
      (insertF useDecls)
      (insertF blockItems)
      (mapF untranslate maybeExpr)
  where
    separateItem :: MSuiMoveTerm BlockItemL -> Either (F.MoveTerm F.UseDeclarationL) (F.MoveTerm F.BlockItemL)
    separateItem (projF -> Just (UseDeclarationIsBlockItem' ud)) = Left (untranslate $ fromProjF ud)
    separateItem (projF -> Just (BlockItemIsBlockItem' bi)) = Right (untranslate $ fromProjF bi)
    separateItem item = error $ "untransBlock: unexpected BlockItem type: " ++ show item

instance {-# OVERLAPPING #-} Untrans BlockIsBlock where
  untrans (BlockIsBlock b) = untransBlock b

untransUnitExpression :: MSuiMoveTerm () -> F.MoveTerm F.UnitExpressionL
untransUnitExpression UnitF' = F.UnitExpression'

instance {-# OVERLAPPING #-} Untrans UnitIsUnitExpression where
  untrans (UnitIsUnitExpression u) = untransUnitExpression u
