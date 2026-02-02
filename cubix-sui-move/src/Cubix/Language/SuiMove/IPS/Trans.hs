{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Cubix.Language.SuiMove.IPS.Trans (
    translate
  , untranslate
  ) where

import Data.Either ( partitionEithers )
import Data.List( (\\) )
import Data.Text qualified as Text
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Data.Comp.Multi ( inject, unTerm, caseCxt, Sum, All, HFunctor(..), (:-<:) )

import Cubix.Language.SuiMove.IPS.Types
import Cubix.Language.SuiMove.Modularized qualified as Modularized
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax

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
  -- Implies operator (..) - no direct equivalent in parametric syntax, keep as-is
  Modularized.iBinaryExpression1 (translate lhs) Modularized.iEqualsSignEqualsSignGreaterThanSignTok (translate rhs)
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
  Modularized.iBinaryExpression10 (translate lhs) Modularized.iFullStopFullStopTok (translate rhs)
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

-------- Unary Expressions

transUnop :: Modularized.MoveTerm Modularized.UnaryOpL -> MSuiMoveTerm UnaryOpL
transUnop (Modularized.UnaryOp' Modularized.ExclamationMarkTok') = Not'

transUnaryExpr :: Modularized.MoveTerm Modularized.UnaryExpressionL -> MSuiMoveTerm ExpressionL
transUnaryExpr (Modularized.UnaryExpression' op expr) =
  Unary' (transUnop op) (translate' expr)

instance Trans Modularized.HiddenExpression where
  trans (Modularized.HiddenExpressionAssignExpression ae) = injF $ transAssignExpr ae
  trans (Modularized.HiddenExpressionBinaryExpression be) = injF $ transBinaryExpr be
  trans he@(Modularized.HiddenExpressionUnaryExpression (Modularized.HiddenUnaryExpression' uexp)) = case uexp of
    Modularized.HiddenUnaryExpressionInternal0UnaryExpression' ue -> injF $ transUnaryExpr ue
    _ -> transDefault he
  trans (Modularized.HiddenExpressionIfExpression ie) = injF $ transIfExpression ie
  trans x = transDefault x

-------- Block

transBlock :: Modularized.MoveTerm Modularized.BlockL -> MSuiMoveTerm BlockL
transBlock (Modularized.Block' _ useDecls blockItems maybeExpr _) =
  let translatedUseDecls = map (iUseDeclarationIsBlockItem . translate') (extractF useDecls)
      translatedBlockItems = map (iBlockItemIsBlockItem . translate') (extractF blockItems)
      allItems = insertF $ translatedUseDecls ++ translatedBlockItems
      blockEnd = iSuiMoveBlockEnd (mapF (injF . translate) maybeExpr)
  in Block' allItems blockEnd

instance Trans Modularized.Block where
  trans b@(Modularized.Block {}) = iBlockIsBlock (transBlock $ inject b)

transUnitExpression :: Modularized.MoveTerm Modularized.UnitExpressionL -> MSuiMoveTerm ()
transUnitExpression (Modularized.UnitExpression' _ _) = UnitF'

instance Trans Modularized.UnitExpression where
  trans (Modularized.UnitExpression leftParen rightParen) = iUnitIsUnitExpression (transUnitExpression $ Modularized.iUnitExpression leftParen rightParen)

-------- Assign Expression

transAssignExpr :: Modularized.MoveTerm Modularized.AssignExpressionL -> MSuiMoveTerm AssignL
transAssignExpr (Modularized.AssignExpression' tuple) =
  let (lhsTok, rhs) = extractF2 tuple
      (lhs, _assignTok) = extractF2 lhsTok
  in Assign' (translate' lhs) AssignOpEquals' (translate' rhs)

-------- Variable Declarations (LetStatement)

transBindList :: Modularized.MoveTerm Modularized.BindListL -> MSuiMoveTerm VarDeclBinderL
transBindList (Modularized.BindListBind' (Modularized.HiddenBindBindInternal0' (Modularized.HiddenBindInternal0VariableIdentifier' (Modularized.HiddenVariableIdentifier' ident)))) =
  -- Single non-mutable identifier: translate to IdentIsVarDeclBinder
  iIdentIsVarDeclBinder $ transIdent ident
transBindList (Modularized.BindListCommaBindList' commaList) =
  -- Tuple destructuring: extract identifiers and use TupleBinder
  let idents = extractBindListIdents commaList
  in iTupleBinder $ insertF idents
transBindList bindList =
  -- Other patterns (mutable variables, struct destructuring, @ patterns, or patterns)
  iBinderIsVarDeclBinder $ translate bindList

-- Extract identifiers from CommaBindList for tuple destructuring
extractBindListIdents :: Modularized.MoveTerm Modularized.CommaBindListL -> [MSuiMoveTerm IdentL]
extractBindListIdents (Modularized.CommaBindList' binds) =
  map extractIdentFromBind $ extractF binds

-- Extract single identifier from a bind
extractIdentFromBind :: Modularized.MoveTerm Modularized.HiddenBindL -> MSuiMoveTerm IdentL
extractIdentFromBind (Modularized.HiddenBindBindInternal0' (Modularized.HiddenBindInternal0VariableIdentifier' (Modularized.HiddenVariableIdentifier' ident))) =
  transIdent ident
extractIdentFromBind (Modularized.HiddenBindBindInternal0' (Modularized.HiddenBindInternal0MutBindVar' (Modularized.MutBindVar' _ (Modularized.HiddenVariableIdentifier' ident)))) =
  transIdent ident
extractIdentFromBind _ =
  error "extractIdentFromBind: unexpected bind pattern in tuple destructuring"

transLetStatement :: Modularized.MoveTerm Modularized.LetStatementL -> MSuiMoveTerm SingleLocalVarDeclL
transLetStatement (Modularized.LetStatement' _ bindList mtype minit) =
  let attrs = case mtype of
        Nothing' -> iEmptyLocalVarDeclAttrs
        Just' (PairF' _ hiddenType) -> iHiddenTypeIsLocalVarDeclAttrs (translate' hiddenType)
      binder = transBindList bindList
      init = case minit of
        Nothing' -> iNoLocalVarInit
        Just' (PairF' _ expr) -> iJustLocalVarInit (translate' expr)
  in iSingleLocalVarDecl attrs binder init

instance Trans Modularized.LetStatement where
  trans ls@(Modularized.LetStatement {}) = injF $ transLetStatement $ inject ls

transIfExpression :: Modularized.MoveTerm Modularized.IfExpressionL -> MSuiMoveTerm ExpressionL
transIfExpression (Modularized.IfExpression' (PairF' (PairF' (PairF' _ condExpr) thenExpr) elseClause)) =
  let elseExpr = case elseClause of
        Nothing' -> iUnitF
        Just' (PairF' _ expr) -> translate' expr
  in iTernary ITE' (translate' condExpr) (translate' thenExpr) elseExpr

transFunctionParametersInternal :: Modularized.MoveTerm Modularized.FunctionParametersInternal0L -> MSuiMoveTerm FunctionParameterL
transFunctionParametersInternal (Modularized.FunctionParametersInternal0FunctionParameter' fp) =
  let (ty, ident, hasDollar) = transFunctionParameter fp
  in iPositionalParameter (iImmutable hasDollar ty) ident
transFunctionParametersInternal (Modularized.FunctionParametersInternal0MutFunctionParameter' (Modularized.MutFunctionParameter' _ fp)) =
  let (ty, ident, _) = transFunctionParameter fp  -- Mutable params don't support $ prefix
  in iPositionalParameter (iMutable ty) ident

transFunctionParameter :: Modularized.MoveTerm Modularized.FunctionParameterL -> (MSuiMoveTerm Modularized.HiddenTypeL, MSuiMoveTerm IdentL, Bool)
transFunctionParameter (Modularized.FunctionParameter' paramInternal _ hiddenType) =
  case paramInternal of
    Modularized.FunctionParameterInternal0Name' (Modularized.HiddenVariableIdentifier' ident) ->
      (translate' hiddenType, transIdent ident, False)  -- No $ prefix
    Modularized.FunctionParameterInternal02' _ (Modularized.HiddenVariableIdentifier' ident) ->
      (translate' hiddenType, transIdent ident, True)  -- Has $ prefix

instance Trans Modularized.FunctionParametersInternal0 where
  trans (Modularized.FunctionParametersInternal0FunctionParameter fp) =
    let (ty, ident, hasDollar) = transFunctionParameter fp
    in iPositionalParameter (iImmutable hasDollar ty) ident
  trans (Modularized.FunctionParametersInternal0MutFunctionParameter (Modularized.MutFunctionParameter' _ fp)) =
    let (ty, ident, _hasDollar) = transFunctionParameter fp
    in iPositionalParameter (iMutable ty) ident

-------- Function Definitions
transFunctionDefinition :: Modularized.MoveTerm Modularized.FunctionDefinitionL -> MSuiMoveTerm FunctionDefL
transFunctionDefinition (Modularized.FunctionDefinition' hiddenFunctionSig block) =
  let (Modularized.HiddenFunctionSignature' mod1 mod2 mod3 _ hiddenFuncIdent typeParams funcParams retType) = hiddenFunctionSig
      -- Extract function name
      (Modularized.HiddenFunctionIdentifier' funcNameIdent) = hiddenFuncIdent
      funcName = transIdent funcNameIdent

      -- Extract and translate parameters
      (Modularized.FunctionParameters' paramsInternalList) = funcParams
      translatedParams = map transFunctionParametersInternal (extractF paramsInternalList)

      -- Store only minimal data in FunctionDefAttrs (not the entire signature)
      funcAttrs = iNormalFunctionDefAttrs
        (translate' mod1)
        (translate' mod2)
        (translate' mod3)
        (translate' typeParams)
        (translate' retType)

      -- Translate body
      funcBody = iBlockIsFunctionBody $ translate' block
  in iFunctionDef funcAttrs funcName (insertF translatedParams) funcBody

instance Trans Modularized.FunctionDefinition where
  trans (Modularized.FunctionDefinition hiddenFunctionSig block) = injF $ transFunctionDefinition $ Modularized.iFunctionDefinition hiddenFunctionSig block

-------- Macro Function Definitions
transMacroFunctionDefinition :: Modularized.MoveTerm Modularized.MacroFunctionDefinitionL -> MSuiMoveTerm FunctionDefL
transMacroFunctionDefinition (Modularized.MacroFunctionDefinition' maybeModifier _ hiddenMacroSig block) =
  let (Modularized.HiddenMacroSignature' _ _ hiddenFuncIdent typeParams funcParams retType) = hiddenMacroSig
      -- Extract function name
      (Modularized.HiddenFunctionIdentifier' funcNameIdent) = hiddenFuncIdent
      funcName = transIdent funcNameIdent

      -- Extract and translate parameters (note: macro params will have $ prefix)
      (Modularized.FunctionParameters' paramsInternalList) = funcParams
      translatedParams = map transFunctionParametersInternal (extractF paramsInternalList)

      -- Store only minimal data in FunctionDefAttrs
      -- Use the modifier from maybeModifier (outer level) as it's typically the visibility modifier
      funcAttrs = iMacroFunctionDefAttrs
        (translate' maybeModifier)
        (translate' typeParams)
        (translate' retType)

      -- Translate body
      funcBody = iBlockIsFunctionBody $ translate' block
  in iFunctionDef funcAttrs funcName (insertF translatedParams) funcBody

instance Trans Modularized.MacroFunctionDefinition where
  trans (Modularized.MacroFunctionDefinition maybeModifier _ hiddenMacroSig block) = iFunctionDefIsMacroFunctionDefinition $ transMacroFunctionDefinition $ Modularized.iMacroFunctionDefinition maybeModifier Modularized.iMacroTok hiddenMacroSig block

-------- Native Function Definitions (Declarations)
transNativeFunctionDefinition :: Modularized.MoveTerm Modularized.NativeFunctionDefinitionL -> MSuiMoveTerm FunctionDeclL
transNativeFunctionDefinition (Modularized.NativeFunctionDefinition' hiddenFunctionSig _) =
  let (Modularized.HiddenFunctionSignature' mod1 mod2 mod3 _ hiddenFuncIdent typeParams funcParams retType) = hiddenFunctionSig
      -- Extract function name
      (Modularized.HiddenFunctionIdentifier' funcNameIdent) = hiddenFuncIdent
      funcName = transIdent funcNameIdent

      -- Extract and translate parameters
      (Modularized.FunctionParameters' paramsInternalList) = funcParams
      translatedParams = map transFunctionParametersInternal (extractF paramsInternalList)
      -- Convert FunctionParameter to FunctionParameterDecl
      translatedParamDecls = map iFunctionParameterIsFunctionParameterDecl translatedParams

      -- Store only minimal data in FunctionDeclAttrs (same as NormalFunctionDefAttrs)
      funcDeclAttrs = iNativeFunctionDeclAttrs
        (translate' mod1)
        (translate' mod2)
        (translate' mod3)
        (translate' typeParams)
        (translate' retType)
  in iFunctionDecl funcDeclAttrs funcName (insertF translatedParamDecls)

instance Trans Modularized.NativeFunctionDefinition where
  trans (Modularized.NativeFunctionDefinition hiddenFunctionSig semicolonTok) = iFunctionDeclIsNativeFunctionDefinition $ transNativeFunctionDefinition $ Modularized.iNativeFunctionDefinition hiddenFunctionSig semicolonTok

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
   let targTs = map ConT $ (ipsNames \\ Modularized.moveSigNames) \\
         [ ''IdentIsIdentifier
         , ''ExpressionIsHiddenExpression
         , ''AssignIsHiddenExpression
         , ''UnitIsUnitExpression
         , ''BlockIsBlock
         , ''SingleLocalVarDeclIsLetStatement
         , ''HiddenExpressionIsLocalVarInit
         , ''BinderIsVarDeclBinder
         , ''HiddenTypeIsLocalVarDeclAttrs
         , ''SuiMoveFunctionDefAttrs
         , ''SuiMoveParameterAttrs
         , ''FunctionParameterIsFunctionParametersInternal0
         , ''FunctionDefIsFunctionDefinition
         , ''FunctionDefIsMacroFunctionDefinition
         , ''BlockIsFunctionBody
         , ''SuiMoveFunctionDeclAttrs
         , ''FunctionParameterIsFunctionParameterDecl
         , ''FunctionDeclIsNativeFunctionDefinition
         ]
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
  untrans (ExpressionIsHiddenExpression (Unary' op expr)) =
    Modularized.iHiddenExpressionUnaryExpression $ untransUnaryOp op (fromProjF expr)
  untrans (ExpressionIsHiddenExpression (Ternary' op cond thenExpr elseExpr)) =
    Modularized.iHiddenExpressionIfExpression $ untransIfExpression op (fromProjF cond) (fromProjF thenExpr) (fromProjF elseExpr)
  untrans (ExpressionIsHiddenExpression e) = untranslate' e

untransBinaryOp :: MSuiMoveTerm BinaryOpL -> MSuiMoveTerm Modularized.HiddenExpressionL -> MSuiMoveTerm Modularized.HiddenExpressionL -> Modularized.MoveTerm Modularized.BinaryExpressionL
untransBinaryOp LogicOr' lhs rhs  = Modularized.iBinaryExpression2 (untranslate lhs) (inject Modularized.VerticalLineVerticalLineTok) (untranslate rhs)
untransBinaryOp LogicAnd' lhs rhs = Modularized.iBinaryExpression3 (untranslate lhs) (inject Modularized.AmpersandAmpersandTok) (untranslate rhs)
untransBinaryOp Eq' lhs rhs       = Modularized.iBinaryExpression4 (untranslate lhs) (inject Modularized.EqualsSignEqualsSignTok) (untranslate rhs)
untransBinaryOp Neq' lhs rhs      = Modularized.iBinaryExpression5 (untranslate lhs) (inject Modularized.ExclamationMarkEqualsSignTok) (untranslate rhs)
untransBinaryOp Lt' lhs rhs       = Modularized.iBinaryExpression6 (untranslate lhs) (inject Modularized.LessThanSignTok) (untranslate rhs)
untransBinaryOp Gt' lhs rhs       = Modularized.iBinaryExpression7 (untranslate lhs) (inject Modularized.GreaterThanSignTok) (untranslate rhs)
untransBinaryOp Lte' lhs rhs      = Modularized.iBinaryExpression8 (untranslate lhs) (inject Modularized.LessThanSignEqualsSignTok) (untranslate rhs)
untransBinaryOp Gte' lhs rhs      = Modularized.iBinaryExpression9 (untranslate lhs) (inject Modularized.GreaterThanSignEqualsSignTok) (untranslate rhs)
untransBinaryOp BitOr' lhs rhs    = Modularized.iBinaryExpression11 (untranslate lhs) (inject Modularized.VerticalLineTok) (untranslate rhs)
untransBinaryOp BitXor' lhs rhs   = Modularized.iBinaryExpression12 (untranslate lhs) (inject Modularized.CircumflexAccentTok) (untranslate rhs)
untransBinaryOp BitAnd' lhs rhs   = Modularized.iBinaryExpression13 (untranslate lhs) (inject Modularized.AmpersandTok) (untranslate rhs)
untransBinaryOp Shl' lhs rhs      = Modularized.iBinaryExpression14 (untranslate lhs) (inject Modularized.LessThanSignLessThanSignTok) (untranslate rhs)
untransBinaryOp ArithShr' lhs rhs = Modularized.iBinaryExpression15 (untranslate lhs) (inject Modularized.GreaterThanSignGreaterThanSignTok) (untranslate rhs)
untransBinaryOp Add' lhs rhs      = Modularized.iBinaryExpression16 (untranslate lhs) (inject Modularized.PlusSignTok) (untranslate rhs)
untransBinaryOp Sub' lhs rhs      = Modularized.iBinaryExpression17 (untranslate lhs) (inject Modularized.HyphenMinusTok) (untranslate rhs)
untransBinaryOp Mul' lhs rhs      = Modularized.iBinaryExpression18 (untranslate lhs) (inject Modularized.AsteriskTok) (untranslate rhs)
untransBinaryOp Div' lhs rhs      = Modularized.iBinaryExpression19 (untranslate lhs) (inject Modularized.SolidusTok) (untranslate rhs)
untransBinaryOp Mod' lhs rhs      = Modularized.iBinaryExpression20 (untranslate lhs) (inject Modularized.PercentSignTok) (untranslate rhs)
untransBinaryOp _ _ _             = error "untransBinaryOp: unsupported operator"

-------- Unary Expressions

untransUnaryOp :: MSuiMoveTerm UnaryOpL -> MSuiMoveTerm Modularized.HiddenExpressionL -> Modularized.MoveTerm Modularized.HiddenUnaryExpressionL
untransUnaryOp Not' expr =
  Modularized.iHiddenUnaryExpression $
    Modularized.iHiddenUnaryExpressionInternal0UnaryExpression $
      Modularized.iUnaryExpression
        (Modularized.iUnaryOp Modularized.iExclamationMarkTok)
        (untranslate expr)
untransUnaryOp _ _ = error "untransUnaryOp: unsupported operator"

-------- If Expression (Ternary operator)

untransIfExpression :: MSuiMoveTerm TernaryOpL -> MSuiMoveTerm Modularized.HiddenExpressionL -> MSuiMoveTerm Modularized.HiddenExpressionL -> MSuiMoveTerm Modularized.HiddenExpressionL -> Modularized.MoveTerm Modularized.IfExpressionL
untransIfExpression ITE' cond thenExpr elseExpr =
  let condTerm = untranslate cond
      thenTerm = untranslate thenExpr
      elseTerm = untranslate elseExpr
      -- Check if elseTerm is a unit expression by examining its structure
      isUnit = case elseTerm of
        Modularized.HiddenExpressionUnaryExpression'
          (Modularized.HiddenUnaryExpression'
            (Modularized.HiddenUnaryExpressionInternal0ExpressionTerm'
              (Modularized.HiddenExpressionTermUnitExpression' _))) -> True
        _ -> False
      maybeElse = if isUnit
                    then Nothing'
                    else Just' $ riPairF Modularized.iElseTok elseTerm
      ifTokCond = riPairF Modularized.iIfTok condTerm
      ifCondThen = riPairF ifTokCond thenTerm
      fullTuple = riPairF ifCondThen maybeElse
  in Modularized.iIfExpression fullTuple
untransIfExpression _ _ _ _ = error "untransIfExpression: unsupported ternary operator"

-------- Block

untransBlock :: MSuiMoveTerm BlockL -> Modularized.MoveTerm Modularized.BlockL
untransBlock (Block' items (SuiMoveBlockEnd' maybeExpr)) =
  let (useDecls, blockItems) = partitionEithers $ map separateItem $ extractF items
  in Modularized.iBlock
      Modularized.iLeftCurlyBracketTok
      (insertF useDecls)
      (insertF blockItems)
      (mapF untranslate maybeExpr)
      Modularized.iRightCurlyBracketTok
  where
    separateItem :: MSuiMoveTerm BlockItemL -> Either (Modularized.MoveTerm Modularized.UseDeclarationL) (Modularized.MoveTerm Modularized.BlockItemL)
    separateItem (UseDeclarationIsBlockItem' ud) = Left (untranslate' ud)
    separateItem (BlockItemIsBlockItem' bi) = Right (untranslate' bi)
    separateItem (SingleLocalVarDeclIsBlockItem' svd) = Right $
      Modularized.iBlockItem
        (Modularized.iBlockItemInternal0LetStatement $ untransLetStatement svd)
        Modularized.iSemicolonTok
    separateItem item = error $ "untransBlock: unexpected BlockItem type: " ++ show item

instance {-# OVERLAPPING #-} Untrans BlockIsBlock where
  untrans (BlockIsBlock b) = untransBlock b

untransUnitExpression :: MSuiMoveTerm () -> Modularized.MoveTerm Modularized.UnitExpressionL
untransUnitExpression UnitF' = Modularized.UnitExpression' Modularized.LeftParenthesisTok' Modularized.RightParenthesisTok'

instance {-# OVERLAPPING #-} Untrans UnitIsUnitExpression where
  untrans (UnitIsUnitExpression u) = untransUnitExpression u

-------- Assign Expression

untransAssignExpr :: MSuiMoveTerm AssignL -> Modularized.MoveTerm Modularized.AssignExpressionL
untransAssignExpr (Assign' lhs _op rhs) =
  let lhsTerm = untranslate' lhs
      rhsTerm = untranslate' rhs
      lhsTokPair = riPairF lhsTerm Modularized.iEqualsSignTok
      fullTuple = riPairF lhsTokPair rhsTerm
  in Modularized.iAssignExpression fullTuple

instance {-# OVERLAPPING #-} Untrans AssignIsHiddenExpression where
  untrans (AssignIsHiddenExpression a) = Modularized.iHiddenExpressionAssignExpression $ untransAssignExpr a

-------- Variable Declarations (LetStatement)

untransBindList :: MSuiMoveTerm VarDeclBinderL -> Modularized.MoveTerm Modularized.BindListL
untransBindList (IdentIsVarDeclBinder' ident) =
  Modularized.iBindListBind $
    Modularized.iHiddenBindBindInternal0 $
      Modularized.iHiddenBindInternal0VariableIdentifier $
        Modularized.iHiddenVariableIdentifier $
          untransIdent ident
untransBindList (TupleBinder' idents) =
  let binds = map (wrapIdentToBind . untransIdent) $ extractF idents
  in Modularized.iBindListCommaBindList $
    Modularized.iCommaBindList (insertF binds)
  where
    wrapIdentToBind :: Modularized.MoveTerm Modularized.IdentifierL -> Modularized.MoveTerm Modularized.HiddenBindL
    wrapIdentToBind ident =
      Modularized.iHiddenBindBindInternal0 $
        Modularized.iHiddenBindInternal0VariableIdentifier $
          Modularized.iHiddenVariableIdentifier ident
untransBindList (BinderIsVarDeclBinder' bindList) =
  untranslate bindList
untransBindList binder =
  error $ "untransBindList: unexpected binder type: " ++ show binder

-- Main untranslation for SingleLocalVarDecl to LetStatement
untransLetStatement :: MSuiMoveTerm SingleLocalVarDeclL -> Modularized.MoveTerm Modularized.LetStatementL
untransLetStatement (SingleLocalVarDecl' attrs binder varInit) =
  let maybeType = case attrs of
                    (HiddenTypeIsLocalVarDeclAttrs' hiddenType) -> Just' $
                      riPairF Modularized.iColonTok (untranslate hiddenType)
                    EmptyLocalVarDeclAttrs' -> Nothing'
                    _ -> error "untransLetStatement: unexpected LocalVarDeclAttrs"
      bindList = untransBindList binder
      maybeInit = case varInit of
                    NoLocalVarInit' -> Nothing'
                    (JustLocalVarInit' expr) -> Just' $
                      riPairF Modularized.iEqualsSignTok (untranslate' expr)
                    _ -> error "untransLetStatement: unexpected OptLocalVarInit"
  in Modularized.iLetStatement Modularized.iLetTok bindList maybeType maybeInit

instance {-# OVERLAPPING #-} Untrans SingleLocalVarDeclIsLetStatement where
  untrans (SingleLocalVarDeclIsLetStatement svd) = untransLetStatement svd

-- HiddenTypeIsLocalVarDeclAttrs is never untranslated directly, only as part of SingleLocalVarDecl
-- But we need an instance to prevent the default instance from being generated
instance {-# OVERLAPPING #-} Untrans HiddenTypeIsLocalVarDeclAttrs where
  untrans = untransError

-- HiddenExpressionIsLocalVarInit is never untranslated directly, only as part of SingleLocalVarDecl
-- But we need an instance to prevent the default instance from being generated
instance {-# OVERLAPPING #-} Untrans HiddenExpressionIsLocalVarInit where
  untrans = untransError

-- BinderIsVarDeclBinder is never untranslated directly, only as part of SingleLocalVarDecl
-- But we need an instance to prevent the default instance from being generated
instance {-# OVERLAPPING #-} Untrans BinderIsVarDeclBinder where
  untrans = untransError

-------- Function Parameters

-- Untranslate parameter, using the hasDollar flag to determine whether to add $ prefix
untransParameter :: MSuiMoveTerm FunctionParameterL -> Modularized.MoveTerm Modularized.FunctionParametersInternal0L
untransParameter (PositionalParameter' paramAttrs ident) =
  case paramAttrs of
    Immutable' True hiddenType ->
      -- Has $ prefix
      let varIdent = Modularized.iHiddenVariableIdentifier (untransIdent ident)
          paramInternal = Modularized.iFunctionParameterInternal02 Modularized.iDollarSignTok varIdent
          fp = Modularized.iFunctionParameter paramInternal Modularized.iColonTok (untranslate hiddenType)
      in Modularized.iFunctionParametersInternal0FunctionParameter fp
    Immutable' False hiddenType ->
      -- No $ prefix
      let varIdent = Modularized.iHiddenVariableIdentifier (untransIdent ident)
          paramInternal = Modularized.iFunctionParameterInternal0Name varIdent
          fp = Modularized.iFunctionParameter paramInternal Modularized.iColonTok (untranslate hiddenType)
      in Modularized.iFunctionParametersInternal0FunctionParameter fp
    Mutable' hiddenType ->
      -- Mutable parameters never have $ prefix
      let varIdent = Modularized.iHiddenVariableIdentifier (untransIdent ident)
          paramInternal = Modularized.iFunctionParameterInternal0Name varIdent
          fp = Modularized.iFunctionParameter paramInternal Modularized.iColonTok (untranslate hiddenType)
      in Modularized.iFunctionParametersInternal0MutFunctionParameter $ Modularized.iMutFunctionParameter Modularized.iMutTok fp
    EmptyParameterAttrs' -> error "untransParameter: expected type annotation"
    _ -> error "untransParameter: unexpected ParameterAttrs"
untransParameter _ = error "untransParameter: expected PositionalParameter"

instance {-# OVERLAPPING #-} Untrans FunctionParameterIsFunctionParametersInternal0 where
  untrans (FunctionParameterIsFunctionParametersInternal0 fp) = untransParameter fp

-------- Function Definitions

untransFunctionDefinition :: MSuiMoveTerm FunctionDefL -> Either (Modularized.MoveTerm Modularized.FunctionDefinitionL) (Modularized.MoveTerm Modularized.MacroFunctionDefinitionL)
untransFunctionDefinition (FunctionDef' funcAttrs funcName paramsListTerm funcBody) =
  let block = case funcBody of
                BlockIsFunctionBody' b -> untranslate' b
                _ -> error "untransFunctionDefinition: unexpected FunctionBody"
      params = extractF paramsListTerm
  in case funcAttrs of
    NormalFunctionDefAttrs' mod1 mod2 mod3 typeParams retType ->
      -- Reconstruct HiddenFunctionSignature from minimal data
      let hiddenFuncIdent = Modularized.iHiddenFunctionIdentifier (untransIdent funcName)
          -- Reconstruct FunctionParameters from parameter list
          funcParams = Modularized.iFunctionParameters $
            insertF $ map untransParameter params
          -- Reconstruct complete signature
          hiddenFunctionSig = Modularized.iHiddenFunctionSignature
            (untranslate mod1)
            (untranslate mod2)
            (untranslate mod3)
            Modularized.iFunTok
            hiddenFuncIdent
            (untranslate typeParams)
            funcParams
            (untranslate retType)
      in Left $ Modularized.iFunctionDefinition hiddenFunctionSig block
    MacroFunctionDefAttrs' maybeModifier typeParams retType ->
      -- Reconstruct HiddenMacroSignature from minimal data
      let hiddenFuncIdent = Modularized.iHiddenFunctionIdentifier (untransIdent funcName)
          -- Reconstruct FunctionParameters from parameter list (preserving $ prefix)
          funcParams = Modularized.iFunctionParameters $
            insertF $ map untransParameter params
          -- Reconstruct complete macro signature
          -- Note: The modifier goes in the outer MacroFunctionDefinition, not in HiddenMacroSignature
          hiddenMacroSig = Modularized.iHiddenMacroSignature
            Nothing'  -- No modifier in the signature itself
            Modularized.iFunTok
            hiddenFuncIdent
            (untranslate typeParams)
            funcParams
            (untranslate retType)
      in Right $ Modularized.iMacroFunctionDefinition (untranslate maybeModifier) Modularized.iMacroTok hiddenMacroSig block
    _ -> error "untransFunctionDefinition: unexpected FunctionDefAttrs"

instance {-# OVERLAPPING #-} Untrans FunctionDefIsFunctionDefinition where
  untrans (FunctionDefIsFunctionDefinition fd) =
    case untransFunctionDefinition fd of
      Left normalFunc -> normalFunc
      Right _ -> error "untrans FunctionDefIsFunctionDefinition: expected normal function but got macro function"

instance {-# OVERLAPPING #-} Untrans FunctionDefIsMacroFunctionDefinition where
  untrans (FunctionDefIsMacroFunctionDefinition fd) =
    case untransFunctionDefinition fd of
      Left _ -> error "untrans FunctionDefIsMacroFunctionDefinition: expected macro function but got normal function"
      Right macroFunc -> macroFunc

-- SuiMoveFunctionDefAttrs is never untranslated directly, only as part of FunctionDef
-- But we need an instance to prevent the default instance from being generated
instance {-# OVERLAPPING #-} Untrans SuiMoveFunctionDefAttrs where
  untrans = untransError

-- SuiMoveParameterAttrs is never untranslated directly, only as part of PositionalParameter
-- But we need an instance to prevent the default instance from being generated
instance {-# OVERLAPPING #-} Untrans SuiMoveParameterAttrs where
  untrans = untransError

-- BlockIsFunctionBody is never untranslated directly, only as part of FunctionDef
-- But we need an instance to prevent the default instance from being generated
instance {-# OVERLAPPING #-} Untrans BlockIsFunctionBody where
  untrans = untransError

-------- Native Function Definitions (Declarations)

-- Untranslate FunctionParameterDecl back to FunctionParametersInternal0
untransParameterDecl :: MSuiMoveTerm FunctionParameterDeclL -> Modularized.MoveTerm Modularized.FunctionParametersInternal0L
untransParameterDecl (FunctionParameterIsFunctionParameterDecl' fp) = untransParameter fp
untransParameterDecl _ = error "untransParameterDecl: expected FunctionParameterIsFunctionParameterDecl"

untransNativeFunctionDefinition :: MSuiMoveTerm FunctionDeclL -> Modularized.MoveTerm Modularized.NativeFunctionDefinitionL
untransNativeFunctionDefinition (FunctionDecl' funcDeclAttrs funcName paramsListTerm) =
  let params = extractF paramsListTerm
  in case funcDeclAttrs of
    NativeFunctionDeclAttrs' mod1 mod2 mod3 typeParams retType ->
      -- Reconstruct HiddenFunctionSignature from minimal data
      let hiddenFuncIdent = Modularized.iHiddenFunctionIdentifier (untransIdent funcName)
          -- Reconstruct FunctionParameters from parameter list
          funcParams = Modularized.iFunctionParameters $
            insertF $ map untransParameterDecl params
          -- Reconstruct complete signature
          hiddenFunctionSig = Modularized.iHiddenFunctionSignature
            (untranslate mod1)
            (untranslate mod2)
            (untranslate mod3)
            Modularized.iFunTok
            hiddenFuncIdent
            (untranslate typeParams)
            funcParams
            (untranslate retType)
      in Modularized.iNativeFunctionDefinition hiddenFunctionSig Modularized.iSemicolonTok
    _ -> error "untransNativeFunctionDefinition: expected NativeFunctionDeclAttrs"

instance {-# OVERLAPPING #-} Untrans FunctionDeclIsNativeFunctionDefinition where
  untrans (FunctionDeclIsNativeFunctionDefinition fd) = untransNativeFunctionDefinition fd

-- SuiMoveFunctionDeclAttrs is never untranslated directly, only as part of FunctionDecl
-- But we need an instance to prevent the default instance from being generated
instance {-# OVERLAPPING #-} Untrans SuiMoveFunctionDeclAttrs where
  untrans = untransError

-- FunctionParameterIsFunctionParameterDecl is never untranslated directly, only as part of FunctionDecl
-- But we need an instance to prevent the default instance from being generated
instance {-# OVERLAPPING #-} Untrans FunctionParameterIsFunctionParameterDecl where
  untrans = untransError
