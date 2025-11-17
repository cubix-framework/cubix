{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Language.SuiMove.Modularized
  where

import Data.ByteString.Char8 qualified as BSC
import Data.Foldable (foldrM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Type.Equality (type (:~:) (..), type (:~~:) (..))
import TreeSitter qualified as TS

import Cubix.Language.Info (TermLab)
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax qualified as Syntax
import Cubix.ParsePretty (type RootSort)
import Data.Comp.Multi (Sort, Term)

--------------------------------------------------------------------------------
-- Sort
--------------------------------------------------------------------------------

data SourceFileL
data ModuleDefinitionL
data ModuleBodyL
data ModuleBodyInternal0L
data ModuleBodyInternal1L
data HiddenEnumItemL
data EnumDefinitionL
data HiddenEnumSignatureL
data HiddenEnumIdentifierL
data EnumIdentifierL
data HiddenEnumSignatureInternal0L
data TypeParametersL
data HiddenSpecAbortWithOrModifiesInternal1L
data TypeParameterL
data HiddenTypeParameterIdentifierL
data TypeParameterIdentifierL
data TypeParameterInternal0L
data TypeParameterInternal1L
data TypeParameterInternal3L
data AbilityL
data TypeParameterInternal2L
data HiddenEnumSignatureInternal1L
data AbilityDeclsL
data AbilityDeclsInternal0L
data EnumDefinitionInternal0L
data EnumDefinitionInternal1L
data PostfixAbilityDeclsL
data EnumVariantsL
data EnumVariantsInternal0L
data VariantL
data HiddenVariantIdentifierL
data VariantIdentifierL
data VariantInternal0L
data DatatypeFieldsL
data NamedFieldsL
data FieldAnnotationL
data HiddenFieldIdentifierL
data FieldIdentifierL
data HiddenTypeL
data ApplyTypeL
data ApplyTypeInternal0L
data TypeArgumentsL
data ModuleAccessL
data HiddenModuleIdentifierL
data ModuleIdentifierL
data IdentifierL
data ModuleIdentityL
data ModuleIdentityAddressInternal0L
data NumLiteralL
data NumLiteralInternal0L
data NumLiteralInternal2L
data NumLiteralInternal1L
data FunctionTypeL
data FunctionTypeInternal0L
data FunctionTypeParametersL
data FunctionTypeParametersInternal0L
data PrimitiveTypeL
data RefTypeL
data HiddenReferenceL
data ImmRefL
data MutRefL
data TupleTypeL
data NamedFieldsInternal0L
data PositionalFieldsL
data HiddenFunctionItemL
data FunctionDefinitionL
data HiddenFunctionSignatureL
data HiddenFunctionIdentifierL
data FunctionIdentifierL
data HiddenFunctionSignatureInternal0L
data ModifierL
data ModifierInternal1L
data ModifierInternal0L
data HiddenFunctionSignatureInternal1L
data RetTypeL
data FunctionParametersL
data FunctionParametersInternal0L
data FunctionParameterL
data FunctionParameterInternal0L
data HiddenVariableIdentifierL
data VariableIdentifierL
data MutFunctionParameterL
data FunctionParametersInternal1L
data BlockL
data ArgListInternal0L
data HiddenExpressionL
data HiddenUnaryExpressionL
data HiddenUnaryExpressionInternal0L
data HiddenExpressionTermL
data HiddenLiteralValueL
data AddressLiteralL
data BoolLiteralL
data ByteStringLiteralL
data HexStringLiteralL
data AnnotationExpressionL
data BreakExpressionL
data BreakExpressionInternal0L
data LabelL
data BreakExpressionInternal1L
data CallExpressionL
data ArgListL
data NameExpressionL
data NameExpressionInternal0L
data ContinueExpressionL
data DotExpressionL
data ExpressionListL
data IfExpressionL
data IfExpressionInternal0L
data IndexExpressionL
data IndexExpressionInternal0L
data MacroCallExpressionL
data MacroModuleAccessL
data MatchExpressionL
data HiddenMatchBodyL
data HiddenMatchBodyInternal0L
data MatchArmL
data BindListL
data HiddenBindL
data HiddenBindInternal0L
data MutBindVarL
data BindVarL
data AtBindL
data BindUnpackL
data BindUnpackInternal0L
data BindFieldsL
data BindNamedFieldsL
data BindNamedFieldsInternal0L
data BindFieldL
data HiddenSpreadOperatorL
data BindFieldInternal0L
data MutBindFieldL
data BindNamedFieldsInternal1L
data BindPositionalFieldsL
data CommaBindListL
data CommaBindListInternal0L
data OrBindListL
data OrBindListInternal0L
data OrBindListInternal1L
data OrBindListInternal2L
data MatchArmInternal0L
data MatchConditionL
data PackExpressionL
data FieldInitializeListL
data ExpFieldL
data ExpFieldInternal0L
data FieldInitializeListInternal0L
data SpecBlockL
data SpecBlockInternal1L
data HiddenSpecFunctionL
data NativeSpecFunctionL
data HiddenSpecFunctionSignatureL
data UninterpretedSpecFunctionL
data UsualSpecFunctionL
data SpecBlockInternal0L
data HiddenSpecBlockTargetL
data SpecBlockTargetSchemaL
data HiddenStructIdentifierL
data StructIdentifierL
data SpecBodyL
data HiddenSpecBlockMemeberL
data SpecApplyL
data SpecApplyInternal0L
data SpecApplyPatternL
data SpecApplyNamePatternL
data SpecApplyPatternInternal1L
data SpecApplyPatternInternal0L
data SpecConditionL
data HiddenSpecAbortIfL
data HiddenSpecAbortIfInternal0L
data ConditionPropertiesL
data ConditionPropertiesInternal0L
data SpecPropertyL
data SpecPropertyInternal0L
data HiddenSpecAbortIfInternal1L
data ConditionKindL
data HiddenSpecAbortWithOrModifiesL
data HiddenSpecConditionL
data HiddenSpecConditionInternal1L
data HiddenSpecConditionInternal0L
data SpecIncludeL
data SpecInvariantL
data SpecInvariantInternal1L
data InvariantModifierL
data SpecLetL
data SpecLetInternal0L
data SpecPragmaL
data SpecVariableL
data SpecVariableInternal1L
data SpecVariableInternal0L
data UseDeclarationL
data UseDeclarationInternal0L
data UseFunL
data UseModuleL
data UseModuleInternal0L
data UseModuleMemberL
data UseMemberL
data UseMemberInternal0L
data UseModuleMembersL
data UnitExpressionL
data VectorExpressionL
data VectorExpressionInternal0L
data BorrowExpressionL
data DereferenceExpressionL
data MoveOrCopyExpressionL
data MoveOrCopyExpressionInternal0L
data UnaryExpressionL
data UnaryOpL
data AbortExpressionL
data AbortExpressionInternal0L
data AssignExpressionL
data BinaryExpressionL
data BinaryOperatorL
data CastExpressionL
data IdentifiedExpressionL
data BlockIdentifierL
data LambdaExpressionL
data LambdaBindingsL
data LambdaBindingL
data LambdaBindingInternal0L
data LambdaBindingsInternal0L
data LambdaExpressionInternal0L
data LoopExpressionL
data QuantifierExpressionL
data QuantifierBindingsL
data QuantifierBindingL
data QuantifierExpressionInternal0L
data HiddenExistsL
data HiddenForallL
data QuantifierExpressionInternal1L
data ReturnExpressionL
data WhileExpressionL
data BlockItemL
data BlockItemInternal0L
data LetStatementL
data LetStatementInternal0L
data LetStatementInternal1L
data MacroFunctionDefinitionL
data HiddenMacroSignatureL
data NativeFunctionDefinitionL
data HiddenStructItemL
data NativeStructDefinitionL
data HiddenStructSignatureL
data StructDefinitionL
data ConstantL
data ConstantIdentifierL
data FriendDeclarationL
data FriendAccessL
data ModuleBodyInternal2L

data BangTokL
data NeqTokL
data DollarTokL
data ModTokL
data BitandTokL
data AndTokL
data MulTokL
data AddTokL
data SubTokL
data RangeTokL
data DivTokL
data LtTokL
data ShlTokL
data LeTokL
data AssignTokL
data EqTokL
data ImpliesTokL
data GtTokL
data GeTokL
data ShrTokL
data XorTokL
data AbortsWithTokL
data AddressTokL
data AssertTokL
data AssumeTokL
data BoolTokL
data BytearrayTokL
data CopyTokL
data DecreasesTokL
data DropTokL
data EnsuresTokL
data EntryTokL
data FalseTokL
data FriendTokL
data GlobalTokL
data InternalTokL
data KeyTokL
data LocalTokL
data ModifiesTokL
data ModuleTokL
data MoveTokL
data NativeTokL
data PackTokL
data PackageTokL
data PhantomTokL
data PostTokL
data PublicTokL
data SignerTokL
data StoreTokL
data SucceedsIfTokL
data TrueTokL
data U128TokL
data U16TokL
data U256TokL
data U32TokL
data U64TokL
data U8TokL
data UnpackTokL
data UpdateTokL
data BitorTokL
data OrTokL

data LabelSing (sort :: Sort) where
  SSourceFileL :: LabelSing SourceFileL
  SModuleDefinitionL :: LabelSing ModuleDefinitionL
  SModuleBodyL :: LabelSing ModuleBodyL
  SModuleBodyInternal0L :: LabelSing ModuleBodyInternal0L
  SModuleBodyInternal1L :: LabelSing ModuleBodyInternal1L
  SHiddenEnumItemL :: LabelSing HiddenEnumItemL
  SEnumDefinitionL :: LabelSing EnumDefinitionL
  SHiddenEnumSignatureL :: LabelSing HiddenEnumSignatureL
  SHiddenEnumIdentifierL :: LabelSing HiddenEnumIdentifierL
  SEnumIdentifierL :: LabelSing EnumIdentifierL
  SHiddenEnumSignatureInternal0L :: LabelSing HiddenEnumSignatureInternal0L
  STypeParametersL :: LabelSing TypeParametersL
  SHiddenSpecAbortWithOrModifiesInternal1L :: LabelSing HiddenSpecAbortWithOrModifiesInternal1L
  STypeParameterL :: LabelSing TypeParameterL
  SHiddenTypeParameterIdentifierL :: LabelSing HiddenTypeParameterIdentifierL
  STypeParameterIdentifierL :: LabelSing TypeParameterIdentifierL
  STypeParameterInternal0L :: LabelSing TypeParameterInternal0L
  STypeParameterInternal1L :: LabelSing TypeParameterInternal1L
  STypeParameterInternal3L :: LabelSing TypeParameterInternal3L
  SAbilityL :: LabelSing AbilityL
  STypeParameterInternal2L :: LabelSing TypeParameterInternal2L
  SHiddenEnumSignatureInternal1L :: LabelSing HiddenEnumSignatureInternal1L
  SAbilityDeclsL :: LabelSing AbilityDeclsL
  SAbilityDeclsInternal0L :: LabelSing AbilityDeclsInternal0L
  SEnumDefinitionInternal0L :: LabelSing EnumDefinitionInternal0L
  SEnumDefinitionInternal1L :: LabelSing EnumDefinitionInternal1L
  SPostfixAbilityDeclsL :: LabelSing PostfixAbilityDeclsL
  SEnumVariantsL :: LabelSing EnumVariantsL
  SEnumVariantsInternal0L :: LabelSing EnumVariantsInternal0L
  SVariantL :: LabelSing VariantL
  SHiddenVariantIdentifierL :: LabelSing HiddenVariantIdentifierL
  SVariantIdentifierL :: LabelSing VariantIdentifierL
  SVariantInternal0L :: LabelSing VariantInternal0L
  SDatatypeFieldsL :: LabelSing DatatypeFieldsL
  SNamedFieldsL :: LabelSing NamedFieldsL
  SFieldAnnotationL :: LabelSing FieldAnnotationL
  SHiddenFieldIdentifierL :: LabelSing HiddenFieldIdentifierL
  SFieldIdentifierL :: LabelSing FieldIdentifierL
  SHiddenTypeL :: LabelSing HiddenTypeL
  SApplyTypeL :: LabelSing ApplyTypeL
  SApplyTypeInternal0L :: LabelSing ApplyTypeInternal0L
  STypeArgumentsL :: LabelSing TypeArgumentsL
  SModuleAccessL :: LabelSing ModuleAccessL
  SHiddenModuleIdentifierL :: LabelSing HiddenModuleIdentifierL
  SModuleIdentifierL :: LabelSing ModuleIdentifierL
  SIdentifierL :: LabelSing IdentifierL
  SModuleIdentityL :: LabelSing ModuleIdentityL
  SModuleIdentityAddressInternal0L :: LabelSing ModuleIdentityAddressInternal0L
  SNumLiteralL :: LabelSing NumLiteralL
  SNumLiteralInternal0L :: LabelSing NumLiteralInternal0L
  SNumLiteralInternal2L :: LabelSing NumLiteralInternal2L
  SNumLiteralInternal1L :: LabelSing NumLiteralInternal1L
  SFunctionTypeL :: LabelSing FunctionTypeL
  SFunctionTypeInternal0L :: LabelSing FunctionTypeInternal0L
  SFunctionTypeParametersL :: LabelSing FunctionTypeParametersL
  SFunctionTypeParametersInternal0L :: LabelSing FunctionTypeParametersInternal0L
  SPrimitiveTypeL :: LabelSing PrimitiveTypeL
  SRefTypeL :: LabelSing RefTypeL
  SHiddenReferenceL :: LabelSing HiddenReferenceL
  SImmRefL :: LabelSing ImmRefL
  SMutRefL :: LabelSing MutRefL
  STupleTypeL :: LabelSing TupleTypeL
  SNamedFieldsInternal0L :: LabelSing NamedFieldsInternal0L
  SPositionalFieldsL :: LabelSing PositionalFieldsL
  SHiddenFunctionItemL :: LabelSing HiddenFunctionItemL
  SFunctionDefinitionL :: LabelSing FunctionDefinitionL
  SHiddenFunctionSignatureL :: LabelSing HiddenFunctionSignatureL
  SHiddenFunctionIdentifierL :: LabelSing HiddenFunctionIdentifierL
  SFunctionIdentifierL :: LabelSing FunctionIdentifierL
  SHiddenFunctionSignatureInternal0L :: LabelSing HiddenFunctionSignatureInternal0L
  SModifierL :: LabelSing ModifierL
  SModifierInternal1L :: LabelSing ModifierInternal1L
  SModifierInternal0L :: LabelSing ModifierInternal0L
  SHiddenFunctionSignatureInternal1L :: LabelSing HiddenFunctionSignatureInternal1L
  SRetTypeL :: LabelSing RetTypeL
  SFunctionParametersL :: LabelSing FunctionParametersL
  SFunctionParametersInternal0L :: LabelSing FunctionParametersInternal0L
  SFunctionParameterL :: LabelSing FunctionParameterL
  SFunctionParameterInternal0L :: LabelSing FunctionParameterInternal0L
  SHiddenVariableIdentifierL :: LabelSing HiddenVariableIdentifierL
  SVariableIdentifierL :: LabelSing VariableIdentifierL
  SMutFunctionParameterL :: LabelSing MutFunctionParameterL
  SFunctionParametersInternal1L :: LabelSing FunctionParametersInternal1L
  SBlockL :: LabelSing BlockL
  SArgListInternal0L :: LabelSing ArgListInternal0L
  SHiddenExpressionL :: LabelSing HiddenExpressionL
  SHiddenUnaryExpressionL :: LabelSing HiddenUnaryExpressionL
  SHiddenUnaryExpressionInternal0L :: LabelSing HiddenUnaryExpressionInternal0L
  SHiddenExpressionTermL :: LabelSing HiddenExpressionTermL
  SHiddenLiteralValueL :: LabelSing HiddenLiteralValueL
  SAddressLiteralL :: LabelSing AddressLiteralL
  SBoolLiteralL :: LabelSing BoolLiteralL
  SByteStringLiteralL :: LabelSing ByteStringLiteralL
  SHexStringLiteralL :: LabelSing HexStringLiteralL
  SAnnotationExpressionL :: LabelSing AnnotationExpressionL
  SBreakExpressionL :: LabelSing BreakExpressionL
  SBreakExpressionInternal0L :: LabelSing BreakExpressionInternal0L
  SLabelL :: LabelSing LabelL
  SBreakExpressionInternal1L :: LabelSing BreakExpressionInternal1L
  SCallExpressionL :: LabelSing CallExpressionL
  SArgListL :: LabelSing ArgListL
  SNameExpressionL :: LabelSing NameExpressionL
  SNameExpressionInternal0L :: LabelSing NameExpressionInternal0L
  SContinueExpressionL :: LabelSing ContinueExpressionL
  SDotExpressionL :: LabelSing DotExpressionL
  SExpressionListL :: LabelSing ExpressionListL
  SIfExpressionL :: LabelSing IfExpressionL
  SIfExpressionInternal0L :: LabelSing IfExpressionInternal0L
  SIndexExpressionL :: LabelSing IndexExpressionL
  SIndexExpressionInternal0L :: LabelSing IndexExpressionInternal0L
  SMacroCallExpressionL :: LabelSing MacroCallExpressionL
  SMacroModuleAccessL :: LabelSing MacroModuleAccessL
  SMatchExpressionL :: LabelSing MatchExpressionL
  SHiddenMatchBodyL :: LabelSing HiddenMatchBodyL
  SHiddenMatchBodyInternal0L :: LabelSing HiddenMatchBodyInternal0L
  SMatchArmL :: LabelSing MatchArmL
  SBindListL :: LabelSing BindListL
  SHiddenBindL :: LabelSing HiddenBindL
  SHiddenBindInternal0L :: LabelSing HiddenBindInternal0L
  SMutBindVarL :: LabelSing MutBindVarL
  SBindVarL :: LabelSing BindVarL
  SAtBindL :: LabelSing AtBindL
  SBindUnpackL :: LabelSing BindUnpackL
  SBindUnpackInternal0L :: LabelSing BindUnpackInternal0L
  SBindFieldsL :: LabelSing BindFieldsL
  SBindNamedFieldsL :: LabelSing BindNamedFieldsL
  SBindNamedFieldsInternal0L :: LabelSing BindNamedFieldsInternal0L
  SBindFieldL :: LabelSing BindFieldL
  SHiddenSpreadOperatorL :: LabelSing HiddenSpreadOperatorL
  SBindFieldInternal0L :: LabelSing BindFieldInternal0L
  SMutBindFieldL :: LabelSing MutBindFieldL
  SBindNamedFieldsInternal1L :: LabelSing BindNamedFieldsInternal1L
  SBindPositionalFieldsL :: LabelSing BindPositionalFieldsL
  SCommaBindListL :: LabelSing CommaBindListL
  SCommaBindListInternal0L :: LabelSing CommaBindListInternal0L
  SOrBindListL :: LabelSing OrBindListL
  SOrBindListInternal0L :: LabelSing OrBindListInternal0L
  SOrBindListInternal1L :: LabelSing OrBindListInternal1L
  SOrBindListInternal2L :: LabelSing OrBindListInternal2L
  SMatchArmInternal0L :: LabelSing MatchArmInternal0L
  SMatchConditionL :: LabelSing MatchConditionL
  SPackExpressionL :: LabelSing PackExpressionL
  SFieldInitializeListL :: LabelSing FieldInitializeListL
  SExpFieldL :: LabelSing ExpFieldL
  SExpFieldInternal0L :: LabelSing ExpFieldInternal0L
  SFieldInitializeListInternal0L :: LabelSing FieldInitializeListInternal0L
  SSpecBlockL :: LabelSing SpecBlockL
  SSpecBlockInternal1L :: LabelSing SpecBlockInternal1L
  SHiddenSpecFunctionL :: LabelSing HiddenSpecFunctionL
  SNativeSpecFunctionL :: LabelSing NativeSpecFunctionL
  SHiddenSpecFunctionSignatureL :: LabelSing HiddenSpecFunctionSignatureL
  SUninterpretedSpecFunctionL :: LabelSing UninterpretedSpecFunctionL
  SUsualSpecFunctionL :: LabelSing UsualSpecFunctionL
  SSpecBlockInternal0L :: LabelSing SpecBlockInternal0L
  SHiddenSpecBlockTargetL :: LabelSing HiddenSpecBlockTargetL
  SSpecBlockTargetSchemaL :: LabelSing SpecBlockTargetSchemaL
  SHiddenStructIdentifierL :: LabelSing HiddenStructIdentifierL
  SStructIdentifierL :: LabelSing StructIdentifierL
  SSpecBodyL :: LabelSing SpecBodyL
  SHiddenSpecBlockMemeberL :: LabelSing HiddenSpecBlockMemeberL
  SSpecApplyL :: LabelSing SpecApplyL
  SSpecApplyInternal0L :: LabelSing SpecApplyInternal0L
  SSpecApplyPatternL :: LabelSing SpecApplyPatternL
  SSpecApplyNamePatternL :: LabelSing SpecApplyNamePatternL
  SSpecApplyPatternInternal1L :: LabelSing SpecApplyPatternInternal1L
  SSpecApplyPatternInternal0L :: LabelSing SpecApplyPatternInternal0L
  SSpecConditionL :: LabelSing SpecConditionL
  SHiddenSpecAbortIfL :: LabelSing HiddenSpecAbortIfL
  SHiddenSpecAbortIfInternal0L :: LabelSing HiddenSpecAbortIfInternal0L
  SConditionPropertiesL :: LabelSing ConditionPropertiesL
  SConditionPropertiesInternal0L :: LabelSing ConditionPropertiesInternal0L
  SSpecPropertyL :: LabelSing SpecPropertyL
  SSpecPropertyInternal0L :: LabelSing SpecPropertyInternal0L
  SHiddenSpecAbortIfInternal1L :: LabelSing HiddenSpecAbortIfInternal1L
  SConditionKindL :: LabelSing ConditionKindL
  SHiddenSpecAbortWithOrModifiesL :: LabelSing HiddenSpecAbortWithOrModifiesL
  SHiddenSpecConditionL :: LabelSing HiddenSpecConditionL
  SHiddenSpecConditionInternal1L :: LabelSing HiddenSpecConditionInternal1L
  SHiddenSpecConditionInternal0L :: LabelSing HiddenSpecConditionInternal0L
  SSpecIncludeL :: LabelSing SpecIncludeL
  SSpecInvariantL :: LabelSing SpecInvariantL
  SSpecInvariantInternal1L :: LabelSing SpecInvariantInternal1L
  SInvariantModifierL :: LabelSing InvariantModifierL
  SSpecLetL :: LabelSing SpecLetL
  SSpecLetInternal0L :: LabelSing SpecLetInternal0L
  SSpecPragmaL :: LabelSing SpecPragmaL
  SSpecVariableL :: LabelSing SpecVariableL
  SSpecVariableInternal1L :: LabelSing SpecVariableInternal1L
  SSpecVariableInternal0L :: LabelSing SpecVariableInternal0L
  SUseDeclarationL :: LabelSing UseDeclarationL
  SUseDeclarationInternal0L :: LabelSing UseDeclarationInternal0L
  SUseFunL :: LabelSing UseFunL
  SUseModuleL :: LabelSing UseModuleL
  SUseModuleInternal0L :: LabelSing UseModuleInternal0L
  SUseModuleMemberL :: LabelSing UseModuleMemberL
  SUseMemberL :: LabelSing UseMemberL
  SUseMemberInternal0L :: LabelSing UseMemberInternal0L
  SUseModuleMembersL :: LabelSing UseModuleMembersL
  SUnitExpressionL :: LabelSing UnitExpressionL
  SVectorExpressionL :: LabelSing VectorExpressionL
  SVectorExpressionInternal0L :: LabelSing VectorExpressionInternal0L
  SBorrowExpressionL :: LabelSing BorrowExpressionL
  SDereferenceExpressionL :: LabelSing DereferenceExpressionL
  SMoveOrCopyExpressionL :: LabelSing MoveOrCopyExpressionL
  SMoveOrCopyExpressionInternal0L :: LabelSing MoveOrCopyExpressionInternal0L
  SUnaryExpressionL :: LabelSing UnaryExpressionL
  SUnaryOpL :: LabelSing UnaryOpL
  SAbortExpressionL :: LabelSing AbortExpressionL
  SAbortExpressionInternal0L :: LabelSing AbortExpressionInternal0L
  SAssignExpressionL :: LabelSing AssignExpressionL
  SBinaryExpressionL :: LabelSing BinaryExpressionL
  SBinaryOperatorL :: LabelSing BinaryOperatorL
  SCastExpressionL :: LabelSing CastExpressionL
  SIdentifiedExpressionL :: LabelSing IdentifiedExpressionL
  SBlockIdentifierL :: LabelSing BlockIdentifierL
  SLambdaExpressionL :: LabelSing LambdaExpressionL
  SLambdaBindingsL :: LabelSing LambdaBindingsL
  SLambdaBindingL :: LabelSing LambdaBindingL
  SLambdaBindingInternal0L :: LabelSing LambdaBindingInternal0L
  SLambdaBindingsInternal0L :: LabelSing LambdaBindingsInternal0L
  SLambdaExpressionInternal0L :: LabelSing LambdaExpressionInternal0L
  SLoopExpressionL :: LabelSing LoopExpressionL
  SQuantifierExpressionL :: LabelSing QuantifierExpressionL
  SQuantifierBindingsL :: LabelSing QuantifierBindingsL
  SQuantifierBindingL :: LabelSing QuantifierBindingL
  SQuantifierExpressionInternal0L :: LabelSing QuantifierExpressionInternal0L
  SHiddenExistsL :: LabelSing HiddenExistsL
  SHiddenForallL :: LabelSing HiddenForallL
  SQuantifierExpressionInternal1L :: LabelSing QuantifierExpressionInternal1L
  SReturnExpressionL :: LabelSing ReturnExpressionL
  SWhileExpressionL :: LabelSing WhileExpressionL
  SBlockItemL :: LabelSing BlockItemL
  SBlockItemInternal0L :: LabelSing BlockItemInternal0L
  SLetStatementL :: LabelSing LetStatementL
  SLetStatementInternal0L :: LabelSing LetStatementInternal0L
  SLetStatementInternal1L :: LabelSing LetStatementInternal1L
  SMacroFunctionDefinitionL :: LabelSing MacroFunctionDefinitionL
  SHiddenMacroSignatureL :: LabelSing HiddenMacroSignatureL
  SNativeFunctionDefinitionL :: LabelSing NativeFunctionDefinitionL
  SHiddenStructItemL :: LabelSing HiddenStructItemL
  SNativeStructDefinitionL :: LabelSing NativeStructDefinitionL
  SHiddenStructSignatureL :: LabelSing HiddenStructSignatureL
  SStructDefinitionL :: LabelSing StructDefinitionL
  SConstantL :: LabelSing ConstantL
  SConstantIdentifierL :: LabelSing ConstantIdentifierL
  SFriendDeclarationL :: LabelSing FriendDeclarationL
  SFriendAccessL :: LabelSing FriendAccessL
  SModuleBodyInternal2L :: LabelSing ModuleBodyInternal2L
  SBangTokL :: LabelSing BangTokL
  SNeqTokL :: LabelSing NeqTokL
  SDollarTokL :: LabelSing DollarTokL
  SModTokL :: LabelSing ModTokL
  SBitandTokL :: LabelSing BitandTokL
  SAndTokL :: LabelSing AndTokL
  SMulTokL :: LabelSing MulTokL
  SAddTokL :: LabelSing AddTokL
  SSubTokL :: LabelSing SubTokL
  SRangeTokL :: LabelSing RangeTokL
  SDivTokL :: LabelSing DivTokL
  SLtTokL :: LabelSing LtTokL
  SShlTokL :: LabelSing ShlTokL
  SLeTokL :: LabelSing LeTokL
  SAssignTokL :: LabelSing AssignTokL
  SEqTokL :: LabelSing EqTokL
  SImpliesTokL :: LabelSing ImpliesTokL
  SGtTokL :: LabelSing GtTokL
  SGeTokL :: LabelSing GeTokL
  SShrTokL :: LabelSing ShrTokL
  SXorTokL :: LabelSing XorTokL
  SAbortsWithTokL :: LabelSing AbortsWithTokL
  SAddressTokL :: LabelSing AddressTokL
  SAssertTokL :: LabelSing AssertTokL
  SAssumeTokL :: LabelSing AssumeTokL
  SBoolTokL :: LabelSing BoolTokL
  SBytearrayTokL :: LabelSing BytearrayTokL
  SCopyTokL :: LabelSing CopyTokL
  SDecreasesTokL :: LabelSing DecreasesTokL
  SDropTokL :: LabelSing DropTokL
  SEnsuresTokL :: LabelSing EnsuresTokL
  SEntryTokL :: LabelSing EntryTokL
  SFalseTokL :: LabelSing FalseTokL
  SFriendTokL :: LabelSing FriendTokL
  SGlobalTokL :: LabelSing GlobalTokL
  SInternalTokL :: LabelSing InternalTokL
  SKeyTokL :: LabelSing KeyTokL
  SLocalTokL :: LabelSing LocalTokL
  SModifiesTokL :: LabelSing ModifiesTokL
  SModuleTokL :: LabelSing ModuleTokL
  SMoveTokL :: LabelSing MoveTokL
  SNativeTokL :: LabelSing NativeTokL
  SPackTokL :: LabelSing PackTokL
  SPackageTokL :: LabelSing PackageTokL
  SPhantomTokL :: LabelSing PhantomTokL
  SPostTokL :: LabelSing PostTokL
  SPublicTokL :: LabelSing PublicTokL
  SSignerTokL :: LabelSing SignerTokL
  SStoreTokL :: LabelSing StoreTokL
  SSucceedsIfTokL :: LabelSing SucceedsIfTokL
  STrueTokL :: LabelSing TrueTokL
  SU128TokL :: LabelSing U128TokL
  SU16TokL :: LabelSing U16TokL
  SU256TokL :: LabelSing U256TokL
  SU32TokL :: LabelSing U32TokL
  SU64TokL :: LabelSing U64TokL
  SU8TokL :: LabelSing U8TokL
  SUnpackTokL :: LabelSing UnpackTokL
  SUpdateTokL :: LabelSing UpdateTokL
  SBitorTokL :: LabelSing BitorTokL
  SOrTokL :: LabelSing OrTokL

deriving instance Eq (LabelSing sort)

deriving instance Show (LabelSing sort)

decLabelSing :: LabelSing sort1 -> LabelSing sort2 -> Maybe (sort1 :~: sort2)
decLabelSing SSourceFileL SSourceFileL = Just Refl
decLabelSing SModuleDefinitionL SModuleDefinitionL = Just Refl
decLabelSing SModuleBodyL SModuleBodyL = Just Refl
decLabelSing SModuleBodyInternal0L SModuleBodyInternal0L = Just Refl
decLabelSing SModuleBodyInternal1L SModuleBodyInternal1L = Just Refl
decLabelSing SHiddenEnumItemL SHiddenEnumItemL = Just Refl
decLabelSing SEnumDefinitionL SEnumDefinitionL = Just Refl
decLabelSing SHiddenEnumSignatureL SHiddenEnumSignatureL = Just Refl
decLabelSing SHiddenEnumIdentifierL SHiddenEnumIdentifierL = Just Refl
decLabelSing SEnumIdentifierL SEnumIdentifierL = Just Refl
decLabelSing SHiddenEnumSignatureInternal0L SHiddenEnumSignatureInternal0L = Just Refl
decLabelSing STypeParametersL STypeParametersL = Just Refl
decLabelSing SHiddenSpecAbortWithOrModifiesInternal1L SHiddenSpecAbortWithOrModifiesInternal1L = Just Refl
decLabelSing STypeParameterL STypeParameterL = Just Refl
decLabelSing SHiddenTypeParameterIdentifierL SHiddenTypeParameterIdentifierL = Just Refl
decLabelSing STypeParameterIdentifierL STypeParameterIdentifierL = Just Refl
decLabelSing STypeParameterInternal0L STypeParameterInternal0L = Just Refl
decLabelSing STypeParameterInternal1L STypeParameterInternal1L = Just Refl
decLabelSing STypeParameterInternal3L STypeParameterInternal3L = Just Refl
decLabelSing SAbilityL SAbilityL = Just Refl
decLabelSing STypeParameterInternal2L STypeParameterInternal2L = Just Refl
decLabelSing SHiddenEnumSignatureInternal1L SHiddenEnumSignatureInternal1L = Just Refl
decLabelSing SAbilityDeclsL SAbilityDeclsL = Just Refl
decLabelSing SAbilityDeclsInternal0L SAbilityDeclsInternal0L = Just Refl
decLabelSing SEnumDefinitionInternal0L SEnumDefinitionInternal0L = Just Refl
decLabelSing SEnumDefinitionInternal1L SEnumDefinitionInternal1L = Just Refl
decLabelSing SPostfixAbilityDeclsL SPostfixAbilityDeclsL = Just Refl
decLabelSing SEnumVariantsL SEnumVariantsL = Just Refl
decLabelSing SEnumVariantsInternal0L SEnumVariantsInternal0L = Just Refl
decLabelSing SVariantL SVariantL = Just Refl
decLabelSing SHiddenVariantIdentifierL SHiddenVariantIdentifierL = Just Refl
decLabelSing SVariantIdentifierL SVariantIdentifierL = Just Refl
decLabelSing SVariantInternal0L SVariantInternal0L = Just Refl
decLabelSing SDatatypeFieldsL SDatatypeFieldsL = Just Refl
decLabelSing SNamedFieldsL SNamedFieldsL = Just Refl
decLabelSing SFieldAnnotationL SFieldAnnotationL = Just Refl
decLabelSing SHiddenFieldIdentifierL SHiddenFieldIdentifierL = Just Refl
decLabelSing SFieldIdentifierL SFieldIdentifierL = Just Refl
decLabelSing SHiddenTypeL SHiddenTypeL = Just Refl
decLabelSing SApplyTypeL SApplyTypeL = Just Refl
decLabelSing SApplyTypeInternal0L SApplyTypeInternal0L = Just Refl
decLabelSing STypeArgumentsL STypeArgumentsL = Just Refl
decLabelSing SModuleAccessL SModuleAccessL = Just Refl
decLabelSing SHiddenModuleIdentifierL SHiddenModuleIdentifierL = Just Refl
decLabelSing SModuleIdentifierL SModuleIdentifierL = Just Refl
decLabelSing SIdentifierL SIdentifierL = Just Refl
decLabelSing SModuleIdentityL SModuleIdentityL = Just Refl
decLabelSing SModuleIdentityAddressInternal0L SModuleIdentityAddressInternal0L = Just Refl
decLabelSing SNumLiteralL SNumLiteralL = Just Refl
decLabelSing SNumLiteralInternal0L SNumLiteralInternal0L = Just Refl
decLabelSing SNumLiteralInternal2L SNumLiteralInternal2L = Just Refl
decLabelSing SNumLiteralInternal1L SNumLiteralInternal1L = Just Refl
decLabelSing SFunctionTypeL SFunctionTypeL = Just Refl
decLabelSing SFunctionTypeInternal0L SFunctionTypeInternal0L = Just Refl
decLabelSing SFunctionTypeParametersL SFunctionTypeParametersL = Just Refl
decLabelSing SFunctionTypeParametersInternal0L SFunctionTypeParametersInternal0L = Just Refl
decLabelSing SPrimitiveTypeL SPrimitiveTypeL = Just Refl
decLabelSing SRefTypeL SRefTypeL = Just Refl
decLabelSing SHiddenReferenceL SHiddenReferenceL = Just Refl
decLabelSing SImmRefL SImmRefL = Just Refl
decLabelSing SMutRefL SMutRefL = Just Refl
decLabelSing STupleTypeL STupleTypeL = Just Refl
decLabelSing SNamedFieldsInternal0L SNamedFieldsInternal0L = Just Refl
decLabelSing SPositionalFieldsL SPositionalFieldsL = Just Refl
decLabelSing SHiddenFunctionItemL SHiddenFunctionItemL = Just Refl
decLabelSing SFunctionDefinitionL SFunctionDefinitionL = Just Refl
decLabelSing SHiddenFunctionSignatureL SHiddenFunctionSignatureL = Just Refl
decLabelSing SHiddenFunctionIdentifierL SHiddenFunctionIdentifierL = Just Refl
decLabelSing SFunctionIdentifierL SFunctionIdentifierL = Just Refl
decLabelSing SHiddenFunctionSignatureInternal0L SHiddenFunctionSignatureInternal0L = Just Refl
decLabelSing SModifierL SModifierL = Just Refl
decLabelSing SModifierInternal1L SModifierInternal1L = Just Refl
decLabelSing SModifierInternal0L SModifierInternal0L = Just Refl
decLabelSing SHiddenFunctionSignatureInternal1L SHiddenFunctionSignatureInternal1L = Just Refl
decLabelSing SRetTypeL SRetTypeL = Just Refl
decLabelSing SFunctionParametersL SFunctionParametersL = Just Refl
decLabelSing SFunctionParametersInternal0L SFunctionParametersInternal0L = Just Refl
decLabelSing SFunctionParameterL SFunctionParameterL = Just Refl
decLabelSing SFunctionParameterInternal0L SFunctionParameterInternal0L = Just Refl
decLabelSing SHiddenVariableIdentifierL SHiddenVariableIdentifierL = Just Refl
decLabelSing SVariableIdentifierL SVariableIdentifierL = Just Refl
decLabelSing SMutFunctionParameterL SMutFunctionParameterL = Just Refl
decLabelSing SFunctionParametersInternal1L SFunctionParametersInternal1L = Just Refl
decLabelSing SBlockL SBlockL = Just Refl
decLabelSing SArgListInternal0L SArgListInternal0L = Just Refl
decLabelSing SHiddenExpressionL SHiddenExpressionL = Just Refl
decLabelSing SHiddenUnaryExpressionL SHiddenUnaryExpressionL = Just Refl
decLabelSing SHiddenUnaryExpressionInternal0L SHiddenUnaryExpressionInternal0L = Just Refl
decLabelSing SHiddenExpressionTermL SHiddenExpressionTermL = Just Refl
decLabelSing SHiddenLiteralValueL SHiddenLiteralValueL = Just Refl
decLabelSing SAddressLiteralL SAddressLiteralL = Just Refl
decLabelSing SBoolLiteralL SBoolLiteralL = Just Refl
decLabelSing SByteStringLiteralL SByteStringLiteralL = Just Refl
decLabelSing SHexStringLiteralL SHexStringLiteralL = Just Refl
decLabelSing SAnnotationExpressionL SAnnotationExpressionL = Just Refl
decLabelSing SBreakExpressionL SBreakExpressionL = Just Refl
decLabelSing SBreakExpressionInternal0L SBreakExpressionInternal0L = Just Refl
decLabelSing SLabelL SLabelL = Just Refl
decLabelSing SBreakExpressionInternal1L SBreakExpressionInternal1L = Just Refl
decLabelSing SCallExpressionL SCallExpressionL = Just Refl
decLabelSing SArgListL SArgListL = Just Refl
decLabelSing SNameExpressionL SNameExpressionL = Just Refl
decLabelSing SNameExpressionInternal0L SNameExpressionInternal0L = Just Refl
decLabelSing SContinueExpressionL SContinueExpressionL = Just Refl
decLabelSing SDotExpressionL SDotExpressionL = Just Refl
decLabelSing SExpressionListL SExpressionListL = Just Refl
decLabelSing SIfExpressionL SIfExpressionL = Just Refl
decLabelSing SIfExpressionInternal0L SIfExpressionInternal0L = Just Refl
decLabelSing SIndexExpressionL SIndexExpressionL = Just Refl
decLabelSing SIndexExpressionInternal0L SIndexExpressionInternal0L = Just Refl
decLabelSing SMacroCallExpressionL SMacroCallExpressionL = Just Refl
decLabelSing SMacroModuleAccessL SMacroModuleAccessL = Just Refl
decLabelSing SMatchExpressionL SMatchExpressionL = Just Refl
decLabelSing SHiddenMatchBodyL SHiddenMatchBodyL = Just Refl
decLabelSing SHiddenMatchBodyInternal0L SHiddenMatchBodyInternal0L = Just Refl
decLabelSing SMatchArmL SMatchArmL = Just Refl
decLabelSing SBindListL SBindListL = Just Refl
decLabelSing SHiddenBindL SHiddenBindL = Just Refl
decLabelSing SHiddenBindInternal0L SHiddenBindInternal0L = Just Refl
decLabelSing SMutBindVarL SMutBindVarL = Just Refl
decLabelSing SBindVarL SBindVarL = Just Refl
decLabelSing SAtBindL SAtBindL = Just Refl
decLabelSing SBindUnpackL SBindUnpackL = Just Refl
decLabelSing SBindUnpackInternal0L SBindUnpackInternal0L = Just Refl
decLabelSing SBindFieldsL SBindFieldsL = Just Refl
decLabelSing SBindNamedFieldsL SBindNamedFieldsL = Just Refl
decLabelSing SBindNamedFieldsInternal0L SBindNamedFieldsInternal0L = Just Refl
decLabelSing SBindFieldL SBindFieldL = Just Refl
decLabelSing SHiddenSpreadOperatorL SHiddenSpreadOperatorL = Just Refl
decLabelSing SBindFieldInternal0L SBindFieldInternal0L = Just Refl
decLabelSing SMutBindFieldL SMutBindFieldL = Just Refl
decLabelSing SBindNamedFieldsInternal1L SBindNamedFieldsInternal1L = Just Refl
decLabelSing SBindPositionalFieldsL SBindPositionalFieldsL = Just Refl
decLabelSing SCommaBindListL SCommaBindListL = Just Refl
decLabelSing SCommaBindListInternal0L SCommaBindListInternal0L = Just Refl
decLabelSing SOrBindListL SOrBindListL = Just Refl
decLabelSing SOrBindListInternal0L SOrBindListInternal0L = Just Refl
decLabelSing SOrBindListInternal1L SOrBindListInternal1L = Just Refl
decLabelSing SOrBindListInternal2L SOrBindListInternal2L = Just Refl
decLabelSing SMatchArmInternal0L SMatchArmInternal0L = Just Refl
decLabelSing SMatchConditionL SMatchConditionL = Just Refl
decLabelSing SPackExpressionL SPackExpressionL = Just Refl
decLabelSing SFieldInitializeListL SFieldInitializeListL = Just Refl
decLabelSing SExpFieldL SExpFieldL = Just Refl
decLabelSing SExpFieldInternal0L SExpFieldInternal0L = Just Refl
decLabelSing SFieldInitializeListInternal0L SFieldInitializeListInternal0L = Just Refl
decLabelSing SSpecBlockL SSpecBlockL = Just Refl
decLabelSing SSpecBlockInternal1L SSpecBlockInternal1L = Just Refl
decLabelSing SHiddenSpecFunctionL SHiddenSpecFunctionL = Just Refl
decLabelSing SNativeSpecFunctionL SNativeSpecFunctionL = Just Refl
decLabelSing SHiddenSpecFunctionSignatureL SHiddenSpecFunctionSignatureL = Just Refl
decLabelSing SUninterpretedSpecFunctionL SUninterpretedSpecFunctionL = Just Refl
decLabelSing SUsualSpecFunctionL SUsualSpecFunctionL = Just Refl
decLabelSing SSpecBlockInternal0L SSpecBlockInternal0L = Just Refl
decLabelSing SHiddenSpecBlockTargetL SHiddenSpecBlockTargetL = Just Refl
decLabelSing SSpecBlockTargetSchemaL SSpecBlockTargetSchemaL = Just Refl
decLabelSing SHiddenStructIdentifierL SHiddenStructIdentifierL = Just Refl
decLabelSing SStructIdentifierL SStructIdentifierL = Just Refl
decLabelSing SSpecBodyL SSpecBodyL = Just Refl
decLabelSing SHiddenSpecBlockMemeberL SHiddenSpecBlockMemeberL = Just Refl
decLabelSing SSpecApplyL SSpecApplyL = Just Refl
decLabelSing SSpecApplyInternal0L SSpecApplyInternal0L = Just Refl
decLabelSing SSpecApplyPatternL SSpecApplyPatternL = Just Refl
decLabelSing SSpecApplyNamePatternL SSpecApplyNamePatternL = Just Refl
decLabelSing SSpecApplyPatternInternal1L SSpecApplyPatternInternal1L = Just Refl
decLabelSing SSpecApplyPatternInternal0L SSpecApplyPatternInternal0L = Just Refl
decLabelSing SSpecConditionL SSpecConditionL = Just Refl
decLabelSing SHiddenSpecAbortIfL SHiddenSpecAbortIfL = Just Refl
decLabelSing SHiddenSpecAbortIfInternal0L SHiddenSpecAbortIfInternal0L = Just Refl
decLabelSing SConditionPropertiesL SConditionPropertiesL = Just Refl
decLabelSing SConditionPropertiesInternal0L SConditionPropertiesInternal0L = Just Refl
decLabelSing SSpecPropertyL SSpecPropertyL = Just Refl
decLabelSing SSpecPropertyInternal0L SSpecPropertyInternal0L = Just Refl
decLabelSing SHiddenSpecAbortIfInternal1L SHiddenSpecAbortIfInternal1L = Just Refl
decLabelSing SConditionKindL SConditionKindL = Just Refl
decLabelSing SHiddenSpecAbortWithOrModifiesL SHiddenSpecAbortWithOrModifiesL = Just Refl
decLabelSing SHiddenSpecConditionL SHiddenSpecConditionL = Just Refl
decLabelSing SHiddenSpecConditionInternal1L SHiddenSpecConditionInternal1L = Just Refl
decLabelSing SHiddenSpecConditionInternal0L SHiddenSpecConditionInternal0L = Just Refl
decLabelSing SSpecIncludeL SSpecIncludeL = Just Refl
decLabelSing SSpecInvariantL SSpecInvariantL = Just Refl
decLabelSing SSpecInvariantInternal1L SSpecInvariantInternal1L = Just Refl
decLabelSing SInvariantModifierL SInvariantModifierL = Just Refl
decLabelSing SSpecLetL SSpecLetL = Just Refl
decLabelSing SSpecLetInternal0L SSpecLetInternal0L = Just Refl
decLabelSing SSpecPragmaL SSpecPragmaL = Just Refl
decLabelSing SSpecVariableL SSpecVariableL = Just Refl
decLabelSing SSpecVariableInternal1L SSpecVariableInternal1L = Just Refl
decLabelSing SSpecVariableInternal0L SSpecVariableInternal0L = Just Refl
decLabelSing SUseDeclarationL SUseDeclarationL = Just Refl
decLabelSing SUseDeclarationInternal0L SUseDeclarationInternal0L = Just Refl
decLabelSing SUseFunL SUseFunL = Just Refl
decLabelSing SUseModuleL SUseModuleL = Just Refl
decLabelSing SUseModuleInternal0L SUseModuleInternal0L = Just Refl
decLabelSing SUseModuleMemberL SUseModuleMemberL = Just Refl
decLabelSing SUseMemberL SUseMemberL = Just Refl
decLabelSing SUseMemberInternal0L SUseMemberInternal0L = Just Refl
decLabelSing SUseModuleMembersL SUseModuleMembersL = Just Refl
decLabelSing SUnitExpressionL SUnitExpressionL = Just Refl
decLabelSing SVectorExpressionL SVectorExpressionL = Just Refl
decLabelSing SVectorExpressionInternal0L SVectorExpressionInternal0L = Just Refl
decLabelSing SBorrowExpressionL SBorrowExpressionL = Just Refl
decLabelSing SDereferenceExpressionL SDereferenceExpressionL = Just Refl
decLabelSing SMoveOrCopyExpressionL SMoveOrCopyExpressionL = Just Refl
decLabelSing SMoveOrCopyExpressionInternal0L SMoveOrCopyExpressionInternal0L = Just Refl
decLabelSing SUnaryExpressionL SUnaryExpressionL = Just Refl
decLabelSing SUnaryOpL SUnaryOpL = Just Refl
decLabelSing SAbortExpressionL SAbortExpressionL = Just Refl
decLabelSing SAbortExpressionInternal0L SAbortExpressionInternal0L = Just Refl
decLabelSing SAssignExpressionL SAssignExpressionL = Just Refl
decLabelSing SBinaryExpressionL SBinaryExpressionL = Just Refl
decLabelSing SBinaryOperatorL SBinaryOperatorL = Just Refl
decLabelSing SCastExpressionL SCastExpressionL = Just Refl
decLabelSing SIdentifiedExpressionL SIdentifiedExpressionL = Just Refl
decLabelSing SBlockIdentifierL SBlockIdentifierL = Just Refl
decLabelSing SLambdaExpressionL SLambdaExpressionL = Just Refl
decLabelSing SLambdaBindingsL SLambdaBindingsL = Just Refl
decLabelSing SLambdaBindingL SLambdaBindingL = Just Refl
decLabelSing SLambdaBindingInternal0L SLambdaBindingInternal0L = Just Refl
decLabelSing SLambdaBindingsInternal0L SLambdaBindingsInternal0L = Just Refl
decLabelSing SLambdaExpressionInternal0L SLambdaExpressionInternal0L = Just Refl
decLabelSing SLoopExpressionL SLoopExpressionL = Just Refl
decLabelSing SQuantifierExpressionL SQuantifierExpressionL = Just Refl
decLabelSing SQuantifierBindingsL SQuantifierBindingsL = Just Refl
decLabelSing SQuantifierBindingL SQuantifierBindingL = Just Refl
decLabelSing SQuantifierExpressionInternal0L SQuantifierExpressionInternal0L = Just Refl
decLabelSing SHiddenExistsL SHiddenExistsL = Just Refl
decLabelSing SHiddenForallL SHiddenForallL = Just Refl
decLabelSing SQuantifierExpressionInternal1L SQuantifierExpressionInternal1L = Just Refl
decLabelSing SReturnExpressionL SReturnExpressionL = Just Refl
decLabelSing SWhileExpressionL SWhileExpressionL = Just Refl
decLabelSing SBlockItemL SBlockItemL = Just Refl
decLabelSing SBlockItemInternal0L SBlockItemInternal0L = Just Refl
decLabelSing SLetStatementL SLetStatementL = Just Refl
decLabelSing SLetStatementInternal0L SLetStatementInternal0L = Just Refl
decLabelSing SLetStatementInternal1L SLetStatementInternal1L = Just Refl
decLabelSing SMacroFunctionDefinitionL SMacroFunctionDefinitionL = Just Refl
decLabelSing SHiddenMacroSignatureL SHiddenMacroSignatureL = Just Refl
decLabelSing SNativeFunctionDefinitionL SNativeFunctionDefinitionL = Just Refl
decLabelSing SHiddenStructItemL SHiddenStructItemL = Just Refl
decLabelSing SNativeStructDefinitionL SNativeStructDefinitionL = Just Refl
decLabelSing SHiddenStructSignatureL SHiddenStructSignatureL = Just Refl
decLabelSing SStructDefinitionL SStructDefinitionL = Just Refl
decLabelSing SConstantL SConstantL = Just Refl
decLabelSing SConstantIdentifierL SConstantIdentifierL = Just Refl
decLabelSing SFriendDeclarationL SFriendDeclarationL = Just Refl
decLabelSing SFriendAccessL SFriendAccessL = Just Refl
decLabelSing SModuleBodyInternal2L SModuleBodyInternal2L = Just Refl
decLabelSing SBangTokL SBangTokL = Just Refl
decLabelSing SNeqTokL SNeqTokL = Just Refl
decLabelSing SDollarTokL SDollarTokL = Just Refl
decLabelSing SModTokL SModTokL = Just Refl
decLabelSing SBitandTokL SBitandTokL = Just Refl
decLabelSing SAndTokL SAndTokL = Just Refl
decLabelSing SMulTokL SMulTokL = Just Refl
decLabelSing SAddTokL SAddTokL = Just Refl
decLabelSing SSubTokL SSubTokL = Just Refl
decLabelSing SRangeTokL SRangeTokL = Just Refl
decLabelSing SDivTokL SDivTokL = Just Refl
decLabelSing SLtTokL SLtTokL = Just Refl
decLabelSing SShlTokL SShlTokL = Just Refl
decLabelSing SLeTokL SLeTokL = Just Refl
decLabelSing SAssignTokL SAssignTokL = Just Refl
decLabelSing SEqTokL SEqTokL = Just Refl
decLabelSing SImpliesTokL SImpliesTokL = Just Refl
decLabelSing SGtTokL SGtTokL = Just Refl
decLabelSing SGeTokL SGeTokL = Just Refl
decLabelSing SShrTokL SShrTokL = Just Refl
decLabelSing SXorTokL SXorTokL = Just Refl
decLabelSing SAbortsWithTokL SAbortsWithTokL = Just Refl
decLabelSing SAddressTokL SAddressTokL = Just Refl
decLabelSing SAssertTokL SAssertTokL = Just Refl
decLabelSing SAssumeTokL SAssumeTokL = Just Refl
decLabelSing SBoolTokL SBoolTokL = Just Refl
decLabelSing SBytearrayTokL SBytearrayTokL = Just Refl
decLabelSing SCopyTokL SCopyTokL = Just Refl
decLabelSing SDecreasesTokL SDecreasesTokL = Just Refl
decLabelSing SDropTokL SDropTokL = Just Refl
decLabelSing SEnsuresTokL SEnsuresTokL = Just Refl
decLabelSing SEntryTokL SEntryTokL = Just Refl
decLabelSing SFalseTokL SFalseTokL = Just Refl
decLabelSing SFriendTokL SFriendTokL = Just Refl
decLabelSing SGlobalTokL SGlobalTokL = Just Refl
decLabelSing SInternalTokL SInternalTokL = Just Refl
decLabelSing SKeyTokL SKeyTokL = Just Refl
decLabelSing SLocalTokL SLocalTokL = Just Refl
decLabelSing SModifiesTokL SModifiesTokL = Just Refl
decLabelSing SModuleTokL SModuleTokL = Just Refl
decLabelSing SMoveTokL SMoveTokL = Just Refl
decLabelSing SNativeTokL SNativeTokL = Just Refl
decLabelSing SPackTokL SPackTokL = Just Refl
decLabelSing SPackageTokL SPackageTokL = Just Refl
decLabelSing SPhantomTokL SPhantomTokL = Just Refl
decLabelSing SPostTokL SPostTokL = Just Refl
decLabelSing SPublicTokL SPublicTokL = Just Refl
decLabelSing SSignerTokL SSignerTokL = Just Refl
decLabelSing SStoreTokL SStoreTokL = Just Refl
decLabelSing SSucceedsIfTokL SSucceedsIfTokL = Just Refl
decLabelSing STrueTokL STrueTokL = Just Refl
decLabelSing SU128TokL SU128TokL = Just Refl
decLabelSing SU16TokL SU16TokL = Just Refl
decLabelSing SU256TokL SU256TokL = Just Refl
decLabelSing SU32TokL SU32TokL = Just Refl
decLabelSing SU64TokL SU64TokL = Just Refl
decLabelSing SU8TokL SU8TokL = Just Refl
decLabelSing SUnpackTokL SUnpackTokL = Just Refl
decLabelSing SUpdateTokL SUpdateTokL = Just Refl
decLabelSing SBitorTokL SBitorTokL = Just Refl
decLabelSing SOrTokL SOrTokL = Just Refl
decLabelSing _ _ = Nothing

--------------------------------------------------------------------------------
-- SymbolType
--------------------------------------------------------------------------------

type data SymbolType where
  Regular :: SymbolType
  Anonymous :: SymbolType
  Auxiliary :: SymbolType
  Virtual :: SymbolType

data SymbolTypeSing (symbolType :: SymbolType) where
  SRegular :: SymbolTypeSing Regular
  SAnonymous :: SymbolTypeSing Anonymous
  SAuxiliary :: SymbolTypeSing Auxiliary
  SVirtual :: SymbolTypeSing Virtual

deriving instance Eq (SymbolTypeSing symbolType)

deriving instance Show (SymbolTypeSing symbolType)

decSymbolTypeSing :: SymbolTypeSing symbolType1 -> SymbolTypeSing symbolType2 -> Maybe (symbolType1 :~: symbolType2)
decSymbolTypeSing SRegular SRegular = Just Refl
decSymbolTypeSing SAnonymous SAnonymous = Just Refl
decSymbolTypeSing SAuxiliary SAuxiliary = Just Refl
decSymbolTypeSing SVirtual SVirtual = Just Refl
decSymbolTypeSing _ _ = Nothing

data IsReal (symbolType :: SymbolType) where
  RegularIsReal :: IsReal Regular
  AnonymousIsReal :: IsReal Anonymous
  AuxiliaryIsReal :: IsReal Auxiliary

deriving instance Eq (IsReal symbolType)

deriving instance Show (IsReal symbolType)

symbolTypeIsReal :: SymbolTypeSing symbolType -> Either (IsReal symbolType) (symbolType :~: Virtual)
symbolTypeIsReal = \case
  SRegular -> Left RegularIsReal
  SAnonymous -> Left AnonymousIsReal
  SAuxiliary -> Left AuxiliaryIsReal
  SVirtual -> Right Refl

--------------------------------------------------------------------------------
-- Modularized syntax
--------------------------------------------------------------------------------

data Token e l where
  Bang :: Token e BangTokL
  Neq :: Token e NeqTokL
  Dollar :: Token e DollarTokL
  Mod :: Token e ModTokL
  Bitand :: Token e BitandTokL
  And :: Token e AndTokL
  Mul :: Token e MulTokL
  Add :: Token e AddTokL
  Sub :: Token e SubTokL
  Range :: Token e RangeTokL
  Div :: Token e DivTokL
  Lt :: Token e LtTokL
  Shl :: Token e ShlTokL
  Le :: Token e LeTokL
  Assign :: Token e AssignTokL
  Eq :: Token e EqTokL
  Implies :: Token e ImpliesTokL
  Gt :: Token e GtTokL
  Ge :: Token e GeTokL
  Shr :: Token e ShrTokL
  Xor :: Token e XorTokL
  AbortsWith :: Token e AbortsWithTokL
  Address :: Token e AddressTokL
  Assert :: Token e AssertTokL
  Assume :: Token e AssumeTokL
  Bool :: Token e BoolTokL
  Bytearray :: Token e BytearrayTokL
  Copy :: Token e CopyTokL
  Decreases :: Token e DecreasesTokL
  Drop :: Token e DropTokL
  Ensures :: Token e EnsuresTokL
  Entry :: Token e EntryTokL
  False :: Token e FalseTokL
  Friend :: Token e FriendTokL
  Global :: Token e GlobalTokL
  Internal :: Token e InternalTokL
  Key :: Token e KeyTokL
  Local :: Token e LocalTokL
  Modifies :: Token e ModifiesTokL
  Module :: Token e ModuleTokL
  Move :: Token e MoveTokL
  Native :: Token e NativeTokL
  Pack :: Token e PackTokL
  Package :: Token e PackageTokL
  Phantom :: Token e PhantomTokL
  Post :: Token e PostTokL
  Public :: Token e PublicTokL
  Signer :: Token e SignerTokL
  Store :: Token e StoreTokL
  SucceedsIf :: Token e SucceedsIfTokL
  True :: Token e TrueTokL
  U128 :: Token e U128TokL
  U16 :: Token e U16TokL
  U256 :: Token e U256TokL
  U32 :: Token e U32TokL
  U64 :: Token e U64TokL
  U8 :: Token e U8TokL
  Unpack :: Token e UnpackTokL
  Update :: Token e UpdateTokL
  Bitor :: Token e BitorTokL
  Or :: Token e OrTokL

deriveAll [''Token]

data SourceFile e l where
  SourceFile
    :: e [ModuleDefinitionL]
    -> SourceFile e SourceFileL

data ModuleDefinition e l where
  ModuleDefinition
    :: e ModuleTokL
    -> e ModuleIdentityL
    -> e ModuleBodyL
    -> ModuleDefinition e ModuleDefinitionL

data ModuleBody e l where
  ModuleBody
    :: e ModuleBodyInternal0L
    -> e [ModuleBodyInternal1L]
    -> e ModuleBodyInternal2L
    -> ModuleBody e ModuleBodyL

data ModuleBodyInternal0 e l where

data ModuleBodyInternal1 e l where
  UseDeclarationModuleBodyInternal1
    :: e UseDeclarationL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  FriendDeclarationModuleBodyInternal1
    :: e FriendDeclarationL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  ConstantModuleBodyInternal1
    :: e ConstantL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  HiddenFunctionItemModuleBodyInternal1
    :: e HiddenFunctionItemL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  HiddenStructItemModuleBodyInternal1
    :: e HiddenStructItemL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  HiddenEnumItemModuleBodyInternal1
    :: e HiddenEnumItemL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  SpecBlockModuleBodyInternal1
    :: e SpecBlockL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L

data HiddenEnumItem e l where
  EnumDefinitionEnumItem
    :: e EnumDefinitionL
    -> HiddenEnumItem e HiddenEnumItemL

data EnumDefinition e l where
  EnumDefinition
    :: e EnumDefinitionInternal0L
    -> e HiddenEnumSignatureL
    -> e EnumVariantsL
    -> e EnumDefinitionInternal1L
    -> EnumDefinition e EnumDefinitionL

data HiddenEnumSignature e l where
  HiddenEnumSignature
    :: e HiddenEnumIdentifierL
    -> e HiddenEnumSignatureInternal0L
    -> e HiddenEnumSignatureInternal1L
    -> HiddenEnumSignature e HiddenEnumSignatureL

data HiddenEnumIdentifier e l where
  HiddenEnumIdentifier
    :: e EnumIdentifierL
    -> HiddenEnumIdentifier e HiddenEnumIdentifierL

data EnumIdentifier e l where
  EnumIdentifier
    :: EnumIdentifier e EnumIdentifierL

data HiddenEnumSignatureInternal0 e l where
  TypeParametersEnumSignatureInternal0
    :: e TypeParametersL
    -> HiddenEnumSignatureInternal0 e HiddenEnumSignatureInternal0L

data TypeParameters e l where
  TypeParameters
    :: e LtTokL
    -> e ((TypeParameterL, [TypeParameterL]), HiddenSpecAbortWithOrModifiesInternal1L)
    -> e GtTokL
    -> TypeParameters e TypeParametersL

data HiddenSpecAbortWithOrModifiesInternal1 e l where

data TypeParameter e l where
  TypeParameter
    :: e TypeParameterInternal0L
    -> e TypeParameterInternal1L
    -> e HiddenTypeParameterIdentifierL
    -> e TypeParameterInternal3L
    -> TypeParameter e TypeParameterL

data HiddenTypeParameterIdentifier e l where
  HiddenTypeParameterIdentifier
    :: e TypeParameterIdentifierL
    -> HiddenTypeParameterIdentifier e HiddenTypeParameterIdentifierL

data TypeParameterIdentifier e l where
  TypeParameterIdentifier
    :: TypeParameterIdentifier e TypeParameterIdentifierL

data TypeParameterInternal0 e l where
  DollarTypeParameterInternal0
    :: e DollarTokL
    -> TypeParameterInternal0 e TypeParameterInternal0L

data TypeParameterInternal1 e l where
  PhantomTypeParameterInternal1
    :: e PhantomTokL
    -> TypeParameterInternal1 e TypeParameterInternal1L

data TypeParameterInternal3 e l where
  TypeParameterInternal31
    :: e ((AbilityL, [(AddTokL, AbilityL)]), TypeParameterInternal2L)
    -> TypeParameterInternal3 e TypeParameterInternal3L

data Ability e l where
  CopyAbility
    :: e CopyTokL
    -> Ability e AbilityL
  DropAbility
    :: e DropTokL
    -> Ability e AbilityL
  StoreAbility
    :: e StoreTokL
    -> Ability e AbilityL
  KeyAbility
    :: e KeyTokL
    -> Ability e AbilityL

data TypeParameterInternal2 e l where
  AddTypeParameterInternal2
    :: e AddTokL
    -> TypeParameterInternal2 e TypeParameterInternal2L

data HiddenEnumSignatureInternal1 e l where
  AbilityDeclarationsEnumSignatureInternal1
    :: e AbilityDeclsL
    -> HiddenEnumSignatureInternal1 e HiddenEnumSignatureInternal1L

data AbilityDecls e l where
  AbilityDecls
    :: e ([AbilityL], AbilityDeclsInternal0L)
    -> AbilityDecls e AbilityDeclsL

data AbilityDeclsInternal0 e l where
  AbilityAbilityDeclsInternal0
    :: e AbilityL
    -> AbilityDeclsInternal0 e AbilityDeclsInternal0L

data EnumDefinitionInternal0 e l where
  PublicEnumDefinitionInternal0
    :: e PublicTokL
    -> EnumDefinitionInternal0 e EnumDefinitionInternal0L

data EnumDefinitionInternal1 e l where
  PostfixAbilityDeclarationsEnumDefinitionInternal1
    :: e PostfixAbilityDeclsL
    -> EnumDefinitionInternal1 e EnumDefinitionInternal1L

data PostfixAbilityDecls e l where
  PostfixAbilityDecls
    :: e ([AbilityL], AbilityDeclsInternal0L)
    -> PostfixAbilityDecls e PostfixAbilityDeclsL

data EnumVariants e l where
  EnumVariants
    :: e ([VariantL], EnumVariantsInternal0L)
    -> EnumVariants e EnumVariantsL

data EnumVariantsInternal0 e l where
  VariantEnumVariantsInternal0
    :: e VariantL
    -> EnumVariantsInternal0 e EnumVariantsInternal0L

data Variant e l where
  Variant
    :: e HiddenVariantIdentifierL
    -> e VariantInternal0L
    -> Variant e VariantL

data HiddenVariantIdentifier e l where
  HiddenVariantIdentifier
    :: e VariantIdentifierL
    -> HiddenVariantIdentifier e HiddenVariantIdentifierL

data VariantIdentifier e l where
  VariantIdentifier
    :: VariantIdentifier e VariantIdentifierL

data VariantInternal0 e l where
  FieldsVariantInternal0
    :: e DatatypeFieldsL
    -> VariantInternal0 e VariantInternal0L

data DatatypeFields e l where
  PositionalFieldsDatatypeFields
    :: e PositionalFieldsL
    -> DatatypeFields e DatatypeFieldsL
  NamedFieldsDatatypeFields
    :: e NamedFieldsL
    -> DatatypeFields e DatatypeFieldsL

data NamedFields e l where
  NamedFields
    :: e ([FieldAnnotationL], NamedFieldsInternal0L)
    -> NamedFields e NamedFieldsL

data FieldAnnotation e l where
  FieldAnnotation
    :: e HiddenFieldIdentifierL
    -> e HiddenTypeL
    -> FieldAnnotation e FieldAnnotationL

data HiddenFieldIdentifier e l where
  HiddenFieldIdentifier
    :: e FieldIdentifierL
    -> HiddenFieldIdentifier e HiddenFieldIdentifierL

data FieldIdentifier e l where
  FieldIdentifier
    :: FieldIdentifier e FieldIdentifierL

data HiddenType e l where
  ApplyTypeType
    :: e ApplyTypeL
    -> HiddenType e HiddenTypeL
  RefTypeType
    :: e RefTypeL
    -> HiddenType e HiddenTypeL
  TupleTypeType
    :: e TupleTypeL
    -> HiddenType e HiddenTypeL
  FunctionTypeType
    :: e FunctionTypeL
    -> HiddenType e HiddenTypeL
  PrimitiveTypeType
    :: e PrimitiveTypeL
    -> HiddenType e HiddenTypeL

data ApplyType e l where
  ApplyType
    :: e ModuleAccessL
    -> e ApplyTypeInternal0L
    -> ApplyType e ApplyTypeL

data ApplyTypeInternal0 e l where
  TypeArgumentsApplyTypeInternal0
    :: e TypeArgumentsL
    -> ApplyTypeInternal0 e ApplyTypeInternal0L

data TypeArguments e l where
  TypeArguments
    :: e LtTokL
    -> e ((HiddenTypeL, [HiddenTypeL]), HiddenSpecAbortWithOrModifiesInternal1L)
    -> e GtTokL
    -> TypeArguments e TypeArgumentsL

data ModuleAccess e l where
  ModuleAccess1
    :: e DollarTokL
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess2
    :: e IdentifierL
    -> ModuleAccess e ModuleAccessL
  MemberModuleAccess
    :: e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess4
    :: e IdentifierL
    -> e ApplyTypeInternal0L
    -> ModuleAccess e ModuleAccessL
  ModuleAccess5
    :: e HiddenModuleIdentifierL
    -> e ApplyTypeInternal0L
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess6
    :: e ModuleIdentityL
    -> e IdentifierL
    -> e TypeArgumentsL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess7
    :: e ModuleIdentityL
    -> e ApplyTypeInternal0L
    -> ModuleAccess e ModuleAccessL
  ModuleAccess8
    :: e ModuleIdentityL
    -> e ApplyTypeInternal0L
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess9
    :: e ModuleIdentityL
    -> e IdentifierL
    -> e ApplyTypeInternal0L
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL

data HiddenModuleIdentifier e l where
  HiddenModuleIdentifier
    :: e ModuleIdentifierL
    -> HiddenModuleIdentifier e HiddenModuleIdentifierL

data ModuleIdentifier e l where
  ModuleIdentifier
    :: ModuleIdentifier e ModuleIdentifierL

data Identifier e l where
  Identifier
    :: Identifier e IdentifierL

data ModuleIdentity e l where
  ModuleIdentity
    :: e ModuleIdentityAddressInternal0L
    -> e HiddenModuleIdentifierL
    -> ModuleIdentity e ModuleIdentityL

data ModuleIdentityAddressInternal0 e l where
  NumLiteralModuleIdentityAddressInternal0
    :: e NumLiteralL
    -> ModuleIdentityAddressInternal0 e ModuleIdentityAddressInternal0L
  HiddenModuleIdentifierModuleIdentityAddressInternal0
    :: e HiddenModuleIdentifierL
    -> ModuleIdentityAddressInternal0 e ModuleIdentityAddressInternal0L

data NumLiteral e l where
  NumLiteral
    :: e NumLiteralInternal0L
    -> e NumLiteralInternal2L
    -> NumLiteral e NumLiteralL

data NumLiteralInternal0 e l where

data NumLiteralInternal2 e l where
  NumLiteralInternal1NumLiteralInternal2
    :: e NumLiteralInternal1L
    -> NumLiteralInternal2 e NumLiteralInternal2L

data NumLiteralInternal1 e l where
  U8NumLiteralInternal1
    :: e U8TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L
  U16NumLiteralInternal1
    :: e U16TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L
  U32NumLiteralInternal1
    :: e U32TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L
  U64NumLiteralInternal1
    :: e U64TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L
  U128NumLiteralInternal1
    :: e U128TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L
  U256NumLiteralInternal1
    :: e U256TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L

data FunctionType e l where
  FunctionType
    :: e FunctionTypeParametersL
    -> e FunctionTypeInternal0L
    -> FunctionType e FunctionTypeL

data FunctionTypeInternal0 e l where
  FunctionTypeInternal01
    :: e HiddenTypeL
    -> FunctionTypeInternal0 e FunctionTypeInternal0L

data FunctionTypeParameters e l where
  FunctionTypeParameters
    :: e BitorTokL
    -> e ([HiddenTypeL], FunctionTypeParametersInternal0L)
    -> e BitorTokL
    -> FunctionTypeParameters e FunctionTypeParametersL

data FunctionTypeParametersInternal0 e l where
  HiddenTypeFunctionTypeParametersInternal0
    :: e HiddenTypeL
    -> FunctionTypeParametersInternal0 e FunctionTypeParametersInternal0L

data PrimitiveType e l where
  U8PrimitiveType
    :: e U8TokL
    -> PrimitiveType e PrimitiveTypeL
  U16PrimitiveType
    :: e U16TokL
    -> PrimitiveType e PrimitiveTypeL
  U32PrimitiveType
    :: e U32TokL
    -> PrimitiveType e PrimitiveTypeL
  U64PrimitiveType
    :: e U64TokL
    -> PrimitiveType e PrimitiveTypeL
  U128PrimitiveType
    :: e U128TokL
    -> PrimitiveType e PrimitiveTypeL
  U256PrimitiveType
    :: e U256TokL
    -> PrimitiveType e PrimitiveTypeL
  BoolPrimitiveType
    :: e BoolTokL
    -> PrimitiveType e PrimitiveTypeL
  AddressPrimitiveType
    :: e AddressTokL
    -> PrimitiveType e PrimitiveTypeL
  SignerPrimitiveType
    :: e SignerTokL
    -> PrimitiveType e PrimitiveTypeL
  BytearrayPrimitiveType
    :: e BytearrayTokL
    -> PrimitiveType e PrimitiveTypeL

data RefType e l where
  RefType
    :: e HiddenReferenceL
    -> e HiddenTypeL
    -> RefType e RefTypeL

data HiddenReference e l where
  ImmRefReference
    :: e ImmRefL
    -> HiddenReference e HiddenReferenceL
  MutRefReference
    :: e MutRefL
    -> HiddenReference e HiddenReferenceL

data ImmRef e l where
  ImmRef
    :: e BitandTokL
    -> ImmRef e ImmRefL

data MutRef e l where
  MutRef
    :: e BitandTokL
    -> MutRef e MutRefL

data TupleType e l where
  TupleType
    :: e ([HiddenTypeL], FunctionTypeParametersInternal0L)
    -> TupleType e TupleTypeL

data NamedFieldsInternal0 e l where
  FieldAnnotationNamedFieldsInternal0
    :: e FieldAnnotationL
    -> NamedFieldsInternal0 e NamedFieldsInternal0L

data PositionalFields e l where
  PositionalFields
    :: e ([HiddenTypeL], FunctionTypeParametersInternal0L)
    -> PositionalFields e PositionalFieldsL

data HiddenFunctionItem e l where
  NativeFunctionDefinitionFunctionItem
    :: e NativeFunctionDefinitionL
    -> HiddenFunctionItem e HiddenFunctionItemL
  MacroFunctionDefinitionFunctionItem
    :: e MacroFunctionDefinitionL
    -> HiddenFunctionItem e HiddenFunctionItemL
  FunctionDefinitionFunctionItem
    :: e FunctionDefinitionL
    -> HiddenFunctionItem e HiddenFunctionItemL

data FunctionDefinition e l where
  FunctionDefinition
    :: e HiddenFunctionSignatureL
    -> e BlockL
    -> FunctionDefinition e FunctionDefinitionL

data HiddenFunctionSignature e l where
  HiddenFunctionSignature
    :: e HiddenFunctionSignatureInternal0L
    -> e HiddenFunctionSignatureInternal0L
    -> e HiddenFunctionSignatureInternal0L
    -> e HiddenFunctionIdentifierL
    -> e HiddenEnumSignatureInternal0L
    -> e FunctionParametersL
    -> e HiddenFunctionSignatureInternal1L
    -> HiddenFunctionSignature e HiddenFunctionSignatureL

data HiddenFunctionIdentifier e l where
  HiddenFunctionIdentifier
    :: e FunctionIdentifierL
    -> HiddenFunctionIdentifier e HiddenFunctionIdentifierL

data FunctionIdentifier e l where
  FunctionIdentifier
    :: FunctionIdentifier e FunctionIdentifierL

data HiddenFunctionSignatureInternal0 e l where
  ModifierFunctionSignatureInternal0
    :: e ModifierL
    -> HiddenFunctionSignatureInternal0 e HiddenFunctionSignatureInternal0L

data Modifier e l where
  Modifier1
    :: e PublicTokL
    -> e ModifierInternal1L
    -> Modifier e ModifierL
  EntryModifier
    :: e EntryTokL
    -> Modifier e ModifierL
  NativeModifier
    :: e NativeTokL
    -> Modifier e ModifierL

data ModifierInternal1 e l where
  ModifierInternal11
    :: e ModifierInternal0L
    -> ModifierInternal1 e ModifierInternal1L

data ModifierInternal0 e l where
  PackageModifierInternal0
    :: e PackageTokL
    -> ModifierInternal0 e ModifierInternal0L
  FriendModifierInternal0
    :: e FriendTokL
    -> ModifierInternal0 e ModifierInternal0L

data HiddenFunctionSignatureInternal1 e l where
  ReturnTypeFunctionSignatureInternal1
    :: e RetTypeL
    -> HiddenFunctionSignatureInternal1 e HiddenFunctionSignatureInternal1L

data RetType e l where
  RetType
    :: e HiddenTypeL
    -> RetType e RetTypeL

data FunctionParameters e l where
  FunctionParameters
    :: e ([FunctionParametersInternal0L], FunctionParametersInternal1L)
    -> FunctionParameters e FunctionParametersL

data FunctionParametersInternal0 e l where
  MutFunctionParameterFunctionParametersInternal0
    :: e MutFunctionParameterL
    -> FunctionParametersInternal0 e FunctionParametersInternal0L
  FunctionParameterFunctionParametersInternal0
    :: e FunctionParameterL
    -> FunctionParametersInternal0 e FunctionParametersInternal0L

data FunctionParameter e l where
  FunctionParameter
    :: e FunctionParameterInternal0L
    -> e HiddenTypeL
    -> FunctionParameter e FunctionParameterL

data FunctionParameterInternal0 e l where
  NameFunctionParameterInternal0
    :: e HiddenVariableIdentifierL
    -> FunctionParameterInternal0 e FunctionParameterInternal0L
  FunctionParameterInternal02
    :: e DollarTokL
    -> e HiddenVariableIdentifierL
    -> FunctionParameterInternal0 e FunctionParameterInternal0L

data HiddenVariableIdentifier e l where
  HiddenVariableIdentifier
    :: e VariableIdentifierL
    -> HiddenVariableIdentifier e HiddenVariableIdentifierL

data VariableIdentifier e l where
  VariableIdentifier
    :: VariableIdentifier e VariableIdentifierL

data MutFunctionParameter e l where
  MutFunctionParameter
    :: e FunctionParameterL
    -> MutFunctionParameter e MutFunctionParameterL

data FunctionParametersInternal1 e l where
  FunctionParametersInternal0FunctionParametersInternal1
    :: e FunctionParametersInternal0L
    -> FunctionParametersInternal1 e FunctionParametersInternal1L

data Block e l where
  Block
    :: e [UseDeclarationL]
    -> e [BlockItemL]
    -> e ArgListInternal0L
    -> Block e BlockL

data ArgListInternal0 e l where
  HiddenExpressionArgListInternal0
    :: e HiddenExpressionL
    -> ArgListInternal0 e ArgListInternal0L

data HiddenExpression e l where
  CallExpressionExpression
    :: e CallExpressionL
    -> HiddenExpression e HiddenExpressionL
  MacroCallExpressionExpression
    :: e MacroCallExpressionL
    -> HiddenExpression e HiddenExpressionL
  LambdaExpressionExpression
    :: e LambdaExpressionL
    -> HiddenExpression e HiddenExpressionL
  IfExpressionExpression
    :: e IfExpressionL
    -> HiddenExpression e HiddenExpressionL
  WhileExpressionExpression
    :: e WhileExpressionL
    -> HiddenExpression e HiddenExpressionL
  ReturnExpressionExpression
    :: e ReturnExpressionL
    -> HiddenExpression e HiddenExpressionL
  AbortExpressionExpression
    :: e AbortExpressionL
    -> HiddenExpression e HiddenExpressionL
  AssignExpressionExpression
    :: e AssignExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenUnaryExpressionExpression
    :: e HiddenUnaryExpressionL
    -> HiddenExpression e HiddenExpressionL
  BinaryExpressionExpression
    :: e BinaryExpressionL
    -> HiddenExpression e HiddenExpressionL
  CastExpressionExpression
    :: e CastExpressionL
    -> HiddenExpression e HiddenExpressionL
  QuantifierExpressionExpression
    :: e QuantifierExpressionL
    -> HiddenExpression e HiddenExpressionL
  MatchExpressionExpression
    :: e MatchExpressionL
    -> HiddenExpression e HiddenExpressionL
  VectorExpressionExpression
    :: e VectorExpressionL
    -> HiddenExpression e HiddenExpressionL
  LoopExpressionExpression
    :: e LoopExpressionL
    -> HiddenExpression e HiddenExpressionL
  IdentifiedExpressionExpression
    :: e IdentifiedExpressionL
    -> HiddenExpression e HiddenExpressionL

data HiddenUnaryExpression e l where
  HiddenUnaryExpression
    :: e HiddenUnaryExpressionInternal0L
    -> HiddenUnaryExpression e HiddenUnaryExpressionL

data HiddenUnaryExpressionInternal0 e l where
  UnaryExpressionUnaryExpressionInternal0
    :: e UnaryExpressionL
    -> HiddenUnaryExpressionInternal0 e HiddenUnaryExpressionInternal0L
  BorrowExpressionUnaryExpressionInternal0
    :: e BorrowExpressionL
    -> HiddenUnaryExpressionInternal0 e HiddenUnaryExpressionInternal0L
  DereferenceExpressionUnaryExpressionInternal0
    :: e DereferenceExpressionL
    -> HiddenUnaryExpressionInternal0 e HiddenUnaryExpressionInternal0L
  MoveOrCopyExpressionUnaryExpressionInternal0
    :: e MoveOrCopyExpressionL
    -> HiddenUnaryExpressionInternal0 e HiddenUnaryExpressionInternal0L
  HiddenExpressionTermUnaryExpressionInternal0
    :: e HiddenExpressionTermL
    -> HiddenUnaryExpressionInternal0 e HiddenUnaryExpressionInternal0L

data HiddenExpressionTerm e l where
  CallExpressionExpressionTerm
    :: e CallExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  BreakExpressionExpressionTerm
    :: e BreakExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  ContinueExpressionExpressionTerm
    :: e ContinueExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  NameExpressionExpressionTerm
    :: e NameExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  MacroCallExpressionExpressionTerm
    :: e MacroCallExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  PackExpressionExpressionTerm
    :: e PackExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenLiteralValueExpressionTerm
    :: e HiddenLiteralValueL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  UnitExpressionExpressionTerm
    :: e UnitExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  ExpressionListExpressionTerm
    :: e ExpressionListL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  AnnotationExpressionExpressionTerm
    :: e AnnotationExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  BlockExpressionTerm
    :: e BlockL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  SpecBlockExpressionTerm
    :: e SpecBlockL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  IfExpressionExpressionTerm
    :: e IfExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  DotExpressionExpressionTerm
    :: e DotExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  IndexExpressionExpressionTerm
    :: e IndexExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  VectorExpressionExpressionTerm
    :: e VectorExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  MatchExpressionExpressionTerm
    :: e MatchExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL

data HiddenLiteralValue e l where
  AddressLiteralLiteralValue
    :: e AddressLiteralL
    -> HiddenLiteralValue e HiddenLiteralValueL
  BoolLiteralLiteralValue
    :: e BoolLiteralL
    -> HiddenLiteralValue e HiddenLiteralValueL
  NumLiteralLiteralValue
    :: e NumLiteralL
    -> HiddenLiteralValue e HiddenLiteralValueL
  HexStringLiteralLiteralValue
    :: e HexStringLiteralL
    -> HiddenLiteralValue e HiddenLiteralValueL
  ByteStringLiteralLiteralValue
    :: e ByteStringLiteralL
    -> HiddenLiteralValue e HiddenLiteralValueL

data AddressLiteral e l where
  AddressLiteral
    :: AddressLiteral e AddressLiteralL

data BoolLiteral e l where
  TrueBoolLiteral
    :: e TrueTokL
    -> BoolLiteral e BoolLiteralL
  FalseBoolLiteral
    :: e FalseTokL
    -> BoolLiteral e BoolLiteralL

data ByteStringLiteral e l where
  ByteStringLiteral
    :: ByteStringLiteral e ByteStringLiteralL

data HexStringLiteral e l where
  HexStringLiteral
    :: HexStringLiteral e HexStringLiteralL

data AnnotationExpression e l where
  AnnotationExpression
    :: e HiddenExpressionL
    -> e HiddenTypeL
    -> AnnotationExpression e AnnotationExpressionL

data BreakExpression e l where
  BreakExpression
    :: e BreakExpressionInternal0L
    -> e BreakExpressionInternal1L
    -> BreakExpression e BreakExpressionL

data BreakExpressionInternal0 e l where
  LabelBreakExpressionInternal0
    :: e LabelL
    -> BreakExpressionInternal0 e BreakExpressionInternal0L

data Label e l where
  Label
    :: e IdentifierL
    -> Label e LabelL

data BreakExpressionInternal1 e l where
  BreakBreakExpressionInternal1
    :: e HiddenExpressionL
    -> BreakExpressionInternal1 e BreakExpressionInternal1L

data CallExpression e l where
  CallExpression
    :: e NameExpressionL
    -> e ArgListL
    -> CallExpression e CallExpressionL

data ArgList e l where
  ArgList
    :: e ([HiddenExpressionL], ArgListInternal0L)
    -> ArgList e ArgListL

data NameExpression e l where
  NameExpression
    :: e NameExpressionInternal0L
    -> e ModuleAccessL
    -> NameExpression e NameExpressionL

data NameExpressionInternal0 e l where

data ContinueExpression e l where
  ContinueExpression
    :: e BreakExpressionInternal0L
    -> ContinueExpression e ContinueExpressionL

data DotExpression e l where
  DotExpression
    :: e HiddenExpressionTermL
    -> e HiddenExpressionTermL
    -> DotExpression e DotExpressionL

data ExpressionList e l where
  ExpressionList
    :: e ((HiddenExpressionL, [HiddenExpressionL]), HiddenSpecAbortWithOrModifiesInternal1L)
    -> ExpressionList e ExpressionListL

data IfExpression e l where
  IfExpression
    :: e HiddenExpressionL
    -> e HiddenExpressionL
    -> e IfExpressionInternal0L
    -> IfExpression e IfExpressionL

data IfExpressionInternal0 e l where
  IfExpressionInternal01
    :: e HiddenExpressionL
    -> IfExpressionInternal0 e IfExpressionInternal0L

data IndexExpression e l where
  IndexExpression
    :: e HiddenExpressionTermL
    -> e ([HiddenExpressionL], IndexExpressionInternal0L)
    -> IndexExpression e IndexExpressionL

data IndexExpressionInternal0 e l where
  IdxIndexExpressionInternal0
    :: e HiddenExpressionL
    -> IndexExpressionInternal0 e IndexExpressionInternal0L

data MacroCallExpression e l where
  MacroCallExpression
    :: e MacroModuleAccessL
    -> e ApplyTypeInternal0L
    -> e ArgListL
    -> MacroCallExpression e MacroCallExpressionL

data MacroModuleAccess e l where
  MacroModuleAccess
    :: e ModuleAccessL
    -> e BangTokL
    -> MacroModuleAccess e MacroModuleAccessL

data MatchExpression e l where
  MatchExpression
    :: e HiddenExpressionL
    -> e HiddenMatchBodyL
    -> MatchExpression e MatchExpressionL

data HiddenMatchBody e l where
  HiddenMatchBody
    :: e ([MatchArmL], HiddenMatchBodyInternal0L)
    -> HiddenMatchBody e HiddenMatchBodyL

data HiddenMatchBodyInternal0 e l where
  MatchArmMatchBodyInternal0
    :: e MatchArmL
    -> HiddenMatchBodyInternal0 e HiddenMatchBodyInternal0L

data MatchArm e l where
  MatchArm
    :: e BindListL
    -> e MatchArmInternal0L
    -> e HiddenExpressionL
    -> MatchArm e MatchArmL

data BindList e l where
  HiddenBindBindList
    :: e HiddenBindL
    -> BindList e BindListL
  CommaBindListBindList
    :: e CommaBindListL
    -> BindList e BindListL
  OrBindListBindList
    :: e OrBindListL
    -> BindList e BindListL

data HiddenBind e l where
  HiddenBindInternal0Bind
    :: e HiddenBindInternal0L
    -> HiddenBind e HiddenBindL
  BindUnpackBind
    :: e BindUnpackL
    -> HiddenBind e HiddenBindL
  AtBindBind
    :: e AtBindL
    -> HiddenBind e HiddenBindL
  HiddenLiteralValueBind
    :: e HiddenLiteralValueL
    -> HiddenBind e HiddenBindL

data HiddenBindInternal0 e l where
  MutBindVarBindInternal0
    :: e MutBindVarL
    -> HiddenBindInternal0 e HiddenBindInternal0L
  BindVarBindInternal0
    :: e HiddenVariableIdentifierL
    -> HiddenBindInternal0 e HiddenBindInternal0L

data MutBindVar e l where
  MutBindVar
    :: e BindVarL
    -> MutBindVar e MutBindVarL

data BindVar e l where
  BindVar
    :: e VariableIdentifierL
    -> BindVar e BindVarL

data AtBind e l where
  AtBind
    :: e HiddenVariableIdentifierL
    -> e BindListL
    -> AtBind e AtBindL

data BindUnpack e l where
  BindUnpack
    :: e NameExpressionL
    -> e BindUnpackInternal0L
    -> BindUnpack e BindUnpackL

data BindUnpackInternal0 e l where
  BindFieldsBindUnpackInternal0
    :: e BindFieldsL
    -> BindUnpackInternal0 e BindUnpackInternal0L

data BindFields e l where
  BindPositionalFieldsBindFields
    :: e BindPositionalFieldsL
    -> BindFields e BindFieldsL
  BindNamedFieldsBindFields
    :: e BindNamedFieldsL
    -> BindFields e BindFieldsL

data BindNamedFields e l where
  BindNamedFields
    :: e ([BindNamedFieldsInternal0L], BindNamedFieldsInternal1L)
    -> BindNamedFields e BindNamedFieldsL

data BindNamedFieldsInternal0 e l where
  BindFieldBindNamedFieldsInternal0
    :: e BindFieldL
    -> BindNamedFieldsInternal0 e BindNamedFieldsInternal0L
  MutBindFieldBindNamedFieldsInternal0
    :: e MutBindFieldL
    -> BindNamedFieldsInternal0 e BindNamedFieldsInternal0L

data BindField e l where
  BindField1
    :: e BindListL
    -> e BindFieldInternal0L
    -> BindField e BindFieldL
  HiddenSpreadOperatorBindField
    :: e HiddenSpreadOperatorL
    -> BindField e BindFieldL

data HiddenSpreadOperator e l where
  HiddenSpreadOperator
    :: e RangeTokL
    -> HiddenSpreadOperator e HiddenSpreadOperatorL

data BindFieldInternal0 e l where
  BindFieldInternal01
    :: e BindListL
    -> BindFieldInternal0 e BindFieldInternal0L

data MutBindField e l where
  MutBindField
    :: e BindFieldL
    -> MutBindField e MutBindFieldL

data BindNamedFieldsInternal1 e l where
  BindNamedFieldsInternal0BindNamedFieldsInternal1
    :: e BindNamedFieldsInternal0L
    -> BindNamedFieldsInternal1 e BindNamedFieldsInternal1L

data BindPositionalFields e l where
  BindPositionalFields
    :: e ([BindNamedFieldsInternal0L], BindNamedFieldsInternal1L)
    -> BindPositionalFields e BindPositionalFieldsL

data CommaBindList e l where
  CommaBindList
    :: e ([HiddenBindL], CommaBindListInternal0L)
    -> CommaBindList e CommaBindListL

data CommaBindListInternal0 e l where
  HiddenBindCommaBindListInternal0
    :: e HiddenBindL
    -> CommaBindListInternal0 e CommaBindListInternal0L

data OrBindList e l where
  OrBindList
    :: e OrBindListInternal0L
    -> e ((((OrBindListInternal0L, HiddenBindL), OrBindListInternal1L), [(BitorTokL, ((OrBindListInternal0L, HiddenBindL), OrBindListInternal1L))]), OrBindListInternal2L)
    -> e OrBindListInternal1L
    -> OrBindList e OrBindListL

data OrBindListInternal0 e l where

data OrBindListInternal1 e l where

data OrBindListInternal2 e l where
  BitorOrBindListInternal2
    :: e BitorTokL
    -> OrBindListInternal2 e OrBindListInternal2L

data MatchArmInternal0 e l where
  MatchConditionMatchArmInternal0
    :: e MatchConditionL
    -> MatchArmInternal0 e MatchArmInternal0L

data MatchCondition e l where
  MatchCondition
    :: e HiddenExpressionL
    -> MatchCondition e MatchConditionL

data PackExpression e l where
  PackExpression
    :: e NameExpressionL
    -> e FieldInitializeListL
    -> PackExpression e PackExpressionL

data FieldInitializeList e l where
  FieldInitializeList
    :: e ([ExpFieldL], FieldInitializeListInternal0L)
    -> FieldInitializeList e FieldInitializeListL

data ExpField e l where
  ExpField
    :: e HiddenFieldIdentifierL
    -> e ExpFieldInternal0L
    -> ExpField e ExpFieldL

data ExpFieldInternal0 e l where
  ExpFieldInternal01
    :: e HiddenExpressionL
    -> ExpFieldInternal0 e ExpFieldInternal0L

data FieldInitializeListInternal0 e l where
  ExpFieldFieldInitializeListInternal0
    :: e ExpFieldL
    -> FieldInitializeListInternal0 e FieldInitializeListInternal0L

data SpecBlock e l where
  SpecBlock
    :: e SpecBlockInternal1L
    -> SpecBlock e SpecBlockL

data SpecBlockInternal1 e l where
  SpecBlockInternal11
    :: e SpecBlockInternal0L
    -> e SpecBodyL
    -> SpecBlockInternal1 e SpecBlockInternal1L
  HiddenSpecFunctionSpecBlockInternal1
    :: e HiddenSpecFunctionL
    -> SpecBlockInternal1 e SpecBlockInternal1L

data HiddenSpecFunction e l where
  NativeSpecFunctionSpecFunction
    :: e NativeSpecFunctionL
    -> HiddenSpecFunction e HiddenSpecFunctionL
  UsualSpecFunctionSpecFunction
    :: e UsualSpecFunctionL
    -> HiddenSpecFunction e HiddenSpecFunctionL
  UninterpretedSpecFunctionSpecFunction
    :: e UninterpretedSpecFunctionL
    -> HiddenSpecFunction e HiddenSpecFunctionL

data NativeSpecFunction e l where
  NativeSpecFunction
    :: e NativeTokL
    -> e HiddenSpecFunctionSignatureL
    -> NativeSpecFunction e NativeSpecFunctionL

data HiddenSpecFunctionSignature e l where
  HiddenSpecFunctionSignature
    :: e HiddenFunctionIdentifierL
    -> e HiddenEnumSignatureInternal0L
    -> e FunctionParametersL
    -> e RetTypeL
    -> HiddenSpecFunctionSignature e HiddenSpecFunctionSignatureL

data UninterpretedSpecFunction e l where
  UninterpretedSpecFunction
    :: e HiddenSpecFunctionSignatureL
    -> UninterpretedSpecFunction e UninterpretedSpecFunctionL

data UsualSpecFunction e l where
  UsualSpecFunction
    :: e HiddenSpecFunctionSignatureL
    -> e BlockL
    -> UsualSpecFunction e UsualSpecFunctionL

data SpecBlockInternal0 e l where
  TargetSpecBlockInternal0
    :: e HiddenSpecBlockTargetL
    -> SpecBlockInternal0 e SpecBlockInternal0L

data HiddenSpecBlockTarget e l where
  IdentifierSpecBlockTarget
    :: e IdentifierL
    -> HiddenSpecBlockTarget e HiddenSpecBlockTargetL
  SpecBlockTargetModuleSpecBlockTarget
    :: e ModuleTokL
    -> HiddenSpecBlockTarget e HiddenSpecBlockTargetL
  SpecBlockTargetSchemaSpecBlockTarget
    :: e SpecBlockTargetSchemaL
    -> HiddenSpecBlockTarget e HiddenSpecBlockTargetL

data SpecBlockTargetSchema e l where
  SpecBlockTargetSchema
    :: e HiddenStructIdentifierL
    -> e HiddenEnumSignatureInternal0L
    -> SpecBlockTargetSchema e SpecBlockTargetSchemaL

data HiddenStructIdentifier e l where
  HiddenStructIdentifier
    :: e StructIdentifierL
    -> HiddenStructIdentifier e HiddenStructIdentifierL

data StructIdentifier e l where
  StructIdentifier
    :: StructIdentifier e StructIdentifierL

data SpecBody e l where
  SpecBody
    :: e [UseDeclarationL]
    -> e [HiddenSpecBlockMemeberL]
    -> SpecBody e SpecBodyL

data HiddenSpecBlockMemeber e l where
  SpecInvariantSpecBlockMemeber
    :: e SpecInvariantL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  HiddenSpecFunctionSpecBlockMemeber
    :: e HiddenSpecFunctionL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  SpecConditionSpecBlockMemeber
    :: e SpecConditionL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  SpecIncludeSpecBlockMemeber
    :: e SpecIncludeL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  SpecApplySpecBlockMemeber
    :: e SpecApplyL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  SpecPragmaSpecBlockMemeber
    :: e SpecPragmaL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  SpecVariableSpecBlockMemeber
    :: e SpecVariableL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  SpecLetSpecBlockMemeber
    :: e SpecLetL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL

data SpecApply e l where
  SpecApply
    :: e HiddenExpressionL
    -> e ((SpecApplyPatternL, [SpecApplyPatternL]), HiddenSpecAbortWithOrModifiesInternal1L)
    -> e SpecApplyInternal0L
    -> SpecApply e SpecApplyL

data SpecApplyInternal0 e l where
  SpecApplyInternal01
    :: e ((SpecApplyPatternL, [SpecApplyPatternL]), HiddenSpecAbortWithOrModifiesInternal1L)
    -> SpecApplyInternal0 e SpecApplyInternal0L

data SpecApplyPattern e l where
  SpecApplyPattern
    :: e SpecApplyPatternInternal1L
    -> e SpecApplyNamePatternL
    -> e HiddenEnumSignatureInternal0L
    -> SpecApplyPattern e SpecApplyPatternL

data SpecApplyNamePattern e l where
  SpecApplyNamePattern
    :: SpecApplyNamePattern e SpecApplyNamePatternL

data SpecApplyPatternInternal1 e l where
  SpecApplyPatternInternal0SpecApplyPatternInternal1
    :: e SpecApplyPatternInternal0L
    -> SpecApplyPatternInternal1 e SpecApplyPatternInternal1L

data SpecApplyPatternInternal0 e l where
  PublicSpecApplyPatternInternal0
    :: e PublicTokL
    -> SpecApplyPatternInternal0 e SpecApplyPatternInternal0L
  InternalSpecApplyPatternInternal0
    :: e InternalTokL
    -> SpecApplyPatternInternal0 e SpecApplyPatternInternal0L

data SpecCondition e l where
  HiddenSpecConditionSpecCondition
    :: e HiddenSpecConditionL
    -> SpecCondition e SpecConditionL
  HiddenSpecAbortIfSpecCondition
    :: e HiddenSpecAbortIfL
    -> SpecCondition e SpecConditionL
  HiddenSpecAbortWithOrModifiesSpecCondition
    :: e HiddenSpecAbortWithOrModifiesL
    -> SpecCondition e SpecConditionL

data HiddenSpecAbortIf e l where
  HiddenSpecAbortIf
    :: e ConditionKindL
    -> e HiddenSpecAbortIfInternal0L
    -> e HiddenExpressionL
    -> e HiddenSpecAbortIfInternal1L
    -> HiddenSpecAbortIf e HiddenSpecAbortIfL

data HiddenSpecAbortIfInternal0 e l where
  ConditionPropertiesSpecAbortIfInternal0
    :: e ConditionPropertiesL
    -> HiddenSpecAbortIfInternal0 e HiddenSpecAbortIfInternal0L

data ConditionProperties e l where
  ConditionProperties
    :: e ([SpecPropertyL], ConditionPropertiesInternal0L)
    -> ConditionProperties e ConditionPropertiesL

data ConditionPropertiesInternal0 e l where
  SpecPropertyConditionPropertiesInternal0
    :: e SpecPropertyL
    -> ConditionPropertiesInternal0 e ConditionPropertiesInternal0L

data SpecProperty e l where
  SpecProperty
    :: e IdentifierL
    -> e SpecPropertyInternal0L
    -> SpecProperty e SpecPropertyL

data SpecPropertyInternal0 e l where
  SpecPropertyInternal01
    :: e AssignTokL
    -> e HiddenLiteralValueL
    -> SpecPropertyInternal0 e SpecPropertyInternal0L

data HiddenSpecAbortIfInternal1 e l where
  HiddenSpecAbortIfInternal11
    :: e HiddenExpressionL
    -> HiddenSpecAbortIfInternal1 e HiddenSpecAbortIfInternal1L

data ConditionKind e l where
  ConditionKind
    :: ConditionKind e ConditionKindL

data HiddenSpecAbortWithOrModifies e l where
  HiddenSpecAbortWithOrModifies
    :: e ConditionKindL
    -> e HiddenSpecAbortIfInternal0L
    -> e ((HiddenExpressionL, [HiddenExpressionL]), HiddenSpecAbortWithOrModifiesInternal1L)
    -> HiddenSpecAbortWithOrModifies e HiddenSpecAbortWithOrModifiesL

data HiddenSpecCondition e l where
  HiddenSpecCondition
    :: e HiddenSpecConditionInternal1L
    -> e HiddenSpecAbortIfInternal0L
    -> e HiddenExpressionL
    -> HiddenSpecCondition e HiddenSpecConditionL

data HiddenSpecConditionInternal1 e l where
  KindSpecConditionInternal1
    :: e ConditionKindL
    -> HiddenSpecConditionInternal1 e HiddenSpecConditionInternal1L
  HiddenSpecConditionInternal12
    :: e ConditionKindL
    -> e HiddenSpecConditionInternal0L
    -> HiddenSpecConditionInternal1 e HiddenSpecConditionInternal1L

data HiddenSpecConditionInternal0 e l where
  ModuleSpecConditionInternal0
    :: e ModuleTokL
    -> HiddenSpecConditionInternal0 e HiddenSpecConditionInternal0L

data SpecInclude e l where
  SpecInclude
    :: e HiddenExpressionL
    -> SpecInclude e SpecIncludeL

data SpecInvariant e l where
  SpecInvariant
    :: e ConditionKindL
    -> e SpecInvariantInternal1L
    -> e HiddenSpecAbortIfInternal0L
    -> e HiddenExpressionL
    -> SpecInvariant e SpecInvariantL

data SpecInvariantInternal1 e l where
  ModifierSpecInvariantInternal1
    :: e InvariantModifierL
    -> SpecInvariantInternal1 e SpecInvariantInternal1L

data InvariantModifier e l where
  UpdateInvariantModifier
    :: e UpdateTokL
    -> InvariantModifier e InvariantModifierL
  PackInvariantModifier
    :: e PackTokL
    -> InvariantModifier e InvariantModifierL
  UnpackInvariantModifier
    :: e UnpackTokL
    -> InvariantModifier e InvariantModifierL
  ModuleInvariantModifier
    :: e ModuleTokL
    -> InvariantModifier e InvariantModifierL

data SpecLet e l where
  SpecLet
    :: e SpecLetInternal0L
    -> e IdentifierL
    -> e AssignTokL
    -> e HiddenExpressionL
    -> SpecLet e SpecLetL

data SpecLetInternal0 e l where
  PostSpecLetInternal0
    :: e PostTokL
    -> SpecLetInternal0 e SpecLetInternal0L

data SpecPragma e l where
  SpecPragma
    :: e ([SpecPropertyL], ConditionPropertiesInternal0L)
    -> SpecPragma e SpecPragmaL

data SpecVariable e l where
  SpecVariable
    :: e SpecVariableInternal1L
    -> e IdentifierL
    -> e HiddenEnumSignatureInternal0L
    -> e HiddenTypeL
    -> SpecVariable e SpecVariableL

data SpecVariableInternal1 e l where
  SpecVariableInternal0SpecVariableInternal1
    :: e SpecVariableInternal0L
    -> SpecVariableInternal1 e SpecVariableInternal1L

data SpecVariableInternal0 e l where
  GlobalSpecVariableInternal0
    :: e GlobalTokL
    -> SpecVariableInternal0 e SpecVariableInternal0L
  LocalSpecVariableInternal0
    :: e LocalTokL
    -> SpecVariableInternal0 e SpecVariableInternal0L

data UseDeclaration e l where
  UseDeclaration
    :: e EnumDefinitionInternal0L
    -> e UseDeclarationInternal0L
    -> UseDeclaration e UseDeclarationL

data UseDeclarationInternal0 e l where
  UseFunUseDeclarationInternal0
    :: e UseFunL
    -> UseDeclarationInternal0 e UseDeclarationInternal0L
  UseModuleUseDeclarationInternal0
    :: e UseModuleL
    -> UseDeclarationInternal0 e UseDeclarationInternal0L
  UseModuleMemberUseDeclarationInternal0
    :: e UseModuleMemberL
    -> UseDeclarationInternal0 e UseDeclarationInternal0L
  UseModuleMembersUseDeclarationInternal0
    :: e UseModuleMembersL
    -> UseDeclarationInternal0 e UseDeclarationInternal0L

data UseFun e l where
  UseFun
    :: e ModuleAccessL
    -> e (ModuleAccessL, HiddenFunctionIdentifierL)
    -> UseFun e UseFunL

data UseModule e l where
  UseModule
    :: e ModuleIdentityL
    -> e UseModuleInternal0L
    -> UseModule e UseModuleL

data UseModuleInternal0 e l where
  UseModuleInternal01
    :: e HiddenModuleIdentifierL
    -> UseModuleInternal0 e UseModuleInternal0L

data UseModuleMember e l where
  UseModuleMember
    :: e ModuleIdentityL
    -> e UseMemberL
    -> UseModuleMember e UseModuleMemberL

data UseMember e l where
  UseMember1
    :: e IdentifierL
    -> e ((UseMemberL, [UseMemberL]), HiddenSpecAbortWithOrModifiesInternal1L)
    -> UseMember e UseMemberL
  UseMember2
    :: e IdentifierL
    -> e IdentifierL
    -> e UseMemberInternal0L
    -> UseMember e UseMemberL
  UseMember3
    :: e IdentifierL
    -> e UseMemberInternal0L
    -> UseMember e UseMemberL

data UseMemberInternal0 e l where
  UseMemberInternal01
    :: e IdentifierL
    -> UseMemberInternal0 e UseMemberInternal0L

data UseModuleMembers e l where
  UseModuleMembers1
    :: e ModuleIdentityAddressInternal0L
    -> e ((UseMemberL, [UseMemberL]), HiddenSpecAbortWithOrModifiesInternal1L)
    -> UseModuleMembers e UseModuleMembersL
  UseModuleMembers2
    :: e ModuleIdentityL
    -> e ((UseMemberL, [UseMemberL]), HiddenSpecAbortWithOrModifiesInternal1L)
    -> UseModuleMembers e UseModuleMembersL

data UnitExpression e l where
  UnitExpression
    :: UnitExpression e UnitExpressionL

data VectorExpression e l where
  VectorExpression
    :: e VectorExpressionInternal0L
    -> e ([HiddenExpressionL], ArgListInternal0L)
    -> VectorExpression e VectorExpressionL

data VectorExpressionInternal0 e l where
  VectorExpressionInternal01
    :: e ((HiddenTypeL, [HiddenTypeL]), HiddenSpecAbortWithOrModifiesInternal1L)
    -> e GtTokL
    -> VectorExpressionInternal0 e VectorExpressionInternal0L

data BorrowExpression e l where
  BorrowExpression
    :: e HiddenReferenceL
    -> e HiddenExpressionL
    -> BorrowExpression e BorrowExpressionL

data DereferenceExpression e l where
  DereferenceExpression
    :: e MulTokL
    -> e HiddenExpressionL
    -> DereferenceExpression e DereferenceExpressionL

data MoveOrCopyExpression e l where
  MoveOrCopyExpression
    :: e MoveOrCopyExpressionInternal0L
    -> e HiddenExpressionL
    -> MoveOrCopyExpression e MoveOrCopyExpressionL

data MoveOrCopyExpressionInternal0 e l where
  MoveMoveOrCopyExpressionInternal0
    :: e MoveTokL
    -> MoveOrCopyExpressionInternal0 e MoveOrCopyExpressionInternal0L
  CopyMoveOrCopyExpressionInternal0
    :: e CopyTokL
    -> MoveOrCopyExpressionInternal0 e MoveOrCopyExpressionInternal0L

data UnaryExpression e l where
  UnaryExpression
    :: e UnaryOpL
    -> e HiddenExpressionL
    -> UnaryExpression e UnaryExpressionL

data UnaryOp e l where
  BangUnaryOp
    :: e BangTokL
    -> UnaryOp e UnaryOpL

data AbortExpression e l where
  AbortExpression
    :: e AbortExpressionInternal0L
    -> AbortExpression e AbortExpressionL

data AbortExpressionInternal0 e l where
  AbortAbortExpressionInternal0
    :: e HiddenExpressionL
    -> AbortExpressionInternal0 e AbortExpressionInternal0L

data AssignExpression e l where
  AssignExpression
    :: e HiddenUnaryExpressionL
    -> e AssignTokL
    -> e HiddenExpressionL
    -> AssignExpression e AssignExpressionL

data BinaryExpression e l where
  BinaryExpression1
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression2
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression3
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression4
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression5
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression6
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression7
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression8
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression9
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression10
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression11
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression12
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression13
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression14
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression15
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression16
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression17
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression18
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression19
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression20
    :: e HiddenExpressionL
    -> e BinaryOperatorL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL

data BinaryOperator e l where
  BinaryOperator
    :: e ImpliesTokL
    -> BinaryOperator e BinaryOperatorL

data CastExpression e l where
  CastExpression
    :: e HiddenExpressionL
    -> e HiddenTypeL
    -> CastExpression e CastExpressionL

data IdentifiedExpression e l where
  IdentifiedExpression
    :: e BlockIdentifierL
    -> e HiddenExpressionL
    -> IdentifiedExpression e IdentifiedExpressionL

data BlockIdentifier e l where
  BlockIdentifier
    :: e LabelL
    -> BlockIdentifier e BlockIdentifierL

data LambdaExpression e l where
  LambdaExpression
    :: e LambdaBindingsL
    -> e LambdaExpressionInternal0L
    -> e HiddenExpressionL
    -> LambdaExpression e LambdaExpressionL

data LambdaBindings e l where
  LambdaBindings
    :: e BitorTokL
    -> e ([LambdaBindingL], LambdaBindingsInternal0L)
    -> e BitorTokL
    -> LambdaBindings e LambdaBindingsL

data LambdaBinding e l where
  CommaBindListLambdaBinding
    :: e CommaBindListL
    -> LambdaBinding e LambdaBindingL
  BindLambdaBinding
    :: e HiddenBindL
    -> LambdaBinding e LambdaBindingL
  LambdaBinding3
    :: e HiddenBindL
    -> e LambdaBindingInternal0L
    -> LambdaBinding e LambdaBindingL

data LambdaBindingInternal0 e l where
  LambdaBindingInternal01
    :: e HiddenTypeL
    -> LambdaBindingInternal0 e LambdaBindingInternal0L

data LambdaBindingsInternal0 e l where
  LambdaBindingLambdaBindingsInternal0
    :: e LambdaBindingL
    -> LambdaBindingsInternal0 e LambdaBindingsInternal0L

data LambdaExpressionInternal0 e l where
  LambdaExpressionInternal01
    :: e HiddenTypeL
    -> LambdaExpressionInternal0 e LambdaExpressionInternal0L

data LoopExpression e l where
  LoopExpression
    :: e HiddenExpressionL
    -> LoopExpression e LoopExpressionL

data QuantifierExpression e l where
  QuantifierExpression
    :: e QuantifierExpressionInternal0L
    -> e QuantifierBindingsL
    -> e QuantifierExpressionInternal1L
    -> e HiddenExpressionL
    -> QuantifierExpression e QuantifierExpressionL

data QuantifierBindings e l where
  QuantifierBindings
    :: e QuantifierBindingL
    -> e [QuantifierBindingL]
    -> e HiddenSpecAbortWithOrModifiesInternal1L
    -> QuantifierBindings e QuantifierBindingsL

data QuantifierBinding e l where
  QuantifierBinding1
    :: e IdentifierL
    -> e HiddenTypeL
    -> QuantifierBinding e QuantifierBindingL
  QuantifierBinding2
    :: e IdentifierL
    -> e HiddenExpressionL
    -> QuantifierBinding e QuantifierBindingL

data QuantifierExpressionInternal0 e l where
  HiddenForallQuantifierExpressionInternal0
    :: e HiddenForallL
    -> QuantifierExpressionInternal0 e QuantifierExpressionInternal0L
  HiddenExistsQuantifierExpressionInternal0
    :: e HiddenExistsL
    -> QuantifierExpressionInternal0 e QuantifierExpressionInternal0L

data HiddenExists e l where
  HiddenExists
    :: HiddenExists e HiddenExistsL

data HiddenForall e l where
  HiddenForall
    :: HiddenForall e HiddenForallL

data QuantifierExpressionInternal1 e l where
  QuantifierExpressionInternal11
    :: e HiddenExpressionL
    -> QuantifierExpressionInternal1 e QuantifierExpressionInternal1L

data ReturnExpression e l where
  ReturnExpression1
    :: e BreakExpressionInternal0L
    -> e HiddenExpressionL
    -> ReturnExpression e ReturnExpressionL
  ReturnExpression2
    :: e BreakExpressionInternal0L
    -> ReturnExpression e ReturnExpressionL

data WhileExpression e l where
  WhileExpression
    :: e HiddenExpressionL
    -> e HiddenExpressionL
    -> WhileExpression e WhileExpressionL

data BlockItem e l where
  BlockItem
    :: e BlockItemInternal0L
    -> BlockItem e BlockItemL

data BlockItemInternal0 e l where
  HiddenExpressionBlockItemInternal0
    :: e HiddenExpressionL
    -> BlockItemInternal0 e BlockItemInternal0L
  LetStatementBlockItemInternal0
    :: e LetStatementL
    -> BlockItemInternal0 e BlockItemInternal0L

data LetStatement e l where
  LetStatement
    :: e BindListL
    -> e LetStatementInternal0L
    -> e LetStatementInternal1L
    -> LetStatement e LetStatementL

data LetStatementInternal0 e l where
  LetStatementInternal01
    :: e HiddenTypeL
    -> LetStatementInternal0 e LetStatementInternal0L

data LetStatementInternal1 e l where
  LetStatementInternal11
    :: e AssignTokL
    -> e HiddenExpressionL
    -> LetStatementInternal1 e LetStatementInternal1L

data MacroFunctionDefinition e l where
  MacroFunctionDefinition
    :: e HiddenFunctionSignatureInternal0L
    -> e HiddenMacroSignatureL
    -> e BlockL
    -> MacroFunctionDefinition e MacroFunctionDefinitionL

data HiddenMacroSignature e l where
  HiddenMacroSignature
    :: e HiddenFunctionSignatureInternal0L
    -> e HiddenFunctionIdentifierL
    -> e HiddenEnumSignatureInternal0L
    -> e FunctionParametersL
    -> e HiddenFunctionSignatureInternal1L
    -> HiddenMacroSignature e HiddenMacroSignatureL

data NativeFunctionDefinition e l where
  NativeFunctionDefinition
    :: e HiddenFunctionSignatureL
    -> NativeFunctionDefinition e NativeFunctionDefinitionL

data HiddenStructItem e l where
  NativeStructDefinitionStructItem
    :: e NativeStructDefinitionL
    -> HiddenStructItem e HiddenStructItemL
  StructDefinitionStructItem
    :: e StructDefinitionL
    -> HiddenStructItem e HiddenStructItemL

data NativeStructDefinition e l where
  NativeStructDefinition
    :: e EnumDefinitionInternal0L
    -> e NativeTokL
    -> e HiddenStructSignatureL
    -> NativeStructDefinition e NativeStructDefinitionL

data HiddenStructSignature e l where
  HiddenStructSignature
    :: e HiddenStructIdentifierL
    -> e HiddenEnumSignatureInternal0L
    -> e HiddenEnumSignatureInternal1L
    -> HiddenStructSignature e HiddenStructSignatureL

data StructDefinition e l where
  StructDefinition
    :: e EnumDefinitionInternal0L
    -> e HiddenStructSignatureL
    -> e DatatypeFieldsL
    -> e EnumDefinitionInternal1L
    -> StructDefinition e StructDefinitionL

data Constant e l where
  Constant
    :: e ConstantIdentifierL
    -> e HiddenTypeL
    -> e AssignTokL
    -> e HiddenExpressionL
    -> Constant e ConstantL

data ConstantIdentifier e l where
  ConstantIdentifier
    :: ConstantIdentifier e ConstantIdentifierL

data FriendDeclaration e l where
  FriendDeclaration
    :: e FriendTokL
    -> e FriendAccessL
    -> FriendDeclaration e FriendDeclarationL

data FriendAccess e l where
  LocalModuleFriendAccess
    :: e IdentifierL
    -> FriendAccess e FriendAccessL
  FullyQualifiedModuleFriendAccess
    :: e ModuleIdentityL
    -> FriendAccess e FriendAccessL

data ModuleBodyInternal2 e l where

--------------------------------------------------------------------------------
-- Compdata derivation

deriveAll
  [ ''SourceFile
  , ''ModuleDefinition
  , ''ModuleBody
  , ''ModuleBodyInternal0
  , ''ModuleBodyInternal1
  , ''HiddenEnumItem
  , ''EnumDefinition
  , ''HiddenEnumSignature
  , ''HiddenEnumIdentifier
  , ''EnumIdentifier
  , ''HiddenEnumSignatureInternal0
  , ''TypeParameters
  , ''HiddenSpecAbortWithOrModifiesInternal1
  , ''TypeParameter
  , ''HiddenTypeParameterIdentifier
  , ''TypeParameterIdentifier
  , ''TypeParameterInternal0
  , ''TypeParameterInternal1
  , ''TypeParameterInternal3
  , ''Ability
  , ''TypeParameterInternal2
  , ''HiddenEnumSignatureInternal1
  , ''AbilityDecls
  , ''AbilityDeclsInternal0
  , ''EnumDefinitionInternal0
  , ''EnumDefinitionInternal1
  , ''PostfixAbilityDecls
  , ''EnumVariants
  , ''EnumVariantsInternal0
  , ''Variant
  , ''HiddenVariantIdentifier
  , ''VariantIdentifier
  , ''VariantInternal0
  , ''DatatypeFields
  , ''NamedFields
  , ''FieldAnnotation
  , ''HiddenFieldIdentifier
  , ''FieldIdentifier
  , ''HiddenType
  , ''ApplyType
  , ''ApplyTypeInternal0
  , ''TypeArguments
  , ''ModuleAccess
  , ''HiddenModuleIdentifier
  , ''ModuleIdentifier
  , ''Identifier
  , ''ModuleIdentity
  , ''ModuleIdentityAddressInternal0
  , ''NumLiteral
  , ''NumLiteralInternal0
  , ''NumLiteralInternal2
  , ''NumLiteralInternal1
  , ''FunctionType
  , ''FunctionTypeInternal0
  , ''FunctionTypeParameters
  , ''FunctionTypeParametersInternal0
  , ''PrimitiveType
  , ''RefType
  , ''HiddenReference
  , ''ImmRef
  , ''MutRef
  , ''TupleType
  , ''NamedFieldsInternal0
  , ''PositionalFields
  , ''HiddenFunctionItem
  , ''FunctionDefinition
  , ''HiddenFunctionSignature
  , ''HiddenFunctionIdentifier
  , ''FunctionIdentifier
  , ''HiddenFunctionSignatureInternal0
  , ''Modifier
  , ''ModifierInternal1
  , ''ModifierInternal0
  , ''HiddenFunctionSignatureInternal1
  , ''RetType
  , ''FunctionParameters
  , ''FunctionParametersInternal0
  , ''FunctionParameter
  , ''FunctionParameterInternal0
  , ''HiddenVariableIdentifier
  , ''VariableIdentifier
  , ''MutFunctionParameter
  , ''FunctionParametersInternal1
  , ''Block
  , ''ArgListInternal0
  , ''HiddenExpression
  , ''HiddenUnaryExpression
  , ''HiddenUnaryExpressionInternal0
  , ''HiddenExpressionTerm
  , ''HiddenLiteralValue
  , ''AddressLiteral
  , ''BoolLiteral
  , ''ByteStringLiteral
  , ''HexStringLiteral
  , ''AnnotationExpression
  , ''BreakExpression
  , ''BreakExpressionInternal0
  , ''Label
  , ''BreakExpressionInternal1
  , ''CallExpression
  , ''ArgList
  , ''NameExpression
  , ''NameExpressionInternal0
  , ''ContinueExpression
  , ''DotExpression
  , ''ExpressionList
  , ''IfExpression
  , ''IfExpressionInternal0
  , ''IndexExpression
  , ''IndexExpressionInternal0
  , ''MacroCallExpression
  , ''MacroModuleAccess
  , ''MatchExpression
  , ''HiddenMatchBody
  , ''HiddenMatchBodyInternal0
  , ''MatchArm
  , ''BindList
  , ''HiddenBind
  , ''HiddenBindInternal0
  , ''MutBindVar
  , ''BindVar
  , ''AtBind
  , ''BindUnpack
  , ''BindUnpackInternal0
  , ''BindFields
  , ''BindNamedFields
  , ''BindNamedFieldsInternal0
  , ''BindField
  , ''HiddenSpreadOperator
  , ''BindFieldInternal0
  , ''MutBindField
  , ''BindNamedFieldsInternal1
  , ''BindPositionalFields
  , ''CommaBindList
  , ''CommaBindListInternal0
  , ''OrBindList
  , ''OrBindListInternal0
  , ''OrBindListInternal1
  , ''OrBindListInternal2
  , ''MatchArmInternal0
  , ''MatchCondition
  , ''PackExpression
  , ''FieldInitializeList
  , ''ExpField
  , ''ExpFieldInternal0
  , ''FieldInitializeListInternal0
  , ''SpecBlock
  , ''SpecBlockInternal1
  , ''HiddenSpecFunction
  , ''NativeSpecFunction
  , ''HiddenSpecFunctionSignature
  , ''UninterpretedSpecFunction
  , ''UsualSpecFunction
  , ''SpecBlockInternal0
  , ''HiddenSpecBlockTarget
  , ''SpecBlockTargetSchema
  , ''HiddenStructIdentifier
  , ''StructIdentifier
  , ''SpecBody
  , ''HiddenSpecBlockMemeber
  , ''SpecApply
  , ''SpecApplyInternal0
  , ''SpecApplyPattern
  , ''SpecApplyNamePattern
  , ''SpecApplyPatternInternal1
  , ''SpecApplyPatternInternal0
  , ''SpecCondition
  , ''HiddenSpecAbortIf
  , ''HiddenSpecAbortIfInternal0
  , ''ConditionProperties
  , ''ConditionPropertiesInternal0
  , ''SpecProperty
  , ''SpecPropertyInternal0
  , ''HiddenSpecAbortIfInternal1
  , ''ConditionKind
  , ''HiddenSpecAbortWithOrModifies
  , ''HiddenSpecCondition
  , ''HiddenSpecConditionInternal1
  , ''HiddenSpecConditionInternal0
  , ''SpecInclude
  , ''SpecInvariant
  , ''SpecInvariantInternal1
  , ''InvariantModifier
  , ''SpecLet
  , ''SpecLetInternal0
  , ''SpecPragma
  , ''SpecVariable
  , ''SpecVariableInternal1
  , ''SpecVariableInternal0
  , ''UseDeclaration
  , ''UseDeclarationInternal0
  , ''UseFun
  , ''UseModule
  , ''UseModuleInternal0
  , ''UseModuleMember
  , ''UseMember
  , ''UseMemberInternal0
  , ''UseModuleMembers
  , ''UnitExpression
  , ''VectorExpression
  , ''VectorExpressionInternal0
  , ''BorrowExpression
  , ''DereferenceExpression
  , ''MoveOrCopyExpression
  , ''MoveOrCopyExpressionInternal0
  , ''UnaryExpression
  , ''UnaryOp
  , ''AbortExpression
  , ''AbortExpressionInternal0
  , ''AssignExpression
  , ''BinaryExpression
  , ''BinaryOperator
  , ''CastExpression
  , ''IdentifiedExpression
  , ''BlockIdentifier
  , ''LambdaExpression
  , ''LambdaBindings
  , ''LambdaBinding
  , ''LambdaBindingInternal0
  , ''LambdaBindingsInternal0
  , ''LambdaExpressionInternal0
  , ''LoopExpression
  , ''QuantifierExpression
  , ''QuantifierBindings
  , ''QuantifierBinding
  , ''QuantifierExpressionInternal0
  , ''HiddenExists
  , ''HiddenForall
  , ''QuantifierExpressionInternal1
  , ''ReturnExpression
  , ''WhileExpression
  , ''BlockItem
  , ''BlockItemInternal0
  , ''LetStatement
  , ''LetStatementInternal0
  , ''LetStatementInternal1
  , ''MacroFunctionDefinition
  , ''HiddenMacroSignature
  , ''NativeFunctionDefinition
  , ''HiddenStructItem
  , ''NativeStructDefinition
  , ''HiddenStructSignature
  , ''StructDefinition
  , ''Constant
  , ''ConstantIdentifier
  , ''FriendDeclaration
  , ''FriendAccess
  , ''ModuleBodyInternal2
  ]

--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

type MoveSig =
  '[ SourceFile
   , ModuleDefinition
   , ModuleBody
   , ModuleBodyInternal0
   , ModuleBodyInternal1
   , HiddenEnumItem
   , EnumDefinition
   , HiddenEnumSignature
   , HiddenEnumIdentifier
   , EnumIdentifier
   , HiddenEnumSignatureInternal0
   , TypeParameters
   , HiddenSpecAbortWithOrModifiesInternal1
   , TypeParameter
   , HiddenTypeParameterIdentifier
   , TypeParameterIdentifier
   , TypeParameterInternal0
   , TypeParameterInternal1
   , TypeParameterInternal3
   , Ability
   , TypeParameterInternal2
   , HiddenEnumSignatureInternal1
   , AbilityDecls
   , AbilityDeclsInternal0
   , EnumDefinitionInternal0
   , EnumDefinitionInternal1
   , PostfixAbilityDecls
   , EnumVariants
   , EnumVariantsInternal0
   , Variant
   , HiddenVariantIdentifier
   , VariantIdentifier
   , VariantInternal0
   , DatatypeFields
   , NamedFields
   , FieldAnnotation
   , HiddenFieldIdentifier
   , FieldIdentifier
   , HiddenType
   , ApplyType
   , ApplyTypeInternal0
   , TypeArguments
   , ModuleAccess
   , HiddenModuleIdentifier
   , ModuleIdentifier
   , Identifier
   , ModuleIdentity
   , ModuleIdentityAddressInternal0
   , NumLiteral
   , NumLiteralInternal0
   , NumLiteralInternal2
   , NumLiteralInternal1
   , FunctionType
   , FunctionTypeInternal0
   , FunctionTypeParameters
   , FunctionTypeParametersInternal0
   , PrimitiveType
   , RefType
   , HiddenReference
   , ImmRef
   , MutRef
   , TupleType
   , NamedFieldsInternal0
   , PositionalFields
   , HiddenFunctionItem
   , FunctionDefinition
   , HiddenFunctionSignature
   , HiddenFunctionIdentifier
   , FunctionIdentifier
   , HiddenFunctionSignatureInternal0
   , Modifier
   , ModifierInternal1
   , ModifierInternal0
   , HiddenFunctionSignatureInternal1
   , RetType
   , FunctionParameters
   , FunctionParametersInternal0
   , FunctionParameter
   , FunctionParameterInternal0
   , HiddenVariableIdentifier
   , VariableIdentifier
   , MutFunctionParameter
   , FunctionParametersInternal1
   , Block
   , ArgListInternal0
   , HiddenExpression
   , HiddenUnaryExpression
   , HiddenUnaryExpressionInternal0
   , HiddenExpressionTerm
   , HiddenLiteralValue
   , AddressLiteral
   , BoolLiteral
   , ByteStringLiteral
   , HexStringLiteral
   , AnnotationExpression
   , BreakExpression
   , BreakExpressionInternal0
   , Label
   , BreakExpressionInternal1
   , CallExpression
   , ArgList
   , NameExpression
   , NameExpressionInternal0
   , ContinueExpression
   , DotExpression
   , ExpressionList
   , IfExpression
   , IfExpressionInternal0
   , IndexExpression
   , IndexExpressionInternal0
   , MacroCallExpression
   , MacroModuleAccess
   , MatchExpression
   , HiddenMatchBody
   , HiddenMatchBodyInternal0
   , MatchArm
   , BindList
   , HiddenBind
   , HiddenBindInternal0
   , MutBindVar
   , BindVar
   , AtBind
   , BindUnpack
   , BindUnpackInternal0
   , BindFields
   , BindNamedFields
   , BindNamedFieldsInternal0
   , BindField
   , HiddenSpreadOperator
   , BindFieldInternal0
   , MutBindField
   , BindNamedFieldsInternal1
   , BindPositionalFields
   , CommaBindList
   , CommaBindListInternal0
   , OrBindList
   , OrBindListInternal0
   , OrBindListInternal1
   , OrBindListInternal2
   , MatchArmInternal0
   , MatchCondition
   , PackExpression
   , FieldInitializeList
   , ExpField
   , ExpFieldInternal0
   , FieldInitializeListInternal0
   , SpecBlock
   , SpecBlockInternal1
   , HiddenSpecFunction
   , NativeSpecFunction
   , HiddenSpecFunctionSignature
   , UninterpretedSpecFunction
   , UsualSpecFunction
   , SpecBlockInternal0
   , HiddenSpecBlockTarget
   , SpecBlockTargetSchema
   , HiddenStructIdentifier
   , StructIdentifier
   , SpecBody
   , HiddenSpecBlockMemeber
   , SpecApply
   , SpecApplyInternal0
   , SpecApplyPattern
   , SpecApplyNamePattern
   , SpecApplyPatternInternal1
   , SpecApplyPatternInternal0
   , SpecCondition
   , HiddenSpecAbortIf
   , HiddenSpecAbortIfInternal0
   , ConditionProperties
   , ConditionPropertiesInternal0
   , SpecProperty
   , SpecPropertyInternal0
   , HiddenSpecAbortIfInternal1
   , ConditionKind
   , HiddenSpecAbortWithOrModifies
   , HiddenSpecCondition
   , HiddenSpecConditionInternal1
   , HiddenSpecConditionInternal0
   , SpecInclude
   , SpecInvariant
   , SpecInvariantInternal1
   , InvariantModifier
   , SpecLet
   , SpecLetInternal0
   , SpecPragma
   , SpecVariable
   , SpecVariableInternal1
   , SpecVariableInternal0
   , UseDeclaration
   , UseDeclarationInternal0
   , UseFun
   , UseModule
   , UseModuleInternal0
   , UseModuleMember
   , UseMember
   , UseMemberInternal0
   , UseModuleMembers
   , UnitExpression
   , VectorExpression
   , VectorExpressionInternal0
   , BorrowExpression
   , DereferenceExpression
   , MoveOrCopyExpression
   , MoveOrCopyExpressionInternal0
   , UnaryExpression
   , UnaryOp
   , AbortExpression
   , AbortExpressionInternal0
   , AssignExpression
   , BinaryExpression
   , BinaryOperator
   , CastExpression
   , IdentifiedExpression
   , BlockIdentifier
   , LambdaExpression
   , LambdaBindings
   , LambdaBinding
   , LambdaBindingInternal0
   , LambdaBindingsInternal0
   , LambdaExpressionInternal0
   , LoopExpression
   , QuantifierExpression
   , QuantifierBindings
   , QuantifierBinding
   , QuantifierExpressionInternal0
   , HiddenExists
   , HiddenForall
   , QuantifierExpressionInternal1
   , ReturnExpression
   , WhileExpression
   , BlockItem
   , BlockItemInternal0
   , LetStatement
   , LetStatementInternal0
   , LetStatementInternal1
   , MacroFunctionDefinition
   , HiddenMacroSignature
   , NativeFunctionDefinition
   , HiddenStructItem
   , NativeStructDefinition
   , HiddenStructSignature
   , StructDefinition
   , Constant
   , ConstantIdentifier
   , FriendDeclaration
   , FriendAccess
   , ModuleBodyInternal2
   , Token
   , Syntax.PairF
   , Syntax.MaybeF
   , Syntax.ListF
   , Syntax.EitherF
   , Syntax.UnitF
   ]

type MoveTerm      = Term MoveSig
type MoveTermLab l = TermLab MoveSig l

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

type instance RootSort MoveSig = SourceFileL

--------------------------------------------------------------------------------
-- Symbol
--------------------------------------------------------------------------------

type data Symbol (symbolType :: SymbolType) where
  SourceFileSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleBodySymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleBodyInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleBodyInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenEnumItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenEnumSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenEnumIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenEnumSignatureInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortWithOrModifiesInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeParameterIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterInternal3Symbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterInternal2Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenEnumSignatureInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilityDeclsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilityDeclsInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumDefinitionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumDefinitionInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  PostfixAbilityDeclsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumVariantsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumVariantsInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  VariantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenVariantIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VariantIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VariantInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  DatatypeFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NamedFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldAnnotationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFieldIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ApplyTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ApplyTypeInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeArgumentsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenModuleIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentityAddressInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralInternal2Symbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeParametersInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  PrimitiveTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RefTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenReferenceSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ImmRefSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutRefSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TupleTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NamedFieldsInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  PositionalFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionSignatureInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionSignatureInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  RetTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParametersInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParameterInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenVariableIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VariableIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutFunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParametersInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ArgListInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenUnaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenUnaryExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExpressionTermSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenLiteralValueSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AddressLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BoolLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ByteStringLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HexStringLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AnnotationExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BreakExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BreakExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  LabelSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BreakExpressionInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  CallExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ArgListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NameExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NameExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  ContinueExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DotExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExpressionListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IfExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IfExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  IndexExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IndexExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroCallExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroModuleAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenMatchBodySymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenMatchBodyInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchArmSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenBindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenBindInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MutBindVarSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindVarSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AtBindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindUnpackSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindUnpackInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  BindFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindNamedFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindNamedFieldsInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  BindFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpreadOperatorSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindFieldInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MutBindFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindNamedFieldsInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  BindPositionalFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CommaBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CommaBindListInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  OrBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  OrBindListInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  OrBindListInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  OrBindListInternal2Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchArmInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PackExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldInitializeListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExpFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExpFieldInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldInitializeListInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecFunctionSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UninterpretedSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UsualSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecBlockTargetSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockTargetSchemaSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  StructIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBodySymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecBlockMemeberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplySymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyPatternSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyNamePatternSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyPatternInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyPatternInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortIfSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortIfInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  ConditionPropertiesSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConditionPropertiesInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPropertySymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPropertyInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortIfInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  ConditionKindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortWithOrModifiesSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecIncludeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecInvariantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecInvariantInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  InvariantModifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecLetSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecLetInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPragmaSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecVariableSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecVariableInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecVariableInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  UseDeclarationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseDeclarationInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  UseFunSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleMemberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseMemberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseMemberInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleMembersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UnitExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VectorExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VectorExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  BorrowExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DereferenceExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MoveOrCopyExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MoveOrCopyExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  UnaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UnaryOpSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbortExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbortExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  AssignExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BinaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BinaryOperatorSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CastExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IdentifiedExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaBindingsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaBindingSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaBindingInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaBindingsInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  LoopExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierBindingsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierBindingSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExistsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenForallSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierExpressionInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  ReturnExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  WhileExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockItemInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  LetStatementSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LetStatementInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  LetStatementInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroFunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenMacroSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeFunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeStructDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  StructDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConstantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConstantIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FriendDeclarationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FriendAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleBodyInternal2Symbol :: (symbolType ~ Regular) => Symbol symbolType
  BangTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NeqTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DollarTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BitandTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AndTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MulTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AddTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SubTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RangeTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DivTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LtTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ShlTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LeTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AssignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EqTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ImpliesTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  GtTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  GeTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ShrTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  XorTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbortsWithTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AddressTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AssertTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AssumeTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BoolTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BytearrayTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CopyTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DecreasesTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DropTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnsuresTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EntryTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FalseTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FriendTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  GlobalTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  InternalTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  KeyTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LocalTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifiesTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MoveTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PackTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PackageTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PhantomTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PostTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PublicTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SignerTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  StoreTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SucceedsIfTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TrueTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U128TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U16TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U256TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U32TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U64TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U8TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UnpackTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UpdateTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BitorTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  OrTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ErrorSymbol :: (symbolType ~ Auxiliary) => Symbol symbolType
  MissingSymbol :: (symbolType ~ Auxiliary) => Symbol symbolType
  SortMismatchSymbol :: (symbolType ~ Virtual) => Symbol symbolType

data SymbolSing (symbolType :: SymbolType) (symbol :: Symbol symbolType) where
  SSourceFileSymbol :: SymbolSing Regular SourceFileSymbol
  SModuleDefinitionSymbol :: SymbolSing Regular ModuleDefinitionSymbol
  SModuleBodySymbol :: SymbolSing Regular ModuleBodySymbol
  SModuleBodyInternal0Symbol :: SymbolSing Regular ModuleBodyInternal0Symbol
  SModuleBodyInternal1Symbol :: SymbolSing Regular ModuleBodyInternal1Symbol
  SHiddenEnumItemSymbol :: SymbolSing Regular HiddenEnumItemSymbol
  SEnumDefinitionSymbol :: SymbolSing Regular EnumDefinitionSymbol
  SHiddenEnumSignatureSymbol :: SymbolSing Regular HiddenEnumSignatureSymbol
  SHiddenEnumIdentifierSymbol :: SymbolSing Regular HiddenEnumIdentifierSymbol
  SEnumIdentifierSymbol :: SymbolSing Regular EnumIdentifierSymbol
  SHiddenEnumSignatureInternal0Symbol :: SymbolSing Regular HiddenEnumSignatureInternal0Symbol
  STypeParametersSymbol :: SymbolSing Regular TypeParametersSymbol
  SHiddenSpecAbortWithOrModifiesInternal1Symbol :: SymbolSing Regular HiddenSpecAbortWithOrModifiesInternal1Symbol
  STypeParameterSymbol :: SymbolSing Regular TypeParameterSymbol
  SHiddenTypeParameterIdentifierSymbol :: SymbolSing Regular HiddenTypeParameterIdentifierSymbol
  STypeParameterIdentifierSymbol :: SymbolSing Regular TypeParameterIdentifierSymbol
  STypeParameterInternal0Symbol :: SymbolSing Regular TypeParameterInternal0Symbol
  STypeParameterInternal1Symbol :: SymbolSing Regular TypeParameterInternal1Symbol
  STypeParameterInternal3Symbol :: SymbolSing Regular TypeParameterInternal3Symbol
  SAbilitySymbol :: SymbolSing Regular AbilitySymbol
  STypeParameterInternal2Symbol :: SymbolSing Regular TypeParameterInternal2Symbol
  SHiddenEnumSignatureInternal1Symbol :: SymbolSing Regular HiddenEnumSignatureInternal1Symbol
  SAbilityDeclsSymbol :: SymbolSing Regular AbilityDeclsSymbol
  SAbilityDeclsInternal0Symbol :: SymbolSing Regular AbilityDeclsInternal0Symbol
  SEnumDefinitionInternal0Symbol :: SymbolSing Regular EnumDefinitionInternal0Symbol
  SEnumDefinitionInternal1Symbol :: SymbolSing Regular EnumDefinitionInternal1Symbol
  SPostfixAbilityDeclsSymbol :: SymbolSing Regular PostfixAbilityDeclsSymbol
  SEnumVariantsSymbol :: SymbolSing Regular EnumVariantsSymbol
  SEnumVariantsInternal0Symbol :: SymbolSing Regular EnumVariantsInternal0Symbol
  SVariantSymbol :: SymbolSing Regular VariantSymbol
  SHiddenVariantIdentifierSymbol :: SymbolSing Regular HiddenVariantIdentifierSymbol
  SVariantIdentifierSymbol :: SymbolSing Regular VariantIdentifierSymbol
  SVariantInternal0Symbol :: SymbolSing Regular VariantInternal0Symbol
  SDatatypeFieldsSymbol :: SymbolSing Regular DatatypeFieldsSymbol
  SNamedFieldsSymbol :: SymbolSing Regular NamedFieldsSymbol
  SFieldAnnotationSymbol :: SymbolSing Regular FieldAnnotationSymbol
  SHiddenFieldIdentifierSymbol :: SymbolSing Regular HiddenFieldIdentifierSymbol
  SFieldIdentifierSymbol :: SymbolSing Regular FieldIdentifierSymbol
  SHiddenTypeSymbol :: SymbolSing Regular HiddenTypeSymbol
  SApplyTypeSymbol :: SymbolSing Regular ApplyTypeSymbol
  SApplyTypeInternal0Symbol :: SymbolSing Regular ApplyTypeInternal0Symbol
  STypeArgumentsSymbol :: SymbolSing Regular TypeArgumentsSymbol
  SModuleAccessSymbol :: SymbolSing Regular ModuleAccessSymbol
  SHiddenModuleIdentifierSymbol :: SymbolSing Regular HiddenModuleIdentifierSymbol
  SModuleIdentifierSymbol :: SymbolSing Regular ModuleIdentifierSymbol
  SIdentifierSymbol :: SymbolSing Regular IdentifierSymbol
  SModuleIdentitySymbol :: SymbolSing Regular ModuleIdentitySymbol
  SModuleIdentityAddressInternal0Symbol :: SymbolSing Regular ModuleIdentityAddressInternal0Symbol
  SNumLiteralSymbol :: SymbolSing Regular NumLiteralSymbol
  SNumLiteralInternal0Symbol :: SymbolSing Regular NumLiteralInternal0Symbol
  SNumLiteralInternal2Symbol :: SymbolSing Regular NumLiteralInternal2Symbol
  SNumLiteralInternal1Symbol :: SymbolSing Regular NumLiteralInternal1Symbol
  SFunctionTypeSymbol :: SymbolSing Regular FunctionTypeSymbol
  SFunctionTypeInternal0Symbol :: SymbolSing Regular FunctionTypeInternal0Symbol
  SFunctionTypeParametersSymbol :: SymbolSing Regular FunctionTypeParametersSymbol
  SFunctionTypeParametersInternal0Symbol :: SymbolSing Regular FunctionTypeParametersInternal0Symbol
  SPrimitiveTypeSymbol :: SymbolSing Regular PrimitiveTypeSymbol
  SRefTypeSymbol :: SymbolSing Regular RefTypeSymbol
  SHiddenReferenceSymbol :: SymbolSing Regular HiddenReferenceSymbol
  SImmRefSymbol :: SymbolSing Regular ImmRefSymbol
  SMutRefSymbol :: SymbolSing Regular MutRefSymbol
  STupleTypeSymbol :: SymbolSing Regular TupleTypeSymbol
  SNamedFieldsInternal0Symbol :: SymbolSing Regular NamedFieldsInternal0Symbol
  SPositionalFieldsSymbol :: SymbolSing Regular PositionalFieldsSymbol
  SHiddenFunctionItemSymbol :: SymbolSing Regular HiddenFunctionItemSymbol
  SFunctionDefinitionSymbol :: SymbolSing Regular FunctionDefinitionSymbol
  SHiddenFunctionSignatureSymbol :: SymbolSing Regular HiddenFunctionSignatureSymbol
  SHiddenFunctionIdentifierSymbol :: SymbolSing Regular HiddenFunctionIdentifierSymbol
  SFunctionIdentifierSymbol :: SymbolSing Regular FunctionIdentifierSymbol
  SHiddenFunctionSignatureInternal0Symbol :: SymbolSing Regular HiddenFunctionSignatureInternal0Symbol
  SModifierSymbol :: SymbolSing Regular ModifierSymbol
  SModifierInternal1Symbol :: SymbolSing Regular ModifierInternal1Symbol
  SModifierInternal0Symbol :: SymbolSing Regular ModifierInternal0Symbol
  SHiddenFunctionSignatureInternal1Symbol :: SymbolSing Regular HiddenFunctionSignatureInternal1Symbol
  SRetTypeSymbol :: SymbolSing Regular RetTypeSymbol
  SFunctionParametersSymbol :: SymbolSing Regular FunctionParametersSymbol
  SFunctionParametersInternal0Symbol :: SymbolSing Regular FunctionParametersInternal0Symbol
  SFunctionParameterSymbol :: SymbolSing Regular FunctionParameterSymbol
  SFunctionParameterInternal0Symbol :: SymbolSing Regular FunctionParameterInternal0Symbol
  SHiddenVariableIdentifierSymbol :: SymbolSing Regular HiddenVariableIdentifierSymbol
  SVariableIdentifierSymbol :: SymbolSing Regular VariableIdentifierSymbol
  SMutFunctionParameterSymbol :: SymbolSing Regular MutFunctionParameterSymbol
  SFunctionParametersInternal1Symbol :: SymbolSing Regular FunctionParametersInternal1Symbol
  SBlockSymbol :: SymbolSing Regular BlockSymbol
  SArgListInternal0Symbol :: SymbolSing Regular ArgListInternal0Symbol
  SHiddenExpressionSymbol :: SymbolSing Regular HiddenExpressionSymbol
  SHiddenUnaryExpressionSymbol :: SymbolSing Regular HiddenUnaryExpressionSymbol
  SHiddenUnaryExpressionInternal0Symbol :: SymbolSing Regular HiddenUnaryExpressionInternal0Symbol
  SHiddenExpressionTermSymbol :: SymbolSing Regular HiddenExpressionTermSymbol
  SHiddenLiteralValueSymbol :: SymbolSing Regular HiddenLiteralValueSymbol
  SAddressLiteralSymbol :: SymbolSing Regular AddressLiteralSymbol
  SBoolLiteralSymbol :: SymbolSing Regular BoolLiteralSymbol
  SByteStringLiteralSymbol :: SymbolSing Regular ByteStringLiteralSymbol
  SHexStringLiteralSymbol :: SymbolSing Regular HexStringLiteralSymbol
  SAnnotationExpressionSymbol :: SymbolSing Regular AnnotationExpressionSymbol
  SBreakExpressionSymbol :: SymbolSing Regular BreakExpressionSymbol
  SBreakExpressionInternal0Symbol :: SymbolSing Regular BreakExpressionInternal0Symbol
  SLabelSymbol :: SymbolSing Regular LabelSymbol
  SBreakExpressionInternal1Symbol :: SymbolSing Regular BreakExpressionInternal1Symbol
  SCallExpressionSymbol :: SymbolSing Regular CallExpressionSymbol
  SArgListSymbol :: SymbolSing Regular ArgListSymbol
  SNameExpressionSymbol :: SymbolSing Regular NameExpressionSymbol
  SNameExpressionInternal0Symbol :: SymbolSing Regular NameExpressionInternal0Symbol
  SContinueExpressionSymbol :: SymbolSing Regular ContinueExpressionSymbol
  SDotExpressionSymbol :: SymbolSing Regular DotExpressionSymbol
  SExpressionListSymbol :: SymbolSing Regular ExpressionListSymbol
  SIfExpressionSymbol :: SymbolSing Regular IfExpressionSymbol
  SIfExpressionInternal0Symbol :: SymbolSing Regular IfExpressionInternal0Symbol
  SIndexExpressionSymbol :: SymbolSing Regular IndexExpressionSymbol
  SIndexExpressionInternal0Symbol :: SymbolSing Regular IndexExpressionInternal0Symbol
  SMacroCallExpressionSymbol :: SymbolSing Regular MacroCallExpressionSymbol
  SMacroModuleAccessSymbol :: SymbolSing Regular MacroModuleAccessSymbol
  SMatchExpressionSymbol :: SymbolSing Regular MatchExpressionSymbol
  SHiddenMatchBodySymbol :: SymbolSing Regular HiddenMatchBodySymbol
  SHiddenMatchBodyInternal0Symbol :: SymbolSing Regular HiddenMatchBodyInternal0Symbol
  SMatchArmSymbol :: SymbolSing Regular MatchArmSymbol
  SBindListSymbol :: SymbolSing Regular BindListSymbol
  SHiddenBindSymbol :: SymbolSing Regular HiddenBindSymbol
  SHiddenBindInternal0Symbol :: SymbolSing Regular HiddenBindInternal0Symbol
  SMutBindVarSymbol :: SymbolSing Regular MutBindVarSymbol
  SBindVarSymbol :: SymbolSing Regular BindVarSymbol
  SAtBindSymbol :: SymbolSing Regular AtBindSymbol
  SBindUnpackSymbol :: SymbolSing Regular BindUnpackSymbol
  SBindUnpackInternal0Symbol :: SymbolSing Regular BindUnpackInternal0Symbol
  SBindFieldsSymbol :: SymbolSing Regular BindFieldsSymbol
  SBindNamedFieldsSymbol :: SymbolSing Regular BindNamedFieldsSymbol
  SBindNamedFieldsInternal0Symbol :: SymbolSing Regular BindNamedFieldsInternal0Symbol
  SBindFieldSymbol :: SymbolSing Regular BindFieldSymbol
  SHiddenSpreadOperatorSymbol :: SymbolSing Regular HiddenSpreadOperatorSymbol
  SBindFieldInternal0Symbol :: SymbolSing Regular BindFieldInternal0Symbol
  SMutBindFieldSymbol :: SymbolSing Regular MutBindFieldSymbol
  SBindNamedFieldsInternal1Symbol :: SymbolSing Regular BindNamedFieldsInternal1Symbol
  SBindPositionalFieldsSymbol :: SymbolSing Regular BindPositionalFieldsSymbol
  SCommaBindListSymbol :: SymbolSing Regular CommaBindListSymbol
  SCommaBindListInternal0Symbol :: SymbolSing Regular CommaBindListInternal0Symbol
  SOrBindListSymbol :: SymbolSing Regular OrBindListSymbol
  SOrBindListInternal0Symbol :: SymbolSing Regular OrBindListInternal0Symbol
  SOrBindListInternal1Symbol :: SymbolSing Regular OrBindListInternal1Symbol
  SOrBindListInternal2Symbol :: SymbolSing Regular OrBindListInternal2Symbol
  SMatchArmInternal0Symbol :: SymbolSing Regular MatchArmInternal0Symbol
  SMatchConditionSymbol :: SymbolSing Regular MatchConditionSymbol
  SPackExpressionSymbol :: SymbolSing Regular PackExpressionSymbol
  SFieldInitializeListSymbol :: SymbolSing Regular FieldInitializeListSymbol
  SExpFieldSymbol :: SymbolSing Regular ExpFieldSymbol
  SExpFieldInternal0Symbol :: SymbolSing Regular ExpFieldInternal0Symbol
  SFieldInitializeListInternal0Symbol :: SymbolSing Regular FieldInitializeListInternal0Symbol
  SSpecBlockSymbol :: SymbolSing Regular SpecBlockSymbol
  SSpecBlockInternal1Symbol :: SymbolSing Regular SpecBlockInternal1Symbol
  SHiddenSpecFunctionSymbol :: SymbolSing Regular HiddenSpecFunctionSymbol
  SNativeSpecFunctionSymbol :: SymbolSing Regular NativeSpecFunctionSymbol
  SHiddenSpecFunctionSignatureSymbol :: SymbolSing Regular HiddenSpecFunctionSignatureSymbol
  SUninterpretedSpecFunctionSymbol :: SymbolSing Regular UninterpretedSpecFunctionSymbol
  SUsualSpecFunctionSymbol :: SymbolSing Regular UsualSpecFunctionSymbol
  SSpecBlockInternal0Symbol :: SymbolSing Regular SpecBlockInternal0Symbol
  SHiddenSpecBlockTargetSymbol :: SymbolSing Regular HiddenSpecBlockTargetSymbol
  SSpecBlockTargetSchemaSymbol :: SymbolSing Regular SpecBlockTargetSchemaSymbol
  SHiddenStructIdentifierSymbol :: SymbolSing Regular HiddenStructIdentifierSymbol
  SStructIdentifierSymbol :: SymbolSing Regular StructIdentifierSymbol
  SSpecBodySymbol :: SymbolSing Regular SpecBodySymbol
  SHiddenSpecBlockMemeberSymbol :: SymbolSing Regular HiddenSpecBlockMemeberSymbol
  SSpecApplySymbol :: SymbolSing Regular SpecApplySymbol
  SSpecApplyInternal0Symbol :: SymbolSing Regular SpecApplyInternal0Symbol
  SSpecApplyPatternSymbol :: SymbolSing Regular SpecApplyPatternSymbol
  SSpecApplyNamePatternSymbol :: SymbolSing Regular SpecApplyNamePatternSymbol
  SSpecApplyPatternInternal1Symbol :: SymbolSing Regular SpecApplyPatternInternal1Symbol
  SSpecApplyPatternInternal0Symbol :: SymbolSing Regular SpecApplyPatternInternal0Symbol
  SSpecConditionSymbol :: SymbolSing Regular SpecConditionSymbol
  SHiddenSpecAbortIfSymbol :: SymbolSing Regular HiddenSpecAbortIfSymbol
  SHiddenSpecAbortIfInternal0Symbol :: SymbolSing Regular HiddenSpecAbortIfInternal0Symbol
  SConditionPropertiesSymbol :: SymbolSing Regular ConditionPropertiesSymbol
  SConditionPropertiesInternal0Symbol :: SymbolSing Regular ConditionPropertiesInternal0Symbol
  SSpecPropertySymbol :: SymbolSing Regular SpecPropertySymbol
  SSpecPropertyInternal0Symbol :: SymbolSing Regular SpecPropertyInternal0Symbol
  SHiddenSpecAbortIfInternal1Symbol :: SymbolSing Regular HiddenSpecAbortIfInternal1Symbol
  SConditionKindSymbol :: SymbolSing Regular ConditionKindSymbol
  SHiddenSpecAbortWithOrModifiesSymbol :: SymbolSing Regular HiddenSpecAbortWithOrModifiesSymbol
  SHiddenSpecConditionSymbol :: SymbolSing Regular HiddenSpecConditionSymbol
  SHiddenSpecConditionInternal1Symbol :: SymbolSing Regular HiddenSpecConditionInternal1Symbol
  SHiddenSpecConditionInternal0Symbol :: SymbolSing Regular HiddenSpecConditionInternal0Symbol
  SSpecIncludeSymbol :: SymbolSing Regular SpecIncludeSymbol
  SSpecInvariantSymbol :: SymbolSing Regular SpecInvariantSymbol
  SSpecInvariantInternal1Symbol :: SymbolSing Regular SpecInvariantInternal1Symbol
  SInvariantModifierSymbol :: SymbolSing Regular InvariantModifierSymbol
  SSpecLetSymbol :: SymbolSing Regular SpecLetSymbol
  SSpecLetInternal0Symbol :: SymbolSing Regular SpecLetInternal0Symbol
  SSpecPragmaSymbol :: SymbolSing Regular SpecPragmaSymbol
  SSpecVariableSymbol :: SymbolSing Regular SpecVariableSymbol
  SSpecVariableInternal1Symbol :: SymbolSing Regular SpecVariableInternal1Symbol
  SSpecVariableInternal0Symbol :: SymbolSing Regular SpecVariableInternal0Symbol
  SUseDeclarationSymbol :: SymbolSing Regular UseDeclarationSymbol
  SUseDeclarationInternal0Symbol :: SymbolSing Regular UseDeclarationInternal0Symbol
  SUseFunSymbol :: SymbolSing Regular UseFunSymbol
  SUseModuleSymbol :: SymbolSing Regular UseModuleSymbol
  SUseModuleInternal0Symbol :: SymbolSing Regular UseModuleInternal0Symbol
  SUseModuleMemberSymbol :: SymbolSing Regular UseModuleMemberSymbol
  SUseMemberSymbol :: SymbolSing Regular UseMemberSymbol
  SUseMemberInternal0Symbol :: SymbolSing Regular UseMemberInternal0Symbol
  SUseModuleMembersSymbol :: SymbolSing Regular UseModuleMembersSymbol
  SUnitExpressionSymbol :: SymbolSing Regular UnitExpressionSymbol
  SVectorExpressionSymbol :: SymbolSing Regular VectorExpressionSymbol
  SVectorExpressionInternal0Symbol :: SymbolSing Regular VectorExpressionInternal0Symbol
  SBorrowExpressionSymbol :: SymbolSing Regular BorrowExpressionSymbol
  SDereferenceExpressionSymbol :: SymbolSing Regular DereferenceExpressionSymbol
  SMoveOrCopyExpressionSymbol :: SymbolSing Regular MoveOrCopyExpressionSymbol
  SMoveOrCopyExpressionInternal0Symbol :: SymbolSing Regular MoveOrCopyExpressionInternal0Symbol
  SUnaryExpressionSymbol :: SymbolSing Regular UnaryExpressionSymbol
  SUnaryOpSymbol :: SymbolSing Regular UnaryOpSymbol
  SAbortExpressionSymbol :: SymbolSing Regular AbortExpressionSymbol
  SAbortExpressionInternal0Symbol :: SymbolSing Regular AbortExpressionInternal0Symbol
  SAssignExpressionSymbol :: SymbolSing Regular AssignExpressionSymbol
  SBinaryExpressionSymbol :: SymbolSing Regular BinaryExpressionSymbol
  SBinaryOperatorSymbol :: SymbolSing Regular BinaryOperatorSymbol
  SCastExpressionSymbol :: SymbolSing Regular CastExpressionSymbol
  SIdentifiedExpressionSymbol :: SymbolSing Regular IdentifiedExpressionSymbol
  SBlockIdentifierSymbol :: SymbolSing Regular BlockIdentifierSymbol
  SLambdaExpressionSymbol :: SymbolSing Regular LambdaExpressionSymbol
  SLambdaBindingsSymbol :: SymbolSing Regular LambdaBindingsSymbol
  SLambdaBindingSymbol :: SymbolSing Regular LambdaBindingSymbol
  SLambdaBindingInternal0Symbol :: SymbolSing Regular LambdaBindingInternal0Symbol
  SLambdaBindingsInternal0Symbol :: SymbolSing Regular LambdaBindingsInternal0Symbol
  SLambdaExpressionInternal0Symbol :: SymbolSing Regular LambdaExpressionInternal0Symbol
  SLoopExpressionSymbol :: SymbolSing Regular LoopExpressionSymbol
  SQuantifierExpressionSymbol :: SymbolSing Regular QuantifierExpressionSymbol
  SQuantifierBindingsSymbol :: SymbolSing Regular QuantifierBindingsSymbol
  SQuantifierBindingSymbol :: SymbolSing Regular QuantifierBindingSymbol
  SQuantifierExpressionInternal0Symbol :: SymbolSing Regular QuantifierExpressionInternal0Symbol
  SHiddenExistsSymbol :: SymbolSing Regular HiddenExistsSymbol
  SHiddenForallSymbol :: SymbolSing Regular HiddenForallSymbol
  SQuantifierExpressionInternal1Symbol :: SymbolSing Regular QuantifierExpressionInternal1Symbol
  SReturnExpressionSymbol :: SymbolSing Regular ReturnExpressionSymbol
  SWhileExpressionSymbol :: SymbolSing Regular WhileExpressionSymbol
  SBlockItemSymbol :: SymbolSing Regular BlockItemSymbol
  SBlockItemInternal0Symbol :: SymbolSing Regular BlockItemInternal0Symbol
  SLetStatementSymbol :: SymbolSing Regular LetStatementSymbol
  SLetStatementInternal0Symbol :: SymbolSing Regular LetStatementInternal0Symbol
  SLetStatementInternal1Symbol :: SymbolSing Regular LetStatementInternal1Symbol
  SMacroFunctionDefinitionSymbol :: SymbolSing Regular MacroFunctionDefinitionSymbol
  SHiddenMacroSignatureSymbol :: SymbolSing Regular HiddenMacroSignatureSymbol
  SNativeFunctionDefinitionSymbol :: SymbolSing Regular NativeFunctionDefinitionSymbol
  SHiddenStructItemSymbol :: SymbolSing Regular HiddenStructItemSymbol
  SNativeStructDefinitionSymbol :: SymbolSing Regular NativeStructDefinitionSymbol
  SHiddenStructSignatureSymbol :: SymbolSing Regular HiddenStructSignatureSymbol
  SStructDefinitionSymbol :: SymbolSing Regular StructDefinitionSymbol
  SConstantSymbol :: SymbolSing Regular ConstantSymbol
  SConstantIdentifierSymbol :: SymbolSing Regular ConstantIdentifierSymbol
  SFriendDeclarationSymbol :: SymbolSing Regular FriendDeclarationSymbol
  SFriendAccessSymbol :: SymbolSing Regular FriendAccessSymbol
  SModuleBodyInternal2Symbol :: SymbolSing Regular ModuleBodyInternal2Symbol
  SBangTokSymbol :: SymbolSing Anonymous BangTokSymbol
  SNeqTokSymbol :: SymbolSing Anonymous NeqTokSymbol
  SDollarTokSymbol :: SymbolSing Anonymous DollarTokSymbol
  SModTokSymbol :: SymbolSing Anonymous ModTokSymbol
  SBitandTokSymbol :: SymbolSing Anonymous BitandTokSymbol
  SAndTokSymbol :: SymbolSing Anonymous AndTokSymbol
  SMulTokSymbol :: SymbolSing Anonymous MulTokSymbol
  SAddTokSymbol :: SymbolSing Anonymous AddTokSymbol
  SSubTokSymbol :: SymbolSing Anonymous SubTokSymbol
  SRangeTokSymbol :: SymbolSing Anonymous RangeTokSymbol
  SDivTokSymbol :: SymbolSing Anonymous DivTokSymbol
  SLtTokSymbol :: SymbolSing Anonymous LtTokSymbol
  SShlTokSymbol :: SymbolSing Anonymous ShlTokSymbol
  SLeTokSymbol :: SymbolSing Anonymous LeTokSymbol
  SAssignTokSymbol :: SymbolSing Anonymous AssignTokSymbol
  SEqTokSymbol :: SymbolSing Anonymous EqTokSymbol
  SImpliesTokSymbol :: SymbolSing Anonymous ImpliesTokSymbol
  SGtTokSymbol :: SymbolSing Anonymous GtTokSymbol
  SGeTokSymbol :: SymbolSing Anonymous GeTokSymbol
  SShrTokSymbol :: SymbolSing Anonymous ShrTokSymbol
  SXorTokSymbol :: SymbolSing Anonymous XorTokSymbol
  SAbortsWithTokSymbol :: SymbolSing Anonymous AbortsWithTokSymbol
  SAddressTokSymbol :: SymbolSing Anonymous AddressTokSymbol
  SAssertTokSymbol :: SymbolSing Anonymous AssertTokSymbol
  SAssumeTokSymbol :: SymbolSing Anonymous AssumeTokSymbol
  SBoolTokSymbol :: SymbolSing Anonymous BoolTokSymbol
  SBytearrayTokSymbol :: SymbolSing Anonymous BytearrayTokSymbol
  SCopyTokSymbol :: SymbolSing Anonymous CopyTokSymbol
  SDecreasesTokSymbol :: SymbolSing Anonymous DecreasesTokSymbol
  SDropTokSymbol :: SymbolSing Anonymous DropTokSymbol
  SEnsuresTokSymbol :: SymbolSing Anonymous EnsuresTokSymbol
  SEntryTokSymbol :: SymbolSing Anonymous EntryTokSymbol
  SFalseTokSymbol :: SymbolSing Anonymous FalseTokSymbol
  SFriendTokSymbol :: SymbolSing Anonymous FriendTokSymbol
  SGlobalTokSymbol :: SymbolSing Anonymous GlobalTokSymbol
  SInternalTokSymbol :: SymbolSing Anonymous InternalTokSymbol
  SKeyTokSymbol :: SymbolSing Anonymous KeyTokSymbol
  SLocalTokSymbol :: SymbolSing Anonymous LocalTokSymbol
  SModifiesTokSymbol :: SymbolSing Anonymous ModifiesTokSymbol
  SModuleTokSymbol :: SymbolSing Anonymous ModuleTokSymbol
  SMoveTokSymbol :: SymbolSing Anonymous MoveTokSymbol
  SNativeTokSymbol :: SymbolSing Anonymous NativeTokSymbol
  SPackTokSymbol :: SymbolSing Anonymous PackTokSymbol
  SPackageTokSymbol :: SymbolSing Anonymous PackageTokSymbol
  SPhantomTokSymbol :: SymbolSing Anonymous PhantomTokSymbol
  SPostTokSymbol :: SymbolSing Anonymous PostTokSymbol
  SPublicTokSymbol :: SymbolSing Anonymous PublicTokSymbol
  SSignerTokSymbol :: SymbolSing Anonymous SignerTokSymbol
  SStoreTokSymbol :: SymbolSing Anonymous StoreTokSymbol
  SSucceedsIfTokSymbol :: SymbolSing Anonymous SucceedsIfTokSymbol
  STrueTokSymbol :: SymbolSing Anonymous TrueTokSymbol
  SU128TokSymbol :: SymbolSing Anonymous U128TokSymbol
  SU16TokSymbol :: SymbolSing Anonymous U16TokSymbol
  SU256TokSymbol :: SymbolSing Anonymous U256TokSymbol
  SU32TokSymbol :: SymbolSing Anonymous U32TokSymbol
  SU64TokSymbol :: SymbolSing Anonymous U64TokSymbol
  SU8TokSymbol :: SymbolSing Anonymous U8TokSymbol
  SUnpackTokSymbol :: SymbolSing Anonymous UnpackTokSymbol
  SUpdateTokSymbol :: SymbolSing Anonymous UpdateTokSymbol
  SBitorTokSymbol :: SymbolSing Anonymous BitorTokSymbol
  SOrTokSymbol :: SymbolSing Anonymous OrTokSymbol
  SErrorSymbol :: SymbolSing Auxiliary ErrorSymbol
  SMissingSymbol :: SymbolSing Auxiliary MissingSymbol
  SSortMismatchSymbol :: SymbolSing Virtual SortMismatchSymbol

deriving instance Eq (SymbolSing sort symbol)
deriving instance Ord (SymbolSing sort symbol)
deriving instance Show (SymbolSing sort symbol)

decSymbolSing :: SymbolSing symbolType1 symbol1 -> SymbolSing symbolType2 symbol2 -> Maybe (symbolType1 :~: symbolType2, symbol1 :~~: symbol2)
decSymbolSing SSourceFileSymbol SSourceFileSymbol = Just (Refl, HRefl)
decSymbolSing SModuleDefinitionSymbol SModuleDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SModuleBodySymbol SModuleBodySymbol = Just (Refl, HRefl)
decSymbolSing SModuleBodyInternal0Symbol SModuleBodyInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SModuleBodyInternal1Symbol SModuleBodyInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenEnumItemSymbol SHiddenEnumItemSymbol = Just (Refl, HRefl)
decSymbolSing SEnumDefinitionSymbol SEnumDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenEnumSignatureSymbol SHiddenEnumSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenEnumIdentifierSymbol SHiddenEnumIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SEnumIdentifierSymbol SEnumIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenEnumSignatureInternal0Symbol SHiddenEnumSignatureInternal0Symbol = Just (Refl, HRefl)
decSymbolSing STypeParametersSymbol STypeParametersSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortWithOrModifiesInternal1Symbol SHiddenSpecAbortWithOrModifiesInternal1Symbol = Just (Refl, HRefl)
decSymbolSing STypeParameterSymbol STypeParameterSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeParameterIdentifierSymbol SHiddenTypeParameterIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing STypeParameterIdentifierSymbol STypeParameterIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing STypeParameterInternal0Symbol STypeParameterInternal0Symbol = Just (Refl, HRefl)
decSymbolSing STypeParameterInternal1Symbol STypeParameterInternal1Symbol = Just (Refl, HRefl)
decSymbolSing STypeParameterInternal3Symbol STypeParameterInternal3Symbol = Just (Refl, HRefl)
decSymbolSing SAbilitySymbol SAbilitySymbol = Just (Refl, HRefl)
decSymbolSing STypeParameterInternal2Symbol STypeParameterInternal2Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenEnumSignatureInternal1Symbol SHiddenEnumSignatureInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SAbilityDeclsSymbol SAbilityDeclsSymbol = Just (Refl, HRefl)
decSymbolSing SAbilityDeclsInternal0Symbol SAbilityDeclsInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SEnumDefinitionInternal0Symbol SEnumDefinitionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SEnumDefinitionInternal1Symbol SEnumDefinitionInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SPostfixAbilityDeclsSymbol SPostfixAbilityDeclsSymbol = Just (Refl, HRefl)
decSymbolSing SEnumVariantsSymbol SEnumVariantsSymbol = Just (Refl, HRefl)
decSymbolSing SEnumVariantsInternal0Symbol SEnumVariantsInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SVariantSymbol SVariantSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenVariantIdentifierSymbol SHiddenVariantIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SVariantIdentifierSymbol SVariantIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SVariantInternal0Symbol SVariantInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SDatatypeFieldsSymbol SDatatypeFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SNamedFieldsSymbol SNamedFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SFieldAnnotationSymbol SFieldAnnotationSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFieldIdentifierSymbol SHiddenFieldIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SFieldIdentifierSymbol SFieldIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeSymbol SHiddenTypeSymbol = Just (Refl, HRefl)
decSymbolSing SApplyTypeSymbol SApplyTypeSymbol = Just (Refl, HRefl)
decSymbolSing SApplyTypeInternal0Symbol SApplyTypeInternal0Symbol = Just (Refl, HRefl)
decSymbolSing STypeArgumentsSymbol STypeArgumentsSymbol = Just (Refl, HRefl)
decSymbolSing SModuleAccessSymbol SModuleAccessSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenModuleIdentifierSymbol SHiddenModuleIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentifierSymbol SModuleIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SIdentifierSymbol SIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentitySymbol SModuleIdentitySymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentityAddressInternal0Symbol SModuleIdentityAddressInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralSymbol SNumLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralInternal0Symbol SNumLiteralInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralInternal2Symbol SNumLiteralInternal2Symbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralInternal1Symbol SNumLiteralInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeSymbol SFunctionTypeSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeInternal0Symbol SFunctionTypeInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeParametersSymbol SFunctionTypeParametersSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeParametersInternal0Symbol SFunctionTypeParametersInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SPrimitiveTypeSymbol SPrimitiveTypeSymbol = Just (Refl, HRefl)
decSymbolSing SRefTypeSymbol SRefTypeSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenReferenceSymbol SHiddenReferenceSymbol = Just (Refl, HRefl)
decSymbolSing SImmRefSymbol SImmRefSymbol = Just (Refl, HRefl)
decSymbolSing SMutRefSymbol SMutRefSymbol = Just (Refl, HRefl)
decSymbolSing STupleTypeSymbol STupleTypeSymbol = Just (Refl, HRefl)
decSymbolSing SNamedFieldsInternal0Symbol SNamedFieldsInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SPositionalFieldsSymbol SPositionalFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionItemSymbol SHiddenFunctionItemSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionDefinitionSymbol SFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionSignatureSymbol SHiddenFunctionSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionIdentifierSymbol SHiddenFunctionIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionIdentifierSymbol SFunctionIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionSignatureInternal0Symbol SHiddenFunctionSignatureInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SModifierSymbol SModifierSymbol = Just (Refl, HRefl)
decSymbolSing SModifierInternal1Symbol SModifierInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SModifierInternal0Symbol SModifierInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionSignatureInternal1Symbol SHiddenFunctionSignatureInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SRetTypeSymbol SRetTypeSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParametersSymbol SFunctionParametersSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParametersInternal0Symbol SFunctionParametersInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SFunctionParameterSymbol SFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParameterInternal0Symbol SFunctionParameterInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenVariableIdentifierSymbol SHiddenVariableIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SVariableIdentifierSymbol SVariableIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SMutFunctionParameterSymbol SMutFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParametersInternal1Symbol SFunctionParametersInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SBlockSymbol SBlockSymbol = Just (Refl, HRefl)
decSymbolSing SArgListInternal0Symbol SArgListInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenExpressionSymbol SHiddenExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenUnaryExpressionSymbol SHiddenUnaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenUnaryExpressionInternal0Symbol SHiddenUnaryExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenExpressionTermSymbol SHiddenExpressionTermSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenLiteralValueSymbol SHiddenLiteralValueSymbol = Just (Refl, HRefl)
decSymbolSing SAddressLiteralSymbol SAddressLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SBoolLiteralSymbol SBoolLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SByteStringLiteralSymbol SByteStringLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SHexStringLiteralSymbol SHexStringLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SAnnotationExpressionSymbol SAnnotationExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBreakExpressionSymbol SBreakExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBreakExpressionInternal0Symbol SBreakExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SLabelSymbol SLabelSymbol = Just (Refl, HRefl)
decSymbolSing SBreakExpressionInternal1Symbol SBreakExpressionInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SCallExpressionSymbol SCallExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SArgListSymbol SArgListSymbol = Just (Refl, HRefl)
decSymbolSing SNameExpressionSymbol SNameExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SNameExpressionInternal0Symbol SNameExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SContinueExpressionSymbol SContinueExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SDotExpressionSymbol SDotExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SExpressionListSymbol SExpressionListSymbol = Just (Refl, HRefl)
decSymbolSing SIfExpressionSymbol SIfExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SIfExpressionInternal0Symbol SIfExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SIndexExpressionSymbol SIndexExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SIndexExpressionInternal0Symbol SIndexExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMacroCallExpressionSymbol SMacroCallExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMacroModuleAccessSymbol SMacroModuleAccessSymbol = Just (Refl, HRefl)
decSymbolSing SMatchExpressionSymbol SMatchExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenMatchBodySymbol SHiddenMatchBodySymbol = Just (Refl, HRefl)
decSymbolSing SHiddenMatchBodyInternal0Symbol SHiddenMatchBodyInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMatchArmSymbol SMatchArmSymbol = Just (Refl, HRefl)
decSymbolSing SBindListSymbol SBindListSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenBindSymbol SHiddenBindSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenBindInternal0Symbol SHiddenBindInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMutBindVarSymbol SMutBindVarSymbol = Just (Refl, HRefl)
decSymbolSing SBindVarSymbol SBindVarSymbol = Just (Refl, HRefl)
decSymbolSing SAtBindSymbol SAtBindSymbol = Just (Refl, HRefl)
decSymbolSing SBindUnpackSymbol SBindUnpackSymbol = Just (Refl, HRefl)
decSymbolSing SBindUnpackInternal0Symbol SBindUnpackInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SBindFieldsSymbol SBindFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SBindNamedFieldsSymbol SBindNamedFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SBindNamedFieldsInternal0Symbol SBindNamedFieldsInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SBindFieldSymbol SBindFieldSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpreadOperatorSymbol SHiddenSpreadOperatorSymbol = Just (Refl, HRefl)
decSymbolSing SBindFieldInternal0Symbol SBindFieldInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMutBindFieldSymbol SMutBindFieldSymbol = Just (Refl, HRefl)
decSymbolSing SBindNamedFieldsInternal1Symbol SBindNamedFieldsInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SBindPositionalFieldsSymbol SBindPositionalFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SCommaBindListSymbol SCommaBindListSymbol = Just (Refl, HRefl)
decSymbolSing SCommaBindListInternal0Symbol SCommaBindListInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SOrBindListSymbol SOrBindListSymbol = Just (Refl, HRefl)
decSymbolSing SOrBindListInternal0Symbol SOrBindListInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SOrBindListInternal1Symbol SOrBindListInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SOrBindListInternal2Symbol SOrBindListInternal2Symbol = Just (Refl, HRefl)
decSymbolSing SMatchArmInternal0Symbol SMatchArmInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMatchConditionSymbol SMatchConditionSymbol = Just (Refl, HRefl)
decSymbolSing SPackExpressionSymbol SPackExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SFieldInitializeListSymbol SFieldInitializeListSymbol = Just (Refl, HRefl)
decSymbolSing SExpFieldSymbol SExpFieldSymbol = Just (Refl, HRefl)
decSymbolSing SExpFieldInternal0Symbol SExpFieldInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SFieldInitializeListInternal0Symbol SFieldInitializeListInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockSymbol SSpecBlockSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockInternal1Symbol SSpecBlockInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecFunctionSymbol SHiddenSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SNativeSpecFunctionSymbol SNativeSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecFunctionSignatureSymbol SHiddenSpecFunctionSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SUninterpretedSpecFunctionSymbol SUninterpretedSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SUsualSpecFunctionSymbol SUsualSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockInternal0Symbol SSpecBlockInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecBlockTargetSymbol SHiddenSpecBlockTargetSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockTargetSchemaSymbol SSpecBlockTargetSchemaSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructIdentifierSymbol SHiddenStructIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SStructIdentifierSymbol SStructIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBodySymbol SSpecBodySymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecBlockMemeberSymbol SHiddenSpecBlockMemeberSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplySymbol SSpecApplySymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyInternal0Symbol SSpecApplyInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyPatternSymbol SSpecApplyPatternSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyNamePatternSymbol SSpecApplyNamePatternSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyPatternInternal1Symbol SSpecApplyPatternInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyPatternInternal0Symbol SSpecApplyPatternInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecConditionSymbol SSpecConditionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortIfSymbol SHiddenSpecAbortIfSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortIfInternal0Symbol SHiddenSpecAbortIfInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SConditionPropertiesSymbol SConditionPropertiesSymbol = Just (Refl, HRefl)
decSymbolSing SConditionPropertiesInternal0Symbol SConditionPropertiesInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecPropertySymbol SSpecPropertySymbol = Just (Refl, HRefl)
decSymbolSing SSpecPropertyInternal0Symbol SSpecPropertyInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortIfInternal1Symbol SHiddenSpecAbortIfInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SConditionKindSymbol SConditionKindSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortWithOrModifiesSymbol SHiddenSpecAbortWithOrModifiesSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionSymbol SHiddenSpecConditionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionInternal1Symbol SHiddenSpecConditionInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionInternal0Symbol SHiddenSpecConditionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecIncludeSymbol SSpecIncludeSymbol = Just (Refl, HRefl)
decSymbolSing SSpecInvariantSymbol SSpecInvariantSymbol = Just (Refl, HRefl)
decSymbolSing SSpecInvariantInternal1Symbol SSpecInvariantInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SInvariantModifierSymbol SInvariantModifierSymbol = Just (Refl, HRefl)
decSymbolSing SSpecLetSymbol SSpecLetSymbol = Just (Refl, HRefl)
decSymbolSing SSpecLetInternal0Symbol SSpecLetInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecPragmaSymbol SSpecPragmaSymbol = Just (Refl, HRefl)
decSymbolSing SSpecVariableSymbol SSpecVariableSymbol = Just (Refl, HRefl)
decSymbolSing SSpecVariableInternal1Symbol SSpecVariableInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SSpecVariableInternal0Symbol SSpecVariableInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SUseDeclarationSymbol SUseDeclarationSymbol = Just (Refl, HRefl)
decSymbolSing SUseDeclarationInternal0Symbol SUseDeclarationInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SUseFunSymbol SUseFunSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleSymbol SUseModuleSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleInternal0Symbol SUseModuleInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SUseModuleMemberSymbol SUseModuleMemberSymbol = Just (Refl, HRefl)
decSymbolSing SUseMemberSymbol SUseMemberSymbol = Just (Refl, HRefl)
decSymbolSing SUseMemberInternal0Symbol SUseMemberInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SUseModuleMembersSymbol SUseModuleMembersSymbol = Just (Refl, HRefl)
decSymbolSing SUnitExpressionSymbol SUnitExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SVectorExpressionSymbol SVectorExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SVectorExpressionInternal0Symbol SVectorExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SBorrowExpressionSymbol SBorrowExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SDereferenceExpressionSymbol SDereferenceExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMoveOrCopyExpressionSymbol SMoveOrCopyExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMoveOrCopyExpressionInternal0Symbol SMoveOrCopyExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SUnaryExpressionSymbol SUnaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SUnaryOpSymbol SUnaryOpSymbol = Just (Refl, HRefl)
decSymbolSing SAbortExpressionSymbol SAbortExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SAbortExpressionInternal0Symbol SAbortExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SAssignExpressionSymbol SAssignExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBinaryExpressionSymbol SBinaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBinaryOperatorSymbol SBinaryOperatorSymbol = Just (Refl, HRefl)
decSymbolSing SCastExpressionSymbol SCastExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SIdentifiedExpressionSymbol SIdentifiedExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBlockIdentifierSymbol SBlockIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaExpressionSymbol SLambdaExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaBindingsSymbol SLambdaBindingsSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaBindingSymbol SLambdaBindingSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaBindingInternal0Symbol SLambdaBindingInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SLambdaBindingsInternal0Symbol SLambdaBindingsInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SLambdaExpressionInternal0Symbol SLambdaExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SLoopExpressionSymbol SLoopExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierExpressionSymbol SQuantifierExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierBindingsSymbol SQuantifierBindingsSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierBindingSymbol SQuantifierBindingSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierExpressionInternal0Symbol SQuantifierExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenExistsSymbol SHiddenExistsSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenForallSymbol SHiddenForallSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierExpressionInternal1Symbol SQuantifierExpressionInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SReturnExpressionSymbol SReturnExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SWhileExpressionSymbol SWhileExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBlockItemSymbol SBlockItemSymbol = Just (Refl, HRefl)
decSymbolSing SBlockItemInternal0Symbol SBlockItemInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SLetStatementSymbol SLetStatementSymbol = Just (Refl, HRefl)
decSymbolSing SLetStatementInternal0Symbol SLetStatementInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SLetStatementInternal1Symbol SLetStatementInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SMacroFunctionDefinitionSymbol SMacroFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenMacroSignatureSymbol SHiddenMacroSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SNativeFunctionDefinitionSymbol SNativeFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructItemSymbol SHiddenStructItemSymbol = Just (Refl, HRefl)
decSymbolSing SNativeStructDefinitionSymbol SNativeStructDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructSignatureSymbol SHiddenStructSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SStructDefinitionSymbol SStructDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SConstantSymbol SConstantSymbol = Just (Refl, HRefl)
decSymbolSing SConstantIdentifierSymbol SConstantIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SFriendDeclarationSymbol SFriendDeclarationSymbol = Just (Refl, HRefl)
decSymbolSing SFriendAccessSymbol SFriendAccessSymbol = Just (Refl, HRefl)
decSymbolSing SModuleBodyInternal2Symbol SModuleBodyInternal2Symbol = Just (Refl, HRefl)
decSymbolSing SBangTokSymbol SBangTokSymbol = Just (Refl, HRefl)
decSymbolSing SNeqTokSymbol SNeqTokSymbol = Just (Refl, HRefl)
decSymbolSing SDollarTokSymbol SDollarTokSymbol = Just (Refl, HRefl)
decSymbolSing SModTokSymbol SModTokSymbol = Just (Refl, HRefl)
decSymbolSing SBitandTokSymbol SBitandTokSymbol = Just (Refl, HRefl)
decSymbolSing SAndTokSymbol SAndTokSymbol = Just (Refl, HRefl)
decSymbolSing SMulTokSymbol SMulTokSymbol = Just (Refl, HRefl)
decSymbolSing SAddTokSymbol SAddTokSymbol = Just (Refl, HRefl)
decSymbolSing SSubTokSymbol SSubTokSymbol = Just (Refl, HRefl)
decSymbolSing SRangeTokSymbol SRangeTokSymbol = Just (Refl, HRefl)
decSymbolSing SDivTokSymbol SDivTokSymbol = Just (Refl, HRefl)
decSymbolSing SLtTokSymbol SLtTokSymbol = Just (Refl, HRefl)
decSymbolSing SShlTokSymbol SShlTokSymbol = Just (Refl, HRefl)
decSymbolSing SLeTokSymbol SLeTokSymbol = Just (Refl, HRefl)
decSymbolSing SAssignTokSymbol SAssignTokSymbol = Just (Refl, HRefl)
decSymbolSing SEqTokSymbol SEqTokSymbol = Just (Refl, HRefl)
decSymbolSing SImpliesTokSymbol SImpliesTokSymbol = Just (Refl, HRefl)
decSymbolSing SGtTokSymbol SGtTokSymbol = Just (Refl, HRefl)
decSymbolSing SGeTokSymbol SGeTokSymbol = Just (Refl, HRefl)
decSymbolSing SShrTokSymbol SShrTokSymbol = Just (Refl, HRefl)
decSymbolSing SXorTokSymbol SXorTokSymbol = Just (Refl, HRefl)
decSymbolSing SAbortsWithTokSymbol SAbortsWithTokSymbol = Just (Refl, HRefl)
decSymbolSing SAddressTokSymbol SAddressTokSymbol = Just (Refl, HRefl)
decSymbolSing SAssertTokSymbol SAssertTokSymbol = Just (Refl, HRefl)
decSymbolSing SAssumeTokSymbol SAssumeTokSymbol = Just (Refl, HRefl)
decSymbolSing SBoolTokSymbol SBoolTokSymbol = Just (Refl, HRefl)
decSymbolSing SBytearrayTokSymbol SBytearrayTokSymbol = Just (Refl, HRefl)
decSymbolSing SCopyTokSymbol SCopyTokSymbol = Just (Refl, HRefl)
decSymbolSing SDecreasesTokSymbol SDecreasesTokSymbol = Just (Refl, HRefl)
decSymbolSing SDropTokSymbol SDropTokSymbol = Just (Refl, HRefl)
decSymbolSing SEnsuresTokSymbol SEnsuresTokSymbol = Just (Refl, HRefl)
decSymbolSing SEntryTokSymbol SEntryTokSymbol = Just (Refl, HRefl)
decSymbolSing SFalseTokSymbol SFalseTokSymbol = Just (Refl, HRefl)
decSymbolSing SFriendTokSymbol SFriendTokSymbol = Just (Refl, HRefl)
decSymbolSing SGlobalTokSymbol SGlobalTokSymbol = Just (Refl, HRefl)
decSymbolSing SInternalTokSymbol SInternalTokSymbol = Just (Refl, HRefl)
decSymbolSing SKeyTokSymbol SKeyTokSymbol = Just (Refl, HRefl)
decSymbolSing SLocalTokSymbol SLocalTokSymbol = Just (Refl, HRefl)
decSymbolSing SModifiesTokSymbol SModifiesTokSymbol = Just (Refl, HRefl)
decSymbolSing SModuleTokSymbol SModuleTokSymbol = Just (Refl, HRefl)
decSymbolSing SMoveTokSymbol SMoveTokSymbol = Just (Refl, HRefl)
decSymbolSing SNativeTokSymbol SNativeTokSymbol = Just (Refl, HRefl)
decSymbolSing SPackTokSymbol SPackTokSymbol = Just (Refl, HRefl)
decSymbolSing SPackageTokSymbol SPackageTokSymbol = Just (Refl, HRefl)
decSymbolSing SPhantomTokSymbol SPhantomTokSymbol = Just (Refl, HRefl)
decSymbolSing SPostTokSymbol SPostTokSymbol = Just (Refl, HRefl)
decSymbolSing SPublicTokSymbol SPublicTokSymbol = Just (Refl, HRefl)
decSymbolSing SSignerTokSymbol SSignerTokSymbol = Just (Refl, HRefl)
decSymbolSing SStoreTokSymbol SStoreTokSymbol = Just (Refl, HRefl)
decSymbolSing SSucceedsIfTokSymbol SSucceedsIfTokSymbol = Just (Refl, HRefl)
decSymbolSing STrueTokSymbol STrueTokSymbol = Just (Refl, HRefl)
decSymbolSing SU128TokSymbol SU128TokSymbol = Just (Refl, HRefl)
decSymbolSing SU16TokSymbol SU16TokSymbol = Just (Refl, HRefl)
decSymbolSing SU256TokSymbol SU256TokSymbol = Just (Refl, HRefl)
decSymbolSing SU32TokSymbol SU32TokSymbol = Just (Refl, HRefl)
decSymbolSing SU64TokSymbol SU64TokSymbol = Just (Refl, HRefl)
decSymbolSing SU8TokSymbol SU8TokSymbol = Just (Refl, HRefl)
decSymbolSing SUnpackTokSymbol SUnpackTokSymbol = Just (Refl, HRefl)
decSymbolSing SUpdateTokSymbol SUpdateTokSymbol = Just (Refl, HRefl)
decSymbolSing SBitorTokSymbol SBitorTokSymbol = Just (Refl, HRefl)
decSymbolSing SOrTokSymbol SOrTokSymbol = Just (Refl, HRefl)
decSymbolSing SErrorSymbol SErrorSymbol = Just (Refl, HRefl)
decSymbolSing SMissingSymbol SMissingSymbol = Just (Refl, HRefl)
decSymbolSing SSortMismatchSymbol SSortMismatchSymbol = Just (Refl, HRefl)
decSymbolSing _ _ = Nothing

data SomeSymbolSing
  = forall (symbolType :: SymbolType) (symbol :: Symbol symbolType).
    SomeSymbolSing !(IsReal symbolType) !(SymbolSing symbolType symbol)

instance Eq SomeSymbolSing where
  (==) :: SomeSymbolSing -> SomeSymbolSing -> Bool
  SomeSymbolSing _isReal1 symbolSing1 == SomeSymbolSing _isReal2 symbolSing2 =
    isJust (decSymbolSing symbolSing1 symbolSing2)

deriving instance Show SomeSymbolSing

pattern SomeRegularSymbolSing :: () => (symbolType ~ Regular) => SymbolSing symbolType symbol -> SomeSymbolSing
pattern SomeRegularSymbolSing symbolSing = SomeSymbolSing RegularIsReal symbolSing

pattern SomeAnonymousSymbolSing :: () => (symbolType ~ Anonymous) => SymbolSing symbolType symbol -> SomeSymbolSing
pattern SomeAnonymousSymbolSing symbolSing = SomeSymbolSing AnonymousIsReal symbolSing

pattern SomeAuxiliarySymbolSing :: () => (symbolType ~ Auxiliary) => SymbolSing symbolType symbol -> SomeSymbolSing
pattern SomeAuxiliarySymbolSing symbolSing = SomeSymbolSing AuxiliaryIsReal symbolSing

--------------------------------------------------------------------------------
-- Symbol Table
--------------------------------------------------------------------------------

newtype SymbolTable = SymbolTable {unSymbolTable :: IntMap SomeSymbolSing}
  deriving Show

symbolMap :: Map String SomeSymbolSing
symbolMap = Map.fromList
    [ ("source_file", SomeRegularSymbolSing SSourceFileSymbol)
    , ("module_definition", SomeRegularSymbolSing SModuleDefinitionSymbol)
    , ("module_body", SomeRegularSymbolSing SModuleBodySymbol)
    , ("module_body_internal0", SomeRegularSymbolSing SModuleBodyInternal0Symbol)
    , ("module_body_internal1", SomeRegularSymbolSing SModuleBodyInternal1Symbol)
    , ("_enum_item", SomeRegularSymbolSing SHiddenEnumItemSymbol)
    , ("enum_definition", SomeRegularSymbolSing SEnumDefinitionSymbol)
    , ("_enum_signature", SomeRegularSymbolSing SHiddenEnumSignatureSymbol)
    , ("_enum_identifier", SomeRegularSymbolSing SHiddenEnumIdentifierSymbol)
    , ("enum_identifier", SomeRegularSymbolSing SEnumIdentifierSymbol)
    , ("_enum_signature_internal0", SomeRegularSymbolSing SHiddenEnumSignatureInternal0Symbol)
    , ("type_parameters", SomeRegularSymbolSing STypeParametersSymbol)
    , ("_spec_abort_with_or_modifies_internal1", SomeRegularSymbolSing SHiddenSpecAbortWithOrModifiesInternal1Symbol)
    , ("type_parameter", SomeRegularSymbolSing STypeParameterSymbol)
    , ("_type_parameter_identifier", SomeRegularSymbolSing SHiddenTypeParameterIdentifierSymbol)
    , ("type_parameter_identifier", SomeRegularSymbolSing STypeParameterIdentifierSymbol)
    , ("type_parameter_internal0", SomeRegularSymbolSing STypeParameterInternal0Symbol)
    , ("type_parameter_internal1", SomeRegularSymbolSing STypeParameterInternal1Symbol)
    , ("type_parameter_internal3", SomeRegularSymbolSing STypeParameterInternal3Symbol)
    , ("ability", SomeRegularSymbolSing SAbilitySymbol)
    , ("type_parameter_internal2", SomeRegularSymbolSing STypeParameterInternal2Symbol)
    , ("_enum_signature_internal1", SomeRegularSymbolSing SHiddenEnumSignatureInternal1Symbol)
    , ("ability_decls", SomeRegularSymbolSing SAbilityDeclsSymbol)
    , ("ability_decls_internal0", SomeRegularSymbolSing SAbilityDeclsInternal0Symbol)
    , ("enum_definition_internal0", SomeRegularSymbolSing SEnumDefinitionInternal0Symbol)
    , ("enum_definition_internal1", SomeRegularSymbolSing SEnumDefinitionInternal1Symbol)
    , ("postfix_ability_decls", SomeRegularSymbolSing SPostfixAbilityDeclsSymbol)
    , ("enum_variants", SomeRegularSymbolSing SEnumVariantsSymbol)
    , ("enum_variants_internal0", SomeRegularSymbolSing SEnumVariantsInternal0Symbol)
    , ("variant", SomeRegularSymbolSing SVariantSymbol)
    , ("_variant_identifier", SomeRegularSymbolSing SHiddenVariantIdentifierSymbol)
    , ("variant_identifier", SomeRegularSymbolSing SVariantIdentifierSymbol)
    , ("variant_internal0", SomeRegularSymbolSing SVariantInternal0Symbol)
    , ("datatype_fields", SomeRegularSymbolSing SDatatypeFieldsSymbol)
    , ("named_fields", SomeRegularSymbolSing SNamedFieldsSymbol)
    , ("field_annotation", SomeRegularSymbolSing SFieldAnnotationSymbol)
    , ("_field_identifier", SomeRegularSymbolSing SHiddenFieldIdentifierSymbol)
    , ("field_identifier", SomeRegularSymbolSing SFieldIdentifierSymbol)
    , ("_type", SomeRegularSymbolSing SHiddenTypeSymbol)
    , ("apply_type", SomeRegularSymbolSing SApplyTypeSymbol)
    , ("apply_type_internal0", SomeRegularSymbolSing SApplyTypeInternal0Symbol)
    , ("type_arguments", SomeRegularSymbolSing STypeArgumentsSymbol)
    , ("module_access", SomeRegularSymbolSing SModuleAccessSymbol)
    , ("_module_identifier", SomeRegularSymbolSing SHiddenModuleIdentifierSymbol)
    , ("module_identifier", SomeRegularSymbolSing SModuleIdentifierSymbol)
    , ("identifier", SomeRegularSymbolSing SIdentifierSymbol)
    , ("module_identity", SomeRegularSymbolSing SModuleIdentitySymbol)
    , ("module_identity_address_internal0", SomeRegularSymbolSing SModuleIdentityAddressInternal0Symbol)
    , ("num_literal", SomeRegularSymbolSing SNumLiteralSymbol)
    , ("num_literal_internal0", SomeRegularSymbolSing SNumLiteralInternal0Symbol)
    , ("num_literal_internal2", SomeRegularSymbolSing SNumLiteralInternal2Symbol)
    , ("num_literal_internal1", SomeRegularSymbolSing SNumLiteralInternal1Symbol)
    , ("function_type", SomeRegularSymbolSing SFunctionTypeSymbol)
    , ("function_type_internal0", SomeRegularSymbolSing SFunctionTypeInternal0Symbol)
    , ("function_type_parameters", SomeRegularSymbolSing SFunctionTypeParametersSymbol)
    , ("function_type_parameters_internal0", SomeRegularSymbolSing SFunctionTypeParametersInternal0Symbol)
    , ("primitive_type", SomeRegularSymbolSing SPrimitiveTypeSymbol)
    , ("ref_type", SomeRegularSymbolSing SRefTypeSymbol)
    , ("_reference", SomeRegularSymbolSing SHiddenReferenceSymbol)
    , ("imm_ref", SomeRegularSymbolSing SImmRefSymbol)
    , ("mut_ref", SomeRegularSymbolSing SMutRefSymbol)
    , ("tuple_type", SomeRegularSymbolSing STupleTypeSymbol)
    , ("named_fields_internal0", SomeRegularSymbolSing SNamedFieldsInternal0Symbol)
    , ("positional_fields", SomeRegularSymbolSing SPositionalFieldsSymbol)
    , ("_function_item", SomeRegularSymbolSing SHiddenFunctionItemSymbol)
    , ("function_definition", SomeRegularSymbolSing SFunctionDefinitionSymbol)
    , ("_function_signature", SomeRegularSymbolSing SHiddenFunctionSignatureSymbol)
    , ("_function_identifier", SomeRegularSymbolSing SHiddenFunctionIdentifierSymbol)
    , ("function_identifier", SomeRegularSymbolSing SFunctionIdentifierSymbol)
    , ("_function_signature_internal0", SomeRegularSymbolSing SHiddenFunctionSignatureInternal0Symbol)
    , ("modifier", SomeRegularSymbolSing SModifierSymbol)
    , ("modifier_internal1", SomeRegularSymbolSing SModifierInternal1Symbol)
    , ("modifier_internal0", SomeRegularSymbolSing SModifierInternal0Symbol)
    , ("_function_signature_internal1", SomeRegularSymbolSing SHiddenFunctionSignatureInternal1Symbol)
    , ("ret_type", SomeRegularSymbolSing SRetTypeSymbol)
    , ("function_parameters", SomeRegularSymbolSing SFunctionParametersSymbol)
    , ("function_parameters_internal0", SomeRegularSymbolSing SFunctionParametersInternal0Symbol)
    , ("function_parameter", SomeRegularSymbolSing SFunctionParameterSymbol)
    , ("function_parameter_internal0", SomeRegularSymbolSing SFunctionParameterInternal0Symbol)
    , ("_variable_identifier", SomeRegularSymbolSing SHiddenVariableIdentifierSymbol)
    , ("variable_identifier", SomeRegularSymbolSing SVariableIdentifierSymbol)
    , ("mut_function_parameter", SomeRegularSymbolSing SMutFunctionParameterSymbol)
    , ("function_parameters_internal1", SomeRegularSymbolSing SFunctionParametersInternal1Symbol)
    , ("block", SomeRegularSymbolSing SBlockSymbol)
    , ("arg_list_internal0", SomeRegularSymbolSing SArgListInternal0Symbol)
    , ("_expression", SomeRegularSymbolSing SHiddenExpressionSymbol)
    , ("_unary_expression", SomeRegularSymbolSing SHiddenUnaryExpressionSymbol)
    , ("_unary_expression_internal0", SomeRegularSymbolSing SHiddenUnaryExpressionInternal0Symbol)
    , ("_expression_term", SomeRegularSymbolSing SHiddenExpressionTermSymbol)
    , ("_literal_value", SomeRegularSymbolSing SHiddenLiteralValueSymbol)
    , ("address_literal", SomeRegularSymbolSing SAddressLiteralSymbol)
    , ("bool_literal", SomeRegularSymbolSing SBoolLiteralSymbol)
    , ("byte_string_literal", SomeRegularSymbolSing SByteStringLiteralSymbol)
    , ("hex_string_literal", SomeRegularSymbolSing SHexStringLiteralSymbol)
    , ("annotation_expression", SomeRegularSymbolSing SAnnotationExpressionSymbol)
    , ("break_expression", SomeRegularSymbolSing SBreakExpressionSymbol)
    , ("break_expression_internal0", SomeRegularSymbolSing SBreakExpressionInternal0Symbol)
    , ("label", SomeRegularSymbolSing SLabelSymbol)
    , ("break_expression_internal1", SomeRegularSymbolSing SBreakExpressionInternal1Symbol)
    , ("call_expression", SomeRegularSymbolSing SCallExpressionSymbol)
    , ("arg_list", SomeRegularSymbolSing SArgListSymbol)
    , ("name_expression", SomeRegularSymbolSing SNameExpressionSymbol)
    , ("name_expression_internal0", SomeRegularSymbolSing SNameExpressionInternal0Symbol)
    , ("continue_expression", SomeRegularSymbolSing SContinueExpressionSymbol)
    , ("dot_expression", SomeRegularSymbolSing SDotExpressionSymbol)
    , ("expression_list", SomeRegularSymbolSing SExpressionListSymbol)
    , ("if_expression", SomeRegularSymbolSing SIfExpressionSymbol)
    , ("if_expression_internal0", SomeRegularSymbolSing SIfExpressionInternal0Symbol)
    , ("index_expression", SomeRegularSymbolSing SIndexExpressionSymbol)
    , ("index_expression_internal0", SomeRegularSymbolSing SIndexExpressionInternal0Symbol)
    , ("macro_call_expression", SomeRegularSymbolSing SMacroCallExpressionSymbol)
    , ("macro_module_access", SomeRegularSymbolSing SMacroModuleAccessSymbol)
    , ("match_expression", SomeRegularSymbolSing SMatchExpressionSymbol)
    , ("_match_body", SomeRegularSymbolSing SHiddenMatchBodySymbol)
    , ("_match_body_internal0", SomeRegularSymbolSing SHiddenMatchBodyInternal0Symbol)
    , ("match_arm", SomeRegularSymbolSing SMatchArmSymbol)
    , ("bind_list", SomeRegularSymbolSing SBindListSymbol)
    , ("_bind", SomeRegularSymbolSing SHiddenBindSymbol)
    , ("_bind_internal0", SomeRegularSymbolSing SHiddenBindInternal0Symbol)
    , ("mut_bind_var", SomeRegularSymbolSing SMutBindVarSymbol)
    , ("bind_var", SomeRegularSymbolSing SBindVarSymbol)
    , ("at_bind", SomeRegularSymbolSing SAtBindSymbol)
    , ("bind_unpack", SomeRegularSymbolSing SBindUnpackSymbol)
    , ("bind_unpack_internal0", SomeRegularSymbolSing SBindUnpackInternal0Symbol)
    , ("bind_fields", SomeRegularSymbolSing SBindFieldsSymbol)
    , ("bind_named_fields", SomeRegularSymbolSing SBindNamedFieldsSymbol)
    , ("bind_named_fields_internal0", SomeRegularSymbolSing SBindNamedFieldsInternal0Symbol)
    , ("bind_field", SomeRegularSymbolSing SBindFieldSymbol)
    , ("_spread_operator", SomeRegularSymbolSing SHiddenSpreadOperatorSymbol)
    , ("bind_field_internal0", SomeRegularSymbolSing SBindFieldInternal0Symbol)
    , ("mut_bind_field", SomeRegularSymbolSing SMutBindFieldSymbol)
    , ("bind_named_fields_internal1", SomeRegularSymbolSing SBindNamedFieldsInternal1Symbol)
    , ("bind_positional_fields", SomeRegularSymbolSing SBindPositionalFieldsSymbol)
    , ("comma_bind_list", SomeRegularSymbolSing SCommaBindListSymbol)
    , ("comma_bind_list_internal0", SomeRegularSymbolSing SCommaBindListInternal0Symbol)
    , ("or_bind_list", SomeRegularSymbolSing SOrBindListSymbol)
    , ("or_bind_list_internal0", SomeRegularSymbolSing SOrBindListInternal0Symbol)
    , ("or_bind_list_internal1", SomeRegularSymbolSing SOrBindListInternal1Symbol)
    , ("or_bind_list_internal2", SomeRegularSymbolSing SOrBindListInternal2Symbol)
    , ("match_arm_internal0", SomeRegularSymbolSing SMatchArmInternal0Symbol)
    , ("match_condition", SomeRegularSymbolSing SMatchConditionSymbol)
    , ("pack_expression", SomeRegularSymbolSing SPackExpressionSymbol)
    , ("field_initialize_list", SomeRegularSymbolSing SFieldInitializeListSymbol)
    , ("exp_field", SomeRegularSymbolSing SExpFieldSymbol)
    , ("exp_field_internal0", SomeRegularSymbolSing SExpFieldInternal0Symbol)
    , ("field_initialize_list_internal0", SomeRegularSymbolSing SFieldInitializeListInternal0Symbol)
    , ("spec_block", SomeRegularSymbolSing SSpecBlockSymbol)
    , ("spec_block_internal1", SomeRegularSymbolSing SSpecBlockInternal1Symbol)
    , ("_spec_function", SomeRegularSymbolSing SHiddenSpecFunctionSymbol)
    , ("native_spec_function", SomeRegularSymbolSing SNativeSpecFunctionSymbol)
    , ("_spec_function_signature", SomeRegularSymbolSing SHiddenSpecFunctionSignatureSymbol)
    , ("uninterpreted_spec_function", SomeRegularSymbolSing SUninterpretedSpecFunctionSymbol)
    , ("usual_spec_function", SomeRegularSymbolSing SUsualSpecFunctionSymbol)
    , ("spec_block_internal0", SomeRegularSymbolSing SSpecBlockInternal0Symbol)
    , ("_spec_block_target", SomeRegularSymbolSing SHiddenSpecBlockTargetSymbol)
    , ("spec_block_target_schema", SomeRegularSymbolSing SSpecBlockTargetSchemaSymbol)
    , ("_struct_identifier", SomeRegularSymbolSing SHiddenStructIdentifierSymbol)
    , ("struct_identifier", SomeRegularSymbolSing SStructIdentifierSymbol)
    , ("spec_body", SomeRegularSymbolSing SSpecBodySymbol)
    , ("_spec_block_memeber", SomeRegularSymbolSing SHiddenSpecBlockMemeberSymbol)
    , ("spec_apply", SomeRegularSymbolSing SSpecApplySymbol)
    , ("spec_apply_internal0", SomeRegularSymbolSing SSpecApplyInternal0Symbol)
    , ("spec_apply_pattern", SomeRegularSymbolSing SSpecApplyPatternSymbol)
    , ("spec_apply_name_pattern", SomeRegularSymbolSing SSpecApplyNamePatternSymbol)
    , ("spec_apply_pattern_internal1", SomeRegularSymbolSing SSpecApplyPatternInternal1Symbol)
    , ("spec_apply_pattern_internal0", SomeRegularSymbolSing SSpecApplyPatternInternal0Symbol)
    , ("spec_condition", SomeRegularSymbolSing SSpecConditionSymbol)
    , ("_spec_abort_if", SomeRegularSymbolSing SHiddenSpecAbortIfSymbol)
    , ("_spec_abort_if_internal0", SomeRegularSymbolSing SHiddenSpecAbortIfInternal0Symbol)
    , ("condition_properties", SomeRegularSymbolSing SConditionPropertiesSymbol)
    , ("condition_properties_internal0", SomeRegularSymbolSing SConditionPropertiesInternal0Symbol)
    , ("spec_property", SomeRegularSymbolSing SSpecPropertySymbol)
    , ("spec_property_internal0", SomeRegularSymbolSing SSpecPropertyInternal0Symbol)
    , ("_spec_abort_if_internal1", SomeRegularSymbolSing SHiddenSpecAbortIfInternal1Symbol)
    , ("condition_kind", SomeRegularSymbolSing SConditionKindSymbol)
    , ("_spec_abort_with_or_modifies", SomeRegularSymbolSing SHiddenSpecAbortWithOrModifiesSymbol)
    , ("_spec_condition", SomeRegularSymbolSing SHiddenSpecConditionSymbol)
    , ("_spec_condition_internal1", SomeRegularSymbolSing SHiddenSpecConditionInternal1Symbol)
    , ("_spec_condition_internal0", SomeRegularSymbolSing SHiddenSpecConditionInternal0Symbol)
    , ("spec_include", SomeRegularSymbolSing SSpecIncludeSymbol)
    , ("spec_invariant", SomeRegularSymbolSing SSpecInvariantSymbol)
    , ("spec_invariant_internal1", SomeRegularSymbolSing SSpecInvariantInternal1Symbol)
    , ("invariant_modifier", SomeRegularSymbolSing SInvariantModifierSymbol)
    , ("spec_let", SomeRegularSymbolSing SSpecLetSymbol)
    , ("spec_let_internal0", SomeRegularSymbolSing SSpecLetInternal0Symbol)
    , ("spec_pragma", SomeRegularSymbolSing SSpecPragmaSymbol)
    , ("spec_variable", SomeRegularSymbolSing SSpecVariableSymbol)
    , ("spec_variable_internal1", SomeRegularSymbolSing SSpecVariableInternal1Symbol)
    , ("spec_variable_internal0", SomeRegularSymbolSing SSpecVariableInternal0Symbol)
    , ("use_declaration", SomeRegularSymbolSing SUseDeclarationSymbol)
    , ("use_declaration_internal0", SomeRegularSymbolSing SUseDeclarationInternal0Symbol)
    , ("use_fun", SomeRegularSymbolSing SUseFunSymbol)
    , ("use_module", SomeRegularSymbolSing SUseModuleSymbol)
    , ("use_module_internal0", SomeRegularSymbolSing SUseModuleInternal0Symbol)
    , ("use_module_member", SomeRegularSymbolSing SUseModuleMemberSymbol)
    , ("use_member", SomeRegularSymbolSing SUseMemberSymbol)
    , ("use_member_internal0", SomeRegularSymbolSing SUseMemberInternal0Symbol)
    , ("use_module_members", SomeRegularSymbolSing SUseModuleMembersSymbol)
    , ("unit_expression", SomeRegularSymbolSing SUnitExpressionSymbol)
    , ("vector_expression", SomeRegularSymbolSing SVectorExpressionSymbol)
    , ("vector_expression_internal0", SomeRegularSymbolSing SVectorExpressionInternal0Symbol)
    , ("borrow_expression", SomeRegularSymbolSing SBorrowExpressionSymbol)
    , ("dereference_expression", SomeRegularSymbolSing SDereferenceExpressionSymbol)
    , ("move_or_copy_expression", SomeRegularSymbolSing SMoveOrCopyExpressionSymbol)
    , ("move_or_copy_expression_internal0", SomeRegularSymbolSing SMoveOrCopyExpressionInternal0Symbol)
    , ("unary_expression", SomeRegularSymbolSing SUnaryExpressionSymbol)
    , ("unary_op", SomeRegularSymbolSing SUnaryOpSymbol)
    , ("abort_expression", SomeRegularSymbolSing SAbortExpressionSymbol)
    , ("abort_expression_internal0", SomeRegularSymbolSing SAbortExpressionInternal0Symbol)
    , ("assign_expression", SomeRegularSymbolSing SAssignExpressionSymbol)
    , ("binary_expression", SomeRegularSymbolSing SBinaryExpressionSymbol)
    , ("binary_operator", SomeRegularSymbolSing SBinaryOperatorSymbol)
    , ("cast_expression", SomeRegularSymbolSing SCastExpressionSymbol)
    , ("identified_expression", SomeRegularSymbolSing SIdentifiedExpressionSymbol)
    , ("block_identifier", SomeRegularSymbolSing SBlockIdentifierSymbol)
    , ("lambda_expression", SomeRegularSymbolSing SLambdaExpressionSymbol)
    , ("lambda_bindings", SomeRegularSymbolSing SLambdaBindingsSymbol)
    , ("lambda_binding", SomeRegularSymbolSing SLambdaBindingSymbol)
    , ("lambda_binding_internal0", SomeRegularSymbolSing SLambdaBindingInternal0Symbol)
    , ("lambda_bindings_internal0", SomeRegularSymbolSing SLambdaBindingsInternal0Symbol)
    , ("lambda_expression_internal0", SomeRegularSymbolSing SLambdaExpressionInternal0Symbol)
    , ("loop_expression", SomeRegularSymbolSing SLoopExpressionSymbol)
    , ("quantifier_expression", SomeRegularSymbolSing SQuantifierExpressionSymbol)
    , ("quantifier_bindings", SomeRegularSymbolSing SQuantifierBindingsSymbol)
    , ("quantifier_binding", SomeRegularSymbolSing SQuantifierBindingSymbol)
    , ("quantifier_expression_internal0", SomeRegularSymbolSing SQuantifierExpressionInternal0Symbol)
    , ("_exists", SomeRegularSymbolSing SHiddenExistsSymbol)
    , ("_forall", SomeRegularSymbolSing SHiddenForallSymbol)
    , ("quantifier_expression_internal1", SomeRegularSymbolSing SQuantifierExpressionInternal1Symbol)
    , ("return_expression", SomeRegularSymbolSing SReturnExpressionSymbol)
    , ("while_expression", SomeRegularSymbolSing SWhileExpressionSymbol)
    , ("block_item", SomeRegularSymbolSing SBlockItemSymbol)
    , ("block_item_internal0", SomeRegularSymbolSing SBlockItemInternal0Symbol)
    , ("let_statement", SomeRegularSymbolSing SLetStatementSymbol)
    , ("let_statement_internal0", SomeRegularSymbolSing SLetStatementInternal0Symbol)
    , ("let_statement_internal1", SomeRegularSymbolSing SLetStatementInternal1Symbol)
    , ("macro_function_definition", SomeRegularSymbolSing SMacroFunctionDefinitionSymbol)
    , ("_macro_signature", SomeRegularSymbolSing SHiddenMacroSignatureSymbol)
    , ("native_function_definition", SomeRegularSymbolSing SNativeFunctionDefinitionSymbol)
    , ("_struct_item", SomeRegularSymbolSing SHiddenStructItemSymbol)
    , ("native_struct_definition", SomeRegularSymbolSing SNativeStructDefinitionSymbol)
    , ("_struct_signature", SomeRegularSymbolSing SHiddenStructSignatureSymbol)
    , ("struct_definition", SomeRegularSymbolSing SStructDefinitionSymbol)
    , ("constant", SomeRegularSymbolSing SConstantSymbol)
    , ("constant_identifier", SomeRegularSymbolSing SConstantIdentifierSymbol)
    , ("friend_declaration", SomeRegularSymbolSing SFriendDeclarationSymbol)
    , ("friend_access", SomeRegularSymbolSing SFriendAccessSymbol)
    , ("module_body_internal2", SomeRegularSymbolSing SModuleBodyInternal2Symbol)
    , ("!", SomeAnonymousSymbolSing SBangTokSymbol)
    , ("!=", SomeAnonymousSymbolSing SNeqTokSymbol)
    , ("$", SomeAnonymousSymbolSing SDollarTokSymbol)
    , ("%", SomeAnonymousSymbolSing SModTokSymbol)
    , ("&", SomeAnonymousSymbolSing SBitandTokSymbol)
    , ("&&", SomeAnonymousSymbolSing SAndTokSymbol)
    , ("*", SomeAnonymousSymbolSing SMulTokSymbol)
    , ("+", SomeAnonymousSymbolSing SAddTokSymbol)
    , ("-", SomeAnonymousSymbolSing SSubTokSymbol)
    , ("..", SomeAnonymousSymbolSing SRangeTokSymbol)
    , ("/", SomeAnonymousSymbolSing SDivTokSymbol)
    , ("<", SomeAnonymousSymbolSing SLtTokSymbol)
    , ("<<", SomeAnonymousSymbolSing SShlTokSymbol)
    , ("<=", SomeAnonymousSymbolSing SLeTokSymbol)
    , ("=", SomeAnonymousSymbolSing SAssignTokSymbol)
    , ("==", SomeAnonymousSymbolSing SEqTokSymbol)
    , ("==>", SomeAnonymousSymbolSing SImpliesTokSymbol)
    , (">", SomeAnonymousSymbolSing SGtTokSymbol)
    , (">=", SomeAnonymousSymbolSing SGeTokSymbol)
    , (">>", SomeAnonymousSymbolSing SShrTokSymbol)
    , ("^", SomeAnonymousSymbolSing SXorTokSymbol)
    , ("aborts_with", SomeAnonymousSymbolSing SAbortsWithTokSymbol)
    , ("address", SomeAnonymousSymbolSing SAddressTokSymbol)
    , ("assert", SomeAnonymousSymbolSing SAssertTokSymbol)
    , ("assume", SomeAnonymousSymbolSing SAssumeTokSymbol)
    , ("bool", SomeAnonymousSymbolSing SBoolTokSymbol)
    , ("bytearray", SomeAnonymousSymbolSing SBytearrayTokSymbol)
    , ("copy", SomeAnonymousSymbolSing SCopyTokSymbol)
    , ("decreases", SomeAnonymousSymbolSing SDecreasesTokSymbol)
    , ("drop", SomeAnonymousSymbolSing SDropTokSymbol)
    , ("ensures", SomeAnonymousSymbolSing SEnsuresTokSymbol)
    , ("entry", SomeAnonymousSymbolSing SEntryTokSymbol)
    , ("false", SomeAnonymousSymbolSing SFalseTokSymbol)
    , ("friend", SomeAnonymousSymbolSing SFriendTokSymbol)
    , ("global", SomeAnonymousSymbolSing SGlobalTokSymbol)
    , ("internal", SomeAnonymousSymbolSing SInternalTokSymbol)
    , ("key", SomeAnonymousSymbolSing SKeyTokSymbol)
    , ("local", SomeAnonymousSymbolSing SLocalTokSymbol)
    , ("modifies", SomeAnonymousSymbolSing SModifiesTokSymbol)
    , ("module", SomeAnonymousSymbolSing SModuleTokSymbol)
    , ("move", SomeAnonymousSymbolSing SMoveTokSymbol)
    , ("native", SomeAnonymousSymbolSing SNativeTokSymbol)
    , ("pack", SomeAnonymousSymbolSing SPackTokSymbol)
    , ("package", SomeAnonymousSymbolSing SPackageTokSymbol)
    , ("phantom", SomeAnonymousSymbolSing SPhantomTokSymbol)
    , ("post", SomeAnonymousSymbolSing SPostTokSymbol)
    , ("public", SomeAnonymousSymbolSing SPublicTokSymbol)
    , ("signer", SomeAnonymousSymbolSing SSignerTokSymbol)
    , ("store", SomeAnonymousSymbolSing SStoreTokSymbol)
    , ("succeeds_if", SomeAnonymousSymbolSing SSucceedsIfTokSymbol)
    , ("true", SomeAnonymousSymbolSing STrueTokSymbol)
    , ("u128", SomeAnonymousSymbolSing SU128TokSymbol)
    , ("u16", SomeAnonymousSymbolSing SU16TokSymbol)
    , ("u256", SomeAnonymousSymbolSing SU256TokSymbol)
    , ("u32", SomeAnonymousSymbolSing SU32TokSymbol)
    , ("u64", SomeAnonymousSymbolSing SU64TokSymbol)
    , ("u8", SomeAnonymousSymbolSing SU8TokSymbol)
    , ("unpack", SomeAnonymousSymbolSing SUnpackTokSymbol)
    , ("update", SomeAnonymousSymbolSing SUpdateTokSymbol)
    , ("|", SomeAnonymousSymbolSing SBitorTokSymbol)
    , ("||", SomeAnonymousSymbolSing SOrTokSymbol)
    ]

mkSymbolTable :: TS.Language -> IO SymbolTable
mkSymbolTable lang = do
  count <- fromIntegral <$> TS.languageSymbolCount lang
  SymbolTable <$> foldrM
    (\id acc -> do
      symName <- TS.languageSymbolName lang id
      let mSymSing = Map.lookup (BSC.unpack symName) symbolMap
      pure (maybe acc (flip (IM.insert (fromIntegral id)) acc) mSymSing)
    )
    (IM.empty :: IntMap SomeSymbolSing)
    [0..count-1]
