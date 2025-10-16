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
data HiddenEnumItemL
data EnumDefinitionL
data HiddenEnumSignatureL
data HiddenEnumIdentifierL
data EnumIdentifierL
data AbilityDeclsL
data AbilityL
data TypeParametersL
data TypeParameterL
data HiddenTypeParameterIdentifierL
data TypeParameterIdentifierL
data EnumVariantsL
data VariantL
data HiddenVariantIdentifierL
data VariantIdentifierL
data DatatypeFieldsL
data NamedFieldsL
data FieldAnnotationL
data HiddenFieldIdentifierL
data FieldIdentifierL
data HiddenTypeL
data ApplyTypeL
data ModuleAccessL
data HiddenModuleIdentifierL
data ModuleIdentifierL
data IdentifierL
data ModuleIdentityL
data NumLiteralL
data TypeArgumentsL
data FunctionTypeL
data FunctionTypeParametersL
data PrimitiveTypeL
data RefTypeL
data HiddenReferenceL
data ImmRefL
data MutRefL
data TupleTypeL
data PositionalFieldsL
data PostfixAbilityDeclsL
data HiddenFunctionItemL
data FunctionDefinitionL
data HiddenFunctionSignatureL
data HiddenFunctionIdentifierL
data FunctionIdentifierL
data FunctionParametersL
data FunctionParameterL
data HiddenVariableIdentifierL
data VariableIdentifierL
data MutFunctionParameterL
data ModifierL
data RetTypeL
data BlockL
data HiddenExpressionL
data HiddenUnaryExpressionL
data HiddenExpressionTermL
data HiddenLiteralValueL
data AddressLiteralL
data BoolLiteralL
data ByteStringLiteralL
data HexStringLiteralL
data AnnotationExpressionL
data BreakExpressionL
data LabelL
data CallExpressionL
data ArgListL
data NameExpressionL
data ContinueExpressionL
data DotExpressionL
data ExpressionListL
data IfExpressionL
data IndexExpressionL
data MacroCallExpressionL
data MacroModuleAccessL
data MatchExpressionL
data HiddenMatchBodyL
data MatchArmL
data BindListL
data HiddenBindL
data AtBindL
data BindUnpackL
data BindFieldsL
data BindNamedFieldsL
data BindFieldL
data HiddenSpreadOperatorL
data MutBindFieldL
data BindPositionalFieldsL
data MutBindVarL
data BindVarL
data CommaBindListL
data OrBindListL
data MatchConditionL
data PackExpressionL
data FieldInitializeListL
data ExpFieldL
data SpecBlockL
data HiddenSpecBlockTargetL
data SpecBlockTargetSchemaL
data HiddenStructIdentifierL
data StructIdentifierL
data HiddenSpecFunctionL
data NativeSpecFunctionL
data HiddenSpecFunctionSignatureL
data UninterpretedSpecFunctionL
data UsualSpecFunctionL
data SpecBodyL
data HiddenSpecBlockMemeberL
data SpecApplyL
data SpecApplyPatternL
data SpecApplyNamePatternL
data SpecConditionL
data HiddenSpecAbortIfL
data ConditionKindL
data ConditionPropertiesL
data SpecPropertyL
data HiddenSpecAbortWithOrModifiesL
data HiddenSpecConditionL
data SpecIncludeL
data SpecInvariantL
data InvariantModifierL
data SpecLetL
data SpecPragmaL
data SpecVariableL
data UseDeclarationL
data UseFunL
data UseModuleL
data UseModuleMemberL
data UseMemberL
data UseModuleMembersL
data UnitExpressionL
data VectorExpressionL
data BorrowExpressionL
data DereferenceExpressionL
data MoveOrCopyExpressionL
data UnaryExpressionL
data UnaryOpL
data AbortExpressionL
data AssignExpressionL
data BinaryExpressionL
data BinaryOperatorL
data CastExpressionL
data IdentifiedExpressionL
data BlockIdentifierL
data LambdaExpressionL
data LambdaBindingsL
data LambdaBindingL
data LoopExpressionL
data QuantifierExpressionL
data HiddenExistsL
data HiddenForallL
data QuantifierBindingsL
data QuantifierBindingL
data ReturnExpressionL
data WhileExpressionL
data BlockItemL
data LetStatementL
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
data FalseTokL
data FriendTokL
data GlobalTokL
data InternalTokL
data KeyTokL
data LocalTokL
data ModifiesTokL
data ModuleTokL
data MoveTokL
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
  SHiddenEnumItemL :: LabelSing HiddenEnumItemL
  SEnumDefinitionL :: LabelSing EnumDefinitionL
  SHiddenEnumSignatureL :: LabelSing HiddenEnumSignatureL
  SHiddenEnumIdentifierL :: LabelSing HiddenEnumIdentifierL
  SEnumIdentifierL :: LabelSing EnumIdentifierL
  SAbilityDeclsL :: LabelSing AbilityDeclsL
  SAbilityL :: LabelSing AbilityL
  STypeParametersL :: LabelSing TypeParametersL
  STypeParameterL :: LabelSing TypeParameterL
  SHiddenTypeParameterIdentifierL :: LabelSing HiddenTypeParameterIdentifierL
  STypeParameterIdentifierL :: LabelSing TypeParameterIdentifierL
  SEnumVariantsL :: LabelSing EnumVariantsL
  SVariantL :: LabelSing VariantL
  SHiddenVariantIdentifierL :: LabelSing HiddenVariantIdentifierL
  SVariantIdentifierL :: LabelSing VariantIdentifierL
  SDatatypeFieldsL :: LabelSing DatatypeFieldsL
  SNamedFieldsL :: LabelSing NamedFieldsL
  SFieldAnnotationL :: LabelSing FieldAnnotationL
  SHiddenFieldIdentifierL :: LabelSing HiddenFieldIdentifierL
  SFieldIdentifierL :: LabelSing FieldIdentifierL
  SHiddenTypeL :: LabelSing HiddenTypeL
  SApplyTypeL :: LabelSing ApplyTypeL
  SModuleAccessL :: LabelSing ModuleAccessL
  SHiddenModuleIdentifierL :: LabelSing HiddenModuleIdentifierL
  SModuleIdentifierL :: LabelSing ModuleIdentifierL
  SIdentifierL :: LabelSing IdentifierL
  SModuleIdentityL :: LabelSing ModuleIdentityL
  SNumLiteralL :: LabelSing NumLiteralL
  STypeArgumentsL :: LabelSing TypeArgumentsL
  SFunctionTypeL :: LabelSing FunctionTypeL
  SFunctionTypeParametersL :: LabelSing FunctionTypeParametersL
  SPrimitiveTypeL :: LabelSing PrimitiveTypeL
  SRefTypeL :: LabelSing RefTypeL
  SHiddenReferenceL :: LabelSing HiddenReferenceL
  SImmRefL :: LabelSing ImmRefL
  SMutRefL :: LabelSing MutRefL
  STupleTypeL :: LabelSing TupleTypeL
  SPositionalFieldsL :: LabelSing PositionalFieldsL
  SPostfixAbilityDeclsL :: LabelSing PostfixAbilityDeclsL
  SHiddenFunctionItemL :: LabelSing HiddenFunctionItemL
  SFunctionDefinitionL :: LabelSing FunctionDefinitionL
  SHiddenFunctionSignatureL :: LabelSing HiddenFunctionSignatureL
  SHiddenFunctionIdentifierL :: LabelSing HiddenFunctionIdentifierL
  SFunctionIdentifierL :: LabelSing FunctionIdentifierL
  SFunctionParametersL :: LabelSing FunctionParametersL
  SFunctionParameterL :: LabelSing FunctionParameterL
  SHiddenVariableIdentifierL :: LabelSing HiddenVariableIdentifierL
  SVariableIdentifierL :: LabelSing VariableIdentifierL
  SMutFunctionParameterL :: LabelSing MutFunctionParameterL
  SModifierL :: LabelSing ModifierL
  SRetTypeL :: LabelSing RetTypeL
  SBlockL :: LabelSing BlockL
  SHiddenExpressionL :: LabelSing HiddenExpressionL
  SHiddenUnaryExpressionL :: LabelSing HiddenUnaryExpressionL
  SHiddenExpressionTermL :: LabelSing HiddenExpressionTermL
  SHiddenLiteralValueL :: LabelSing HiddenLiteralValueL
  SAddressLiteralL :: LabelSing AddressLiteralL
  SBoolLiteralL :: LabelSing BoolLiteralL
  SByteStringLiteralL :: LabelSing ByteStringLiteralL
  SHexStringLiteralL :: LabelSing HexStringLiteralL
  SAnnotationExpressionL :: LabelSing AnnotationExpressionL
  SBreakExpressionL :: LabelSing BreakExpressionL
  SLabelL :: LabelSing LabelL
  SCallExpressionL :: LabelSing CallExpressionL
  SArgListL :: LabelSing ArgListL
  SNameExpressionL :: LabelSing NameExpressionL
  SContinueExpressionL :: LabelSing ContinueExpressionL
  SDotExpressionL :: LabelSing DotExpressionL
  SExpressionListL :: LabelSing ExpressionListL
  SIfExpressionL :: LabelSing IfExpressionL
  SIndexExpressionL :: LabelSing IndexExpressionL
  SMacroCallExpressionL :: LabelSing MacroCallExpressionL
  SMacroModuleAccessL :: LabelSing MacroModuleAccessL
  SMatchExpressionL :: LabelSing MatchExpressionL
  SHiddenMatchBodyL :: LabelSing HiddenMatchBodyL
  SMatchArmL :: LabelSing MatchArmL
  SBindListL :: LabelSing BindListL
  SHiddenBindL :: LabelSing HiddenBindL
  SAtBindL :: LabelSing AtBindL
  SBindUnpackL :: LabelSing BindUnpackL
  SBindFieldsL :: LabelSing BindFieldsL
  SBindNamedFieldsL :: LabelSing BindNamedFieldsL
  SBindFieldL :: LabelSing BindFieldL
  SHiddenSpreadOperatorL :: LabelSing HiddenSpreadOperatorL
  SMutBindFieldL :: LabelSing MutBindFieldL
  SBindPositionalFieldsL :: LabelSing BindPositionalFieldsL
  SMutBindVarL :: LabelSing MutBindVarL
  SBindVarL :: LabelSing BindVarL
  SCommaBindListL :: LabelSing CommaBindListL
  SOrBindListL :: LabelSing OrBindListL
  SMatchConditionL :: LabelSing MatchConditionL
  SPackExpressionL :: LabelSing PackExpressionL
  SFieldInitializeListL :: LabelSing FieldInitializeListL
  SExpFieldL :: LabelSing ExpFieldL
  SSpecBlockL :: LabelSing SpecBlockL
  SHiddenSpecBlockTargetL :: LabelSing HiddenSpecBlockTargetL
  SSpecBlockTargetSchemaL :: LabelSing SpecBlockTargetSchemaL
  SHiddenStructIdentifierL :: LabelSing HiddenStructIdentifierL
  SStructIdentifierL :: LabelSing StructIdentifierL
  SHiddenSpecFunctionL :: LabelSing HiddenSpecFunctionL
  SNativeSpecFunctionL :: LabelSing NativeSpecFunctionL
  SHiddenSpecFunctionSignatureL :: LabelSing HiddenSpecFunctionSignatureL
  SUninterpretedSpecFunctionL :: LabelSing UninterpretedSpecFunctionL
  SUsualSpecFunctionL :: LabelSing UsualSpecFunctionL
  SSpecBodyL :: LabelSing SpecBodyL
  SHiddenSpecBlockMemeberL :: LabelSing HiddenSpecBlockMemeberL
  SSpecApplyL :: LabelSing SpecApplyL
  SSpecApplyPatternL :: LabelSing SpecApplyPatternL
  SSpecApplyNamePatternL :: LabelSing SpecApplyNamePatternL
  SSpecConditionL :: LabelSing SpecConditionL
  SHiddenSpecAbortIfL :: LabelSing HiddenSpecAbortIfL
  SConditionKindL :: LabelSing ConditionKindL
  SConditionPropertiesL :: LabelSing ConditionPropertiesL
  SSpecPropertyL :: LabelSing SpecPropertyL
  SHiddenSpecAbortWithOrModifiesL :: LabelSing HiddenSpecAbortWithOrModifiesL
  SHiddenSpecConditionL :: LabelSing HiddenSpecConditionL
  SSpecIncludeL :: LabelSing SpecIncludeL
  SSpecInvariantL :: LabelSing SpecInvariantL
  SInvariantModifierL :: LabelSing InvariantModifierL
  SSpecLetL :: LabelSing SpecLetL
  SSpecPragmaL :: LabelSing SpecPragmaL
  SSpecVariableL :: LabelSing SpecVariableL
  SUseDeclarationL :: LabelSing UseDeclarationL
  SUseFunL :: LabelSing UseFunL
  SUseModuleL :: LabelSing UseModuleL
  SUseModuleMemberL :: LabelSing UseModuleMemberL
  SUseMemberL :: LabelSing UseMemberL
  SUseModuleMembersL :: LabelSing UseModuleMembersL
  SUnitExpressionL :: LabelSing UnitExpressionL
  SVectorExpressionL :: LabelSing VectorExpressionL
  SBorrowExpressionL :: LabelSing BorrowExpressionL
  SDereferenceExpressionL :: LabelSing DereferenceExpressionL
  SMoveOrCopyExpressionL :: LabelSing MoveOrCopyExpressionL
  SUnaryExpressionL :: LabelSing UnaryExpressionL
  SUnaryOpL :: LabelSing UnaryOpL
  SAbortExpressionL :: LabelSing AbortExpressionL
  SAssignExpressionL :: LabelSing AssignExpressionL
  SBinaryExpressionL :: LabelSing BinaryExpressionL
  SBinaryOperatorL :: LabelSing BinaryOperatorL
  SCastExpressionL :: LabelSing CastExpressionL
  SIdentifiedExpressionL :: LabelSing IdentifiedExpressionL
  SBlockIdentifierL :: LabelSing BlockIdentifierL
  SLambdaExpressionL :: LabelSing LambdaExpressionL
  SLambdaBindingsL :: LabelSing LambdaBindingsL
  SLambdaBindingL :: LabelSing LambdaBindingL
  SLoopExpressionL :: LabelSing LoopExpressionL
  SQuantifierExpressionL :: LabelSing QuantifierExpressionL
  SHiddenExistsL :: LabelSing HiddenExistsL
  SHiddenForallL :: LabelSing HiddenForallL
  SQuantifierBindingsL :: LabelSing QuantifierBindingsL
  SQuantifierBindingL :: LabelSing QuantifierBindingL
  SReturnExpressionL :: LabelSing ReturnExpressionL
  SWhileExpressionL :: LabelSing WhileExpressionL
  SBlockItemL :: LabelSing BlockItemL
  SLetStatementL :: LabelSing LetStatementL
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
  SFalseTokL :: LabelSing FalseTokL
  SFriendTokL :: LabelSing FriendTokL
  SGlobalTokL :: LabelSing GlobalTokL
  SInternalTokL :: LabelSing InternalTokL
  SKeyTokL :: LabelSing KeyTokL
  SLocalTokL :: LabelSing LocalTokL
  SModifiesTokL :: LabelSing ModifiesTokL
  SModuleTokL :: LabelSing ModuleTokL
  SMoveTokL :: LabelSing MoveTokL
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
decLabelSing SHiddenEnumItemL SHiddenEnumItemL = Just Refl
decLabelSing SEnumDefinitionL SEnumDefinitionL = Just Refl
decLabelSing SHiddenEnumSignatureL SHiddenEnumSignatureL = Just Refl
decLabelSing SHiddenEnumIdentifierL SHiddenEnumIdentifierL = Just Refl
decLabelSing SEnumIdentifierL SEnumIdentifierL = Just Refl
decLabelSing SAbilityDeclsL SAbilityDeclsL = Just Refl
decLabelSing SAbilityL SAbilityL = Just Refl
decLabelSing STypeParametersL STypeParametersL = Just Refl
decLabelSing STypeParameterL STypeParameterL = Just Refl
decLabelSing SHiddenTypeParameterIdentifierL SHiddenTypeParameterIdentifierL = Just Refl
decLabelSing STypeParameterIdentifierL STypeParameterIdentifierL = Just Refl
decLabelSing SEnumVariantsL SEnumVariantsL = Just Refl
decLabelSing SVariantL SVariantL = Just Refl
decLabelSing SHiddenVariantIdentifierL SHiddenVariantIdentifierL = Just Refl
decLabelSing SVariantIdentifierL SVariantIdentifierL = Just Refl
decLabelSing SDatatypeFieldsL SDatatypeFieldsL = Just Refl
decLabelSing SNamedFieldsL SNamedFieldsL = Just Refl
decLabelSing SFieldAnnotationL SFieldAnnotationL = Just Refl
decLabelSing SHiddenFieldIdentifierL SHiddenFieldIdentifierL = Just Refl
decLabelSing SFieldIdentifierL SFieldIdentifierL = Just Refl
decLabelSing SHiddenTypeL SHiddenTypeL = Just Refl
decLabelSing SApplyTypeL SApplyTypeL = Just Refl
decLabelSing SModuleAccessL SModuleAccessL = Just Refl
decLabelSing SHiddenModuleIdentifierL SHiddenModuleIdentifierL = Just Refl
decLabelSing SModuleIdentifierL SModuleIdentifierL = Just Refl
decLabelSing SIdentifierL SIdentifierL = Just Refl
decLabelSing SModuleIdentityL SModuleIdentityL = Just Refl
decLabelSing SNumLiteralL SNumLiteralL = Just Refl
decLabelSing STypeArgumentsL STypeArgumentsL = Just Refl
decLabelSing SFunctionTypeL SFunctionTypeL = Just Refl
decLabelSing SFunctionTypeParametersL SFunctionTypeParametersL = Just Refl
decLabelSing SPrimitiveTypeL SPrimitiveTypeL = Just Refl
decLabelSing SRefTypeL SRefTypeL = Just Refl
decLabelSing SHiddenReferenceL SHiddenReferenceL = Just Refl
decLabelSing SImmRefL SImmRefL = Just Refl
decLabelSing SMutRefL SMutRefL = Just Refl
decLabelSing STupleTypeL STupleTypeL = Just Refl
decLabelSing SPositionalFieldsL SPositionalFieldsL = Just Refl
decLabelSing SPostfixAbilityDeclsL SPostfixAbilityDeclsL = Just Refl
decLabelSing SHiddenFunctionItemL SHiddenFunctionItemL = Just Refl
decLabelSing SFunctionDefinitionL SFunctionDefinitionL = Just Refl
decLabelSing SHiddenFunctionSignatureL SHiddenFunctionSignatureL = Just Refl
decLabelSing SHiddenFunctionIdentifierL SHiddenFunctionIdentifierL = Just Refl
decLabelSing SFunctionIdentifierL SFunctionIdentifierL = Just Refl
decLabelSing SFunctionParametersL SFunctionParametersL = Just Refl
decLabelSing SFunctionParameterL SFunctionParameterL = Just Refl
decLabelSing SHiddenVariableIdentifierL SHiddenVariableIdentifierL = Just Refl
decLabelSing SVariableIdentifierL SVariableIdentifierL = Just Refl
decLabelSing SMutFunctionParameterL SMutFunctionParameterL = Just Refl
decLabelSing SModifierL SModifierL = Just Refl
decLabelSing SRetTypeL SRetTypeL = Just Refl
decLabelSing SBlockL SBlockL = Just Refl
decLabelSing SHiddenExpressionL SHiddenExpressionL = Just Refl
decLabelSing SHiddenUnaryExpressionL SHiddenUnaryExpressionL = Just Refl
decLabelSing SHiddenExpressionTermL SHiddenExpressionTermL = Just Refl
decLabelSing SHiddenLiteralValueL SHiddenLiteralValueL = Just Refl
decLabelSing SAddressLiteralL SAddressLiteralL = Just Refl
decLabelSing SBoolLiteralL SBoolLiteralL = Just Refl
decLabelSing SByteStringLiteralL SByteStringLiteralL = Just Refl
decLabelSing SHexStringLiteralL SHexStringLiteralL = Just Refl
decLabelSing SAnnotationExpressionL SAnnotationExpressionL = Just Refl
decLabelSing SBreakExpressionL SBreakExpressionL = Just Refl
decLabelSing SLabelL SLabelL = Just Refl
decLabelSing SCallExpressionL SCallExpressionL = Just Refl
decLabelSing SArgListL SArgListL = Just Refl
decLabelSing SNameExpressionL SNameExpressionL = Just Refl
decLabelSing SContinueExpressionL SContinueExpressionL = Just Refl
decLabelSing SDotExpressionL SDotExpressionL = Just Refl
decLabelSing SExpressionListL SExpressionListL = Just Refl
decLabelSing SIfExpressionL SIfExpressionL = Just Refl
decLabelSing SIndexExpressionL SIndexExpressionL = Just Refl
decLabelSing SMacroCallExpressionL SMacroCallExpressionL = Just Refl
decLabelSing SMacroModuleAccessL SMacroModuleAccessL = Just Refl
decLabelSing SMatchExpressionL SMatchExpressionL = Just Refl
decLabelSing SHiddenMatchBodyL SHiddenMatchBodyL = Just Refl
decLabelSing SMatchArmL SMatchArmL = Just Refl
decLabelSing SBindListL SBindListL = Just Refl
decLabelSing SHiddenBindL SHiddenBindL = Just Refl
decLabelSing SAtBindL SAtBindL = Just Refl
decLabelSing SBindUnpackL SBindUnpackL = Just Refl
decLabelSing SBindFieldsL SBindFieldsL = Just Refl
decLabelSing SBindNamedFieldsL SBindNamedFieldsL = Just Refl
decLabelSing SBindFieldL SBindFieldL = Just Refl
decLabelSing SHiddenSpreadOperatorL SHiddenSpreadOperatorL = Just Refl
decLabelSing SMutBindFieldL SMutBindFieldL = Just Refl
decLabelSing SBindPositionalFieldsL SBindPositionalFieldsL = Just Refl
decLabelSing SMutBindVarL SMutBindVarL = Just Refl
decLabelSing SBindVarL SBindVarL = Just Refl
decLabelSing SCommaBindListL SCommaBindListL = Just Refl
decLabelSing SOrBindListL SOrBindListL = Just Refl
decLabelSing SMatchConditionL SMatchConditionL = Just Refl
decLabelSing SPackExpressionL SPackExpressionL = Just Refl
decLabelSing SFieldInitializeListL SFieldInitializeListL = Just Refl
decLabelSing SExpFieldL SExpFieldL = Just Refl
decLabelSing SSpecBlockL SSpecBlockL = Just Refl
decLabelSing SHiddenSpecBlockTargetL SHiddenSpecBlockTargetL = Just Refl
decLabelSing SSpecBlockTargetSchemaL SSpecBlockTargetSchemaL = Just Refl
decLabelSing SHiddenStructIdentifierL SHiddenStructIdentifierL = Just Refl
decLabelSing SStructIdentifierL SStructIdentifierL = Just Refl
decLabelSing SHiddenSpecFunctionL SHiddenSpecFunctionL = Just Refl
decLabelSing SNativeSpecFunctionL SNativeSpecFunctionL = Just Refl
decLabelSing SHiddenSpecFunctionSignatureL SHiddenSpecFunctionSignatureL = Just Refl
decLabelSing SUninterpretedSpecFunctionL SUninterpretedSpecFunctionL = Just Refl
decLabelSing SUsualSpecFunctionL SUsualSpecFunctionL = Just Refl
decLabelSing SSpecBodyL SSpecBodyL = Just Refl
decLabelSing SHiddenSpecBlockMemeberL SHiddenSpecBlockMemeberL = Just Refl
decLabelSing SSpecApplyL SSpecApplyL = Just Refl
decLabelSing SSpecApplyPatternL SSpecApplyPatternL = Just Refl
decLabelSing SSpecApplyNamePatternL SSpecApplyNamePatternL = Just Refl
decLabelSing SSpecConditionL SSpecConditionL = Just Refl
decLabelSing SHiddenSpecAbortIfL SHiddenSpecAbortIfL = Just Refl
decLabelSing SConditionKindL SConditionKindL = Just Refl
decLabelSing SConditionPropertiesL SConditionPropertiesL = Just Refl
decLabelSing SSpecPropertyL SSpecPropertyL = Just Refl
decLabelSing SHiddenSpecAbortWithOrModifiesL SHiddenSpecAbortWithOrModifiesL = Just Refl
decLabelSing SHiddenSpecConditionL SHiddenSpecConditionL = Just Refl
decLabelSing SSpecIncludeL SSpecIncludeL = Just Refl
decLabelSing SSpecInvariantL SSpecInvariantL = Just Refl
decLabelSing SInvariantModifierL SInvariantModifierL = Just Refl
decLabelSing SSpecLetL SSpecLetL = Just Refl
decLabelSing SSpecPragmaL SSpecPragmaL = Just Refl
decLabelSing SSpecVariableL SSpecVariableL = Just Refl
decLabelSing SUseDeclarationL SUseDeclarationL = Just Refl
decLabelSing SUseFunL SUseFunL = Just Refl
decLabelSing SUseModuleL SUseModuleL = Just Refl
decLabelSing SUseModuleMemberL SUseModuleMemberL = Just Refl
decLabelSing SUseMemberL SUseMemberL = Just Refl
decLabelSing SUseModuleMembersL SUseModuleMembersL = Just Refl
decLabelSing SUnitExpressionL SUnitExpressionL = Just Refl
decLabelSing SVectorExpressionL SVectorExpressionL = Just Refl
decLabelSing SBorrowExpressionL SBorrowExpressionL = Just Refl
decLabelSing SDereferenceExpressionL SDereferenceExpressionL = Just Refl
decLabelSing SMoveOrCopyExpressionL SMoveOrCopyExpressionL = Just Refl
decLabelSing SUnaryExpressionL SUnaryExpressionL = Just Refl
decLabelSing SUnaryOpL SUnaryOpL = Just Refl
decLabelSing SAbortExpressionL SAbortExpressionL = Just Refl
decLabelSing SAssignExpressionL SAssignExpressionL = Just Refl
decLabelSing SBinaryExpressionL SBinaryExpressionL = Just Refl
decLabelSing SBinaryOperatorL SBinaryOperatorL = Just Refl
decLabelSing SCastExpressionL SCastExpressionL = Just Refl
decLabelSing SIdentifiedExpressionL SIdentifiedExpressionL = Just Refl
decLabelSing SBlockIdentifierL SBlockIdentifierL = Just Refl
decLabelSing SLambdaExpressionL SLambdaExpressionL = Just Refl
decLabelSing SLambdaBindingsL SLambdaBindingsL = Just Refl
decLabelSing SLambdaBindingL SLambdaBindingL = Just Refl
decLabelSing SLoopExpressionL SLoopExpressionL = Just Refl
decLabelSing SQuantifierExpressionL SQuantifierExpressionL = Just Refl
decLabelSing SHiddenExistsL SHiddenExistsL = Just Refl
decLabelSing SHiddenForallL SHiddenForallL = Just Refl
decLabelSing SQuantifierBindingsL SQuantifierBindingsL = Just Refl
decLabelSing SQuantifierBindingL SQuantifierBindingL = Just Refl
decLabelSing SReturnExpressionL SReturnExpressionL = Just Refl
decLabelSing SWhileExpressionL SWhileExpressionL = Just Refl
decLabelSing SBlockItemL SBlockItemL = Just Refl
decLabelSing SLetStatementL SLetStatementL = Just Refl
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
decLabelSing SFalseTokL SFalseTokL = Just Refl
decLabelSing SFriendTokL SFriendTokL = Just Refl
decLabelSing SGlobalTokL SGlobalTokL = Just Refl
decLabelSing SInternalTokL SInternalTokL = Just Refl
decLabelSing SKeyTokL SKeyTokL = Just Refl
decLabelSing SLocalTokL SLocalTokL = Just Refl
decLabelSing SModifiesTokL SModifiesTokL = Just Refl
decLabelSing SModuleTokL SModuleTokL = Just Refl
decLabelSing SMoveTokL SMoveTokL = Just Refl
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
  False :: Token e FalseTokL
  Friend :: Token e FriendTokL
  Global :: Token e GlobalTokL
  Internal :: Token e InternalTokL
  Key :: Token e KeyTokL
  Local :: Token e LocalTokL
  Modifies :: Token e ModifiesTokL
  Module :: Token e ModuleTokL
  Move :: Token e MoveTokL
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
    :: e [Either FriendDeclarationL (Either ConstantL (Either HiddenFunctionItemL (Either HiddenStructItemL (Either HiddenEnumItemL (Either SpecBlockL UseDeclarationL)))))]
    -> ModuleBody e ModuleBodyL

data HiddenEnumItem e l where
  EnumDefinitionEnumItem
    :: e EnumDefinitionL
    -> HiddenEnumItem e HiddenEnumItemL

data EnumDefinition e l where
  EnumDefinition
    :: e (Maybe PublicTokL)
    -> e HiddenEnumSignatureL
    -> e EnumVariantsL
    -> e (Maybe PostfixAbilityDeclsL)
    -> EnumDefinition e EnumDefinitionL

data HiddenEnumSignature e l where
  HiddenEnumSignature
    :: e HiddenEnumIdentifierL
    -> e (Maybe TypeParametersL)
    -> e (Maybe AbilityDeclsL)
    -> HiddenEnumSignature e HiddenEnumSignatureL

data HiddenEnumIdentifier e l where
  HiddenEnumIdentifier
    :: e EnumIdentifierL
    -> HiddenEnumIdentifier e HiddenEnumIdentifierL

data EnumIdentifier e l where
  EnumIdentifier
    :: EnumIdentifier e EnumIdentifierL

data AbilityDecls e l where
  AbilityDecls
    :: e (Maybe AbilityL, [AbilityL])
    -> AbilityDecls e AbilityDeclsL

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

data TypeParameters e l where
  TypeParameters
    :: e LtTokL
    -> e ([TypeParameterL], TypeParameterL)
    -> e GtTokL
    -> TypeParameters e TypeParametersL

data TypeParameter e l where
  TypeParameter
    :: e (Maybe DollarTokL)
    -> e (Maybe PhantomTokL)
    -> e HiddenTypeParameterIdentifierL
    -> e (Maybe ([(AbilityL, AddTokL)], (Maybe AddTokL, AbilityL)))
    -> TypeParameter e TypeParameterL

data HiddenTypeParameterIdentifier e l where
  HiddenTypeParameterIdentifier
    :: e TypeParameterIdentifierL
    -> HiddenTypeParameterIdentifier e HiddenTypeParameterIdentifierL

data TypeParameterIdentifier e l where
  TypeParameterIdentifier
    :: TypeParameterIdentifier e TypeParameterIdentifierL

data EnumVariants e l where
  EnumVariants
    :: e (Maybe VariantL, [VariantL])
    -> EnumVariants e EnumVariantsL

data Variant e l where
  Variant
    :: e HiddenVariantIdentifierL
    -> e (Maybe DatatypeFieldsL)
    -> Variant e VariantL

data HiddenVariantIdentifier e l where
  HiddenVariantIdentifier
    :: e VariantIdentifierL
    -> HiddenVariantIdentifier e HiddenVariantIdentifierL

data VariantIdentifier e l where
  VariantIdentifier
    :: VariantIdentifier e VariantIdentifierL

data DatatypeFields e l where
  PositionalFieldsDatatypeFields
    :: e PositionalFieldsL
    -> DatatypeFields e DatatypeFieldsL
  NamedFieldsDatatypeFields
    :: e NamedFieldsL
    -> DatatypeFields e DatatypeFieldsL

data NamedFields e l where
  NamedFields
    :: e (Maybe FieldAnnotationL, [FieldAnnotationL])
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
    -> e (Maybe TypeArgumentsL)
    -> ApplyType e ApplyTypeL

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
    -> e (Maybe TypeArgumentsL)
    -> ModuleAccess e ModuleAccessL
  ModuleAccess5
    :: e HiddenModuleIdentifierL
    -> e (Maybe TypeArgumentsL)
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess6
    :: e ModuleIdentityL
    -> e IdentifierL
    -> e TypeArgumentsL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess7
    :: e ModuleIdentityL
    -> e (Maybe TypeArgumentsL)
    -> ModuleAccess e ModuleAccessL
  ModuleAccess8
    :: e ModuleIdentityL
    -> e (Maybe TypeArgumentsL)
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess9
    :: e ModuleIdentityL
    -> e IdentifierL
    -> e (Maybe TypeArgumentsL)
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
    :: e (Either HiddenModuleIdentifierL NumLiteralL)
    -> e HiddenModuleIdentifierL
    -> ModuleIdentity e ModuleIdentityL

data NumLiteral e l where
  NumLiteral
    :: e (Maybe (Either U16TokL (Either U32TokL (Either U64TokL (Either U128TokL (Either U256TokL U8TokL))))))
    -> NumLiteral e NumLiteralL

data TypeArguments e l where
  TypeArguments
    :: e LtTokL
    -> e ([HiddenTypeL], HiddenTypeL)
    -> e GtTokL
    -> TypeArguments e TypeArgumentsL

data FunctionType e l where
  FunctionType
    :: e FunctionTypeParametersL
    -> e (Maybe HiddenTypeL)
    -> FunctionType e FunctionTypeL

data FunctionTypeParameters e l where
  FunctionTypeParameters
    :: e BitorTokL
    -> e (Maybe HiddenTypeL, [HiddenTypeL])
    -> e BitorTokL
    -> FunctionTypeParameters e FunctionTypeParametersL

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
    :: e (Maybe HiddenTypeL, [HiddenTypeL])
    -> TupleType e TupleTypeL

data PositionalFields e l where
  PositionalFields
    :: e (Maybe HiddenTypeL, [HiddenTypeL])
    -> PositionalFields e PositionalFieldsL

data PostfixAbilityDecls e l where
  PostfixAbilityDecls
    :: e (Maybe AbilityL, [AbilityL])
    -> PostfixAbilityDecls e PostfixAbilityDeclsL

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
    :: e (Maybe ModifierL)
    -> e (Maybe ModifierL)
    -> e (Maybe ModifierL)
    -> e HiddenFunctionIdentifierL
    -> e (Maybe TypeParametersL)
    -> e FunctionParametersL
    -> e (Maybe RetTypeL)
    -> HiddenFunctionSignature e HiddenFunctionSignatureL

data HiddenFunctionIdentifier e l where
  HiddenFunctionIdentifier
    :: e FunctionIdentifierL
    -> HiddenFunctionIdentifier e HiddenFunctionIdentifierL

data FunctionIdentifier e l where
  FunctionIdentifier
    :: FunctionIdentifier e FunctionIdentifierL

data FunctionParameters e l where
  FunctionParameters
    :: e (Maybe (Either FunctionParameterL MutFunctionParameterL), [Either FunctionParameterL MutFunctionParameterL])
    -> FunctionParameters e FunctionParametersL

data FunctionParameter e l where
  FunctionParameter
    :: e (Either (HiddenVariableIdentifierL, DollarTokL) HiddenVariableIdentifierL)
    -> e HiddenTypeL
    -> FunctionParameter e FunctionParameterL

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

data Modifier e l where
  Modifier1
    :: e PublicTokL
    -> e (Maybe (Either FriendTokL PackageTokL))
    -> Modifier e ModifierL

data RetType e l where
  RetType
    :: e HiddenTypeL
    -> RetType e RetTypeL

data Block e l where
  Block
    :: e [UseDeclarationL]
    -> e [BlockItemL]
    -> e (Maybe HiddenExpressionL)
    -> Block e BlockL

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
  UnaryExpressionUnaryExpression
    :: e UnaryExpressionL
    -> HiddenUnaryExpression e HiddenUnaryExpressionL
  BorrowExpressionUnaryExpression
    :: e BorrowExpressionL
    -> HiddenUnaryExpression e HiddenUnaryExpressionL
  DereferenceExpressionUnaryExpression
    :: e DereferenceExpressionL
    -> HiddenUnaryExpression e HiddenUnaryExpressionL
  MoveOrCopyExpressionUnaryExpression
    :: e MoveOrCopyExpressionL
    -> HiddenUnaryExpression e HiddenUnaryExpressionL
  HiddenExpressionTermUnaryExpression
    :: e HiddenExpressionTermL
    -> HiddenUnaryExpression e HiddenUnaryExpressionL

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
    :: e (Maybe LabelL)
    -> e (Maybe HiddenExpressionL)
    -> BreakExpression e BreakExpressionL

data Label e l where
  Label
    :: e IdentifierL
    -> Label e LabelL

data CallExpression e l where
  CallExpression
    :: e NameExpressionL
    -> e ArgListL
    -> CallExpression e CallExpressionL

data ArgList e l where
  ArgList
    :: e (Maybe HiddenExpressionL, [HiddenExpressionL])
    -> ArgList e ArgListL

data NameExpression e l where
  NameExpression
    :: e ModuleAccessL
    -> NameExpression e NameExpressionL

data ContinueExpression e l where
  ContinueExpression
    :: e (Maybe LabelL)
    -> ContinueExpression e ContinueExpressionL

data DotExpression e l where
  DotExpression
    :: e HiddenExpressionTermL
    -> e HiddenExpressionTermL
    -> DotExpression e DotExpressionL

data ExpressionList e l where
  ExpressionList
    :: e ([HiddenExpressionL], HiddenExpressionL)
    -> ExpressionList e ExpressionListL

data IfExpression e l where
  IfExpression
    :: e HiddenExpressionL
    -> e HiddenExpressionL
    -> e (Maybe HiddenExpressionL)
    -> IfExpression e IfExpressionL

data IndexExpression e l where
  IndexExpression
    :: e HiddenExpressionTermL
    -> e (Maybe HiddenExpressionL, [HiddenExpressionL])
    -> IndexExpression e IndexExpressionL

data MacroCallExpression e l where
  MacroCallExpression
    :: e MacroModuleAccessL
    -> e (Maybe TypeArgumentsL)
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
    :: e (Maybe MatchArmL, [MatchArmL])
    -> HiddenMatchBody e HiddenMatchBodyL

data MatchArm e l where
  MatchArm
    :: e BindListL
    -> e (Maybe MatchConditionL)
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
  MutBindVarBind
    :: e MutBindVarL
    -> HiddenBind e HiddenBindL
  BindVarBind
    :: e HiddenVariableIdentifierL
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

data AtBind e l where
  AtBind
    :: e HiddenVariableIdentifierL
    -> e BindListL
    -> AtBind e AtBindL

data BindUnpack e l where
  BindUnpack
    :: e NameExpressionL
    -> e (Maybe BindFieldsL)
    -> BindUnpack e BindUnpackL

data BindFields e l where
  BindPositionalFieldsBindFields
    :: e BindPositionalFieldsL
    -> BindFields e BindFieldsL
  BindNamedFieldsBindFields
    :: e BindNamedFieldsL
    -> BindFields e BindFieldsL

data BindNamedFields e l where
  BindNamedFields
    :: e (Maybe (Either MutBindFieldL BindFieldL), [Either MutBindFieldL BindFieldL])
    -> BindNamedFields e BindNamedFieldsL

data BindField e l where
  BindField1
    :: e BindListL
    -> e (Maybe BindListL)
    -> BindField e BindFieldL
  HiddenSpreadOperatorBindField
    :: e HiddenSpreadOperatorL
    -> BindField e BindFieldL

data HiddenSpreadOperator e l where
  HiddenSpreadOperator
    :: e RangeTokL
    -> HiddenSpreadOperator e HiddenSpreadOperatorL

data MutBindField e l where
  MutBindField
    :: e BindFieldL
    -> MutBindField e MutBindFieldL

data BindPositionalFields e l where
  BindPositionalFields
    :: e (Maybe (Either MutBindFieldL BindFieldL), [Either MutBindFieldL BindFieldL])
    -> BindPositionalFields e BindPositionalFieldsL

data MutBindVar e l where
  MutBindVar
    :: e BindVarL
    -> MutBindVar e MutBindVarL

data BindVar e l where
  BindVar
    :: e VariableIdentifierL
    -> BindVar e BindVarL

data CommaBindList e l where
  CommaBindList
    :: e (Maybe HiddenBindL, [HiddenBindL])
    -> CommaBindList e CommaBindListL

data OrBindList e l where
  OrBindList
    :: e ([(HiddenBindL, BitorTokL)], (Maybe BitorTokL, HiddenBindL))
    -> OrBindList e OrBindListL

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
    :: e (Maybe ExpFieldL, [ExpFieldL])
    -> FieldInitializeList e FieldInitializeListL

data ExpField e l where
  ExpField
    :: e HiddenFieldIdentifierL
    -> e (Maybe HiddenExpressionL)
    -> ExpField e ExpFieldL

data SpecBlock e l where
  SpecBlock
    :: e (Either HiddenSpecFunctionL (SpecBodyL, Maybe HiddenSpecBlockTargetL))
    -> SpecBlock e SpecBlockL

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
    -> e (Maybe TypeParametersL)
    -> SpecBlockTargetSchema e SpecBlockTargetSchemaL

data HiddenStructIdentifier e l where
  HiddenStructIdentifier
    :: e StructIdentifierL
    -> HiddenStructIdentifier e HiddenStructIdentifierL

data StructIdentifier e l where
  StructIdentifier
    :: StructIdentifier e StructIdentifierL

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
    :: e HiddenSpecFunctionSignatureL
    -> NativeSpecFunction e NativeSpecFunctionL

data HiddenSpecFunctionSignature e l where
  HiddenSpecFunctionSignature
    :: e HiddenFunctionIdentifierL
    -> e (Maybe TypeParametersL)
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
    -> e ([SpecApplyPatternL], SpecApplyPatternL)
    -> e (Maybe ([SpecApplyPatternL], SpecApplyPatternL))
    -> SpecApply e SpecApplyL

data SpecApplyPattern e l where
  SpecApplyPattern
    :: e (Maybe (Either InternalTokL PublicTokL))
    -> e SpecApplyNamePatternL
    -> e (Maybe TypeParametersL)
    -> SpecApplyPattern e SpecApplyPatternL

data SpecApplyNamePattern e l where
  SpecApplyNamePattern
    :: SpecApplyNamePattern e SpecApplyNamePatternL

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
    -> e (Maybe ConditionPropertiesL)
    -> e HiddenExpressionL
    -> e (Maybe HiddenExpressionL)
    -> HiddenSpecAbortIf e HiddenSpecAbortIfL

data ConditionKind e l where
  ConditionKind
    :: ConditionKind e ConditionKindL

data ConditionProperties e l where
  ConditionProperties
    :: e (Maybe SpecPropertyL, [SpecPropertyL])
    -> ConditionProperties e ConditionPropertiesL

data SpecProperty e l where
  SpecProperty
    :: e IdentifierL
    -> e (Maybe (HiddenLiteralValueL, AssignTokL))
    -> SpecProperty e SpecPropertyL

data HiddenSpecAbortWithOrModifies e l where
  HiddenSpecAbortWithOrModifies
    :: e ConditionKindL
    -> e (Maybe ConditionPropertiesL)
    -> e ([HiddenExpressionL], HiddenExpressionL)
    -> HiddenSpecAbortWithOrModifies e HiddenSpecAbortWithOrModifiesL

data HiddenSpecCondition e l where
  HiddenSpecCondition
    :: e (Either (Maybe ModuleTokL, ConditionKindL) ConditionKindL)
    -> e (Maybe ConditionPropertiesL)
    -> e HiddenExpressionL
    -> HiddenSpecCondition e HiddenSpecConditionL

data SpecInclude e l where
  SpecInclude
    :: e HiddenExpressionL
    -> SpecInclude e SpecIncludeL

data SpecInvariant e l where
  SpecInvariant
    :: e ConditionKindL
    -> e (Maybe InvariantModifierL)
    -> e (Maybe ConditionPropertiesL)
    -> e HiddenExpressionL
    -> SpecInvariant e SpecInvariantL

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
    :: e (Maybe PostTokL)
    -> e IdentifierL
    -> e AssignTokL
    -> e HiddenExpressionL
    -> SpecLet e SpecLetL

data SpecPragma e l where
  SpecPragma
    :: e (Maybe SpecPropertyL, [SpecPropertyL])
    -> SpecPragma e SpecPragmaL

data SpecVariable e l where
  SpecVariable
    :: e (Maybe (Either LocalTokL GlobalTokL))
    -> e IdentifierL
    -> e (Maybe TypeParametersL)
    -> e HiddenTypeL
    -> SpecVariable e SpecVariableL

data UseDeclaration e l where
  UseDeclaration
    :: e (Maybe PublicTokL)
    -> e (Either UseModuleL (Either UseModuleMemberL (Either UseModuleMembersL UseFunL)))
    -> UseDeclaration e UseDeclarationL

data UseFun e l where
  UseFun
    :: e ModuleAccessL
    -> e (HiddenFunctionIdentifierL, ModuleAccessL)
    -> UseFun e UseFunL

data UseModule e l where
  UseModule
    :: e ModuleIdentityL
    -> e (Maybe HiddenModuleIdentifierL)
    -> UseModule e UseModuleL

data UseModuleMember e l where
  UseModuleMember
    :: e ModuleIdentityL
    -> e UseMemberL
    -> UseModuleMember e UseModuleMemberL

data UseMember e l where
  UseMember1
    :: e IdentifierL
    -> e ([UseMemberL], UseMemberL)
    -> UseMember e UseMemberL
  UseMember2
    :: e IdentifierL
    -> e IdentifierL
    -> e (Maybe IdentifierL)
    -> UseMember e UseMemberL
  UseMember3
    :: e IdentifierL
    -> e (Maybe IdentifierL)
    -> UseMember e UseMemberL

data UseModuleMembers e l where
  UseModuleMembers1
    :: e (Either HiddenModuleIdentifierL NumLiteralL)
    -> e ([UseMemberL], UseMemberL)
    -> UseModuleMembers e UseModuleMembersL
  UseModuleMembers2
    :: e ModuleIdentityL
    -> e ([UseMemberL], UseMemberL)
    -> UseModuleMembers e UseModuleMembersL

data UnitExpression e l where
  UnitExpression
    :: UnitExpression e UnitExpressionL

data VectorExpression e l where
  VectorExpression
    :: e (Maybe (([HiddenTypeL], HiddenTypeL), GtTokL))
    -> e (Maybe HiddenExpressionL, [HiddenExpressionL])
    -> VectorExpression e VectorExpressionL

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
    :: e (Either CopyTokL MoveTokL)
    -> e HiddenExpressionL
    -> MoveOrCopyExpression e MoveOrCopyExpressionL

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
    :: e (Maybe HiddenExpressionL)
    -> AbortExpression e AbortExpressionL

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
    -> e (Maybe HiddenTypeL)
    -> e HiddenExpressionL
    -> LambdaExpression e LambdaExpressionL

data LambdaBindings e l where
  LambdaBindings
    :: e BitorTokL
    -> e (Maybe LambdaBindingL, [LambdaBindingL])
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
    -> e (Maybe HiddenTypeL)
    -> LambdaBinding e LambdaBindingL

data LoopExpression e l where
  LoopExpression
    :: e HiddenExpressionL
    -> LoopExpression e LoopExpressionL

data QuantifierExpression e l where
  QuantifierExpression
    :: e (Either HiddenExistsL HiddenForallL)
    -> e QuantifierBindingsL
    -> e (Maybe HiddenExpressionL)
    -> e HiddenExpressionL
    -> QuantifierExpression e QuantifierExpressionL

data HiddenExists e l where
  HiddenExists
    :: HiddenExists e HiddenExistsL

data HiddenForall e l where
  HiddenForall
    :: HiddenForall e HiddenForallL

data QuantifierBindings e l where
  QuantifierBindings
    :: e QuantifierBindingL
    -> e [QuantifierBindingL]
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

data ReturnExpression e l where
  ReturnExpression1
    :: e (Maybe LabelL)
    -> e HiddenExpressionL
    -> ReturnExpression e ReturnExpressionL
  ReturnExpression2
    :: e (Maybe LabelL)
    -> ReturnExpression e ReturnExpressionL

data WhileExpression e l where
  WhileExpression
    :: e HiddenExpressionL
    -> e HiddenExpressionL
    -> WhileExpression e WhileExpressionL

data BlockItem e l where
  BlockItem
    :: e (Either LetStatementL HiddenExpressionL)
    -> BlockItem e BlockItemL

data LetStatement e l where
  LetStatement
    :: e BindListL
    -> e (Maybe HiddenTypeL)
    -> e (Maybe (HiddenExpressionL, AssignTokL))
    -> LetStatement e LetStatementL

data MacroFunctionDefinition e l where
  MacroFunctionDefinition
    :: e (Maybe ModifierL)
    -> e HiddenMacroSignatureL
    -> e BlockL
    -> MacroFunctionDefinition e MacroFunctionDefinitionL

data HiddenMacroSignature e l where
  HiddenMacroSignature
    :: e (Maybe ModifierL)
    -> e HiddenFunctionIdentifierL
    -> e (Maybe TypeParametersL)
    -> e FunctionParametersL
    -> e (Maybe RetTypeL)
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
    :: e (Maybe PublicTokL)
    -> e HiddenStructSignatureL
    -> NativeStructDefinition e NativeStructDefinitionL

data HiddenStructSignature e l where
  HiddenStructSignature
    :: e HiddenStructIdentifierL
    -> e (Maybe TypeParametersL)
    -> e (Maybe AbilityDeclsL)
    -> HiddenStructSignature e HiddenStructSignatureL

data StructDefinition e l where
  StructDefinition
    :: e (Maybe PublicTokL)
    -> e HiddenStructSignatureL
    -> e DatatypeFieldsL
    -> e (Maybe PostfixAbilityDeclsL)
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

--------------------------------------------------------------------------------
-- Compdata derivation

deriveAll
  [ ''SourceFile
  , ''ModuleDefinition
  , ''ModuleBody
  , ''HiddenEnumItem
  , ''EnumDefinition
  , ''HiddenEnumSignature
  , ''HiddenEnumIdentifier
  , ''EnumIdentifier
  , ''AbilityDecls
  , ''Ability
  , ''TypeParameters
  , ''TypeParameter
  , ''HiddenTypeParameterIdentifier
  , ''TypeParameterIdentifier
  , ''EnumVariants
  , ''Variant
  , ''HiddenVariantIdentifier
  , ''VariantIdentifier
  , ''DatatypeFields
  , ''NamedFields
  , ''FieldAnnotation
  , ''HiddenFieldIdentifier
  , ''FieldIdentifier
  , ''HiddenType
  , ''ApplyType
  , ''ModuleAccess
  , ''HiddenModuleIdentifier
  , ''ModuleIdentifier
  , ''Identifier
  , ''ModuleIdentity
  , ''NumLiteral
  , ''TypeArguments
  , ''FunctionType
  , ''FunctionTypeParameters
  , ''PrimitiveType
  , ''RefType
  , ''HiddenReference
  , ''ImmRef
  , ''MutRef
  , ''TupleType
  , ''PositionalFields
  , ''PostfixAbilityDecls
  , ''HiddenFunctionItem
  , ''FunctionDefinition
  , ''HiddenFunctionSignature
  , ''HiddenFunctionIdentifier
  , ''FunctionIdentifier
  , ''FunctionParameters
  , ''FunctionParameter
  , ''HiddenVariableIdentifier
  , ''VariableIdentifier
  , ''MutFunctionParameter
  , ''Modifier
  , ''RetType
  , ''Block
  , ''HiddenExpression
  , ''HiddenUnaryExpression
  , ''HiddenExpressionTerm
  , ''HiddenLiteralValue
  , ''AddressLiteral
  , ''BoolLiteral
  , ''ByteStringLiteral
  , ''HexStringLiteral
  , ''AnnotationExpression
  , ''BreakExpression
  , ''Label
  , ''CallExpression
  , ''ArgList
  , ''NameExpression
  , ''ContinueExpression
  , ''DotExpression
  , ''ExpressionList
  , ''IfExpression
  , ''IndexExpression
  , ''MacroCallExpression
  , ''MacroModuleAccess
  , ''MatchExpression
  , ''HiddenMatchBody
  , ''MatchArm
  , ''BindList
  , ''HiddenBind
  , ''AtBind
  , ''BindUnpack
  , ''BindFields
  , ''BindNamedFields
  , ''BindField
  , ''HiddenSpreadOperator
  , ''MutBindField
  , ''BindPositionalFields
  , ''MutBindVar
  , ''BindVar
  , ''CommaBindList
  , ''OrBindList
  , ''MatchCondition
  , ''PackExpression
  , ''FieldInitializeList
  , ''ExpField
  , ''SpecBlock
  , ''HiddenSpecBlockTarget
  , ''SpecBlockTargetSchema
  , ''HiddenStructIdentifier
  , ''StructIdentifier
  , ''HiddenSpecFunction
  , ''NativeSpecFunction
  , ''HiddenSpecFunctionSignature
  , ''UninterpretedSpecFunction
  , ''UsualSpecFunction
  , ''SpecBody
  , ''HiddenSpecBlockMemeber
  , ''SpecApply
  , ''SpecApplyPattern
  , ''SpecApplyNamePattern
  , ''SpecCondition
  , ''HiddenSpecAbortIf
  , ''ConditionKind
  , ''ConditionProperties
  , ''SpecProperty
  , ''HiddenSpecAbortWithOrModifies
  , ''HiddenSpecCondition
  , ''SpecInclude
  , ''SpecInvariant
  , ''InvariantModifier
  , ''SpecLet
  , ''SpecPragma
  , ''SpecVariable
  , ''UseDeclaration
  , ''UseFun
  , ''UseModule
  , ''UseModuleMember
  , ''UseMember
  , ''UseModuleMembers
  , ''UnitExpression
  , ''VectorExpression
  , ''BorrowExpression
  , ''DereferenceExpression
  , ''MoveOrCopyExpression
  , ''UnaryExpression
  , ''UnaryOp
  , ''AbortExpression
  , ''AssignExpression
  , ''BinaryExpression
  , ''BinaryOperator
  , ''CastExpression
  , ''IdentifiedExpression
  , ''BlockIdentifier
  , ''LambdaExpression
  , ''LambdaBindings
  , ''LambdaBinding
  , ''LoopExpression
  , ''QuantifierExpression
  , ''HiddenExists
  , ''HiddenForall
  , ''QuantifierBindings
  , ''QuantifierBinding
  , ''ReturnExpression
  , ''WhileExpression
  , ''BlockItem
  , ''LetStatement
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
  ]

--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

type MoveSig =
  '[ SourceFile
   , ModuleDefinition
   , ModuleBody
   , HiddenEnumItem
   , EnumDefinition
   , HiddenEnumSignature
   , HiddenEnumIdentifier
   , EnumIdentifier
   , AbilityDecls
   , Ability
   , TypeParameters
   , TypeParameter
   , HiddenTypeParameterIdentifier
   , TypeParameterIdentifier
   , EnumVariants
   , Variant
   , HiddenVariantIdentifier
   , VariantIdentifier
   , DatatypeFields
   , NamedFields
   , FieldAnnotation
   , HiddenFieldIdentifier
   , FieldIdentifier
   , HiddenType
   , ApplyType
   , ModuleAccess
   , HiddenModuleIdentifier
   , ModuleIdentifier
   , Identifier
   , ModuleIdentity
   , NumLiteral
   , TypeArguments
   , FunctionType
   , FunctionTypeParameters
   , PrimitiveType
   , RefType
   , HiddenReference
   , ImmRef
   , MutRef
   , TupleType
   , PositionalFields
   , PostfixAbilityDecls
   , HiddenFunctionItem
   , FunctionDefinition
   , HiddenFunctionSignature
   , HiddenFunctionIdentifier
   , FunctionIdentifier
   , FunctionParameters
   , FunctionParameter
   , HiddenVariableIdentifier
   , VariableIdentifier
   , MutFunctionParameter
   , Modifier
   , RetType
   , Block
   , HiddenExpression
   , HiddenUnaryExpression
   , HiddenExpressionTerm
   , HiddenLiteralValue
   , AddressLiteral
   , BoolLiteral
   , ByteStringLiteral
   , HexStringLiteral
   , AnnotationExpression
   , BreakExpression
   , Label
   , CallExpression
   , ArgList
   , NameExpression
   , ContinueExpression
   , DotExpression
   , ExpressionList
   , IfExpression
   , IndexExpression
   , MacroCallExpression
   , MacroModuleAccess
   , MatchExpression
   , HiddenMatchBody
   , MatchArm
   , BindList
   , HiddenBind
   , AtBind
   , BindUnpack
   , BindFields
   , BindNamedFields
   , BindField
   , HiddenSpreadOperator
   , MutBindField
   , BindPositionalFields
   , MutBindVar
   , BindVar
   , CommaBindList
   , OrBindList
   , MatchCondition
   , PackExpression
   , FieldInitializeList
   , ExpField
   , SpecBlock
   , HiddenSpecBlockTarget
   , SpecBlockTargetSchema
   , HiddenStructIdentifier
   , StructIdentifier
   , HiddenSpecFunction
   , NativeSpecFunction
   , HiddenSpecFunctionSignature
   , UninterpretedSpecFunction
   , UsualSpecFunction
   , SpecBody
   , HiddenSpecBlockMemeber
   , SpecApply
   , SpecApplyPattern
   , SpecApplyNamePattern
   , SpecCondition
   , HiddenSpecAbortIf
   , ConditionKind
   , ConditionProperties
   , SpecProperty
   , HiddenSpecAbortWithOrModifies
   , HiddenSpecCondition
   , SpecInclude
   , SpecInvariant
   , InvariantModifier
   , SpecLet
   , SpecPragma
   , SpecVariable
   , UseDeclaration
   , UseFun
   , UseModule
   , UseModuleMember
   , UseMember
   , UseModuleMembers
   , UnitExpression
   , VectorExpression
   , BorrowExpression
   , DereferenceExpression
   , MoveOrCopyExpression
   , UnaryExpression
   , UnaryOp
   , AbortExpression
   , AssignExpression
   , BinaryExpression
   , BinaryOperator
   , CastExpression
   , IdentifiedExpression
   , BlockIdentifier
   , LambdaExpression
   , LambdaBindings
   , LambdaBinding
   , LoopExpression
   , QuantifierExpression
   , HiddenExists
   , HiddenForall
   , QuantifierBindings
   , QuantifierBinding
   , ReturnExpression
   , WhileExpression
   , BlockItem
   , LetStatement
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
  HiddenEnumItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenEnumSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenEnumIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilityDeclsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeParameterIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumVariantsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VariantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenVariantIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VariantIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DatatypeFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NamedFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldAnnotationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFieldIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ApplyTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenModuleIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeArgumentsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PrimitiveTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RefTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenReferenceSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ImmRefSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutRefSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TupleTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PositionalFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PostfixAbilityDeclsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenVariableIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VariableIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutFunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RetTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenUnaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExpressionTermSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenLiteralValueSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AddressLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BoolLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ByteStringLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HexStringLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AnnotationExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BreakExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LabelSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CallExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ArgListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NameExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ContinueExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DotExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExpressionListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IfExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IndexExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroCallExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroModuleAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenMatchBodySymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchArmSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenBindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AtBindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindUnpackSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindNamedFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpreadOperatorSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutBindFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindPositionalFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutBindVarSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindVarSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CommaBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  OrBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PackExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldInitializeListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExpFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecBlockTargetSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockTargetSchemaSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  StructIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecFunctionSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UninterpretedSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UsualSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBodySymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecBlockMemeberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplySymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyPatternSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyNamePatternSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortIfSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConditionKindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConditionPropertiesSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPropertySymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortWithOrModifiesSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecIncludeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecInvariantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  InvariantModifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecLetSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPragmaSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecVariableSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseDeclarationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseFunSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleMemberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseMemberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleMembersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UnitExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VectorExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BorrowExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DereferenceExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MoveOrCopyExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UnaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UnaryOpSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbortExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AssignExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BinaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BinaryOperatorSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CastExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IdentifiedExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaBindingsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaBindingSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LoopExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExistsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenForallSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierBindingsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierBindingSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ReturnExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  WhileExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LetStatementSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  FalseTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FriendTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  GlobalTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  InternalTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  KeyTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LocalTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifiesTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MoveTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  SHiddenEnumItemSymbol :: SymbolSing Regular HiddenEnumItemSymbol
  SEnumDefinitionSymbol :: SymbolSing Regular EnumDefinitionSymbol
  SHiddenEnumSignatureSymbol :: SymbolSing Regular HiddenEnumSignatureSymbol
  SHiddenEnumIdentifierSymbol :: SymbolSing Regular HiddenEnumIdentifierSymbol
  SEnumIdentifierSymbol :: SymbolSing Regular EnumIdentifierSymbol
  SAbilityDeclsSymbol :: SymbolSing Regular AbilityDeclsSymbol
  SAbilitySymbol :: SymbolSing Regular AbilitySymbol
  STypeParametersSymbol :: SymbolSing Regular TypeParametersSymbol
  STypeParameterSymbol :: SymbolSing Regular TypeParameterSymbol
  SHiddenTypeParameterIdentifierSymbol :: SymbolSing Regular HiddenTypeParameterIdentifierSymbol
  STypeParameterIdentifierSymbol :: SymbolSing Regular TypeParameterIdentifierSymbol
  SEnumVariantsSymbol :: SymbolSing Regular EnumVariantsSymbol
  SVariantSymbol :: SymbolSing Regular VariantSymbol
  SHiddenVariantIdentifierSymbol :: SymbolSing Regular HiddenVariantIdentifierSymbol
  SVariantIdentifierSymbol :: SymbolSing Regular VariantIdentifierSymbol
  SDatatypeFieldsSymbol :: SymbolSing Regular DatatypeFieldsSymbol
  SNamedFieldsSymbol :: SymbolSing Regular NamedFieldsSymbol
  SFieldAnnotationSymbol :: SymbolSing Regular FieldAnnotationSymbol
  SHiddenFieldIdentifierSymbol :: SymbolSing Regular HiddenFieldIdentifierSymbol
  SFieldIdentifierSymbol :: SymbolSing Regular FieldIdentifierSymbol
  SHiddenTypeSymbol :: SymbolSing Regular HiddenTypeSymbol
  SApplyTypeSymbol :: SymbolSing Regular ApplyTypeSymbol
  SModuleAccessSymbol :: SymbolSing Regular ModuleAccessSymbol
  SHiddenModuleIdentifierSymbol :: SymbolSing Regular HiddenModuleIdentifierSymbol
  SModuleIdentifierSymbol :: SymbolSing Regular ModuleIdentifierSymbol
  SIdentifierSymbol :: SymbolSing Regular IdentifierSymbol
  SModuleIdentitySymbol :: SymbolSing Regular ModuleIdentitySymbol
  SNumLiteralSymbol :: SymbolSing Regular NumLiteralSymbol
  STypeArgumentsSymbol :: SymbolSing Regular TypeArgumentsSymbol
  SFunctionTypeSymbol :: SymbolSing Regular FunctionTypeSymbol
  SFunctionTypeParametersSymbol :: SymbolSing Regular FunctionTypeParametersSymbol
  SPrimitiveTypeSymbol :: SymbolSing Regular PrimitiveTypeSymbol
  SRefTypeSymbol :: SymbolSing Regular RefTypeSymbol
  SHiddenReferenceSymbol :: SymbolSing Regular HiddenReferenceSymbol
  SImmRefSymbol :: SymbolSing Regular ImmRefSymbol
  SMutRefSymbol :: SymbolSing Regular MutRefSymbol
  STupleTypeSymbol :: SymbolSing Regular TupleTypeSymbol
  SPositionalFieldsSymbol :: SymbolSing Regular PositionalFieldsSymbol
  SPostfixAbilityDeclsSymbol :: SymbolSing Regular PostfixAbilityDeclsSymbol
  SHiddenFunctionItemSymbol :: SymbolSing Regular HiddenFunctionItemSymbol
  SFunctionDefinitionSymbol :: SymbolSing Regular FunctionDefinitionSymbol
  SHiddenFunctionSignatureSymbol :: SymbolSing Regular HiddenFunctionSignatureSymbol
  SHiddenFunctionIdentifierSymbol :: SymbolSing Regular HiddenFunctionIdentifierSymbol
  SFunctionIdentifierSymbol :: SymbolSing Regular FunctionIdentifierSymbol
  SFunctionParametersSymbol :: SymbolSing Regular FunctionParametersSymbol
  SFunctionParameterSymbol :: SymbolSing Regular FunctionParameterSymbol
  SHiddenVariableIdentifierSymbol :: SymbolSing Regular HiddenVariableIdentifierSymbol
  SVariableIdentifierSymbol :: SymbolSing Regular VariableIdentifierSymbol
  SMutFunctionParameterSymbol :: SymbolSing Regular MutFunctionParameterSymbol
  SModifierSymbol :: SymbolSing Regular ModifierSymbol
  SRetTypeSymbol :: SymbolSing Regular RetTypeSymbol
  SBlockSymbol :: SymbolSing Regular BlockSymbol
  SHiddenExpressionSymbol :: SymbolSing Regular HiddenExpressionSymbol
  SHiddenUnaryExpressionSymbol :: SymbolSing Regular HiddenUnaryExpressionSymbol
  SHiddenExpressionTermSymbol :: SymbolSing Regular HiddenExpressionTermSymbol
  SHiddenLiteralValueSymbol :: SymbolSing Regular HiddenLiteralValueSymbol
  SAddressLiteralSymbol :: SymbolSing Regular AddressLiteralSymbol
  SBoolLiteralSymbol :: SymbolSing Regular BoolLiteralSymbol
  SByteStringLiteralSymbol :: SymbolSing Regular ByteStringLiteralSymbol
  SHexStringLiteralSymbol :: SymbolSing Regular HexStringLiteralSymbol
  SAnnotationExpressionSymbol :: SymbolSing Regular AnnotationExpressionSymbol
  SBreakExpressionSymbol :: SymbolSing Regular BreakExpressionSymbol
  SLabelSymbol :: SymbolSing Regular LabelSymbol
  SCallExpressionSymbol :: SymbolSing Regular CallExpressionSymbol
  SArgListSymbol :: SymbolSing Regular ArgListSymbol
  SNameExpressionSymbol :: SymbolSing Regular NameExpressionSymbol
  SContinueExpressionSymbol :: SymbolSing Regular ContinueExpressionSymbol
  SDotExpressionSymbol :: SymbolSing Regular DotExpressionSymbol
  SExpressionListSymbol :: SymbolSing Regular ExpressionListSymbol
  SIfExpressionSymbol :: SymbolSing Regular IfExpressionSymbol
  SIndexExpressionSymbol :: SymbolSing Regular IndexExpressionSymbol
  SMacroCallExpressionSymbol :: SymbolSing Regular MacroCallExpressionSymbol
  SMacroModuleAccessSymbol :: SymbolSing Regular MacroModuleAccessSymbol
  SMatchExpressionSymbol :: SymbolSing Regular MatchExpressionSymbol
  SHiddenMatchBodySymbol :: SymbolSing Regular HiddenMatchBodySymbol
  SMatchArmSymbol :: SymbolSing Regular MatchArmSymbol
  SBindListSymbol :: SymbolSing Regular BindListSymbol
  SHiddenBindSymbol :: SymbolSing Regular HiddenBindSymbol
  SAtBindSymbol :: SymbolSing Regular AtBindSymbol
  SBindUnpackSymbol :: SymbolSing Regular BindUnpackSymbol
  SBindFieldsSymbol :: SymbolSing Regular BindFieldsSymbol
  SBindNamedFieldsSymbol :: SymbolSing Regular BindNamedFieldsSymbol
  SBindFieldSymbol :: SymbolSing Regular BindFieldSymbol
  SHiddenSpreadOperatorSymbol :: SymbolSing Regular HiddenSpreadOperatorSymbol
  SMutBindFieldSymbol :: SymbolSing Regular MutBindFieldSymbol
  SBindPositionalFieldsSymbol :: SymbolSing Regular BindPositionalFieldsSymbol
  SMutBindVarSymbol :: SymbolSing Regular MutBindVarSymbol
  SBindVarSymbol :: SymbolSing Regular BindVarSymbol
  SCommaBindListSymbol :: SymbolSing Regular CommaBindListSymbol
  SOrBindListSymbol :: SymbolSing Regular OrBindListSymbol
  SMatchConditionSymbol :: SymbolSing Regular MatchConditionSymbol
  SPackExpressionSymbol :: SymbolSing Regular PackExpressionSymbol
  SFieldInitializeListSymbol :: SymbolSing Regular FieldInitializeListSymbol
  SExpFieldSymbol :: SymbolSing Regular ExpFieldSymbol
  SSpecBlockSymbol :: SymbolSing Regular SpecBlockSymbol
  SHiddenSpecBlockTargetSymbol :: SymbolSing Regular HiddenSpecBlockTargetSymbol
  SSpecBlockTargetSchemaSymbol :: SymbolSing Regular SpecBlockTargetSchemaSymbol
  SHiddenStructIdentifierSymbol :: SymbolSing Regular HiddenStructIdentifierSymbol
  SStructIdentifierSymbol :: SymbolSing Regular StructIdentifierSymbol
  SHiddenSpecFunctionSymbol :: SymbolSing Regular HiddenSpecFunctionSymbol
  SNativeSpecFunctionSymbol :: SymbolSing Regular NativeSpecFunctionSymbol
  SHiddenSpecFunctionSignatureSymbol :: SymbolSing Regular HiddenSpecFunctionSignatureSymbol
  SUninterpretedSpecFunctionSymbol :: SymbolSing Regular UninterpretedSpecFunctionSymbol
  SUsualSpecFunctionSymbol :: SymbolSing Regular UsualSpecFunctionSymbol
  SSpecBodySymbol :: SymbolSing Regular SpecBodySymbol
  SHiddenSpecBlockMemeberSymbol :: SymbolSing Regular HiddenSpecBlockMemeberSymbol
  SSpecApplySymbol :: SymbolSing Regular SpecApplySymbol
  SSpecApplyPatternSymbol :: SymbolSing Regular SpecApplyPatternSymbol
  SSpecApplyNamePatternSymbol :: SymbolSing Regular SpecApplyNamePatternSymbol
  SSpecConditionSymbol :: SymbolSing Regular SpecConditionSymbol
  SHiddenSpecAbortIfSymbol :: SymbolSing Regular HiddenSpecAbortIfSymbol
  SConditionKindSymbol :: SymbolSing Regular ConditionKindSymbol
  SConditionPropertiesSymbol :: SymbolSing Regular ConditionPropertiesSymbol
  SSpecPropertySymbol :: SymbolSing Regular SpecPropertySymbol
  SHiddenSpecAbortWithOrModifiesSymbol :: SymbolSing Regular HiddenSpecAbortWithOrModifiesSymbol
  SHiddenSpecConditionSymbol :: SymbolSing Regular HiddenSpecConditionSymbol
  SSpecIncludeSymbol :: SymbolSing Regular SpecIncludeSymbol
  SSpecInvariantSymbol :: SymbolSing Regular SpecInvariantSymbol
  SInvariantModifierSymbol :: SymbolSing Regular InvariantModifierSymbol
  SSpecLetSymbol :: SymbolSing Regular SpecLetSymbol
  SSpecPragmaSymbol :: SymbolSing Regular SpecPragmaSymbol
  SSpecVariableSymbol :: SymbolSing Regular SpecVariableSymbol
  SUseDeclarationSymbol :: SymbolSing Regular UseDeclarationSymbol
  SUseFunSymbol :: SymbolSing Regular UseFunSymbol
  SUseModuleSymbol :: SymbolSing Regular UseModuleSymbol
  SUseModuleMemberSymbol :: SymbolSing Regular UseModuleMemberSymbol
  SUseMemberSymbol :: SymbolSing Regular UseMemberSymbol
  SUseModuleMembersSymbol :: SymbolSing Regular UseModuleMembersSymbol
  SUnitExpressionSymbol :: SymbolSing Regular UnitExpressionSymbol
  SVectorExpressionSymbol :: SymbolSing Regular VectorExpressionSymbol
  SBorrowExpressionSymbol :: SymbolSing Regular BorrowExpressionSymbol
  SDereferenceExpressionSymbol :: SymbolSing Regular DereferenceExpressionSymbol
  SMoveOrCopyExpressionSymbol :: SymbolSing Regular MoveOrCopyExpressionSymbol
  SUnaryExpressionSymbol :: SymbolSing Regular UnaryExpressionSymbol
  SUnaryOpSymbol :: SymbolSing Regular UnaryOpSymbol
  SAbortExpressionSymbol :: SymbolSing Regular AbortExpressionSymbol
  SAssignExpressionSymbol :: SymbolSing Regular AssignExpressionSymbol
  SBinaryExpressionSymbol :: SymbolSing Regular BinaryExpressionSymbol
  SBinaryOperatorSymbol :: SymbolSing Regular BinaryOperatorSymbol
  SCastExpressionSymbol :: SymbolSing Regular CastExpressionSymbol
  SIdentifiedExpressionSymbol :: SymbolSing Regular IdentifiedExpressionSymbol
  SBlockIdentifierSymbol :: SymbolSing Regular BlockIdentifierSymbol
  SLambdaExpressionSymbol :: SymbolSing Regular LambdaExpressionSymbol
  SLambdaBindingsSymbol :: SymbolSing Regular LambdaBindingsSymbol
  SLambdaBindingSymbol :: SymbolSing Regular LambdaBindingSymbol
  SLoopExpressionSymbol :: SymbolSing Regular LoopExpressionSymbol
  SQuantifierExpressionSymbol :: SymbolSing Regular QuantifierExpressionSymbol
  SHiddenExistsSymbol :: SymbolSing Regular HiddenExistsSymbol
  SHiddenForallSymbol :: SymbolSing Regular HiddenForallSymbol
  SQuantifierBindingsSymbol :: SymbolSing Regular QuantifierBindingsSymbol
  SQuantifierBindingSymbol :: SymbolSing Regular QuantifierBindingSymbol
  SReturnExpressionSymbol :: SymbolSing Regular ReturnExpressionSymbol
  SWhileExpressionSymbol :: SymbolSing Regular WhileExpressionSymbol
  SBlockItemSymbol :: SymbolSing Regular BlockItemSymbol
  SLetStatementSymbol :: SymbolSing Regular LetStatementSymbol
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
  SFalseTokSymbol :: SymbolSing Anonymous FalseTokSymbol
  SFriendTokSymbol :: SymbolSing Anonymous FriendTokSymbol
  SGlobalTokSymbol :: SymbolSing Anonymous GlobalTokSymbol
  SInternalTokSymbol :: SymbolSing Anonymous InternalTokSymbol
  SKeyTokSymbol :: SymbolSing Anonymous KeyTokSymbol
  SLocalTokSymbol :: SymbolSing Anonymous LocalTokSymbol
  SModifiesTokSymbol :: SymbolSing Anonymous ModifiesTokSymbol
  SModuleTokSymbol :: SymbolSing Anonymous ModuleTokSymbol
  SMoveTokSymbol :: SymbolSing Anonymous MoveTokSymbol
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
decSymbolSing SHiddenEnumItemSymbol SHiddenEnumItemSymbol = Just (Refl, HRefl)
decSymbolSing SEnumDefinitionSymbol SEnumDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenEnumSignatureSymbol SHiddenEnumSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenEnumIdentifierSymbol SHiddenEnumIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SEnumIdentifierSymbol SEnumIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SAbilityDeclsSymbol SAbilityDeclsSymbol = Just (Refl, HRefl)
decSymbolSing SAbilitySymbol SAbilitySymbol = Just (Refl, HRefl)
decSymbolSing STypeParametersSymbol STypeParametersSymbol = Just (Refl, HRefl)
decSymbolSing STypeParameterSymbol STypeParameterSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeParameterIdentifierSymbol SHiddenTypeParameterIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing STypeParameterIdentifierSymbol STypeParameterIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SEnumVariantsSymbol SEnumVariantsSymbol = Just (Refl, HRefl)
decSymbolSing SVariantSymbol SVariantSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenVariantIdentifierSymbol SHiddenVariantIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SVariantIdentifierSymbol SVariantIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SDatatypeFieldsSymbol SDatatypeFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SNamedFieldsSymbol SNamedFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SFieldAnnotationSymbol SFieldAnnotationSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFieldIdentifierSymbol SHiddenFieldIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SFieldIdentifierSymbol SFieldIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeSymbol SHiddenTypeSymbol = Just (Refl, HRefl)
decSymbolSing SApplyTypeSymbol SApplyTypeSymbol = Just (Refl, HRefl)
decSymbolSing SModuleAccessSymbol SModuleAccessSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenModuleIdentifierSymbol SHiddenModuleIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentifierSymbol SModuleIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SIdentifierSymbol SIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentitySymbol SModuleIdentitySymbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralSymbol SNumLiteralSymbol = Just (Refl, HRefl)
decSymbolSing STypeArgumentsSymbol STypeArgumentsSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeSymbol SFunctionTypeSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeParametersSymbol SFunctionTypeParametersSymbol = Just (Refl, HRefl)
decSymbolSing SPrimitiveTypeSymbol SPrimitiveTypeSymbol = Just (Refl, HRefl)
decSymbolSing SRefTypeSymbol SRefTypeSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenReferenceSymbol SHiddenReferenceSymbol = Just (Refl, HRefl)
decSymbolSing SImmRefSymbol SImmRefSymbol = Just (Refl, HRefl)
decSymbolSing SMutRefSymbol SMutRefSymbol = Just (Refl, HRefl)
decSymbolSing STupleTypeSymbol STupleTypeSymbol = Just (Refl, HRefl)
decSymbolSing SPositionalFieldsSymbol SPositionalFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SPostfixAbilityDeclsSymbol SPostfixAbilityDeclsSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionItemSymbol SHiddenFunctionItemSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionDefinitionSymbol SFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionSignatureSymbol SHiddenFunctionSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionIdentifierSymbol SHiddenFunctionIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionIdentifierSymbol SFunctionIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParametersSymbol SFunctionParametersSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParameterSymbol SFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenVariableIdentifierSymbol SHiddenVariableIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SVariableIdentifierSymbol SVariableIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SMutFunctionParameterSymbol SMutFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SModifierSymbol SModifierSymbol = Just (Refl, HRefl)
decSymbolSing SRetTypeSymbol SRetTypeSymbol = Just (Refl, HRefl)
decSymbolSing SBlockSymbol SBlockSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenExpressionSymbol SHiddenExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenUnaryExpressionSymbol SHiddenUnaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenExpressionTermSymbol SHiddenExpressionTermSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenLiteralValueSymbol SHiddenLiteralValueSymbol = Just (Refl, HRefl)
decSymbolSing SAddressLiteralSymbol SAddressLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SBoolLiteralSymbol SBoolLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SByteStringLiteralSymbol SByteStringLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SHexStringLiteralSymbol SHexStringLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SAnnotationExpressionSymbol SAnnotationExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBreakExpressionSymbol SBreakExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SLabelSymbol SLabelSymbol = Just (Refl, HRefl)
decSymbolSing SCallExpressionSymbol SCallExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SArgListSymbol SArgListSymbol = Just (Refl, HRefl)
decSymbolSing SNameExpressionSymbol SNameExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SContinueExpressionSymbol SContinueExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SDotExpressionSymbol SDotExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SExpressionListSymbol SExpressionListSymbol = Just (Refl, HRefl)
decSymbolSing SIfExpressionSymbol SIfExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SIndexExpressionSymbol SIndexExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMacroCallExpressionSymbol SMacroCallExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMacroModuleAccessSymbol SMacroModuleAccessSymbol = Just (Refl, HRefl)
decSymbolSing SMatchExpressionSymbol SMatchExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenMatchBodySymbol SHiddenMatchBodySymbol = Just (Refl, HRefl)
decSymbolSing SMatchArmSymbol SMatchArmSymbol = Just (Refl, HRefl)
decSymbolSing SBindListSymbol SBindListSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenBindSymbol SHiddenBindSymbol = Just (Refl, HRefl)
decSymbolSing SAtBindSymbol SAtBindSymbol = Just (Refl, HRefl)
decSymbolSing SBindUnpackSymbol SBindUnpackSymbol = Just (Refl, HRefl)
decSymbolSing SBindFieldsSymbol SBindFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SBindNamedFieldsSymbol SBindNamedFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SBindFieldSymbol SBindFieldSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpreadOperatorSymbol SHiddenSpreadOperatorSymbol = Just (Refl, HRefl)
decSymbolSing SMutBindFieldSymbol SMutBindFieldSymbol = Just (Refl, HRefl)
decSymbolSing SBindPositionalFieldsSymbol SBindPositionalFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SMutBindVarSymbol SMutBindVarSymbol = Just (Refl, HRefl)
decSymbolSing SBindVarSymbol SBindVarSymbol = Just (Refl, HRefl)
decSymbolSing SCommaBindListSymbol SCommaBindListSymbol = Just (Refl, HRefl)
decSymbolSing SOrBindListSymbol SOrBindListSymbol = Just (Refl, HRefl)
decSymbolSing SMatchConditionSymbol SMatchConditionSymbol = Just (Refl, HRefl)
decSymbolSing SPackExpressionSymbol SPackExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SFieldInitializeListSymbol SFieldInitializeListSymbol = Just (Refl, HRefl)
decSymbolSing SExpFieldSymbol SExpFieldSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockSymbol SSpecBlockSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecBlockTargetSymbol SHiddenSpecBlockTargetSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockTargetSchemaSymbol SSpecBlockTargetSchemaSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructIdentifierSymbol SHiddenStructIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SStructIdentifierSymbol SStructIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecFunctionSymbol SHiddenSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SNativeSpecFunctionSymbol SNativeSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecFunctionSignatureSymbol SHiddenSpecFunctionSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SUninterpretedSpecFunctionSymbol SUninterpretedSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SUsualSpecFunctionSymbol SUsualSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBodySymbol SSpecBodySymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecBlockMemeberSymbol SHiddenSpecBlockMemeberSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplySymbol SSpecApplySymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyPatternSymbol SSpecApplyPatternSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyNamePatternSymbol SSpecApplyNamePatternSymbol = Just (Refl, HRefl)
decSymbolSing SSpecConditionSymbol SSpecConditionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortIfSymbol SHiddenSpecAbortIfSymbol = Just (Refl, HRefl)
decSymbolSing SConditionKindSymbol SConditionKindSymbol = Just (Refl, HRefl)
decSymbolSing SConditionPropertiesSymbol SConditionPropertiesSymbol = Just (Refl, HRefl)
decSymbolSing SSpecPropertySymbol SSpecPropertySymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortWithOrModifiesSymbol SHiddenSpecAbortWithOrModifiesSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionSymbol SHiddenSpecConditionSymbol = Just (Refl, HRefl)
decSymbolSing SSpecIncludeSymbol SSpecIncludeSymbol = Just (Refl, HRefl)
decSymbolSing SSpecInvariantSymbol SSpecInvariantSymbol = Just (Refl, HRefl)
decSymbolSing SInvariantModifierSymbol SInvariantModifierSymbol = Just (Refl, HRefl)
decSymbolSing SSpecLetSymbol SSpecLetSymbol = Just (Refl, HRefl)
decSymbolSing SSpecPragmaSymbol SSpecPragmaSymbol = Just (Refl, HRefl)
decSymbolSing SSpecVariableSymbol SSpecVariableSymbol = Just (Refl, HRefl)
decSymbolSing SUseDeclarationSymbol SUseDeclarationSymbol = Just (Refl, HRefl)
decSymbolSing SUseFunSymbol SUseFunSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleSymbol SUseModuleSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleMemberSymbol SUseModuleMemberSymbol = Just (Refl, HRefl)
decSymbolSing SUseMemberSymbol SUseMemberSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleMembersSymbol SUseModuleMembersSymbol = Just (Refl, HRefl)
decSymbolSing SUnitExpressionSymbol SUnitExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SVectorExpressionSymbol SVectorExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBorrowExpressionSymbol SBorrowExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SDereferenceExpressionSymbol SDereferenceExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMoveOrCopyExpressionSymbol SMoveOrCopyExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SUnaryExpressionSymbol SUnaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SUnaryOpSymbol SUnaryOpSymbol = Just (Refl, HRefl)
decSymbolSing SAbortExpressionSymbol SAbortExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SAssignExpressionSymbol SAssignExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBinaryExpressionSymbol SBinaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBinaryOperatorSymbol SBinaryOperatorSymbol = Just (Refl, HRefl)
decSymbolSing SCastExpressionSymbol SCastExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SIdentifiedExpressionSymbol SIdentifiedExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBlockIdentifierSymbol SBlockIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaExpressionSymbol SLambdaExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaBindingsSymbol SLambdaBindingsSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaBindingSymbol SLambdaBindingSymbol = Just (Refl, HRefl)
decSymbolSing SLoopExpressionSymbol SLoopExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierExpressionSymbol SQuantifierExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenExistsSymbol SHiddenExistsSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenForallSymbol SHiddenForallSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierBindingsSymbol SQuantifierBindingsSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierBindingSymbol SQuantifierBindingSymbol = Just (Refl, HRefl)
decSymbolSing SReturnExpressionSymbol SReturnExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SWhileExpressionSymbol SWhileExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBlockItemSymbol SBlockItemSymbol = Just (Refl, HRefl)
decSymbolSing SLetStatementSymbol SLetStatementSymbol = Just (Refl, HRefl)
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
decSymbolSing SFalseTokSymbol SFalseTokSymbol = Just (Refl, HRefl)
decSymbolSing SFriendTokSymbol SFriendTokSymbol = Just (Refl, HRefl)
decSymbolSing SGlobalTokSymbol SGlobalTokSymbol = Just (Refl, HRefl)
decSymbolSing SInternalTokSymbol SInternalTokSymbol = Just (Refl, HRefl)
decSymbolSing SKeyTokSymbol SKeyTokSymbol = Just (Refl, HRefl)
decSymbolSing SLocalTokSymbol SLocalTokSymbol = Just (Refl, HRefl)
decSymbolSing SModifiesTokSymbol SModifiesTokSymbol = Just (Refl, HRefl)
decSymbolSing SModuleTokSymbol SModuleTokSymbol = Just (Refl, HRefl)
decSymbolSing SMoveTokSymbol SMoveTokSymbol = Just (Refl, HRefl)
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

symbolMap :: Map String SomeSymbolSing
symbolMap = Map.fromList
    [ ("source_file", SomeRegularSymbolSing SSourceFileSymbol)
    , ("module_definition", SomeRegularSymbolSing SModuleDefinitionSymbol)
    , ("module_body", SomeRegularSymbolSing SModuleBodySymbol)
    , ("_enum_item", SomeRegularSymbolSing SHiddenEnumItemSymbol)
    , ("enum_definition", SomeRegularSymbolSing SEnumDefinitionSymbol)
    , ("_enum_signature", SomeRegularSymbolSing SHiddenEnumSignatureSymbol)
    , ("_enum_identifier", SomeRegularSymbolSing SHiddenEnumIdentifierSymbol)
    , ("enum_identifier", SomeRegularSymbolSing SEnumIdentifierSymbol)
    , ("ability_decls", SomeRegularSymbolSing SAbilityDeclsSymbol)
    , ("ability", SomeRegularSymbolSing SAbilitySymbol)
    , ("type_parameters", SomeRegularSymbolSing STypeParametersSymbol)
    , ("type_parameter", SomeRegularSymbolSing STypeParameterSymbol)
    , ("_type_parameter_identifier", SomeRegularSymbolSing SHiddenTypeParameterIdentifierSymbol)
    , ("type_parameter_identifier", SomeRegularSymbolSing STypeParameterIdentifierSymbol)
    , ("enum_variants", SomeRegularSymbolSing SEnumVariantsSymbol)
    , ("variant", SomeRegularSymbolSing SVariantSymbol)
    , ("_variant_identifier", SomeRegularSymbolSing SHiddenVariantIdentifierSymbol)
    , ("variant_identifier", SomeRegularSymbolSing SVariantIdentifierSymbol)
    , ("datatype_fields", SomeRegularSymbolSing SDatatypeFieldsSymbol)
    , ("named_fields", SomeRegularSymbolSing SNamedFieldsSymbol)
    , ("field_annotation", SomeRegularSymbolSing SFieldAnnotationSymbol)
    , ("_field_identifier", SomeRegularSymbolSing SHiddenFieldIdentifierSymbol)
    , ("field_identifier", SomeRegularSymbolSing SFieldIdentifierSymbol)
    , ("_type", SomeRegularSymbolSing SHiddenTypeSymbol)
    , ("apply_type", SomeRegularSymbolSing SApplyTypeSymbol)
    , ("module_access", SomeRegularSymbolSing SModuleAccessSymbol)
    , ("_module_identifier", SomeRegularSymbolSing SHiddenModuleIdentifierSymbol)
    , ("module_identifier", SomeRegularSymbolSing SModuleIdentifierSymbol)
    , ("identifier", SomeRegularSymbolSing SIdentifierSymbol)
    , ("module_identity", SomeRegularSymbolSing SModuleIdentitySymbol)
    , ("num_literal", SomeRegularSymbolSing SNumLiteralSymbol)
    , ("type_arguments", SomeRegularSymbolSing STypeArgumentsSymbol)
    , ("function_type", SomeRegularSymbolSing SFunctionTypeSymbol)
    , ("function_type_parameters", SomeRegularSymbolSing SFunctionTypeParametersSymbol)
    , ("primitive_type", SomeRegularSymbolSing SPrimitiveTypeSymbol)
    , ("ref_type", SomeRegularSymbolSing SRefTypeSymbol)
    , ("_reference", SomeRegularSymbolSing SHiddenReferenceSymbol)
    , ("imm_ref", SomeRegularSymbolSing SImmRefSymbol)
    , ("mut_ref", SomeRegularSymbolSing SMutRefSymbol)
    , ("tuple_type", SomeRegularSymbolSing STupleTypeSymbol)
    , ("positional_fields", SomeRegularSymbolSing SPositionalFieldsSymbol)
    , ("postfix_ability_decls", SomeRegularSymbolSing SPostfixAbilityDeclsSymbol)
    , ("_function_item", SomeRegularSymbolSing SHiddenFunctionItemSymbol)
    , ("function_definition", SomeRegularSymbolSing SFunctionDefinitionSymbol)
    , ("_function_signature", SomeRegularSymbolSing SHiddenFunctionSignatureSymbol)
    , ("_function_identifier", SomeRegularSymbolSing SHiddenFunctionIdentifierSymbol)
    , ("function_identifier", SomeRegularSymbolSing SFunctionIdentifierSymbol)
    , ("function_parameters", SomeRegularSymbolSing SFunctionParametersSymbol)
    , ("function_parameter", SomeRegularSymbolSing SFunctionParameterSymbol)
    , ("_variable_identifier", SomeRegularSymbolSing SHiddenVariableIdentifierSymbol)
    , ("variable_identifier", SomeRegularSymbolSing SVariableIdentifierSymbol)
    , ("mut_function_parameter", SomeRegularSymbolSing SMutFunctionParameterSymbol)
    , ("modifier", SomeRegularSymbolSing SModifierSymbol)
    , ("ret_type", SomeRegularSymbolSing SRetTypeSymbol)
    , ("block", SomeRegularSymbolSing SBlockSymbol)
    , ("_expression", SomeRegularSymbolSing SHiddenExpressionSymbol)
    , ("_unary_expression", SomeRegularSymbolSing SHiddenUnaryExpressionSymbol)
    , ("_expression_term", SomeRegularSymbolSing SHiddenExpressionTermSymbol)
    , ("_literal_value", SomeRegularSymbolSing SHiddenLiteralValueSymbol)
    , ("address_literal", SomeRegularSymbolSing SAddressLiteralSymbol)
    , ("bool_literal", SomeRegularSymbolSing SBoolLiteralSymbol)
    , ("byte_string_literal", SomeRegularSymbolSing SByteStringLiteralSymbol)
    , ("hex_string_literal", SomeRegularSymbolSing SHexStringLiteralSymbol)
    , ("annotation_expression", SomeRegularSymbolSing SAnnotationExpressionSymbol)
    , ("break_expression", SomeRegularSymbolSing SBreakExpressionSymbol)
    , ("label", SomeRegularSymbolSing SLabelSymbol)
    , ("call_expression", SomeRegularSymbolSing SCallExpressionSymbol)
    , ("arg_list", SomeRegularSymbolSing SArgListSymbol)
    , ("name_expression", SomeRegularSymbolSing SNameExpressionSymbol)
    , ("continue_expression", SomeRegularSymbolSing SContinueExpressionSymbol)
    , ("dot_expression", SomeRegularSymbolSing SDotExpressionSymbol)
    , ("expression_list", SomeRegularSymbolSing SExpressionListSymbol)
    , ("if_expression", SomeRegularSymbolSing SIfExpressionSymbol)
    , ("index_expression", SomeRegularSymbolSing SIndexExpressionSymbol)
    , ("macro_call_expression", SomeRegularSymbolSing SMacroCallExpressionSymbol)
    , ("macro_module_access", SomeRegularSymbolSing SMacroModuleAccessSymbol)
    , ("match_expression", SomeRegularSymbolSing SMatchExpressionSymbol)
    , ("_match_body", SomeRegularSymbolSing SHiddenMatchBodySymbol)
    , ("match_arm", SomeRegularSymbolSing SMatchArmSymbol)
    , ("bind_list", SomeRegularSymbolSing SBindListSymbol)
    , ("_bind", SomeRegularSymbolSing SHiddenBindSymbol)
    , ("at_bind", SomeRegularSymbolSing SAtBindSymbol)
    , ("bind_unpack", SomeRegularSymbolSing SBindUnpackSymbol)
    , ("bind_fields", SomeRegularSymbolSing SBindFieldsSymbol)
    , ("bind_named_fields", SomeRegularSymbolSing SBindNamedFieldsSymbol)
    , ("bind_field", SomeRegularSymbolSing SBindFieldSymbol)
    , ("_spread_operator", SomeRegularSymbolSing SHiddenSpreadOperatorSymbol)
    , ("mut_bind_field", SomeRegularSymbolSing SMutBindFieldSymbol)
    , ("bind_positional_fields", SomeRegularSymbolSing SBindPositionalFieldsSymbol)
    , ("mut_bind_var", SomeRegularSymbolSing SMutBindVarSymbol)
    , ("bind_var", SomeRegularSymbolSing SBindVarSymbol)
    , ("comma_bind_list", SomeRegularSymbolSing SCommaBindListSymbol)
    , ("or_bind_list", SomeRegularSymbolSing SOrBindListSymbol)
    , ("match_condition", SomeRegularSymbolSing SMatchConditionSymbol)
    , ("pack_expression", SomeRegularSymbolSing SPackExpressionSymbol)
    , ("field_initialize_list", SomeRegularSymbolSing SFieldInitializeListSymbol)
    , ("exp_field", SomeRegularSymbolSing SExpFieldSymbol)
    , ("spec_block", SomeRegularSymbolSing SSpecBlockSymbol)
    , ("_spec_block_target", SomeRegularSymbolSing SHiddenSpecBlockTargetSymbol)
    , ("spec_block_target_schema", SomeRegularSymbolSing SSpecBlockTargetSchemaSymbol)
    , ("_struct_identifier", SomeRegularSymbolSing SHiddenStructIdentifierSymbol)
    , ("struct_identifier", SomeRegularSymbolSing SStructIdentifierSymbol)
    , ("_spec_function", SomeRegularSymbolSing SHiddenSpecFunctionSymbol)
    , ("native_spec_function", SomeRegularSymbolSing SNativeSpecFunctionSymbol)
    , ("_spec_function_signature", SomeRegularSymbolSing SHiddenSpecFunctionSignatureSymbol)
    , ("uninterpreted_spec_function", SomeRegularSymbolSing SUninterpretedSpecFunctionSymbol)
    , ("usual_spec_function", SomeRegularSymbolSing SUsualSpecFunctionSymbol)
    , ("spec_body", SomeRegularSymbolSing SSpecBodySymbol)
    , ("_spec_block_memeber", SomeRegularSymbolSing SHiddenSpecBlockMemeberSymbol)
    , ("spec_apply", SomeRegularSymbolSing SSpecApplySymbol)
    , ("spec_apply_pattern", SomeRegularSymbolSing SSpecApplyPatternSymbol)
    , ("spec_apply_name_pattern", SomeRegularSymbolSing SSpecApplyNamePatternSymbol)
    , ("spec_condition", SomeRegularSymbolSing SSpecConditionSymbol)
    , ("_spec_abort_if", SomeRegularSymbolSing SHiddenSpecAbortIfSymbol)
    , ("condition_kind", SomeRegularSymbolSing SConditionKindSymbol)
    , ("condition_properties", SomeRegularSymbolSing SConditionPropertiesSymbol)
    , ("spec_property", SomeRegularSymbolSing SSpecPropertySymbol)
    , ("_spec_abort_with_or_modifies", SomeRegularSymbolSing SHiddenSpecAbortWithOrModifiesSymbol)
    , ("_spec_condition", SomeRegularSymbolSing SHiddenSpecConditionSymbol)
    , ("spec_include", SomeRegularSymbolSing SSpecIncludeSymbol)
    , ("spec_invariant", SomeRegularSymbolSing SSpecInvariantSymbol)
    , ("invariant_modifier", SomeRegularSymbolSing SInvariantModifierSymbol)
    , ("spec_let", SomeRegularSymbolSing SSpecLetSymbol)
    , ("spec_pragma", SomeRegularSymbolSing SSpecPragmaSymbol)
    , ("spec_variable", SomeRegularSymbolSing SSpecVariableSymbol)
    , ("use_declaration", SomeRegularSymbolSing SUseDeclarationSymbol)
    , ("use_fun", SomeRegularSymbolSing SUseFunSymbol)
    , ("use_module", SomeRegularSymbolSing SUseModuleSymbol)
    , ("use_module_member", SomeRegularSymbolSing SUseModuleMemberSymbol)
    , ("use_member", SomeRegularSymbolSing SUseMemberSymbol)
    , ("use_module_members", SomeRegularSymbolSing SUseModuleMembersSymbol)
    , ("unit_expression", SomeRegularSymbolSing SUnitExpressionSymbol)
    , ("vector_expression", SomeRegularSymbolSing SVectorExpressionSymbol)
    , ("borrow_expression", SomeRegularSymbolSing SBorrowExpressionSymbol)
    , ("dereference_expression", SomeRegularSymbolSing SDereferenceExpressionSymbol)
    , ("move_or_copy_expression", SomeRegularSymbolSing SMoveOrCopyExpressionSymbol)
    , ("unary_expression", SomeRegularSymbolSing SUnaryExpressionSymbol)
    , ("unary_op", SomeRegularSymbolSing SUnaryOpSymbol)
    , ("abort_expression", SomeRegularSymbolSing SAbortExpressionSymbol)
    , ("assign_expression", SomeRegularSymbolSing SAssignExpressionSymbol)
    , ("binary_expression", SomeRegularSymbolSing SBinaryExpressionSymbol)
    , ("binary_operator", SomeRegularSymbolSing SBinaryOperatorSymbol)
    , ("cast_expression", SomeRegularSymbolSing SCastExpressionSymbol)
    , ("identified_expression", SomeRegularSymbolSing SIdentifiedExpressionSymbol)
    , ("block_identifier", SomeRegularSymbolSing SBlockIdentifierSymbol)
    , ("lambda_expression", SomeRegularSymbolSing SLambdaExpressionSymbol)
    , ("lambda_bindings", SomeRegularSymbolSing SLambdaBindingsSymbol)
    , ("lambda_binding", SomeRegularSymbolSing SLambdaBindingSymbol)
    , ("loop_expression", SomeRegularSymbolSing SLoopExpressionSymbol)
    , ("quantifier_expression", SomeRegularSymbolSing SQuantifierExpressionSymbol)
    , ("_exists", SomeRegularSymbolSing SHiddenExistsSymbol)
    , ("_forall", SomeRegularSymbolSing SHiddenForallSymbol)
    , ("quantifier_bindings", SomeRegularSymbolSing SQuantifierBindingsSymbol)
    , ("quantifier_binding", SomeRegularSymbolSing SQuantifierBindingSymbol)
    , ("return_expression", SomeRegularSymbolSing SReturnExpressionSymbol)
    , ("while_expression", SomeRegularSymbolSing SWhileExpressionSymbol)
    , ("block_item", SomeRegularSymbolSing SBlockItemSymbol)
    , ("let_statement", SomeRegularSymbolSing SLetStatementSymbol)
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
    , ("false", SomeAnonymousSymbolSing SFalseTokSymbol)
    , ("friend", SomeAnonymousSymbolSing SFriendTokSymbol)
    , ("global", SomeAnonymousSymbolSing SGlobalTokSymbol)
    , ("internal", SomeAnonymousSymbolSing SInternalTokSymbol)
    , ("key", SomeAnonymousSymbolSing SKeyTokSymbol)
    , ("local", SomeAnonymousSymbolSing SLocalTokSymbol)
    , ("modifies", SomeAnonymousSymbolSing SModifiesTokSymbol)
    , ("module", SomeAnonymousSymbolSing SModuleTokSymbol)
    , ("move", SomeAnonymousSymbolSing SMoveTokSymbol)
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
