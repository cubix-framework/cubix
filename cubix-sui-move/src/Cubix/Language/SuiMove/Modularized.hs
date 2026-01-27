{-# LANGUAGE TypeData #-}
{-# LANGUAGE TemplateHaskell #-}

module Cubix.Language.SuiMove.Modularized
  where

import Data.ByteString.Char8 qualified as BSC
import Data.Comp.Multi.Strategy.Classification (DynCase (..))
import Data.Foldable (foldrM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Type.Equality (type (:~:) (..), type (:~~:) (..))
import Language.Haskell.TH qualified as TH
import TreeSitter qualified as TS

import Cubix.Language.Info (TermLab)
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF (projF)
import Cubix.Language.Parametric.Syntax qualified as Syntax
import Cubix.ParsePretty (type RootSort)
import Data.Comp.Multi (Term, project)

--------------------------------------------------------------------------------
-- Labels
--------------------------------------------------------------------------------

data HiddenBindL
data HiddenBindInternal0L
data HiddenEnumIdentifierL
data HiddenEnumItemL
data HiddenEnumSignatureL
data HiddenExistsL
data HiddenExpressionL
data HiddenExpressionTermL
data HiddenFieldIdentifierL
data HiddenForallL
data HiddenFunctionIdentifierL
data HiddenFunctionItemL
data HiddenFunctionSignatureL
data HiddenLiteralValueL
data HiddenMacroSignatureL
data HiddenMatchBodyL
data HiddenModuleIdentifierL
data HiddenReferenceL
data HiddenReservedIdentifierL
data HiddenSpecAbortIfL
data HiddenSpecAbortWithOrModifiesL
data HiddenSpecAbortWithOrModifiesInternal0L
data HiddenSpecBlockMemeberL
data HiddenSpecBlockTargetL
data HiddenSpecConditionL
data HiddenSpecConditionInternal0L
data HiddenSpecConditionKindL
data HiddenSpecFunctionL
data HiddenSpecFunctionSignatureL
data HiddenSpreadOperatorL
data HiddenStructIdentifierL
data HiddenStructItemL
data HiddenStructSignatureL
data HiddenTypeL
data HiddenTypeIdentifierL
data HiddenTypeParameterIdentifierL
data HiddenUnaryExpressionL
data HiddenUnaryExpressionInternal0L
data HiddenVariableIdentifierL
data HiddenVariantIdentifierL
data HiddenWhitespaceL
data AbilityL
data AbilityDeclsL
data AbortExpressionL
data AddressLiteralL
data AnnotationL
data AnnotationExprL
data AnnotationExprInternal0L
data AnnotationExpressionL
data AnnotationItemL
data AnnotationListL
data AnnotationListInternal0L
data ApplyTypeL
data ArgListL
data AssignExpressionL
data AtBindL
data BinaryExpressionL
data BindFieldL
data BindFieldsL
data BindListL
data BindNamedFieldsL
data BindNamedFieldsInternal0L
data BindPositionalFieldsL
data BindUnpackL
data BlockL
data BlockCommentL
data BlockIdentifierL
data BlockItemL
data BlockItemInternal0L
data BoolLiteralL
data BorrowExpressionL
data BreakExpressionL
data ByteStringLiteralL
data CallExpressionL
data CastExpressionL
data CommaBindListL
data ConditionPropertiesL
data ConstantL
data ContinueExpressionL
data DatatypeFieldsL
data DereferenceExpressionL
data DotExpressionL
data EnumDefinitionL
data EnumVariantsL
data ExpFieldL
data ExpressionListL
data FieldAnnotationL
data FieldInitializeListL
data FriendAccessL
data FriendDeclarationL
data FunctionDefinitionL
data FunctionParameterL
data FunctionParameterInternal0L
data FunctionParametersL
data FunctionParametersInternal0L
data FunctionTypeL
data FunctionTypeParametersL
data HexStringLiteralL
data IdentifiedExpressionL
data IdentifierL
data IfExpressionL
data ImmRefL
data IndexExpressionL
data LabelL
data LambdaBindingL
data LambdaBindingsL
data LambdaExpressionL
data LetStatementL
data LineCommentL
data LoopExpressionL
data MacroCallExpressionL
data MacroFunctionDefinitionL
data MacroIdentifierL
data MacroModuleAccessL
data MatchArmL
data MatchConditionL
data MatchExpressionL
data ModifierL
data ModifierInternal0L
data ModuleAccessL
data ModuleBodyL
data ModuleBodyInternal0L
data ModuleBodyInternal1L
data ModuleDefinitionL
data ModuleIdentityL
data ModuleIdentityInternal0L
data MoveOrCopyExpressionL
data MoveOrCopyExpressionInternal0L
data MutBindFieldL
data MutBindVarL
data MutFunctionParameterL
data MutRefL
data NameExpressionL
data NamedFieldsL
data NativeFunctionDefinitionL
data NativeSpecFunctionL
data NativeStructDefinitionL
data NewlineL
data NumLiteralL
data NumLiteralInternal0L
data OrBindListL
data PackExpressionL
data PositionalFieldsL
data PostfixAbilityDeclsL
data PrimitiveTypeL
data QuantifierBindingL
data QuantifierBindingsL
data QuantifierExpressionL
data RefTypeL
data RetTypeL
data ReturnExpressionL
data SourceFileL
data SpecApplyL
data SpecApplyNamePatternL
data SpecApplyPatternL
data SpecApplyPatternInternal0L
data SpecBlockL
data SpecBlockInternal0L
data SpecBlockTargetFunL
data SpecBlockTargetSchemaL
data SpecBlockTargetStructL
data SpecBodyL
data SpecConditionL
data SpecIncludeL
data SpecInvariantL
data SpecInvariantInternal0L
data SpecLetL
data SpecPragmaL
data SpecPropertyL
data SpecVariableL
data SpecVariableInternal0L
data StructDefinitionL
data TupleTypeL
data TypeArgumentsL
data TypeParameterL
data TypeParametersL
data UnaryExpressionL
data UnaryOpL
data UninterpretedSpecFunctionL
data UnitExpressionL
data UseDeclarationL
data UseDeclarationInternal0L
data UseFunL
data UseMemberL
data UseModuleL
data UseModuleMemberL
data UseModuleMembersL
data UsualSpecFunctionL
data VariantL
data VectorExpressionL
data VectorExpressionInternal0L
data WhileExpressionL

data ExclamationMarkTokL
data ExclamationMarkEqualsSignTokL
data NumberSignLeftSquareBracketTokL
data DollarSignTokL
data PercentSignTokL
data AmpersandTokL
data AmpersandAmpersandTokL
data ApostropheTokL
data LeftParenthesisTokL
data RightParenthesisTokL
data AsteriskTokL
data PlusSignTokL
data CommaTokL
data HyphenMinusTokL
data HyphenMinusGreaterThanSignTokL
data FullStopTokL
data FullStopFullStopTokL
data SolidusTokL
data SolidusAsteriskTokL
data SolidusSolidusTokL
data ColonTokL
data ColonColonTokL
data SemicolonTokL
data LessThanSignTokL
data LessThanSignLessThanSignTokL
data LessThanSignEqualsSignTokL
data EqualsSignTokL
data EqualsSignEqualsSignTokL
data EqualsSignEqualsSignGreaterThanSignTokL
data EqualsSignGreaterThanSignTokL
data GreaterThanSignTokL
data GreaterThanSignEqualsSignTokL
data GreaterThanSignGreaterThanSignTokL
data CommercialAtTokL
data LeftSquareBracketTokL
data RightSquareBracketTokL
data CircumflexAccentTokL
data AbortTokL
data AbortsIfTokL
data AbortsWithTokL
data AddressTokL
data ApplyTokL
data AsTokL
data AssertTokL
data AssumeTokL
data BoolTokL
data BreakTokL
data BytearrayTokL
data ConstTokL
data ContinueTokL
data CopyTokL
data DecreasesTokL
data DropTokL
data ElseTokL
data EnsuresTokL
data EntryTokL
data EnumTokL
data ExceptTokL
data ExistsTokL
data FalseTokL
data ForallTokL
data FriendTokL
data FunTokL
data GlobalTokL
data HasTokL
data IfTokL
data InTokL
data IncludeTokL
data InternalTokL
data InvariantTokL
data KeyTokL
data LetTokL
data LocalTokL
data LoopTokL
data MacroTokL
data MatchTokL
data ModifiesTokL
data ModuleTokL
data MoveTokL
data MutTokL
data NativeTokL
data PackTokL
data PackageTokL
data PhantomTokL
data PostTokL
data PragmaTokL
data PublicTokL
data RequiresTokL
data ReturnTokL
data SchemaTokL
data SignerTokL
data SpecTokL
data StoreTokL
data StructTokL
data SucceedsIfTokL
data ToTokL
data TrueTokL
data U128TokL
data U16TokL
data U256TokL
data U32TokL
data U64TokL
data U8TokL
data UnpackTokL
data UpdateTokL
data UseTokL
data VectorLessThanSignTokL
data VectorLeftSquareBracketTokL
data WhereTokL
data WhileTokL
data WithTokL
data LeftCurlyBracketTokL
data VerticalLineTokL
data VerticalLineVerticalLineTokL
data RightCurlyBracketTokL

--------------------------------------------------------------------------------
-- Modularized syntax
--------------------------------------------------------------------------------

data Token e l where
  ExclamationMarkTok :: Token e ExclamationMarkTokL
  ExclamationMarkEqualsSignTok :: Token e ExclamationMarkEqualsSignTokL
  NumberSignLeftSquareBracketTok :: Token e NumberSignLeftSquareBracketTokL
  DollarSignTok :: Token e DollarSignTokL
  PercentSignTok :: Token e PercentSignTokL
  AmpersandTok :: Token e AmpersandTokL
  AmpersandAmpersandTok :: Token e AmpersandAmpersandTokL
  ApostropheTok :: Token e ApostropheTokL
  LeftParenthesisTok :: Token e LeftParenthesisTokL
  RightParenthesisTok :: Token e RightParenthesisTokL
  AsteriskTok :: Token e AsteriskTokL
  PlusSignTok :: Token e PlusSignTokL
  CommaTok :: Token e CommaTokL
  HyphenMinusTok :: Token e HyphenMinusTokL
  HyphenMinusGreaterThanSignTok :: Token e HyphenMinusGreaterThanSignTokL
  FullStopTok :: Token e FullStopTokL
  FullStopFullStopTok :: Token e FullStopFullStopTokL
  SolidusTok :: Token e SolidusTokL
  SolidusAsteriskTok :: Token e SolidusAsteriskTokL
  SolidusSolidusTok :: Token e SolidusSolidusTokL
  ColonTok :: Token e ColonTokL
  ColonColonTok :: Token e ColonColonTokL
  SemicolonTok :: Token e SemicolonTokL
  LessThanSignTok :: Token e LessThanSignTokL
  LessThanSignLessThanSignTok :: Token e LessThanSignLessThanSignTokL
  LessThanSignEqualsSignTok :: Token e LessThanSignEqualsSignTokL
  EqualsSignTok :: Token e EqualsSignTokL
  EqualsSignEqualsSignTok :: Token e EqualsSignEqualsSignTokL
  EqualsSignEqualsSignGreaterThanSignTok :: Token e EqualsSignEqualsSignGreaterThanSignTokL
  EqualsSignGreaterThanSignTok :: Token e EqualsSignGreaterThanSignTokL
  GreaterThanSignTok :: Token e GreaterThanSignTokL
  GreaterThanSignEqualsSignTok :: Token e GreaterThanSignEqualsSignTokL
  GreaterThanSignGreaterThanSignTok :: Token e GreaterThanSignGreaterThanSignTokL
  CommercialAtTok :: Token e CommercialAtTokL
  LeftSquareBracketTok :: Token e LeftSquareBracketTokL
  RightSquareBracketTok :: Token e RightSquareBracketTokL
  CircumflexAccentTok :: Token e CircumflexAccentTokL
  AbortTok :: Token e AbortTokL
  AbortsIfTok :: Token e AbortsIfTokL
  AbortsWithTok :: Token e AbortsWithTokL
  AddressTok :: Token e AddressTokL
  ApplyTok :: Token e ApplyTokL
  AsTok :: Token e AsTokL
  AssertTok :: Token e AssertTokL
  AssumeTok :: Token e AssumeTokL
  BoolTok :: Token e BoolTokL
  BreakTok :: Token e BreakTokL
  BytearrayTok :: Token e BytearrayTokL
  ConstTok :: Token e ConstTokL
  ContinueTok :: Token e ContinueTokL
  CopyTok :: Token e CopyTokL
  DecreasesTok :: Token e DecreasesTokL
  DropTok :: Token e DropTokL
  ElseTok :: Token e ElseTokL
  EnsuresTok :: Token e EnsuresTokL
  EntryTok :: Token e EntryTokL
  EnumTok :: Token e EnumTokL
  ExceptTok :: Token e ExceptTokL
  ExistsTok :: Token e ExistsTokL
  FalseTok :: Token e FalseTokL
  ForallTok :: Token e ForallTokL
  FriendTok :: Token e FriendTokL
  FunTok :: Token e FunTokL
  GlobalTok :: Token e GlobalTokL
  HasTok :: Token e HasTokL
  IfTok :: Token e IfTokL
  InTok :: Token e InTokL
  IncludeTok :: Token e IncludeTokL
  InternalTok :: Token e InternalTokL
  InvariantTok :: Token e InvariantTokL
  KeyTok :: Token e KeyTokL
  LetTok :: Token e LetTokL
  LocalTok :: Token e LocalTokL
  LoopTok :: Token e LoopTokL
  MacroTok :: Token e MacroTokL
  MatchTok :: Token e MatchTokL
  ModifiesTok :: Token e ModifiesTokL
  ModuleTok :: Token e ModuleTokL
  MoveTok :: Token e MoveTokL
  MutTok :: Token e MutTokL
  NativeTok :: Token e NativeTokL
  PackTok :: Token e PackTokL
  PackageTok :: Token e PackageTokL
  PhantomTok :: Token e PhantomTokL
  PostTok :: Token e PostTokL
  PragmaTok :: Token e PragmaTokL
  PublicTok :: Token e PublicTokL
  RequiresTok :: Token e RequiresTokL
  ReturnTok :: Token e ReturnTokL
  SchemaTok :: Token e SchemaTokL
  SignerTok :: Token e SignerTokL
  SpecTok :: Token e SpecTokL
  StoreTok :: Token e StoreTokL
  StructTok :: Token e StructTokL
  SucceedsIfTok :: Token e SucceedsIfTokL
  ToTok :: Token e ToTokL
  TrueTok :: Token e TrueTokL
  U128Tok :: Token e U128TokL
  U16Tok :: Token e U16TokL
  U256Tok :: Token e U256TokL
  U32Tok :: Token e U32TokL
  U64Tok :: Token e U64TokL
  U8Tok :: Token e U8TokL
  UnpackTok :: Token e UnpackTokL
  UpdateTok :: Token e UpdateTokL
  UseTok :: Token e UseTokL
  VectorLessThanSignTok :: Token e VectorLessThanSignTokL
  VectorLeftSquareBracketTok :: Token e VectorLeftSquareBracketTokL
  WhereTok :: Token e WhereTokL
  WhileTok :: Token e WhileTokL
  WithTok :: Token e WithTokL
  LeftCurlyBracketTok :: Token e LeftCurlyBracketTokL
  VerticalLineTok :: Token e VerticalLineTokL
  VerticalLineVerticalLineTok :: Token e VerticalLineVerticalLineTokL
  RightCurlyBracketTok :: Token e RightCurlyBracketTokL

deriveAllButDynCase [''Token]

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
    -> e (Maybe RightCurlyBracketTokL)
    -> ModuleBody e ModuleBodyL

data ModuleBodyInternal0 e l where
  ModuleBodyInternal0Semicolon
    :: e SemicolonTokL
    -> ModuleBodyInternal0 e ModuleBodyInternal0L
  ModuleBodyInternal0LeftCurlyBracket
    :: e LeftCurlyBracketTokL
    -> ModuleBodyInternal0 e ModuleBodyInternal0L

data ModuleBodyInternal1 e l where
  ModuleBodyInternal1UseDeclaration
    :: e UseDeclarationL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  ModuleBodyInternal1FriendDeclaration
    :: e FriendDeclarationL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  ModuleBodyInternal1Constant
    :: e ConstantL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  ModuleBodyInternal1FunctionItem
    :: e HiddenFunctionItemL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  ModuleBodyInternal1StructItem
    :: e HiddenStructItemL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  ModuleBodyInternal1EnumItem
    :: e HiddenEnumItemL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  ModuleBodyInternal1SpecBlock
    :: e SpecBlockL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L

data HiddenEnumItem e l where
  HiddenEnumItem
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
    :: e EnumTokL
    -> e HiddenEnumIdentifierL
    -> e (Maybe TypeParametersL)
    -> e (Maybe AbilityDeclsL)
    -> HiddenEnumSignature e HiddenEnumSignatureL

data HiddenEnumIdentifier e l where
  HiddenEnumIdentifier
    :: e IdentifierL
    -> HiddenEnumIdentifier e HiddenEnumIdentifierL

data Identifier e l where
  Identifier
    :: Text
    -> Identifier e IdentifierL

data AbilityDecls e l where
  AbilityDecls
    :: e HasTokL
    -> e [AbilityL]
    -> AbilityDecls e AbilityDeclsL

data Ability e l where
  AbilityCopy
    :: e CopyTokL
    -> Ability e AbilityL
  AbilityDrop
    :: e DropTokL
    -> Ability e AbilityL
  AbilityStore
    :: e StoreTokL
    -> Ability e AbilityL
  AbilityKey
    :: e KeyTokL
    -> Ability e AbilityL

data TypeParameters e l where
  TypeParameters
    :: e [TypeParameterL]
    -> TypeParameters e TypeParametersL

data TypeParameter e l where
  TypeParameter
    :: e (Maybe DollarSignTokL)
    -> e (Maybe PhantomTokL)
    -> e HiddenTypeParameterIdentifierL
    -> e (Maybe (ColonTokL, [AbilityL]))
    -> TypeParameter e TypeParameterL

data HiddenTypeParameterIdentifier e l where
  HiddenTypeParameterIdentifier
    :: e IdentifierL
    -> HiddenTypeParameterIdentifier e HiddenTypeParameterIdentifierL

data EnumVariants e l where
  EnumVariants
    :: e [VariantL]
    -> EnumVariants e EnumVariantsL

data Variant e l where
  Variant
    :: e HiddenVariantIdentifierL
    -> e (Maybe DatatypeFieldsL)
    -> Variant e VariantL

data HiddenVariantIdentifier e l where
  HiddenVariantIdentifier
    :: e IdentifierL
    -> HiddenVariantIdentifier e HiddenVariantIdentifierL

data DatatypeFields e l where
  DatatypeFieldsPositionalFields
    :: e PositionalFieldsL
    -> DatatypeFields e DatatypeFieldsL
  DatatypeFieldsNamedFields
    :: e NamedFieldsL
    -> DatatypeFields e DatatypeFieldsL

data NamedFields e l where
  NamedFields
    :: e [FieldAnnotationL]
    -> NamedFields e NamedFieldsL

data FieldAnnotation e l where
  FieldAnnotation
    :: e HiddenFieldIdentifierL
    -> e ColonTokL
    -> e HiddenTypeL
    -> FieldAnnotation e FieldAnnotationL

data HiddenFieldIdentifier e l where
  HiddenFieldIdentifier
    :: e IdentifierL
    -> HiddenFieldIdentifier e HiddenFieldIdentifierL

data HiddenType e l where
  HiddenTypeApplyType
    :: e ApplyTypeL
    -> HiddenType e HiddenTypeL
  HiddenTypeRefType
    :: e RefTypeL
    -> HiddenType e HiddenTypeL
  HiddenTypeTupleType
    :: e TupleTypeL
    -> HiddenType e HiddenTypeL
  HiddenTypeFunctionType
    :: e FunctionTypeL
    -> HiddenType e HiddenTypeL
  HiddenTypePrimitiveType
    :: e PrimitiveTypeL
    -> HiddenType e HiddenTypeL

data ApplyType e l where
  ApplyType
    :: e (ModuleAccessL, Maybe TypeArgumentsL)
    -> ApplyType e ApplyTypeL

data ModuleAccess e l where
  ModuleAccess1
    :: e DollarSignTokL
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess2
    :: e CommercialAtTokL
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess3
    :: e ModuleIdentityL
    -> e ColonColonTokL
    -> e IdentifierL
    -> e (Maybe TypeArgumentsL)
    -> e ColonColonTokL
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess4
    :: e ModuleIdentityL
    -> e ColonColonTokL
    -> e IdentifierL
    -> e TypeArgumentsL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess5
    :: e ModuleIdentityL
    -> e (Maybe TypeArgumentsL)
    -> e ColonColonTokL
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess6
    :: e HiddenModuleIdentifierL
    -> e (Maybe TypeArgumentsL)
    -> e ColonColonTokL
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess7
    :: e ModuleIdentityL
    -> e (Maybe TypeArgumentsL)
    -> ModuleAccess e ModuleAccessL
  ModuleAccess8
    :: e IdentifierL
    -> e (Maybe TypeArgumentsL)
    -> ModuleAccess e ModuleAccessL
  ModuleAccessMember
    :: e HiddenReservedIdentifierL
    -> ModuleAccess e ModuleAccessL

data HiddenModuleIdentifier e l where
  HiddenModuleIdentifier
    :: e IdentifierL
    -> HiddenModuleIdentifier e HiddenModuleIdentifierL

data HiddenReservedIdentifier e l where
  HiddenReservedIdentifierForall
    :: e HiddenForallL
    -> HiddenReservedIdentifier e HiddenReservedIdentifierL
  HiddenReservedIdentifierExists
    :: e HiddenExistsL
    -> HiddenReservedIdentifier e HiddenReservedIdentifierL

data HiddenExists e l where
  HiddenExists
    :: e ExistsTokL
    -> HiddenExists e HiddenExistsL

data HiddenForall e l where
  HiddenForall
    :: e ForallTokL
    -> HiddenForall e HiddenForallL

data ModuleIdentity e l where
  ModuleIdentity
    :: e ModuleIdentityInternal0L
    -> e ColonColonTokL
    -> e HiddenModuleIdentifierL
    -> ModuleIdentity e ModuleIdentityL

data ModuleIdentityInternal0 e l where
  ModuleIdentityInternal0NumLiteral
    :: e NumLiteralL
    -> ModuleIdentityInternal0 e ModuleIdentityInternal0L
  ModuleIdentityInternal0ModuleIdentifier
    :: e HiddenModuleIdentifierL
    -> ModuleIdentityInternal0 e ModuleIdentityInternal0L

data NumLiteral e l where
  NumLiteral
    :: Text
    -> e (Maybe NumLiteralInternal0L)
    -> NumLiteral e NumLiteralL

data NumLiteralInternal0 e l where
  NumLiteralInternal0U8
    :: e U8TokL
    -> NumLiteralInternal0 e NumLiteralInternal0L
  NumLiteralInternal0U16
    :: e U16TokL
    -> NumLiteralInternal0 e NumLiteralInternal0L
  NumLiteralInternal0U32
    :: e U32TokL
    -> NumLiteralInternal0 e NumLiteralInternal0L
  NumLiteralInternal0U64
    :: e U64TokL
    -> NumLiteralInternal0 e NumLiteralInternal0L
  NumLiteralInternal0U128
    :: e U128TokL
    -> NumLiteralInternal0 e NumLiteralInternal0L
  NumLiteralInternal0U256
    :: e U256TokL
    -> NumLiteralInternal0 e NumLiteralInternal0L

data TypeArguments e l where
  TypeArguments
    :: e [HiddenTypeL]
    -> TypeArguments e TypeArgumentsL

data FunctionType e l where
  FunctionType
    :: e FunctionTypeParametersL
    -> e (Maybe (HyphenMinusGreaterThanSignTokL, HiddenTypeL))
    -> FunctionType e FunctionTypeL

data FunctionTypeParameters e l where
  FunctionTypeParameters
    :: e [HiddenTypeL]
    -> FunctionTypeParameters e FunctionTypeParametersL

data PrimitiveType e l where
  PrimitiveTypeU8
    :: e U8TokL
    -> PrimitiveType e PrimitiveTypeL
  PrimitiveTypeU16
    :: e U16TokL
    -> PrimitiveType e PrimitiveTypeL
  PrimitiveTypeU32
    :: e U32TokL
    -> PrimitiveType e PrimitiveTypeL
  PrimitiveTypeU64
    :: e U64TokL
    -> PrimitiveType e PrimitiveTypeL
  PrimitiveTypeU128
    :: e U128TokL
    -> PrimitiveType e PrimitiveTypeL
  PrimitiveTypeU256
    :: e U256TokL
    -> PrimitiveType e PrimitiveTypeL
  PrimitiveTypeBool
    :: e BoolTokL
    -> PrimitiveType e PrimitiveTypeL
  PrimitiveTypeAddress
    :: e AddressTokL
    -> PrimitiveType e PrimitiveTypeL
  PrimitiveTypeSigner
    :: e SignerTokL
    -> PrimitiveType e PrimitiveTypeL
  PrimitiveTypeBytearray
    :: e BytearrayTokL
    -> PrimitiveType e PrimitiveTypeL

data RefType e l where
  RefType
    :: e HiddenReferenceL
    -> e HiddenTypeL
    -> RefType e RefTypeL

data HiddenReference e l where
  HiddenReferenceImmRef
    :: e ImmRefL
    -> HiddenReference e HiddenReferenceL
  HiddenReferenceMutRef
    :: e MutRefL
    -> HiddenReference e HiddenReferenceL

data ImmRef e l where
  ImmRef
    :: e AmpersandTokL
    -> ImmRef e ImmRefL

data MutRef e l where
  MutRef
    :: e AmpersandTokL
    -> e MutTokL
    -> MutRef e MutRefL

data TupleType e l where
  TupleType
    :: e [HiddenTypeL]
    -> TupleType e TupleTypeL

data PositionalFields e l where
  PositionalFields
    :: e [HiddenTypeL]
    -> PositionalFields e PositionalFieldsL

data PostfixAbilityDecls e l where
  PostfixAbilityDecls
    :: e [AbilityL]
    -> PostfixAbilityDecls e PostfixAbilityDeclsL

data HiddenFunctionItem e l where
  HiddenFunctionItemNativeFunctionDefinition
    :: e NativeFunctionDefinitionL
    -> HiddenFunctionItem e HiddenFunctionItemL
  HiddenFunctionItemMacroFunctionDefinition
    :: e MacroFunctionDefinitionL
    -> HiddenFunctionItem e HiddenFunctionItemL
  HiddenFunctionItemFunctionDefinition
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
    -> e FunTokL
    -> e HiddenFunctionIdentifierL
    -> e (Maybe TypeParametersL)
    -> e FunctionParametersL
    -> e (Maybe RetTypeL)
    -> HiddenFunctionSignature e HiddenFunctionSignatureL

data HiddenFunctionIdentifier e l where
  HiddenFunctionIdentifier
    :: e IdentifierL
    -> HiddenFunctionIdentifier e HiddenFunctionIdentifierL

data FunctionParameters e l where
  FunctionParameters
    :: e [FunctionParametersInternal0L]
    -> FunctionParameters e FunctionParametersL

data FunctionParametersInternal0 e l where
  FunctionParametersInternal0MutFunctionParameter
    :: e MutFunctionParameterL
    -> FunctionParametersInternal0 e FunctionParametersInternal0L
  FunctionParametersInternal0FunctionParameter
    :: e FunctionParameterL
    -> FunctionParametersInternal0 e FunctionParametersInternal0L

data FunctionParameter e l where
  FunctionParameter
    :: e FunctionParameterInternal0L
    -> e ColonTokL
    -> e HiddenTypeL
    -> FunctionParameter e FunctionParameterL

data FunctionParameterInternal0 e l where
  FunctionParameterInternal0Name
    :: e HiddenVariableIdentifierL
    -> FunctionParameterInternal0 e FunctionParameterInternal0L
  FunctionParameterInternal02
    :: e DollarSignTokL
    -> e HiddenVariableIdentifierL
    -> FunctionParameterInternal0 e FunctionParameterInternal0L

data HiddenVariableIdentifier e l where
  HiddenVariableIdentifier
    :: e IdentifierL
    -> HiddenVariableIdentifier e HiddenVariableIdentifierL

data MutFunctionParameter e l where
  MutFunctionParameter
    :: e MutTokL
    -> e FunctionParameterL
    -> MutFunctionParameter e MutFunctionParameterL

data Modifier e l where
  Modifier1
    :: e PublicTokL
    -> e (Maybe ModifierInternal0L)
    -> Modifier e ModifierL
  ModifierEntry
    :: e EntryTokL
    -> Modifier e ModifierL
  ModifierNative
    :: e NativeTokL
    -> Modifier e ModifierL

data ModifierInternal0 e l where
  ModifierInternal0Package
    :: e PackageTokL
    -> ModifierInternal0 e ModifierInternal0L
  ModifierInternal0Friend
    :: e FriendTokL
    -> ModifierInternal0 e ModifierInternal0L

data RetType e l where
  RetType
    :: e ColonTokL
    -> e HiddenTypeL
    -> RetType e RetTypeL

data Block e l where
  Block
    :: e LeftCurlyBracketTokL
    -> e [UseDeclarationL]
    -> e [BlockItemL]
    -> e (Maybe HiddenExpressionL)
    -> e RightCurlyBracketTokL
    -> Block e BlockL

data HiddenExpression e l where
  HiddenExpressionCallExpression
    :: e CallExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionMacroCallExpression
    :: e MacroCallExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionLambdaExpression
    :: e LambdaExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionIfExpression
    :: e IfExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionWhileExpression
    :: e WhileExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionReturnExpression
    :: e ReturnExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionAbortExpression
    :: e AbortExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionAssignExpression
    :: e AssignExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionUnaryExpression
    :: e HiddenUnaryExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionBinaryExpression
    :: e BinaryExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionCastExpression
    :: e CastExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionQuantifierExpression
    :: e QuantifierExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionMatchExpression
    :: e MatchExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionVectorExpression
    :: e VectorExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionLoopExpression
    :: e LoopExpressionL
    -> HiddenExpression e HiddenExpressionL
  HiddenExpressionIdentifiedExpression
    :: e IdentifiedExpressionL
    -> HiddenExpression e HiddenExpressionL

data HiddenUnaryExpression e l where
  HiddenUnaryExpression
    :: e HiddenUnaryExpressionInternal0L
    -> HiddenUnaryExpression e HiddenUnaryExpressionL

data HiddenUnaryExpressionInternal0 e l where
  HiddenUnaryExpressionInternal0UnaryExpression
    :: e UnaryExpressionL
    -> HiddenUnaryExpressionInternal0 e HiddenUnaryExpressionInternal0L
  HiddenUnaryExpressionInternal0BorrowExpression
    :: e BorrowExpressionL
    -> HiddenUnaryExpressionInternal0 e HiddenUnaryExpressionInternal0L
  HiddenUnaryExpressionInternal0DereferenceExpression
    :: e DereferenceExpressionL
    -> HiddenUnaryExpressionInternal0 e HiddenUnaryExpressionInternal0L
  HiddenUnaryExpressionInternal0MoveOrCopyExpression
    :: e MoveOrCopyExpressionL
    -> HiddenUnaryExpressionInternal0 e HiddenUnaryExpressionInternal0L
  HiddenUnaryExpressionInternal0ExpressionTerm
    :: e HiddenExpressionTermL
    -> HiddenUnaryExpressionInternal0 e HiddenUnaryExpressionInternal0L

data HiddenExpressionTerm e l where
  HiddenExpressionTermCallExpression
    :: e CallExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermBreakExpression
    :: e BreakExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermContinueExpression
    :: e ContinueExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermNameExpression
    :: e NameExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermMacroCallExpression
    :: e MacroCallExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermPackExpression
    :: e PackExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermLiteralValue
    :: e HiddenLiteralValueL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermUnitExpression
    :: e UnitExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermExpressionList
    :: e ExpressionListL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermAnnotationExpression
    :: e AnnotationExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermBlock
    :: e BlockL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermSpecBlock
    :: e SpecBlockL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermIfExpression
    :: e IfExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermDotExpression
    :: e DotExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermIndexExpression
    :: e IndexExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermVectorExpression
    :: e VectorExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL
  HiddenExpressionTermMatchExpression
    :: e MatchExpressionL
    -> HiddenExpressionTerm e HiddenExpressionTermL

data HiddenLiteralValue e l where
  HiddenLiteralValueAddressLiteral
    :: e AddressLiteralL
    -> HiddenLiteralValue e HiddenLiteralValueL
  HiddenLiteralValueBoolLiteral
    :: e BoolLiteralL
    -> HiddenLiteralValue e HiddenLiteralValueL
  HiddenLiteralValueNumLiteral
    :: e NumLiteralL
    -> HiddenLiteralValue e HiddenLiteralValueL
  HiddenLiteralValueHexStringLiteral
    :: e HexStringLiteralL
    -> HiddenLiteralValue e HiddenLiteralValueL
  HiddenLiteralValueByteStringLiteral
    :: e ByteStringLiteralL
    -> HiddenLiteralValue e HiddenLiteralValueL

data AddressLiteral e l where
  AddressLiteral
    :: Text
    -> AddressLiteral e AddressLiteralL

data BoolLiteral e l where
  BoolLiteralTrue
    :: e TrueTokL
    -> BoolLiteral e BoolLiteralL
  BoolLiteralFalse
    :: e FalseTokL
    -> BoolLiteral e BoolLiteralL

data ByteStringLiteral e l where
  ByteStringLiteral
    :: Text
    -> ByteStringLiteral e ByteStringLiteralL

data HexStringLiteral e l where
  HexStringLiteral
    :: Text
    -> HexStringLiteral e HexStringLiteralL

data AnnotationExpression e l where
  AnnotationExpression
    :: e HiddenExpressionL
    -> e HiddenTypeL
    -> e RightParenthesisTokL
    -> AnnotationExpression e AnnotationExpressionL

data BreakExpression e l where
  BreakExpression
    :: e BreakTokL
    -> e (Maybe LabelL)
    -> e (Maybe HiddenExpressionL)
    -> BreakExpression e BreakExpressionL

data Label e l where
  Label
    :: e ApostropheTokL
    -> e IdentifierL
    -> Label e LabelL

data CallExpression e l where
  CallExpression
    :: e (NameExpressionL, ArgListL)
    -> CallExpression e CallExpressionL

data ArgList e l where
  ArgList
    :: e [HiddenExpressionL]
    -> ArgList e ArgListL

data NameExpression e l where
  NameExpression
    :: e (Maybe ColonColonTokL)
    -> e ModuleAccessL
    -> NameExpression e NameExpressionL

data ContinueExpression e l where
  ContinueExpression
    :: e ContinueTokL
    -> e (Maybe LabelL)
    -> ContinueExpression e ContinueExpressionL

data DotExpression e l where
  DotExpression
    :: e ((HiddenExpressionTermL, FullStopTokL), HiddenExpressionTermL)
    -> DotExpression e DotExpressionL

data ExpressionList e l where
  ExpressionList
    :: e [HiddenExpressionL]
    -> ExpressionList e ExpressionListL

data IfExpression e l where
  IfExpression
    :: e (((IfTokL, HiddenExpressionL), HiddenExpressionL), Maybe (ElseTokL, HiddenExpressionL))
    -> IfExpression e IfExpressionL

data IndexExpression e l where
  IndexExpression
    :: e (HiddenExpressionTermL, [HiddenExpressionL])
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
    -> e ExclamationMarkTokL
    -> MacroModuleAccess e MacroModuleAccessL

data MatchExpression e l where
  MatchExpression
    :: e MatchTokL
    -> e HiddenExpressionL
    -> e HiddenMatchBodyL
    -> MatchExpression e MatchExpressionL

data HiddenMatchBody e l where
  HiddenMatchBody
    :: e [MatchArmL]
    -> HiddenMatchBody e HiddenMatchBodyL

data MatchArm e l where
  MatchArm
    :: e BindListL
    -> e (Maybe MatchConditionL)
    -> e EqualsSignGreaterThanSignTokL
    -> e HiddenExpressionL
    -> MatchArm e MatchArmL

data BindList e l where
  BindListBind
    :: e HiddenBindL
    -> BindList e BindListL
  BindListCommaBindList
    :: e CommaBindListL
    -> BindList e BindListL
  BindListOrBindList
    :: e OrBindListL
    -> BindList e BindListL

data HiddenBind e l where
  HiddenBindBindInternal0
    :: e HiddenBindInternal0L
    -> HiddenBind e HiddenBindL
  HiddenBindBindUnpack
    :: e BindUnpackL
    -> HiddenBind e HiddenBindL
  HiddenBindAtBind
    :: e AtBindL
    -> HiddenBind e HiddenBindL
  HiddenBindLiteralValue
    :: e HiddenLiteralValueL
    -> HiddenBind e HiddenBindL

data HiddenBindInternal0 e l where
  HiddenBindInternal0MutBindVar
    :: e MutBindVarL
    -> HiddenBindInternal0 e HiddenBindInternal0L
  HiddenBindInternal0VariableIdentifier
    :: e HiddenVariableIdentifierL
    -> HiddenBindInternal0 e HiddenBindInternal0L

data MutBindVar e l where
  MutBindVar
    :: e MutTokL
    -> e HiddenVariableIdentifierL
    -> MutBindVar e MutBindVarL

data AtBind e l where
  AtBind
    :: e HiddenVariableIdentifierL
    -> e CommercialAtTokL
    -> e BindListL
    -> AtBind e AtBindL

data BindUnpack e l where
  BindUnpack
    :: e NameExpressionL
    -> e (Maybe BindFieldsL)
    -> BindUnpack e BindUnpackL

data BindFields e l where
  BindFieldsBindPositionalFields
    :: e BindPositionalFieldsL
    -> BindFields e BindFieldsL
  BindFieldsBindNamedFields
    :: e BindNamedFieldsL
    -> BindFields e BindFieldsL

data BindNamedFields e l where
  BindNamedFields
    :: e [BindNamedFieldsInternal0L]
    -> BindNamedFields e BindNamedFieldsL

data BindNamedFieldsInternal0 e l where
  BindNamedFieldsInternal0BindField
    :: e BindFieldL
    -> BindNamedFieldsInternal0 e BindNamedFieldsInternal0L
  BindNamedFieldsInternal0MutBindField
    :: e MutBindFieldL
    -> BindNamedFieldsInternal0 e BindNamedFieldsInternal0L

data BindField e l where
  BindField1
    :: e BindListL
    -> e (Maybe (ColonTokL, BindListL))
    -> BindField e BindFieldL
  BindFieldSpreadOperator
    :: e HiddenSpreadOperatorL
    -> BindField e BindFieldL

data HiddenSpreadOperator e l where
  HiddenSpreadOperator
    :: e FullStopFullStopTokL
    -> HiddenSpreadOperator e HiddenSpreadOperatorL

data MutBindField e l where
  MutBindField
    :: e MutTokL
    -> e BindFieldL
    -> MutBindField e MutBindFieldL

data BindPositionalFields e l where
  BindPositionalFields
    :: e [BindNamedFieldsInternal0L]
    -> BindPositionalFields e BindPositionalFieldsL

data CommaBindList e l where
  CommaBindList
    :: e [HiddenBindL]
    -> CommaBindList e CommaBindListL

data OrBindList e l where
  OrBindList
    :: e (Maybe LeftParenthesisTokL)
    -> e [((Maybe LeftParenthesisTokL, HiddenBindL), Maybe RightParenthesisTokL)]
    -> e (Maybe RightParenthesisTokL)
    -> OrBindList e OrBindListL

data MatchCondition e l where
  MatchCondition
    :: e IfTokL
    -> e HiddenExpressionL
    -> MatchCondition e MatchConditionL

data PackExpression e l where
  PackExpression
    :: e NameExpressionL
    -> e FieldInitializeListL
    -> PackExpression e PackExpressionL

data FieldInitializeList e l where
  FieldInitializeList
    :: e [ExpFieldL]
    -> FieldInitializeList e FieldInitializeListL

data ExpField e l where
  ExpField
    :: e HiddenFieldIdentifierL
    -> e (Maybe (ColonTokL, HiddenExpressionL))
    -> ExpField e ExpFieldL

data SpecBlock e l where
  SpecBlock
    :: e SpecTokL
    -> e SpecBlockInternal0L
    -> SpecBlock e SpecBlockL

data SpecBlockInternal0 e l where
  SpecBlockInternal01
    :: e (Maybe HiddenSpecBlockTargetL)
    -> e SpecBodyL
    -> SpecBlockInternal0 e SpecBlockInternal0L
  SpecBlockInternal0SpecFunction
    :: e HiddenSpecFunctionL
    -> SpecBlockInternal0 e SpecBlockInternal0L

data HiddenSpecBlockTarget e l where
  HiddenSpecBlockTargetIdentifier
    :: e IdentifierL
    -> HiddenSpecBlockTarget e HiddenSpecBlockTargetL
  HiddenSpecBlockTargetModule
    :: e ModuleTokL
    -> HiddenSpecBlockTarget e HiddenSpecBlockTargetL
  HiddenSpecBlockTargetSpecBlockTargetSchema
    :: e SpecBlockTargetSchemaL
    -> HiddenSpecBlockTarget e HiddenSpecBlockTargetL

data SpecBlockTargetSchema e l where
  SpecBlockTargetSchema
    :: e SchemaTokL
    -> e HiddenStructIdentifierL
    -> e (Maybe TypeParametersL)
    -> SpecBlockTargetSchema e SpecBlockTargetSchemaL

data HiddenStructIdentifier e l where
  HiddenStructIdentifier
    :: e IdentifierL
    -> HiddenStructIdentifier e HiddenStructIdentifierL

data HiddenSpecFunction e l where
  HiddenSpecFunctionNativeSpecFunction
    :: e NativeSpecFunctionL
    -> HiddenSpecFunction e HiddenSpecFunctionL
  HiddenSpecFunctionUsualSpecFunction
    :: e UsualSpecFunctionL
    -> HiddenSpecFunction e HiddenSpecFunctionL
  HiddenSpecFunctionUninterpretedSpecFunction
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
    :: e FunTokL
    -> e HiddenSpecFunctionSignatureL
    -> e BlockL
    -> UsualSpecFunction e UsualSpecFunctionL

data SpecBody e l where
  SpecBody
    :: e LeftCurlyBracketTokL
    -> e [UseDeclarationL]
    -> e [HiddenSpecBlockMemeberL]
    -> e RightCurlyBracketTokL
    -> SpecBody e SpecBodyL

data HiddenSpecBlockMemeber e l where
  HiddenSpecBlockMemeberSpecInvariant
    :: e SpecInvariantL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  HiddenSpecBlockMemeberSpecFunction
    :: e HiddenSpecFunctionL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  HiddenSpecBlockMemeberSpecCondition
    :: e SpecConditionL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  HiddenSpecBlockMemeberSpecInclude
    :: e SpecIncludeL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  HiddenSpecBlockMemeberSpecApply
    :: e SpecApplyL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  HiddenSpecBlockMemeberSpecPragma
    :: e SpecPragmaL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  HiddenSpecBlockMemeberSpecVariable
    :: e SpecVariableL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL
  HiddenSpecBlockMemeberSpecLet
    :: e SpecLetL
    -> HiddenSpecBlockMemeber e HiddenSpecBlockMemeberL

data SpecApply e l where
  SpecApply
    :: e HiddenExpressionL
    -> e [SpecApplyPatternL]
    -> e (Maybe (ExceptTokL, [SpecApplyPatternL]))
    -> e SemicolonTokL
    -> SpecApply e SpecApplyL

data SpecApplyPattern e l where
  SpecApplyPattern
    :: e (Maybe SpecApplyPatternInternal0L)
    -> e SpecApplyNamePatternL
    -> e (Maybe TypeParametersL)
    -> SpecApplyPattern e SpecApplyPatternL

data SpecApplyNamePattern e l where
  SpecApplyNamePattern
    :: Text
    -> SpecApplyNamePattern e SpecApplyNamePatternL

data SpecApplyPatternInternal0 e l where
  SpecApplyPatternInternal0Public
    :: e PublicTokL
    -> SpecApplyPatternInternal0 e SpecApplyPatternInternal0L
  SpecApplyPatternInternal0Internal
    :: e InternalTokL
    -> SpecApplyPatternInternal0 e SpecApplyPatternInternal0L

data SpecCondition e l where
  SpecConditionSpecCondition
    :: e HiddenSpecConditionL
    -> SpecCondition e SpecConditionL
  SpecConditionSpecAbortIf
    :: e HiddenSpecAbortIfL
    -> SpecCondition e SpecConditionL
  SpecConditionSpecAbortWithOrModifies
    :: e HiddenSpecAbortWithOrModifiesL
    -> SpecCondition e SpecConditionL

data HiddenSpecAbortIf e l where
  HiddenSpecAbortIf
    :: e AbortsIfTokL
    -> e (Maybe ConditionPropertiesL)
    -> e HiddenExpressionL
    -> e (Maybe (WithTokL, HiddenExpressionL))
    -> e SemicolonTokL
    -> HiddenSpecAbortIf e HiddenSpecAbortIfL

data ConditionProperties e l where
  ConditionProperties
    :: e [SpecPropertyL]
    -> ConditionProperties e ConditionPropertiesL

data SpecProperty e l where
  SpecProperty
    :: e IdentifierL
    -> e (Maybe (EqualsSignTokL, HiddenLiteralValueL))
    -> SpecProperty e SpecPropertyL

data HiddenSpecAbortWithOrModifies e l where
  HiddenSpecAbortWithOrModifies
    :: e HiddenSpecAbortWithOrModifiesInternal0L
    -> e (Maybe ConditionPropertiesL)
    -> e [HiddenExpressionL]
    -> e SemicolonTokL
    -> HiddenSpecAbortWithOrModifies e HiddenSpecAbortWithOrModifiesL

data HiddenSpecAbortWithOrModifiesInternal0 e l where
  HiddenSpecAbortWithOrModifiesInternal0AbortsWith
    :: e AbortsWithTokL
    -> HiddenSpecAbortWithOrModifiesInternal0 e HiddenSpecAbortWithOrModifiesInternal0L
  HiddenSpecAbortWithOrModifiesInternal0Modifies
    :: e ModifiesTokL
    -> HiddenSpecAbortWithOrModifiesInternal0 e HiddenSpecAbortWithOrModifiesInternal0L

data HiddenSpecCondition e l where
  HiddenSpecCondition
    :: e HiddenSpecConditionInternal0L
    -> e (Maybe ConditionPropertiesL)
    -> e HiddenExpressionL
    -> e SemicolonTokL
    -> HiddenSpecCondition e HiddenSpecConditionL

data HiddenSpecConditionInternal0 e l where
  HiddenSpecConditionInternal0Kind
    :: e HiddenSpecConditionKindL
    -> HiddenSpecConditionInternal0 e HiddenSpecConditionInternal0L
  HiddenSpecConditionInternal02
    :: e RequiresTokL
    -> e (Maybe ModuleTokL)
    -> HiddenSpecConditionInternal0 e HiddenSpecConditionInternal0L

data HiddenSpecConditionKind e l where
  HiddenSpecConditionKindAssert
    :: e AssertTokL
    -> HiddenSpecConditionKind e HiddenSpecConditionKindL
  HiddenSpecConditionKindAssume
    :: e AssumeTokL
    -> HiddenSpecConditionKind e HiddenSpecConditionKindL
  HiddenSpecConditionKindDecreases
    :: e DecreasesTokL
    -> HiddenSpecConditionKind e HiddenSpecConditionKindL
  HiddenSpecConditionKindEnsures
    :: e EnsuresTokL
    -> HiddenSpecConditionKind e HiddenSpecConditionKindL
  HiddenSpecConditionKindSucceedsIf
    :: e SucceedsIfTokL
    -> HiddenSpecConditionKind e HiddenSpecConditionKindL

data SpecInclude e l where
  SpecInclude
    :: e HiddenExpressionL
    -> SpecInclude e SpecIncludeL

data SpecInvariant e l where
  SpecInvariant
    :: e InvariantTokL
    -> e (Maybe SpecInvariantInternal0L)
    -> e (Maybe ConditionPropertiesL)
    -> e HiddenExpressionL
    -> e SemicolonTokL
    -> SpecInvariant e SpecInvariantL

data SpecInvariantInternal0 e l where
  SpecInvariantInternal0Update
    :: e UpdateTokL
    -> SpecInvariantInternal0 e SpecInvariantInternal0L
  SpecInvariantInternal0Pack
    :: e PackTokL
    -> SpecInvariantInternal0 e SpecInvariantInternal0L
  SpecInvariantInternal0Unpack
    :: e UnpackTokL
    -> SpecInvariantInternal0 e SpecInvariantInternal0L
  SpecInvariantInternal0Module
    :: e ModuleTokL
    -> SpecInvariantInternal0 e SpecInvariantInternal0L

data SpecLet e l where
  SpecLet
    :: e LetTokL
    -> e (Maybe PostTokL)
    -> e IdentifierL
    -> e HiddenExpressionL
    -> SpecLet e SpecLetL

data SpecPragma e l where
  SpecPragma
    :: e [SpecPropertyL]
    -> SpecPragma e SpecPragmaL

data SpecVariable e l where
  SpecVariable
    :: e (Maybe SpecVariableInternal0L)
    -> e IdentifierL
    -> e (Maybe TypeParametersL)
    -> e HiddenTypeL
    -> SpecVariable e SpecVariableL

data SpecVariableInternal0 e l where
  SpecVariableInternal0Global
    :: e GlobalTokL
    -> SpecVariableInternal0 e SpecVariableInternal0L
  SpecVariableInternal0Local
    :: e LocalTokL
    -> SpecVariableInternal0 e SpecVariableInternal0L

data UseDeclaration e l where
  UseDeclaration
    :: e (Maybe PublicTokL)
    -> e UseDeclarationInternal0L
    -> UseDeclaration e UseDeclarationL

data UseDeclarationInternal0 e l where
  UseDeclarationInternal0UseFun
    :: e UseFunL
    -> UseDeclarationInternal0 e UseDeclarationInternal0L
  UseDeclarationInternal0UseModule
    :: e UseModuleL
    -> UseDeclarationInternal0 e UseDeclarationInternal0L
  UseDeclarationInternal0UseModuleMember
    :: e UseModuleMemberL
    -> UseDeclarationInternal0 e UseDeclarationInternal0L
  UseDeclarationInternal0UseModuleMembers
    :: e UseModuleMembersL
    -> UseDeclarationInternal0 e UseDeclarationInternal0L

data UseFun e l where
  UseFun
    :: e ModuleAccessL
    -> e ((ModuleAccessL, FullStopTokL), HiddenFunctionIdentifierL)
    -> UseFun e UseFunL

data UseModule e l where
  UseModule
    :: e ModuleIdentityL
    -> e (Maybe (AsTokL, HiddenModuleIdentifierL))
    -> UseModule e UseModuleL

data UseModuleMember e l where
  UseModuleMember
    :: e ModuleIdentityL
    -> e ColonColonTokL
    -> e UseMemberL
    -> UseModuleMember e UseModuleMemberL

data UseMember e l where
  UseMember1
    :: e IdentifierL
    -> e ColonColonTokL
    -> e [UseMemberL]
    -> UseMember e UseMemberL
  UseMember2
    :: e IdentifierL
    -> e ColonColonTokL
    -> e IdentifierL
    -> e (Maybe (AsTokL, IdentifierL))
    -> UseMember e UseMemberL
  UseMember3
    :: e IdentifierL
    -> e (Maybe (AsTokL, IdentifierL))
    -> UseMember e UseMemberL

data UseModuleMembers e l where
  UseModuleMembers1
    :: e ModuleIdentityInternal0L
    -> e ColonColonTokL
    -> e [UseMemberL]
    -> UseModuleMembers e UseModuleMembersL
  UseModuleMembers2
    :: e ModuleIdentityL
    -> e ColonColonTokL
    -> e [UseMemberL]
    -> UseModuleMembers e UseModuleMembersL

data UnitExpression e l where
  UnitExpression
    :: e LeftParenthesisTokL
    -> e RightParenthesisTokL
    -> UnitExpression e UnitExpressionL

data VectorExpression e l where
  VectorExpression
    :: e VectorExpressionInternal0L
    -> e [HiddenExpressionL]
    -> e RightSquareBracketTokL
    -> VectorExpression e VectorExpressionL

data VectorExpressionInternal0 e l where
  VectorExpressionInternal0VectorLeftSquareBracket
    :: e VectorLeftSquareBracketTokL
    -> VectorExpressionInternal0 e VectorExpressionInternal0L
  VectorExpressionInternal02
    :: e [HiddenTypeL]
    -> e LeftSquareBracketTokL
    -> VectorExpressionInternal0 e VectorExpressionInternal0L

data BorrowExpression e l where
  BorrowExpression
    :: e (HiddenReferenceL, HiddenExpressionL)
    -> BorrowExpression e BorrowExpressionL

data DereferenceExpression e l where
  DereferenceExpression
    :: e (AsteriskTokL, HiddenExpressionL)
    -> DereferenceExpression e DereferenceExpressionL

data MoveOrCopyExpression e l where
  MoveOrCopyExpression
    :: e (MoveOrCopyExpressionInternal0L, HiddenExpressionL)
    -> MoveOrCopyExpression e MoveOrCopyExpressionL

data MoveOrCopyExpressionInternal0 e l where
  MoveOrCopyExpressionInternal0Move
    :: e MoveTokL
    -> MoveOrCopyExpressionInternal0 e MoveOrCopyExpressionInternal0L
  MoveOrCopyExpressionInternal0Copy
    :: e CopyTokL
    -> MoveOrCopyExpressionInternal0 e MoveOrCopyExpressionInternal0L

data UnaryExpression e l where
  UnaryExpression
    :: e UnaryOpL
    -> e HiddenExpressionL
    -> UnaryExpression e UnaryExpressionL

data UnaryOp e l where
  UnaryOp
    :: e ExclamationMarkTokL
    -> UnaryOp e UnaryOpL

data AbortExpression e l where
  AbortExpression
    :: e AbortTokL
    -> e (Maybe HiddenExpressionL)
    -> AbortExpression e AbortExpressionL

data AssignExpression e l where
  AssignExpression
    :: e ((HiddenUnaryExpressionL, EqualsSignTokL), HiddenExpressionL)
    -> AssignExpression e AssignExpressionL

data BinaryExpression e l where
  BinaryExpression1
    :: e HiddenExpressionL
    -> e EqualsSignEqualsSignGreaterThanSignTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression2
    :: e HiddenExpressionL
    -> e VerticalLineVerticalLineTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression3
    :: e HiddenExpressionL
    -> e AmpersandAmpersandTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression4
    :: e HiddenExpressionL
    -> e EqualsSignEqualsSignTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression5
    :: e HiddenExpressionL
    -> e ExclamationMarkEqualsSignTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression6
    :: e HiddenExpressionL
    -> e LessThanSignTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression7
    :: e HiddenExpressionL
    -> e GreaterThanSignTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression8
    :: e HiddenExpressionL
    -> e LessThanSignEqualsSignTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression9
    :: e HiddenExpressionL
    -> e GreaterThanSignEqualsSignTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression10
    :: e HiddenExpressionL
    -> e FullStopFullStopTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression11
    :: e HiddenExpressionL
    -> e VerticalLineTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression12
    :: e HiddenExpressionL
    -> e CircumflexAccentTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression13
    :: e HiddenExpressionL
    -> e AmpersandTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression14
    :: e HiddenExpressionL
    -> e LessThanSignLessThanSignTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression15
    :: e HiddenExpressionL
    -> e GreaterThanSignGreaterThanSignTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression16
    :: e HiddenExpressionL
    -> e PlusSignTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression17
    :: e HiddenExpressionL
    -> e HyphenMinusTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression18
    :: e HiddenExpressionL
    -> e AsteriskTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression19
    :: e HiddenExpressionL
    -> e SolidusTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression20
    :: e HiddenExpressionL
    -> e PercentSignTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL

data CastExpression e l where
  CastExpression
    :: e ((HiddenExpressionL, AsTokL), HiddenTypeL)
    -> CastExpression e CastExpressionL

data IdentifiedExpression e l where
  IdentifiedExpression
    :: e BlockIdentifierL
    -> e HiddenExpressionL
    -> IdentifiedExpression e IdentifiedExpressionL

data BlockIdentifier e l where
  BlockIdentifier
    :: e LabelL
    -> e ColonTokL
    -> BlockIdentifier e BlockIdentifierL

data LambdaExpression e l where
  LambdaExpression
    :: e LambdaBindingsL
    -> e (Maybe (HyphenMinusGreaterThanSignTokL, HiddenTypeL))
    -> e HiddenExpressionL
    -> LambdaExpression e LambdaExpressionL

data LambdaBindings e l where
  LambdaBindings
    :: e [LambdaBindingL]
    -> LambdaBindings e LambdaBindingsL

data LambdaBinding e l where
  LambdaBindingCommaBindList
    :: e CommaBindListL
    -> LambdaBinding e LambdaBindingL
  LambdaBinding2
    :: e HiddenBindL
    -> e (Maybe (ColonTokL, HiddenTypeL))
    -> LambdaBinding e LambdaBindingL

data LoopExpression e l where
  LoopExpression
    :: e LoopTokL
    -> e HiddenExpressionL
    -> LoopExpression e LoopExpressionL

data QuantifierExpression e l where
  QuantifierExpression
    :: e ((((HiddenReservedIdentifierL, QuantifierBindingsL), Maybe (WhereTokL, HiddenExpressionL)), ColonTokL), HiddenExpressionL)
    -> QuantifierExpression e QuantifierExpressionL

data QuantifierBindings e l where
  QuantifierBindings
    :: e [QuantifierBindingL]
    -> QuantifierBindings e QuantifierBindingsL

data QuantifierBinding e l where
  QuantifierBinding1
    :: e IdentifierL
    -> e ColonTokL
    -> e HiddenTypeL
    -> QuantifierBinding e QuantifierBindingL
  QuantifierBinding2
    :: e IdentifierL
    -> e InTokL
    -> e HiddenExpressionL
    -> QuantifierBinding e QuantifierBindingL

data ReturnExpression e l where
  ReturnExpression1
    :: e ReturnTokL
    -> e (Maybe LabelL)
    -> e HiddenExpressionL
    -> ReturnExpression e ReturnExpressionL
  ReturnExpression2
    :: e ReturnTokL
    -> e (Maybe LabelL)
    -> ReturnExpression e ReturnExpressionL

data WhileExpression e l where
  WhileExpression
    :: e WhileTokL
    -> e HiddenExpressionL
    -> e HiddenExpressionL
    -> WhileExpression e WhileExpressionL

data BlockItem e l where
  BlockItem
    :: e BlockItemInternal0L
    -> e SemicolonTokL
    -> BlockItem e BlockItemL

data BlockItemInternal0 e l where
  BlockItemInternal0Expression
    :: e HiddenExpressionL
    -> BlockItemInternal0 e BlockItemInternal0L
  BlockItemInternal0LetStatement
    :: e LetStatementL
    -> BlockItemInternal0 e BlockItemInternal0L

data LetStatement e l where
  LetStatement
    :: e LetTokL
    -> e BindListL
    -> e (Maybe (ColonTokL, HiddenTypeL))
    -> e (Maybe (EqualsSignTokL, HiddenExpressionL))
    -> LetStatement e LetStatementL

data MacroFunctionDefinition e l where
  MacroFunctionDefinition
    :: e (Maybe ModifierL)
    -> e MacroTokL
    -> e HiddenMacroSignatureL
    -> e BlockL
    -> MacroFunctionDefinition e MacroFunctionDefinitionL

data HiddenMacroSignature e l where
  HiddenMacroSignature
    :: e (Maybe ModifierL)
    -> e FunTokL
    -> e HiddenFunctionIdentifierL
    -> e (Maybe TypeParametersL)
    -> e FunctionParametersL
    -> e (Maybe RetTypeL)
    -> HiddenMacroSignature e HiddenMacroSignatureL

data NativeFunctionDefinition e l where
  NativeFunctionDefinition
    :: e HiddenFunctionSignatureL
    -> e SemicolonTokL
    -> NativeFunctionDefinition e NativeFunctionDefinitionL

data HiddenStructItem e l where
  HiddenStructItemNativeStructDefinition
    :: e NativeStructDefinitionL
    -> HiddenStructItem e HiddenStructItemL
  HiddenStructItemStructDefinition
    :: e StructDefinitionL
    -> HiddenStructItem e HiddenStructItemL

data NativeStructDefinition e l where
  NativeStructDefinition
    :: e (Maybe PublicTokL)
    -> e HiddenStructSignatureL
    -> NativeStructDefinition e NativeStructDefinitionL

data HiddenStructSignature e l where
  HiddenStructSignature
    :: e StructTokL
    -> e HiddenStructIdentifierL
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
    :: e IdentifierL
    -> e HiddenTypeL
    -> e HiddenExpressionL
    -> Constant e ConstantL

data FriendDeclaration e l where
  FriendDeclaration
    :: e FriendAccessL
    -> FriendDeclaration e FriendDeclarationL

data FriendAccess e l where
  FriendAccessLocalModule
    :: e IdentifierL
    -> FriendAccess e FriendAccessL
  FriendAccessFullyQualifiedModule
    :: e ModuleIdentityL
    -> FriendAccess e FriendAccessL

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
  , ''Identifier
  , ''AbilityDecls
  , ''Ability
  , ''TypeParameters
  , ''TypeParameter
  , ''HiddenTypeParameterIdentifier
  , ''EnumVariants
  , ''Variant
  , ''HiddenVariantIdentifier
  , ''DatatypeFields
  , ''NamedFields
  , ''FieldAnnotation
  , ''HiddenFieldIdentifier
  , ''HiddenType
  , ''ApplyType
  , ''ModuleAccess
  , ''HiddenModuleIdentifier
  , ''HiddenReservedIdentifier
  , ''HiddenExists
  , ''HiddenForall
  , ''ModuleIdentity
  , ''ModuleIdentityInternal0
  , ''NumLiteral
  , ''NumLiteralInternal0
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
  , ''FunctionParameters
  , ''FunctionParametersInternal0
  , ''FunctionParameter
  , ''FunctionParameterInternal0
  , ''HiddenVariableIdentifier
  , ''MutFunctionParameter
  , ''Modifier
  , ''ModifierInternal0
  , ''RetType
  , ''Block
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
  , ''HiddenBindInternal0
  , ''MutBindVar
  , ''AtBind
  , ''BindUnpack
  , ''BindFields
  , ''BindNamedFields
  , ''BindNamedFieldsInternal0
  , ''BindField
  , ''HiddenSpreadOperator
  , ''MutBindField
  , ''BindPositionalFields
  , ''CommaBindList
  , ''OrBindList
  , ''MatchCondition
  , ''PackExpression
  , ''FieldInitializeList
  , ''ExpField
  , ''SpecBlock
  , ''SpecBlockInternal0
  , ''HiddenSpecBlockTarget
  , ''SpecBlockTargetSchema
  , ''HiddenStructIdentifier
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
  , ''SpecApplyPatternInternal0
  , ''SpecCondition
  , ''HiddenSpecAbortIf
  , ''ConditionProperties
  , ''SpecProperty
  , ''HiddenSpecAbortWithOrModifies
  , ''HiddenSpecAbortWithOrModifiesInternal0
  , ''HiddenSpecCondition
  , ''HiddenSpecConditionInternal0
  , ''HiddenSpecConditionKind
  , ''SpecInclude
  , ''SpecInvariant
  , ''SpecInvariantInternal0
  , ''SpecLet
  , ''SpecPragma
  , ''SpecVariable
  , ''SpecVariableInternal0
  , ''UseDeclaration
  , ''UseDeclarationInternal0
  , ''UseFun
  , ''UseModule
  , ''UseModuleMember
  , ''UseMember
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
  , ''AssignExpression
  , ''BinaryExpression
  , ''CastExpression
  , ''IdentifiedExpression
  , ''BlockIdentifier
  , ''LambdaExpression
  , ''LambdaBindings
  , ''LambdaBinding
  , ''LoopExpression
  , ''QuantifierExpression
  , ''QuantifierBindings
  , ''QuantifierBinding
  , ''ReturnExpression
  , ''WhileExpression
  , ''BlockItem
  , ''BlockItemInternal0
  , ''LetStatement
  , ''MacroFunctionDefinition
  , ''HiddenMacroSignature
  , ''NativeFunctionDefinition
  , ''HiddenStructItem
  , ''NativeStructDefinition
  , ''HiddenStructSignature
  , ''StructDefinition
  , ''Constant
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
   , ModuleBodyInternal0
   , ModuleBodyInternal1
   , HiddenEnumItem
   , EnumDefinition
   , HiddenEnumSignature
   , HiddenEnumIdentifier
   , Identifier
   , AbilityDecls
   , Ability
   , TypeParameters
   , TypeParameter
   , HiddenTypeParameterIdentifier
   , EnumVariants
   , Variant
   , HiddenVariantIdentifier
   , DatatypeFields
   , NamedFields
   , FieldAnnotation
   , HiddenFieldIdentifier
   , HiddenType
   , ApplyType
   , ModuleAccess
   , HiddenModuleIdentifier
   , HiddenReservedIdentifier
   , HiddenExists
   , HiddenForall
   , ModuleIdentity
   , ModuleIdentityInternal0
   , NumLiteral
   , NumLiteralInternal0
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
   , FunctionParameters
   , FunctionParametersInternal0
   , FunctionParameter
   , FunctionParameterInternal0
   , HiddenVariableIdentifier
   , MutFunctionParameter
   , Modifier
   , ModifierInternal0
   , RetType
   , Block
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
   , HiddenBindInternal0
   , MutBindVar
   , AtBind
   , BindUnpack
   , BindFields
   , BindNamedFields
   , BindNamedFieldsInternal0
   , BindField
   , HiddenSpreadOperator
   , MutBindField
   , BindPositionalFields
   , CommaBindList
   , OrBindList
   , MatchCondition
   , PackExpression
   , FieldInitializeList
   , ExpField
   , SpecBlock
   , SpecBlockInternal0
   , HiddenSpecBlockTarget
   , SpecBlockTargetSchema
   , HiddenStructIdentifier
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
   , SpecApplyPatternInternal0
   , SpecCondition
   , HiddenSpecAbortIf
   , ConditionProperties
   , SpecProperty
   , HiddenSpecAbortWithOrModifies
   , HiddenSpecAbortWithOrModifiesInternal0
   , HiddenSpecCondition
   , HiddenSpecConditionInternal0
   , HiddenSpecConditionKind
   , SpecInclude
   , SpecInvariant
   , SpecInvariantInternal0
   , SpecLet
   , SpecPragma
   , SpecVariable
   , SpecVariableInternal0
   , UseDeclaration
   , UseDeclarationInternal0
   , UseFun
   , UseModule
   , UseModuleMember
   , UseMember
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
   , AssignExpression
   , BinaryExpression
   , CastExpression
   , IdentifiedExpression
   , BlockIdentifier
   , LambdaExpression
   , LambdaBindings
   , LambdaBinding
   , LoopExpression
   , QuantifierExpression
   , QuantifierBindings
   , QuantifierBinding
   , ReturnExpression
   , WhileExpression
   , BlockItem
   , BlockItemInternal0
   , LetStatement
   , MacroFunctionDefinition
   , HiddenMacroSignature
   , NativeFunctionDefinition
   , HiddenStructItem
   , NativeStructDefinition
   , HiddenStructSignature
   , StructDefinition
   , Constant
   , FriendDeclaration
   , FriendAccess
   , Token
   , Syntax.PairF
   , Syntax.MaybeF
   , Syntax.ListF
   ]

type MoveTerm      = Term MoveSig
type MoveTermLab l = TermLab MoveSig l

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

type instance RootSort MoveSig = SourceFileL

--------------------------------------------------------------------------------
-- Signature TH Names
--------------------------------------------------------------------------------

moveSigNames :: [TH.Name]
moveSigNames =
   [ ''SourceFile
   , ''ModuleDefinition
   , ''ModuleBody
   , ''ModuleBodyInternal0
   , ''ModuleBodyInternal1
   , ''HiddenEnumItem
   , ''EnumDefinition
   , ''HiddenEnumSignature
   , ''HiddenEnumIdentifier
   , ''Identifier
   , ''AbilityDecls
   , ''Ability
   , ''TypeParameters
   , ''TypeParameter
   , ''HiddenTypeParameterIdentifier
   , ''EnumVariants
   , ''Variant
   , ''HiddenVariantIdentifier
   , ''DatatypeFields
   , ''NamedFields
   , ''FieldAnnotation
   , ''HiddenFieldIdentifier
   , ''HiddenType
   , ''ApplyType
   , ''ModuleAccess
   , ''HiddenModuleIdentifier
   , ''HiddenReservedIdentifier
   , ''HiddenExists
   , ''HiddenForall
   , ''ModuleIdentity
   , ''ModuleIdentityInternal0
   , ''NumLiteral
   , ''NumLiteralInternal0
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
   , ''FunctionParameters
   , ''FunctionParametersInternal0
   , ''FunctionParameter
   , ''FunctionParameterInternal0
   , ''HiddenVariableIdentifier
   , ''MutFunctionParameter
   , ''Modifier
   , ''ModifierInternal0
   , ''RetType
   , ''Block
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
   , ''HiddenBindInternal0
   , ''MutBindVar
   , ''AtBind
   , ''BindUnpack
   , ''BindFields
   , ''BindNamedFields
   , ''BindNamedFieldsInternal0
   , ''BindField
   , ''HiddenSpreadOperator
   , ''MutBindField
   , ''BindPositionalFields
   , ''CommaBindList
   , ''OrBindList
   , ''MatchCondition
   , ''PackExpression
   , ''FieldInitializeList
   , ''ExpField
   , ''SpecBlock
   , ''SpecBlockInternal0
   , ''HiddenSpecBlockTarget
   , ''SpecBlockTargetSchema
   , ''HiddenStructIdentifier
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
   , ''SpecApplyPatternInternal0
   , ''SpecCondition
   , ''HiddenSpecAbortIf
   , ''ConditionProperties
   , ''SpecProperty
   , ''HiddenSpecAbortWithOrModifies
   , ''HiddenSpecAbortWithOrModifiesInternal0
   , ''HiddenSpecCondition
   , ''HiddenSpecConditionInternal0
   , ''HiddenSpecConditionKind
   , ''SpecInclude
   , ''SpecInvariant
   , ''SpecInvariantInternal0
   , ''SpecLet
   , ''SpecPragma
   , ''SpecVariable
   , ''SpecVariableInternal0
   , ''UseDeclaration
   , ''UseDeclarationInternal0
   , ''UseFun
   , ''UseModule
   , ''UseModuleMember
   , ''UseMember
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
   , ''AssignExpression
   , ''BinaryExpression
   , ''CastExpression
   , ''IdentifiedExpression
   , ''BlockIdentifier
   , ''LambdaExpression
   , ''LambdaBindings
   , ''LambdaBinding
   , ''LoopExpression
   , ''QuantifierExpression
   , ''QuantifierBindings
   , ''QuantifierBinding
   , ''ReturnExpression
   , ''WhileExpression
   , ''BlockItem
   , ''BlockItemInternal0
   , ''LetStatement
   , ''MacroFunctionDefinition
   , ''HiddenMacroSignature
   , ''NativeFunctionDefinition
   , ''HiddenStructItem
   , ''NativeStructDefinition
   , ''HiddenStructSignature
   , ''StructDefinition
   , ''Constant
   , ''FriendDeclaration
   , ''FriendAccess
   , ''Token
   , ''Syntax.PairF
   , ''Syntax.MaybeF
   , ''Syntax.ListF
   ]

--------------------------------------------------------------------------------
-- Generated instances
--------------------------------------------------------------------------------

instance {-# OVERLAPPING #-} DynCase MoveTerm ExclamationMarkTokL where
  dyncase (project -> Just ExclamationMarkTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ExclamationMarkEqualsSignTokL where
  dyncase (project -> Just ExclamationMarkEqualsSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm NumberSignLeftSquareBracketTokL where
  dyncase (project -> Just NumberSignLeftSquareBracketTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm DollarSignTokL where
  dyncase (project -> Just DollarSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm PercentSignTokL where
  dyncase (project -> Just PercentSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm AmpersandTokL where
  dyncase (project -> Just AmpersandTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm AmpersandAmpersandTokL where
  dyncase (project -> Just AmpersandAmpersandTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ApostropheTokL where
  dyncase (project -> Just ApostropheTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm LeftParenthesisTokL where
  dyncase (project -> Just LeftParenthesisTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm RightParenthesisTokL where
  dyncase (project -> Just RightParenthesisTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm AsteriskTokL where
  dyncase (project -> Just AsteriskTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm PlusSignTokL where
  dyncase (project -> Just PlusSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm CommaTokL where
  dyncase (project -> Just CommaTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm HyphenMinusTokL where
  dyncase (project -> Just HyphenMinusTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm HyphenMinusGreaterThanSignTokL where
  dyncase (project -> Just HyphenMinusGreaterThanSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm FullStopTokL where
  dyncase (project -> Just FullStopTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm FullStopFullStopTokL where
  dyncase (project -> Just FullStopFullStopTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm SolidusTokL where
  dyncase (project -> Just SolidusTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm SolidusAsteriskTokL where
  dyncase (project -> Just SolidusAsteriskTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm SolidusSolidusTokL where
  dyncase (project -> Just SolidusSolidusTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ColonTokL where
  dyncase (project -> Just ColonTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ColonColonTokL where
  dyncase (project -> Just ColonColonTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm SemicolonTokL where
  dyncase (project -> Just SemicolonTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm LessThanSignTokL where
  dyncase (project -> Just LessThanSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm LessThanSignLessThanSignTokL where
  dyncase (project -> Just LessThanSignLessThanSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm LessThanSignEqualsSignTokL where
  dyncase (project -> Just LessThanSignEqualsSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm EqualsSignTokL where
  dyncase (project -> Just EqualsSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm EqualsSignEqualsSignTokL where
  dyncase (project -> Just EqualsSignEqualsSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm EqualsSignEqualsSignGreaterThanSignTokL where
  dyncase (project -> Just EqualsSignEqualsSignGreaterThanSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm EqualsSignGreaterThanSignTokL where
  dyncase (project -> Just EqualsSignGreaterThanSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm GreaterThanSignTokL where
  dyncase (project -> Just GreaterThanSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm GreaterThanSignEqualsSignTokL where
  dyncase (project -> Just GreaterThanSignEqualsSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm GreaterThanSignGreaterThanSignTokL where
  dyncase (project -> Just GreaterThanSignGreaterThanSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm CommercialAtTokL where
  dyncase (project -> Just CommercialAtTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm LeftSquareBracketTokL where
  dyncase (project -> Just LeftSquareBracketTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm RightSquareBracketTokL where
  dyncase (project -> Just RightSquareBracketTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm CircumflexAccentTokL where
  dyncase (project -> Just CircumflexAccentTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm AbortTokL where
  dyncase (project -> Just AbortTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm AbortsIfTokL where
  dyncase (project -> Just AbortsIfTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm AbortsWithTokL where
  dyncase (project -> Just AbortsWithTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm AddressTokL where
  dyncase (project -> Just AddressTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ApplyTokL where
  dyncase (project -> Just ApplyTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm AsTokL where
  dyncase (project -> Just AsTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm AssertTokL where
  dyncase (project -> Just AssertTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm AssumeTokL where
  dyncase (project -> Just AssumeTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm BoolTokL where
  dyncase (project -> Just BoolTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm BreakTokL where
  dyncase (project -> Just BreakTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm BytearrayTokL where
  dyncase (project -> Just BytearrayTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ConstTokL where
  dyncase (project -> Just ConstTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ContinueTokL where
  dyncase (project -> Just ContinueTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm CopyTokL where
  dyncase (project -> Just CopyTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm DecreasesTokL where
  dyncase (project -> Just DecreasesTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm DropTokL where
  dyncase (project -> Just DropTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ElseTokL where
  dyncase (project -> Just ElseTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm EnsuresTokL where
  dyncase (project -> Just EnsuresTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm EntryTokL where
  dyncase (project -> Just EntryTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm EnumTokL where
  dyncase (project -> Just EnumTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ExceptTokL where
  dyncase (project -> Just ExceptTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ExistsTokL where
  dyncase (project -> Just ExistsTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm FalseTokL where
  dyncase (project -> Just FalseTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ForallTokL where
  dyncase (project -> Just ForallTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm FriendTokL where
  dyncase (project -> Just FriendTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm FunTokL where
  dyncase (project -> Just FunTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm GlobalTokL where
  dyncase (project -> Just GlobalTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm HasTokL where
  dyncase (project -> Just HasTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm IfTokL where
  dyncase (project -> Just IfTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm InTokL where
  dyncase (project -> Just InTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm IncludeTokL where
  dyncase (project -> Just IncludeTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm InternalTokL where
  dyncase (project -> Just InternalTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm InvariantTokL where
  dyncase (project -> Just InvariantTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm KeyTokL where
  dyncase (project -> Just KeyTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm LetTokL where
  dyncase (project -> Just LetTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm LocalTokL where
  dyncase (project -> Just LocalTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm LoopTokL where
  dyncase (project -> Just LoopTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm MacroTokL where
  dyncase (project -> Just MacroTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm MatchTokL where
  dyncase (project -> Just MatchTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ModifiesTokL where
  dyncase (project -> Just ModifiesTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ModuleTokL where
  dyncase (project -> Just ModuleTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm MoveTokL where
  dyncase (project -> Just MoveTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm MutTokL where
  dyncase (project -> Just MutTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm NativeTokL where
  dyncase (project -> Just NativeTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm PackTokL where
  dyncase (project -> Just PackTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm PackageTokL where
  dyncase (project -> Just PackageTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm PhantomTokL where
  dyncase (project -> Just PhantomTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm PostTokL where
  dyncase (project -> Just PostTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm PragmaTokL where
  dyncase (project -> Just PragmaTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm PublicTokL where
  dyncase (project -> Just PublicTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm RequiresTokL where
  dyncase (project -> Just RequiresTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ReturnTokL where
  dyncase (project -> Just ReturnTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm SchemaTokL where
  dyncase (project -> Just SchemaTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm SignerTokL where
  dyncase (project -> Just SignerTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm SpecTokL where
  dyncase (project -> Just SpecTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm StoreTokL where
  dyncase (project -> Just StoreTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm StructTokL where
  dyncase (project -> Just StructTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm SucceedsIfTokL where
  dyncase (project -> Just SucceedsIfTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm ToTokL where
  dyncase (project -> Just ToTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm TrueTokL where
  dyncase (project -> Just TrueTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm U128TokL where
  dyncase (project -> Just U128Tok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm U16TokL where
  dyncase (project -> Just U16Tok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm U256TokL where
  dyncase (project -> Just U256Tok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm U32TokL where
  dyncase (project -> Just U32Tok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm U64TokL where
  dyncase (project -> Just U64Tok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm U8TokL where
  dyncase (project -> Just U8Tok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm UnpackTokL where
  dyncase (project -> Just UnpackTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm UpdateTokL where
  dyncase (project -> Just UpdateTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm UseTokL where
  dyncase (project -> Just UseTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm VectorLessThanSignTokL where
  dyncase (project -> Just VectorLessThanSignTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm VectorLeftSquareBracketTokL where
  dyncase (project -> Just VectorLeftSquareBracketTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm WhereTokL where
  dyncase (project -> Just WhereTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm WhileTokL where
  dyncase (project -> Just WhileTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm WithTokL where
  dyncase (project -> Just WithTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm LeftCurlyBracketTokL where
  dyncase (project -> Just LeftCurlyBracketTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm VerticalLineTokL where
  dyncase (project -> Just VerticalLineTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm VerticalLineVerticalLineTokL where
  dyncase (project -> Just VerticalLineVerticalLineTok) = Just Refl
  dyncase _ = Nothing

instance {-# OVERLAPPING #-} DynCase MoveTerm RightCurlyBracketTokL where
  dyncase (project -> Just RightCurlyBracketTok) = Just Refl
  dyncase _ = Nothing
