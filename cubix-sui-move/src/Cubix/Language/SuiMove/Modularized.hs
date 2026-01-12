{-# LANGUAGE TypeData #-}
{-# LANGUAGE TemplateHaskell #-}

module Cubix.Language.SuiMove.Modularized
  where

import Data.ByteString.Char8 qualified as BSC
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
import Cubix.Language.Parametric.Syntax qualified as Syntax
import Cubix.ParsePretty (type RootSort)
import Data.Comp.Multi (Term)

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
    :: e [HiddenBindL]
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
  IdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilityDeclsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeParameterIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumVariantsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VariantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenVariantIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DatatypeFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NamedFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldAnnotationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFieldIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ApplyTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenModuleIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenReservedIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExistsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenForallSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentityInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
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
  FunctionParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParametersInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParameterInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenVariableIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutFunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  RetTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  HiddenBindInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MutBindVarSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AtBindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindUnpackSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindNamedFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindNamedFieldsInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  BindFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpreadOperatorSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutBindFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindPositionalFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CommaBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  OrBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PackExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldInitializeListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExpFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecBlockTargetSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockTargetSchemaSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  SpecApplyPatternInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortIfSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConditionPropertiesSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPropertySymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortWithOrModifiesSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortWithOrModifiesInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionKindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecIncludeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecInvariantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecInvariantInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecLetSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPragmaSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecVariableSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecVariableInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  UseDeclarationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseDeclarationInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  UseFunSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleMemberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseMemberSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  AssignExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BinaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CastExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IdentifiedExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaBindingsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaBindingSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LoopExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierBindingsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierBindingSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ReturnExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  WhileExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockItemInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  LetStatementSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroFunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenMacroSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeFunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeStructDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  StructDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConstantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FriendDeclarationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FriendAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExclamationMarkTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExclamationMarkEqualsSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NumberSignLeftSquareBracketTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DollarSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PercentSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AmpersandTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AmpersandAmpersandTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ApostropheTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LeftParenthesisTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RightParenthesisTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AsteriskTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PlusSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CommaTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HyphenMinusTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HyphenMinusGreaterThanSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FullStopTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FullStopFullStopTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SolidusTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SolidusAsteriskTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SolidusSolidusTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ColonTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ColonColonTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SemicolonTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LessThanSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LessThanSignLessThanSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LessThanSignEqualsSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EqualsSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EqualsSignEqualsSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EqualsSignEqualsSignGreaterThanSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EqualsSignGreaterThanSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  GreaterThanSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  GreaterThanSignEqualsSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  GreaterThanSignGreaterThanSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CommercialAtTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LeftSquareBracketTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RightSquareBracketTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CircumflexAccentTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbortTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbortsIfTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbortsWithTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AddressTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ApplyTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AsTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AssertTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AssumeTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BoolTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BreakTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BytearrayTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConstTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ContinueTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CopyTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DecreasesTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DropTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ElseTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnsuresTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EntryTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExceptTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExistsTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FalseTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ForallTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FriendTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  GlobalTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HasTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IfTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  InTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IncludeTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  InternalTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  InvariantTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  KeyTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LetTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LocalTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LoopTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifiesTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MoveTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PackTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PackageTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PhantomTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PostTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PragmaTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PublicTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RequiresTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ReturnTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SchemaTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SignerTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  StoreTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  StructTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SucceedsIfTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ToTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TrueTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U128TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U16TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U256TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U32TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U64TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  U8TokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UnpackTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UpdateTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VectorLessThanSignTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VectorLeftSquareBracketTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  WhereTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  WhileTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  WithTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LeftCurlyBracketTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VerticalLineTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VerticalLineVerticalLineTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RightCurlyBracketTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  SIdentifierSymbol :: SymbolSing Regular IdentifierSymbol
  SAbilityDeclsSymbol :: SymbolSing Regular AbilityDeclsSymbol
  SAbilitySymbol :: SymbolSing Regular AbilitySymbol
  STypeParametersSymbol :: SymbolSing Regular TypeParametersSymbol
  STypeParameterSymbol :: SymbolSing Regular TypeParameterSymbol
  SHiddenTypeParameterIdentifierSymbol :: SymbolSing Regular HiddenTypeParameterIdentifierSymbol
  SEnumVariantsSymbol :: SymbolSing Regular EnumVariantsSymbol
  SVariantSymbol :: SymbolSing Regular VariantSymbol
  SHiddenVariantIdentifierSymbol :: SymbolSing Regular HiddenVariantIdentifierSymbol
  SDatatypeFieldsSymbol :: SymbolSing Regular DatatypeFieldsSymbol
  SNamedFieldsSymbol :: SymbolSing Regular NamedFieldsSymbol
  SFieldAnnotationSymbol :: SymbolSing Regular FieldAnnotationSymbol
  SHiddenFieldIdentifierSymbol :: SymbolSing Regular HiddenFieldIdentifierSymbol
  SHiddenTypeSymbol :: SymbolSing Regular HiddenTypeSymbol
  SApplyTypeSymbol :: SymbolSing Regular ApplyTypeSymbol
  SModuleAccessSymbol :: SymbolSing Regular ModuleAccessSymbol
  SHiddenModuleIdentifierSymbol :: SymbolSing Regular HiddenModuleIdentifierSymbol
  SHiddenReservedIdentifierSymbol :: SymbolSing Regular HiddenReservedIdentifierSymbol
  SHiddenExistsSymbol :: SymbolSing Regular HiddenExistsSymbol
  SHiddenForallSymbol :: SymbolSing Regular HiddenForallSymbol
  SModuleIdentitySymbol :: SymbolSing Regular ModuleIdentitySymbol
  SModuleIdentityInternal0Symbol :: SymbolSing Regular ModuleIdentityInternal0Symbol
  SNumLiteralSymbol :: SymbolSing Regular NumLiteralSymbol
  SNumLiteralInternal0Symbol :: SymbolSing Regular NumLiteralInternal0Symbol
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
  SFunctionParametersSymbol :: SymbolSing Regular FunctionParametersSymbol
  SFunctionParametersInternal0Symbol :: SymbolSing Regular FunctionParametersInternal0Symbol
  SFunctionParameterSymbol :: SymbolSing Regular FunctionParameterSymbol
  SFunctionParameterInternal0Symbol :: SymbolSing Regular FunctionParameterInternal0Symbol
  SHiddenVariableIdentifierSymbol :: SymbolSing Regular HiddenVariableIdentifierSymbol
  SMutFunctionParameterSymbol :: SymbolSing Regular MutFunctionParameterSymbol
  SModifierSymbol :: SymbolSing Regular ModifierSymbol
  SModifierInternal0Symbol :: SymbolSing Regular ModifierInternal0Symbol
  SRetTypeSymbol :: SymbolSing Regular RetTypeSymbol
  SBlockSymbol :: SymbolSing Regular BlockSymbol
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
  SHiddenBindInternal0Symbol :: SymbolSing Regular HiddenBindInternal0Symbol
  SMutBindVarSymbol :: SymbolSing Regular MutBindVarSymbol
  SAtBindSymbol :: SymbolSing Regular AtBindSymbol
  SBindUnpackSymbol :: SymbolSing Regular BindUnpackSymbol
  SBindFieldsSymbol :: SymbolSing Regular BindFieldsSymbol
  SBindNamedFieldsSymbol :: SymbolSing Regular BindNamedFieldsSymbol
  SBindNamedFieldsInternal0Symbol :: SymbolSing Regular BindNamedFieldsInternal0Symbol
  SBindFieldSymbol :: SymbolSing Regular BindFieldSymbol
  SHiddenSpreadOperatorSymbol :: SymbolSing Regular HiddenSpreadOperatorSymbol
  SMutBindFieldSymbol :: SymbolSing Regular MutBindFieldSymbol
  SBindPositionalFieldsSymbol :: SymbolSing Regular BindPositionalFieldsSymbol
  SCommaBindListSymbol :: SymbolSing Regular CommaBindListSymbol
  SOrBindListSymbol :: SymbolSing Regular OrBindListSymbol
  SMatchConditionSymbol :: SymbolSing Regular MatchConditionSymbol
  SPackExpressionSymbol :: SymbolSing Regular PackExpressionSymbol
  SFieldInitializeListSymbol :: SymbolSing Regular FieldInitializeListSymbol
  SExpFieldSymbol :: SymbolSing Regular ExpFieldSymbol
  SSpecBlockSymbol :: SymbolSing Regular SpecBlockSymbol
  SSpecBlockInternal0Symbol :: SymbolSing Regular SpecBlockInternal0Symbol
  SHiddenSpecBlockTargetSymbol :: SymbolSing Regular HiddenSpecBlockTargetSymbol
  SSpecBlockTargetSchemaSymbol :: SymbolSing Regular SpecBlockTargetSchemaSymbol
  SHiddenStructIdentifierSymbol :: SymbolSing Regular HiddenStructIdentifierSymbol
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
  SSpecApplyPatternInternal0Symbol :: SymbolSing Regular SpecApplyPatternInternal0Symbol
  SSpecConditionSymbol :: SymbolSing Regular SpecConditionSymbol
  SHiddenSpecAbortIfSymbol :: SymbolSing Regular HiddenSpecAbortIfSymbol
  SConditionPropertiesSymbol :: SymbolSing Regular ConditionPropertiesSymbol
  SSpecPropertySymbol :: SymbolSing Regular SpecPropertySymbol
  SHiddenSpecAbortWithOrModifiesSymbol :: SymbolSing Regular HiddenSpecAbortWithOrModifiesSymbol
  SHiddenSpecAbortWithOrModifiesInternal0Symbol :: SymbolSing Regular HiddenSpecAbortWithOrModifiesInternal0Symbol
  SHiddenSpecConditionSymbol :: SymbolSing Regular HiddenSpecConditionSymbol
  SHiddenSpecConditionInternal0Symbol :: SymbolSing Regular HiddenSpecConditionInternal0Symbol
  SHiddenSpecConditionKindSymbol :: SymbolSing Regular HiddenSpecConditionKindSymbol
  SSpecIncludeSymbol :: SymbolSing Regular SpecIncludeSymbol
  SSpecInvariantSymbol :: SymbolSing Regular SpecInvariantSymbol
  SSpecInvariantInternal0Symbol :: SymbolSing Regular SpecInvariantInternal0Symbol
  SSpecLetSymbol :: SymbolSing Regular SpecLetSymbol
  SSpecPragmaSymbol :: SymbolSing Regular SpecPragmaSymbol
  SSpecVariableSymbol :: SymbolSing Regular SpecVariableSymbol
  SSpecVariableInternal0Symbol :: SymbolSing Regular SpecVariableInternal0Symbol
  SUseDeclarationSymbol :: SymbolSing Regular UseDeclarationSymbol
  SUseDeclarationInternal0Symbol :: SymbolSing Regular UseDeclarationInternal0Symbol
  SUseFunSymbol :: SymbolSing Regular UseFunSymbol
  SUseModuleSymbol :: SymbolSing Regular UseModuleSymbol
  SUseModuleMemberSymbol :: SymbolSing Regular UseModuleMemberSymbol
  SUseMemberSymbol :: SymbolSing Regular UseMemberSymbol
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
  SAssignExpressionSymbol :: SymbolSing Regular AssignExpressionSymbol
  SBinaryExpressionSymbol :: SymbolSing Regular BinaryExpressionSymbol
  SCastExpressionSymbol :: SymbolSing Regular CastExpressionSymbol
  SIdentifiedExpressionSymbol :: SymbolSing Regular IdentifiedExpressionSymbol
  SBlockIdentifierSymbol :: SymbolSing Regular BlockIdentifierSymbol
  SLambdaExpressionSymbol :: SymbolSing Regular LambdaExpressionSymbol
  SLambdaBindingsSymbol :: SymbolSing Regular LambdaBindingsSymbol
  SLambdaBindingSymbol :: SymbolSing Regular LambdaBindingSymbol
  SLoopExpressionSymbol :: SymbolSing Regular LoopExpressionSymbol
  SQuantifierExpressionSymbol :: SymbolSing Regular QuantifierExpressionSymbol
  SQuantifierBindingsSymbol :: SymbolSing Regular QuantifierBindingsSymbol
  SQuantifierBindingSymbol :: SymbolSing Regular QuantifierBindingSymbol
  SReturnExpressionSymbol :: SymbolSing Regular ReturnExpressionSymbol
  SWhileExpressionSymbol :: SymbolSing Regular WhileExpressionSymbol
  SBlockItemSymbol :: SymbolSing Regular BlockItemSymbol
  SBlockItemInternal0Symbol :: SymbolSing Regular BlockItemInternal0Symbol
  SLetStatementSymbol :: SymbolSing Regular LetStatementSymbol
  SMacroFunctionDefinitionSymbol :: SymbolSing Regular MacroFunctionDefinitionSymbol
  SHiddenMacroSignatureSymbol :: SymbolSing Regular HiddenMacroSignatureSymbol
  SNativeFunctionDefinitionSymbol :: SymbolSing Regular NativeFunctionDefinitionSymbol
  SHiddenStructItemSymbol :: SymbolSing Regular HiddenStructItemSymbol
  SNativeStructDefinitionSymbol :: SymbolSing Regular NativeStructDefinitionSymbol
  SHiddenStructSignatureSymbol :: SymbolSing Regular HiddenStructSignatureSymbol
  SStructDefinitionSymbol :: SymbolSing Regular StructDefinitionSymbol
  SConstantSymbol :: SymbolSing Regular ConstantSymbol
  SFriendDeclarationSymbol :: SymbolSing Regular FriendDeclarationSymbol
  SFriendAccessSymbol :: SymbolSing Regular FriendAccessSymbol
  SExclamationMarkTokSymbol :: SymbolSing Anonymous ExclamationMarkTokSymbol
  SExclamationMarkEqualsSignTokSymbol :: SymbolSing Anonymous ExclamationMarkEqualsSignTokSymbol
  SNumberSignLeftSquareBracketTokSymbol :: SymbolSing Anonymous NumberSignLeftSquareBracketTokSymbol
  SDollarSignTokSymbol :: SymbolSing Anonymous DollarSignTokSymbol
  SPercentSignTokSymbol :: SymbolSing Anonymous PercentSignTokSymbol
  SAmpersandTokSymbol :: SymbolSing Anonymous AmpersandTokSymbol
  SAmpersandAmpersandTokSymbol :: SymbolSing Anonymous AmpersandAmpersandTokSymbol
  SApostropheTokSymbol :: SymbolSing Anonymous ApostropheTokSymbol
  SLeftParenthesisTokSymbol :: SymbolSing Anonymous LeftParenthesisTokSymbol
  SRightParenthesisTokSymbol :: SymbolSing Anonymous RightParenthesisTokSymbol
  SAsteriskTokSymbol :: SymbolSing Anonymous AsteriskTokSymbol
  SPlusSignTokSymbol :: SymbolSing Anonymous PlusSignTokSymbol
  SCommaTokSymbol :: SymbolSing Anonymous CommaTokSymbol
  SHyphenMinusTokSymbol :: SymbolSing Anonymous HyphenMinusTokSymbol
  SHyphenMinusGreaterThanSignTokSymbol :: SymbolSing Anonymous HyphenMinusGreaterThanSignTokSymbol
  SFullStopTokSymbol :: SymbolSing Anonymous FullStopTokSymbol
  SFullStopFullStopTokSymbol :: SymbolSing Anonymous FullStopFullStopTokSymbol
  SSolidusTokSymbol :: SymbolSing Anonymous SolidusTokSymbol
  SSolidusAsteriskTokSymbol :: SymbolSing Anonymous SolidusAsteriskTokSymbol
  SSolidusSolidusTokSymbol :: SymbolSing Anonymous SolidusSolidusTokSymbol
  SColonTokSymbol :: SymbolSing Anonymous ColonTokSymbol
  SColonColonTokSymbol :: SymbolSing Anonymous ColonColonTokSymbol
  SSemicolonTokSymbol :: SymbolSing Anonymous SemicolonTokSymbol
  SLessThanSignTokSymbol :: SymbolSing Anonymous LessThanSignTokSymbol
  SLessThanSignLessThanSignTokSymbol :: SymbolSing Anonymous LessThanSignLessThanSignTokSymbol
  SLessThanSignEqualsSignTokSymbol :: SymbolSing Anonymous LessThanSignEqualsSignTokSymbol
  SEqualsSignTokSymbol :: SymbolSing Anonymous EqualsSignTokSymbol
  SEqualsSignEqualsSignTokSymbol :: SymbolSing Anonymous EqualsSignEqualsSignTokSymbol
  SEqualsSignEqualsSignGreaterThanSignTokSymbol :: SymbolSing Anonymous EqualsSignEqualsSignGreaterThanSignTokSymbol
  SEqualsSignGreaterThanSignTokSymbol :: SymbolSing Anonymous EqualsSignGreaterThanSignTokSymbol
  SGreaterThanSignTokSymbol :: SymbolSing Anonymous GreaterThanSignTokSymbol
  SGreaterThanSignEqualsSignTokSymbol :: SymbolSing Anonymous GreaterThanSignEqualsSignTokSymbol
  SGreaterThanSignGreaterThanSignTokSymbol :: SymbolSing Anonymous GreaterThanSignGreaterThanSignTokSymbol
  SCommercialAtTokSymbol :: SymbolSing Anonymous CommercialAtTokSymbol
  SLeftSquareBracketTokSymbol :: SymbolSing Anonymous LeftSquareBracketTokSymbol
  SRightSquareBracketTokSymbol :: SymbolSing Anonymous RightSquareBracketTokSymbol
  SCircumflexAccentTokSymbol :: SymbolSing Anonymous CircumflexAccentTokSymbol
  SAbortTokSymbol :: SymbolSing Anonymous AbortTokSymbol
  SAbortsIfTokSymbol :: SymbolSing Anonymous AbortsIfTokSymbol
  SAbortsWithTokSymbol :: SymbolSing Anonymous AbortsWithTokSymbol
  SAddressTokSymbol :: SymbolSing Anonymous AddressTokSymbol
  SApplyTokSymbol :: SymbolSing Anonymous ApplyTokSymbol
  SAsTokSymbol :: SymbolSing Anonymous AsTokSymbol
  SAssertTokSymbol :: SymbolSing Anonymous AssertTokSymbol
  SAssumeTokSymbol :: SymbolSing Anonymous AssumeTokSymbol
  SBoolTokSymbol :: SymbolSing Anonymous BoolTokSymbol
  SBreakTokSymbol :: SymbolSing Anonymous BreakTokSymbol
  SBytearrayTokSymbol :: SymbolSing Anonymous BytearrayTokSymbol
  SConstTokSymbol :: SymbolSing Anonymous ConstTokSymbol
  SContinueTokSymbol :: SymbolSing Anonymous ContinueTokSymbol
  SCopyTokSymbol :: SymbolSing Anonymous CopyTokSymbol
  SDecreasesTokSymbol :: SymbolSing Anonymous DecreasesTokSymbol
  SDropTokSymbol :: SymbolSing Anonymous DropTokSymbol
  SElseTokSymbol :: SymbolSing Anonymous ElseTokSymbol
  SEnsuresTokSymbol :: SymbolSing Anonymous EnsuresTokSymbol
  SEntryTokSymbol :: SymbolSing Anonymous EntryTokSymbol
  SEnumTokSymbol :: SymbolSing Anonymous EnumTokSymbol
  SExceptTokSymbol :: SymbolSing Anonymous ExceptTokSymbol
  SExistsTokSymbol :: SymbolSing Anonymous ExistsTokSymbol
  SFalseTokSymbol :: SymbolSing Anonymous FalseTokSymbol
  SForallTokSymbol :: SymbolSing Anonymous ForallTokSymbol
  SFriendTokSymbol :: SymbolSing Anonymous FriendTokSymbol
  SFunTokSymbol :: SymbolSing Anonymous FunTokSymbol
  SGlobalTokSymbol :: SymbolSing Anonymous GlobalTokSymbol
  SHasTokSymbol :: SymbolSing Anonymous HasTokSymbol
  SIfTokSymbol :: SymbolSing Anonymous IfTokSymbol
  SInTokSymbol :: SymbolSing Anonymous InTokSymbol
  SIncludeTokSymbol :: SymbolSing Anonymous IncludeTokSymbol
  SInternalTokSymbol :: SymbolSing Anonymous InternalTokSymbol
  SInvariantTokSymbol :: SymbolSing Anonymous InvariantTokSymbol
  SKeyTokSymbol :: SymbolSing Anonymous KeyTokSymbol
  SLetTokSymbol :: SymbolSing Anonymous LetTokSymbol
  SLocalTokSymbol :: SymbolSing Anonymous LocalTokSymbol
  SLoopTokSymbol :: SymbolSing Anonymous LoopTokSymbol
  SMacroTokSymbol :: SymbolSing Anonymous MacroTokSymbol
  SMatchTokSymbol :: SymbolSing Anonymous MatchTokSymbol
  SModifiesTokSymbol :: SymbolSing Anonymous ModifiesTokSymbol
  SModuleTokSymbol :: SymbolSing Anonymous ModuleTokSymbol
  SMoveTokSymbol :: SymbolSing Anonymous MoveTokSymbol
  SMutTokSymbol :: SymbolSing Anonymous MutTokSymbol
  SNativeTokSymbol :: SymbolSing Anonymous NativeTokSymbol
  SPackTokSymbol :: SymbolSing Anonymous PackTokSymbol
  SPackageTokSymbol :: SymbolSing Anonymous PackageTokSymbol
  SPhantomTokSymbol :: SymbolSing Anonymous PhantomTokSymbol
  SPostTokSymbol :: SymbolSing Anonymous PostTokSymbol
  SPragmaTokSymbol :: SymbolSing Anonymous PragmaTokSymbol
  SPublicTokSymbol :: SymbolSing Anonymous PublicTokSymbol
  SRequiresTokSymbol :: SymbolSing Anonymous RequiresTokSymbol
  SReturnTokSymbol :: SymbolSing Anonymous ReturnTokSymbol
  SSchemaTokSymbol :: SymbolSing Anonymous SchemaTokSymbol
  SSignerTokSymbol :: SymbolSing Anonymous SignerTokSymbol
  SSpecTokSymbol :: SymbolSing Anonymous SpecTokSymbol
  SStoreTokSymbol :: SymbolSing Anonymous StoreTokSymbol
  SStructTokSymbol :: SymbolSing Anonymous StructTokSymbol
  SSucceedsIfTokSymbol :: SymbolSing Anonymous SucceedsIfTokSymbol
  SToTokSymbol :: SymbolSing Anonymous ToTokSymbol
  STrueTokSymbol :: SymbolSing Anonymous TrueTokSymbol
  SU128TokSymbol :: SymbolSing Anonymous U128TokSymbol
  SU16TokSymbol :: SymbolSing Anonymous U16TokSymbol
  SU256TokSymbol :: SymbolSing Anonymous U256TokSymbol
  SU32TokSymbol :: SymbolSing Anonymous U32TokSymbol
  SU64TokSymbol :: SymbolSing Anonymous U64TokSymbol
  SU8TokSymbol :: SymbolSing Anonymous U8TokSymbol
  SUnpackTokSymbol :: SymbolSing Anonymous UnpackTokSymbol
  SUpdateTokSymbol :: SymbolSing Anonymous UpdateTokSymbol
  SUseTokSymbol :: SymbolSing Anonymous UseTokSymbol
  SVectorLessThanSignTokSymbol :: SymbolSing Anonymous VectorLessThanSignTokSymbol
  SVectorLeftSquareBracketTokSymbol :: SymbolSing Anonymous VectorLeftSquareBracketTokSymbol
  SWhereTokSymbol :: SymbolSing Anonymous WhereTokSymbol
  SWhileTokSymbol :: SymbolSing Anonymous WhileTokSymbol
  SWithTokSymbol :: SymbolSing Anonymous WithTokSymbol
  SLeftCurlyBracketTokSymbol :: SymbolSing Anonymous LeftCurlyBracketTokSymbol
  SVerticalLineTokSymbol :: SymbolSing Anonymous VerticalLineTokSymbol
  SVerticalLineVerticalLineTokSymbol :: SymbolSing Anonymous VerticalLineVerticalLineTokSymbol
  SRightCurlyBracketTokSymbol :: SymbolSing Anonymous RightCurlyBracketTokSymbol
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
decSymbolSing SIdentifierSymbol SIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SAbilityDeclsSymbol SAbilityDeclsSymbol = Just (Refl, HRefl)
decSymbolSing SAbilitySymbol SAbilitySymbol = Just (Refl, HRefl)
decSymbolSing STypeParametersSymbol STypeParametersSymbol = Just (Refl, HRefl)
decSymbolSing STypeParameterSymbol STypeParameterSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeParameterIdentifierSymbol SHiddenTypeParameterIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SEnumVariantsSymbol SEnumVariantsSymbol = Just (Refl, HRefl)
decSymbolSing SVariantSymbol SVariantSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenVariantIdentifierSymbol SHiddenVariantIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SDatatypeFieldsSymbol SDatatypeFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SNamedFieldsSymbol SNamedFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SFieldAnnotationSymbol SFieldAnnotationSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFieldIdentifierSymbol SHiddenFieldIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeSymbol SHiddenTypeSymbol = Just (Refl, HRefl)
decSymbolSing SApplyTypeSymbol SApplyTypeSymbol = Just (Refl, HRefl)
decSymbolSing SModuleAccessSymbol SModuleAccessSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenModuleIdentifierSymbol SHiddenModuleIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenReservedIdentifierSymbol SHiddenReservedIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenExistsSymbol SHiddenExistsSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenForallSymbol SHiddenForallSymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentitySymbol SModuleIdentitySymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentityInternal0Symbol SModuleIdentityInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralSymbol SNumLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralInternal0Symbol SNumLiteralInternal0Symbol = Just (Refl, HRefl)
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
decSymbolSing SFunctionParametersSymbol SFunctionParametersSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParametersInternal0Symbol SFunctionParametersInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SFunctionParameterSymbol SFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParameterInternal0Symbol SFunctionParameterInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenVariableIdentifierSymbol SHiddenVariableIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SMutFunctionParameterSymbol SMutFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SModifierSymbol SModifierSymbol = Just (Refl, HRefl)
decSymbolSing SModifierInternal0Symbol SModifierInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SRetTypeSymbol SRetTypeSymbol = Just (Refl, HRefl)
decSymbolSing SBlockSymbol SBlockSymbol = Just (Refl, HRefl)
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
decSymbolSing SHiddenBindInternal0Symbol SHiddenBindInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMutBindVarSymbol SMutBindVarSymbol = Just (Refl, HRefl)
decSymbolSing SAtBindSymbol SAtBindSymbol = Just (Refl, HRefl)
decSymbolSing SBindUnpackSymbol SBindUnpackSymbol = Just (Refl, HRefl)
decSymbolSing SBindFieldsSymbol SBindFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SBindNamedFieldsSymbol SBindNamedFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SBindNamedFieldsInternal0Symbol SBindNamedFieldsInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SBindFieldSymbol SBindFieldSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpreadOperatorSymbol SHiddenSpreadOperatorSymbol = Just (Refl, HRefl)
decSymbolSing SMutBindFieldSymbol SMutBindFieldSymbol = Just (Refl, HRefl)
decSymbolSing SBindPositionalFieldsSymbol SBindPositionalFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SCommaBindListSymbol SCommaBindListSymbol = Just (Refl, HRefl)
decSymbolSing SOrBindListSymbol SOrBindListSymbol = Just (Refl, HRefl)
decSymbolSing SMatchConditionSymbol SMatchConditionSymbol = Just (Refl, HRefl)
decSymbolSing SPackExpressionSymbol SPackExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SFieldInitializeListSymbol SFieldInitializeListSymbol = Just (Refl, HRefl)
decSymbolSing SExpFieldSymbol SExpFieldSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockSymbol SSpecBlockSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockInternal0Symbol SSpecBlockInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecBlockTargetSymbol SHiddenSpecBlockTargetSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockTargetSchemaSymbol SSpecBlockTargetSchemaSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructIdentifierSymbol SHiddenStructIdentifierSymbol = Just (Refl, HRefl)
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
decSymbolSing SSpecApplyPatternInternal0Symbol SSpecApplyPatternInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecConditionSymbol SSpecConditionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortIfSymbol SHiddenSpecAbortIfSymbol = Just (Refl, HRefl)
decSymbolSing SConditionPropertiesSymbol SConditionPropertiesSymbol = Just (Refl, HRefl)
decSymbolSing SSpecPropertySymbol SSpecPropertySymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortWithOrModifiesSymbol SHiddenSpecAbortWithOrModifiesSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortWithOrModifiesInternal0Symbol SHiddenSpecAbortWithOrModifiesInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionSymbol SHiddenSpecConditionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionInternal0Symbol SHiddenSpecConditionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionKindSymbol SHiddenSpecConditionKindSymbol = Just (Refl, HRefl)
decSymbolSing SSpecIncludeSymbol SSpecIncludeSymbol = Just (Refl, HRefl)
decSymbolSing SSpecInvariantSymbol SSpecInvariantSymbol = Just (Refl, HRefl)
decSymbolSing SSpecInvariantInternal0Symbol SSpecInvariantInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecLetSymbol SSpecLetSymbol = Just (Refl, HRefl)
decSymbolSing SSpecPragmaSymbol SSpecPragmaSymbol = Just (Refl, HRefl)
decSymbolSing SSpecVariableSymbol SSpecVariableSymbol = Just (Refl, HRefl)
decSymbolSing SSpecVariableInternal0Symbol SSpecVariableInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SUseDeclarationSymbol SUseDeclarationSymbol = Just (Refl, HRefl)
decSymbolSing SUseDeclarationInternal0Symbol SUseDeclarationInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SUseFunSymbol SUseFunSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleSymbol SUseModuleSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleMemberSymbol SUseModuleMemberSymbol = Just (Refl, HRefl)
decSymbolSing SUseMemberSymbol SUseMemberSymbol = Just (Refl, HRefl)
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
decSymbolSing SAssignExpressionSymbol SAssignExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBinaryExpressionSymbol SBinaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SCastExpressionSymbol SCastExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SIdentifiedExpressionSymbol SIdentifiedExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBlockIdentifierSymbol SBlockIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaExpressionSymbol SLambdaExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaBindingsSymbol SLambdaBindingsSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaBindingSymbol SLambdaBindingSymbol = Just (Refl, HRefl)
decSymbolSing SLoopExpressionSymbol SLoopExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierExpressionSymbol SQuantifierExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierBindingsSymbol SQuantifierBindingsSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierBindingSymbol SQuantifierBindingSymbol = Just (Refl, HRefl)
decSymbolSing SReturnExpressionSymbol SReturnExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SWhileExpressionSymbol SWhileExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBlockItemSymbol SBlockItemSymbol = Just (Refl, HRefl)
decSymbolSing SBlockItemInternal0Symbol SBlockItemInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SLetStatementSymbol SLetStatementSymbol = Just (Refl, HRefl)
decSymbolSing SMacroFunctionDefinitionSymbol SMacroFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenMacroSignatureSymbol SHiddenMacroSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SNativeFunctionDefinitionSymbol SNativeFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructItemSymbol SHiddenStructItemSymbol = Just (Refl, HRefl)
decSymbolSing SNativeStructDefinitionSymbol SNativeStructDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructSignatureSymbol SHiddenStructSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SStructDefinitionSymbol SStructDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SConstantSymbol SConstantSymbol = Just (Refl, HRefl)
decSymbolSing SFriendDeclarationSymbol SFriendDeclarationSymbol = Just (Refl, HRefl)
decSymbolSing SFriendAccessSymbol SFriendAccessSymbol = Just (Refl, HRefl)
decSymbolSing SExclamationMarkTokSymbol SExclamationMarkTokSymbol = Just (Refl, HRefl)
decSymbolSing SExclamationMarkEqualsSignTokSymbol SExclamationMarkEqualsSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SNumberSignLeftSquareBracketTokSymbol SNumberSignLeftSquareBracketTokSymbol = Just (Refl, HRefl)
decSymbolSing SDollarSignTokSymbol SDollarSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SPercentSignTokSymbol SPercentSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SAmpersandTokSymbol SAmpersandTokSymbol = Just (Refl, HRefl)
decSymbolSing SAmpersandAmpersandTokSymbol SAmpersandAmpersandTokSymbol = Just (Refl, HRefl)
decSymbolSing SApostropheTokSymbol SApostropheTokSymbol = Just (Refl, HRefl)
decSymbolSing SLeftParenthesisTokSymbol SLeftParenthesisTokSymbol = Just (Refl, HRefl)
decSymbolSing SRightParenthesisTokSymbol SRightParenthesisTokSymbol = Just (Refl, HRefl)
decSymbolSing SAsteriskTokSymbol SAsteriskTokSymbol = Just (Refl, HRefl)
decSymbolSing SPlusSignTokSymbol SPlusSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SCommaTokSymbol SCommaTokSymbol = Just (Refl, HRefl)
decSymbolSing SHyphenMinusTokSymbol SHyphenMinusTokSymbol = Just (Refl, HRefl)
decSymbolSing SHyphenMinusGreaterThanSignTokSymbol SHyphenMinusGreaterThanSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SFullStopTokSymbol SFullStopTokSymbol = Just (Refl, HRefl)
decSymbolSing SFullStopFullStopTokSymbol SFullStopFullStopTokSymbol = Just (Refl, HRefl)
decSymbolSing SSolidusTokSymbol SSolidusTokSymbol = Just (Refl, HRefl)
decSymbolSing SSolidusAsteriskTokSymbol SSolidusAsteriskTokSymbol = Just (Refl, HRefl)
decSymbolSing SSolidusSolidusTokSymbol SSolidusSolidusTokSymbol = Just (Refl, HRefl)
decSymbolSing SColonTokSymbol SColonTokSymbol = Just (Refl, HRefl)
decSymbolSing SColonColonTokSymbol SColonColonTokSymbol = Just (Refl, HRefl)
decSymbolSing SSemicolonTokSymbol SSemicolonTokSymbol = Just (Refl, HRefl)
decSymbolSing SLessThanSignTokSymbol SLessThanSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SLessThanSignLessThanSignTokSymbol SLessThanSignLessThanSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SLessThanSignEqualsSignTokSymbol SLessThanSignEqualsSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SEqualsSignTokSymbol SEqualsSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SEqualsSignEqualsSignTokSymbol SEqualsSignEqualsSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SEqualsSignEqualsSignGreaterThanSignTokSymbol SEqualsSignEqualsSignGreaterThanSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SEqualsSignGreaterThanSignTokSymbol SEqualsSignGreaterThanSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SGreaterThanSignTokSymbol SGreaterThanSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SGreaterThanSignEqualsSignTokSymbol SGreaterThanSignEqualsSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SGreaterThanSignGreaterThanSignTokSymbol SGreaterThanSignGreaterThanSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SCommercialAtTokSymbol SCommercialAtTokSymbol = Just (Refl, HRefl)
decSymbolSing SLeftSquareBracketTokSymbol SLeftSquareBracketTokSymbol = Just (Refl, HRefl)
decSymbolSing SRightSquareBracketTokSymbol SRightSquareBracketTokSymbol = Just (Refl, HRefl)
decSymbolSing SCircumflexAccentTokSymbol SCircumflexAccentTokSymbol = Just (Refl, HRefl)
decSymbolSing SAbortTokSymbol SAbortTokSymbol = Just (Refl, HRefl)
decSymbolSing SAbortsIfTokSymbol SAbortsIfTokSymbol = Just (Refl, HRefl)
decSymbolSing SAbortsWithTokSymbol SAbortsWithTokSymbol = Just (Refl, HRefl)
decSymbolSing SAddressTokSymbol SAddressTokSymbol = Just (Refl, HRefl)
decSymbolSing SApplyTokSymbol SApplyTokSymbol = Just (Refl, HRefl)
decSymbolSing SAsTokSymbol SAsTokSymbol = Just (Refl, HRefl)
decSymbolSing SAssertTokSymbol SAssertTokSymbol = Just (Refl, HRefl)
decSymbolSing SAssumeTokSymbol SAssumeTokSymbol = Just (Refl, HRefl)
decSymbolSing SBoolTokSymbol SBoolTokSymbol = Just (Refl, HRefl)
decSymbolSing SBreakTokSymbol SBreakTokSymbol = Just (Refl, HRefl)
decSymbolSing SBytearrayTokSymbol SBytearrayTokSymbol = Just (Refl, HRefl)
decSymbolSing SConstTokSymbol SConstTokSymbol = Just (Refl, HRefl)
decSymbolSing SContinueTokSymbol SContinueTokSymbol = Just (Refl, HRefl)
decSymbolSing SCopyTokSymbol SCopyTokSymbol = Just (Refl, HRefl)
decSymbolSing SDecreasesTokSymbol SDecreasesTokSymbol = Just (Refl, HRefl)
decSymbolSing SDropTokSymbol SDropTokSymbol = Just (Refl, HRefl)
decSymbolSing SElseTokSymbol SElseTokSymbol = Just (Refl, HRefl)
decSymbolSing SEnsuresTokSymbol SEnsuresTokSymbol = Just (Refl, HRefl)
decSymbolSing SEntryTokSymbol SEntryTokSymbol = Just (Refl, HRefl)
decSymbolSing SEnumTokSymbol SEnumTokSymbol = Just (Refl, HRefl)
decSymbolSing SExceptTokSymbol SExceptTokSymbol = Just (Refl, HRefl)
decSymbolSing SExistsTokSymbol SExistsTokSymbol = Just (Refl, HRefl)
decSymbolSing SFalseTokSymbol SFalseTokSymbol = Just (Refl, HRefl)
decSymbolSing SForallTokSymbol SForallTokSymbol = Just (Refl, HRefl)
decSymbolSing SFriendTokSymbol SFriendTokSymbol = Just (Refl, HRefl)
decSymbolSing SFunTokSymbol SFunTokSymbol = Just (Refl, HRefl)
decSymbolSing SGlobalTokSymbol SGlobalTokSymbol = Just (Refl, HRefl)
decSymbolSing SHasTokSymbol SHasTokSymbol = Just (Refl, HRefl)
decSymbolSing SIfTokSymbol SIfTokSymbol = Just (Refl, HRefl)
decSymbolSing SInTokSymbol SInTokSymbol = Just (Refl, HRefl)
decSymbolSing SIncludeTokSymbol SIncludeTokSymbol = Just (Refl, HRefl)
decSymbolSing SInternalTokSymbol SInternalTokSymbol = Just (Refl, HRefl)
decSymbolSing SInvariantTokSymbol SInvariantTokSymbol = Just (Refl, HRefl)
decSymbolSing SKeyTokSymbol SKeyTokSymbol = Just (Refl, HRefl)
decSymbolSing SLetTokSymbol SLetTokSymbol = Just (Refl, HRefl)
decSymbolSing SLocalTokSymbol SLocalTokSymbol = Just (Refl, HRefl)
decSymbolSing SLoopTokSymbol SLoopTokSymbol = Just (Refl, HRefl)
decSymbolSing SMacroTokSymbol SMacroTokSymbol = Just (Refl, HRefl)
decSymbolSing SMatchTokSymbol SMatchTokSymbol = Just (Refl, HRefl)
decSymbolSing SModifiesTokSymbol SModifiesTokSymbol = Just (Refl, HRefl)
decSymbolSing SModuleTokSymbol SModuleTokSymbol = Just (Refl, HRefl)
decSymbolSing SMoveTokSymbol SMoveTokSymbol = Just (Refl, HRefl)
decSymbolSing SMutTokSymbol SMutTokSymbol = Just (Refl, HRefl)
decSymbolSing SNativeTokSymbol SNativeTokSymbol = Just (Refl, HRefl)
decSymbolSing SPackTokSymbol SPackTokSymbol = Just (Refl, HRefl)
decSymbolSing SPackageTokSymbol SPackageTokSymbol = Just (Refl, HRefl)
decSymbolSing SPhantomTokSymbol SPhantomTokSymbol = Just (Refl, HRefl)
decSymbolSing SPostTokSymbol SPostTokSymbol = Just (Refl, HRefl)
decSymbolSing SPragmaTokSymbol SPragmaTokSymbol = Just (Refl, HRefl)
decSymbolSing SPublicTokSymbol SPublicTokSymbol = Just (Refl, HRefl)
decSymbolSing SRequiresTokSymbol SRequiresTokSymbol = Just (Refl, HRefl)
decSymbolSing SReturnTokSymbol SReturnTokSymbol = Just (Refl, HRefl)
decSymbolSing SSchemaTokSymbol SSchemaTokSymbol = Just (Refl, HRefl)
decSymbolSing SSignerTokSymbol SSignerTokSymbol = Just (Refl, HRefl)
decSymbolSing SSpecTokSymbol SSpecTokSymbol = Just (Refl, HRefl)
decSymbolSing SStoreTokSymbol SStoreTokSymbol = Just (Refl, HRefl)
decSymbolSing SStructTokSymbol SStructTokSymbol = Just (Refl, HRefl)
decSymbolSing SSucceedsIfTokSymbol SSucceedsIfTokSymbol = Just (Refl, HRefl)
decSymbolSing SToTokSymbol SToTokSymbol = Just (Refl, HRefl)
decSymbolSing STrueTokSymbol STrueTokSymbol = Just (Refl, HRefl)
decSymbolSing SU128TokSymbol SU128TokSymbol = Just (Refl, HRefl)
decSymbolSing SU16TokSymbol SU16TokSymbol = Just (Refl, HRefl)
decSymbolSing SU256TokSymbol SU256TokSymbol = Just (Refl, HRefl)
decSymbolSing SU32TokSymbol SU32TokSymbol = Just (Refl, HRefl)
decSymbolSing SU64TokSymbol SU64TokSymbol = Just (Refl, HRefl)
decSymbolSing SU8TokSymbol SU8TokSymbol = Just (Refl, HRefl)
decSymbolSing SUnpackTokSymbol SUnpackTokSymbol = Just (Refl, HRefl)
decSymbolSing SUpdateTokSymbol SUpdateTokSymbol = Just (Refl, HRefl)
decSymbolSing SUseTokSymbol SUseTokSymbol = Just (Refl, HRefl)
decSymbolSing SVectorLessThanSignTokSymbol SVectorLessThanSignTokSymbol = Just (Refl, HRefl)
decSymbolSing SVectorLeftSquareBracketTokSymbol SVectorLeftSquareBracketTokSymbol = Just (Refl, HRefl)
decSymbolSing SWhereTokSymbol SWhereTokSymbol = Just (Refl, HRefl)
decSymbolSing SWhileTokSymbol SWhileTokSymbol = Just (Refl, HRefl)
decSymbolSing SWithTokSymbol SWithTokSymbol = Just (Refl, HRefl)
decSymbolSing SLeftCurlyBracketTokSymbol SLeftCurlyBracketTokSymbol = Just (Refl, HRefl)
decSymbolSing SVerticalLineTokSymbol SVerticalLineTokSymbol = Just (Refl, HRefl)
decSymbolSing SVerticalLineVerticalLineTokSymbol SVerticalLineVerticalLineTokSymbol = Just (Refl, HRefl)
decSymbolSing SRightCurlyBracketTokSymbol SRightCurlyBracketTokSymbol = Just (Refl, HRefl)
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
    , ("identifier", SomeRegularSymbolSing SIdentifierSymbol)
    , ("ability_decls", SomeRegularSymbolSing SAbilityDeclsSymbol)
    , ("ability", SomeRegularSymbolSing SAbilitySymbol)
    , ("type_parameters", SomeRegularSymbolSing STypeParametersSymbol)
    , ("type_parameter", SomeRegularSymbolSing STypeParameterSymbol)
    , ("_type_parameter_identifier", SomeRegularSymbolSing SHiddenTypeParameterIdentifierSymbol)
    , ("enum_variants", SomeRegularSymbolSing SEnumVariantsSymbol)
    , ("variant", SomeRegularSymbolSing SVariantSymbol)
    , ("_variant_identifier", SomeRegularSymbolSing SHiddenVariantIdentifierSymbol)
    , ("datatype_fields", SomeRegularSymbolSing SDatatypeFieldsSymbol)
    , ("named_fields", SomeRegularSymbolSing SNamedFieldsSymbol)
    , ("field_annotation", SomeRegularSymbolSing SFieldAnnotationSymbol)
    , ("_field_identifier", SomeRegularSymbolSing SHiddenFieldIdentifierSymbol)
    , ("_type", SomeRegularSymbolSing SHiddenTypeSymbol)
    , ("apply_type", SomeRegularSymbolSing SApplyTypeSymbol)
    , ("module_access", SomeRegularSymbolSing SModuleAccessSymbol)
    , ("_module_identifier", SomeRegularSymbolSing SHiddenModuleIdentifierSymbol)
    , ("_reserved_identifier", SomeRegularSymbolSing SHiddenReservedIdentifierSymbol)
    , ("_exists", SomeRegularSymbolSing SHiddenExistsSymbol)
    , ("_forall", SomeRegularSymbolSing SHiddenForallSymbol)
    , ("module_identity", SomeRegularSymbolSing SModuleIdentitySymbol)
    , ("module_identity_internal0", SomeRegularSymbolSing SModuleIdentityInternal0Symbol)
    , ("num_literal", SomeRegularSymbolSing SNumLiteralSymbol)
    , ("num_literal_internal0", SomeRegularSymbolSing SNumLiteralInternal0Symbol)
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
    , ("function_parameters", SomeRegularSymbolSing SFunctionParametersSymbol)
    , ("function_parameters_internal0", SomeRegularSymbolSing SFunctionParametersInternal0Symbol)
    , ("function_parameter", SomeRegularSymbolSing SFunctionParameterSymbol)
    , ("function_parameter_internal0", SomeRegularSymbolSing SFunctionParameterInternal0Symbol)
    , ("_variable_identifier", SomeRegularSymbolSing SHiddenVariableIdentifierSymbol)
    , ("mut_function_parameter", SomeRegularSymbolSing SMutFunctionParameterSymbol)
    , ("modifier", SomeRegularSymbolSing SModifierSymbol)
    , ("modifier_internal0", SomeRegularSymbolSing SModifierInternal0Symbol)
    , ("ret_type", SomeRegularSymbolSing SRetTypeSymbol)
    , ("block", SomeRegularSymbolSing SBlockSymbol)
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
    , ("_bind_internal0", SomeRegularSymbolSing SHiddenBindInternal0Symbol)
    , ("mut_bind_var", SomeRegularSymbolSing SMutBindVarSymbol)
    , ("at_bind", SomeRegularSymbolSing SAtBindSymbol)
    , ("bind_unpack", SomeRegularSymbolSing SBindUnpackSymbol)
    , ("bind_fields", SomeRegularSymbolSing SBindFieldsSymbol)
    , ("bind_named_fields", SomeRegularSymbolSing SBindNamedFieldsSymbol)
    , ("bind_named_fields_internal0", SomeRegularSymbolSing SBindNamedFieldsInternal0Symbol)
    , ("bind_field", SomeRegularSymbolSing SBindFieldSymbol)
    , ("_spread_operator", SomeRegularSymbolSing SHiddenSpreadOperatorSymbol)
    , ("mut_bind_field", SomeRegularSymbolSing SMutBindFieldSymbol)
    , ("bind_positional_fields", SomeRegularSymbolSing SBindPositionalFieldsSymbol)
    , ("comma_bind_list", SomeRegularSymbolSing SCommaBindListSymbol)
    , ("or_bind_list", SomeRegularSymbolSing SOrBindListSymbol)
    , ("match_condition", SomeRegularSymbolSing SMatchConditionSymbol)
    , ("pack_expression", SomeRegularSymbolSing SPackExpressionSymbol)
    , ("field_initialize_list", SomeRegularSymbolSing SFieldInitializeListSymbol)
    , ("exp_field", SomeRegularSymbolSing SExpFieldSymbol)
    , ("spec_block", SomeRegularSymbolSing SSpecBlockSymbol)
    , ("spec_block_internal0", SomeRegularSymbolSing SSpecBlockInternal0Symbol)
    , ("_spec_block_target", SomeRegularSymbolSing SHiddenSpecBlockTargetSymbol)
    , ("spec_block_target_schema", SomeRegularSymbolSing SSpecBlockTargetSchemaSymbol)
    , ("_struct_identifier", SomeRegularSymbolSing SHiddenStructIdentifierSymbol)
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
    , ("spec_apply_pattern_internal0", SomeRegularSymbolSing SSpecApplyPatternInternal0Symbol)
    , ("spec_condition", SomeRegularSymbolSing SSpecConditionSymbol)
    , ("_spec_abort_if", SomeRegularSymbolSing SHiddenSpecAbortIfSymbol)
    , ("condition_properties", SomeRegularSymbolSing SConditionPropertiesSymbol)
    , ("spec_property", SomeRegularSymbolSing SSpecPropertySymbol)
    , ("_spec_abort_with_or_modifies", SomeRegularSymbolSing SHiddenSpecAbortWithOrModifiesSymbol)
    , ("_spec_abort_with_or_modifies_internal0", SomeRegularSymbolSing SHiddenSpecAbortWithOrModifiesInternal0Symbol)
    , ("_spec_condition", SomeRegularSymbolSing SHiddenSpecConditionSymbol)
    , ("_spec_condition_internal0", SomeRegularSymbolSing SHiddenSpecConditionInternal0Symbol)
    , ("_spec_condition_kind", SomeRegularSymbolSing SHiddenSpecConditionKindSymbol)
    , ("spec_include", SomeRegularSymbolSing SSpecIncludeSymbol)
    , ("spec_invariant", SomeRegularSymbolSing SSpecInvariantSymbol)
    , ("spec_invariant_internal0", SomeRegularSymbolSing SSpecInvariantInternal0Symbol)
    , ("spec_let", SomeRegularSymbolSing SSpecLetSymbol)
    , ("spec_pragma", SomeRegularSymbolSing SSpecPragmaSymbol)
    , ("spec_variable", SomeRegularSymbolSing SSpecVariableSymbol)
    , ("spec_variable_internal0", SomeRegularSymbolSing SSpecVariableInternal0Symbol)
    , ("use_declaration", SomeRegularSymbolSing SUseDeclarationSymbol)
    , ("use_declaration_internal0", SomeRegularSymbolSing SUseDeclarationInternal0Symbol)
    , ("use_fun", SomeRegularSymbolSing SUseFunSymbol)
    , ("use_module", SomeRegularSymbolSing SUseModuleSymbol)
    , ("use_module_member", SomeRegularSymbolSing SUseModuleMemberSymbol)
    , ("use_member", SomeRegularSymbolSing SUseMemberSymbol)
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
    , ("assign_expression", SomeRegularSymbolSing SAssignExpressionSymbol)
    , ("binary_expression", SomeRegularSymbolSing SBinaryExpressionSymbol)
    , ("cast_expression", SomeRegularSymbolSing SCastExpressionSymbol)
    , ("identified_expression", SomeRegularSymbolSing SIdentifiedExpressionSymbol)
    , ("block_identifier", SomeRegularSymbolSing SBlockIdentifierSymbol)
    , ("lambda_expression", SomeRegularSymbolSing SLambdaExpressionSymbol)
    , ("lambda_bindings", SomeRegularSymbolSing SLambdaBindingsSymbol)
    , ("lambda_binding", SomeRegularSymbolSing SLambdaBindingSymbol)
    , ("loop_expression", SomeRegularSymbolSing SLoopExpressionSymbol)
    , ("quantifier_expression", SomeRegularSymbolSing SQuantifierExpressionSymbol)
    , ("quantifier_bindings", SomeRegularSymbolSing SQuantifierBindingsSymbol)
    , ("quantifier_binding", SomeRegularSymbolSing SQuantifierBindingSymbol)
    , ("return_expression", SomeRegularSymbolSing SReturnExpressionSymbol)
    , ("while_expression", SomeRegularSymbolSing SWhileExpressionSymbol)
    , ("block_item", SomeRegularSymbolSing SBlockItemSymbol)
    , ("block_item_internal0", SomeRegularSymbolSing SBlockItemInternal0Symbol)
    , ("let_statement", SomeRegularSymbolSing SLetStatementSymbol)
    , ("macro_function_definition", SomeRegularSymbolSing SMacroFunctionDefinitionSymbol)
    , ("_macro_signature", SomeRegularSymbolSing SHiddenMacroSignatureSymbol)
    , ("native_function_definition", SomeRegularSymbolSing SNativeFunctionDefinitionSymbol)
    , ("_struct_item", SomeRegularSymbolSing SHiddenStructItemSymbol)
    , ("native_struct_definition", SomeRegularSymbolSing SNativeStructDefinitionSymbol)
    , ("_struct_signature", SomeRegularSymbolSing SHiddenStructSignatureSymbol)
    , ("struct_definition", SomeRegularSymbolSing SStructDefinitionSymbol)
    , ("constant", SomeRegularSymbolSing SConstantSymbol)
    , ("friend_declaration", SomeRegularSymbolSing SFriendDeclarationSymbol)
    , ("friend_access", SomeRegularSymbolSing SFriendAccessSymbol)
    , ("!", SomeAnonymousSymbolSing SExclamationMarkTokSymbol)
    , ("!=", SomeAnonymousSymbolSing SExclamationMarkEqualsSignTokSymbol)
    , ("#[", SomeAnonymousSymbolSing SNumberSignLeftSquareBracketTokSymbol)
    , ("$", SomeAnonymousSymbolSing SDollarSignTokSymbol)
    , ("%", SomeAnonymousSymbolSing SPercentSignTokSymbol)
    , ("&", SomeAnonymousSymbolSing SAmpersandTokSymbol)
    , ("&&", SomeAnonymousSymbolSing SAmpersandAmpersandTokSymbol)
    , ("'", SomeAnonymousSymbolSing SApostropheTokSymbol)
    , ("(", SomeAnonymousSymbolSing SLeftParenthesisTokSymbol)
    , (")", SomeAnonymousSymbolSing SRightParenthesisTokSymbol)
    , ("*", SomeAnonymousSymbolSing SAsteriskTokSymbol)
    , ("+", SomeAnonymousSymbolSing SPlusSignTokSymbol)
    , (",", SomeAnonymousSymbolSing SCommaTokSymbol)
    , ("-", SomeAnonymousSymbolSing SHyphenMinusTokSymbol)
    , ("->", SomeAnonymousSymbolSing SHyphenMinusGreaterThanSignTokSymbol)
    , (".", SomeAnonymousSymbolSing SFullStopTokSymbol)
    , ("..", SomeAnonymousSymbolSing SFullStopFullStopTokSymbol)
    , ("/", SomeAnonymousSymbolSing SSolidusTokSymbol)
    , ("/*", SomeAnonymousSymbolSing SSolidusAsteriskTokSymbol)
    , ("//", SomeAnonymousSymbolSing SSolidusSolidusTokSymbol)
    , (":", SomeAnonymousSymbolSing SColonTokSymbol)
    , ("::", SomeAnonymousSymbolSing SColonColonTokSymbol)
    , (";", SomeAnonymousSymbolSing SSemicolonTokSymbol)
    , ("<", SomeAnonymousSymbolSing SLessThanSignTokSymbol)
    , ("<<", SomeAnonymousSymbolSing SLessThanSignLessThanSignTokSymbol)
    , ("<=", SomeAnonymousSymbolSing SLessThanSignEqualsSignTokSymbol)
    , ("=", SomeAnonymousSymbolSing SEqualsSignTokSymbol)
    , ("==", SomeAnonymousSymbolSing SEqualsSignEqualsSignTokSymbol)
    , ("==>", SomeAnonymousSymbolSing SEqualsSignEqualsSignGreaterThanSignTokSymbol)
    , ("=>", SomeAnonymousSymbolSing SEqualsSignGreaterThanSignTokSymbol)
    , (">", SomeAnonymousSymbolSing SGreaterThanSignTokSymbol)
    , (">=", SomeAnonymousSymbolSing SGreaterThanSignEqualsSignTokSymbol)
    , (">>", SomeAnonymousSymbolSing SGreaterThanSignGreaterThanSignTokSymbol)
    , ("@", SomeAnonymousSymbolSing SCommercialAtTokSymbol)
    , ("[", SomeAnonymousSymbolSing SLeftSquareBracketTokSymbol)
    , ("]", SomeAnonymousSymbolSing SRightSquareBracketTokSymbol)
    , ("^", SomeAnonymousSymbolSing SCircumflexAccentTokSymbol)
    , ("abort", SomeAnonymousSymbolSing SAbortTokSymbol)
    , ("aborts_if", SomeAnonymousSymbolSing SAbortsIfTokSymbol)
    , ("aborts_with", SomeAnonymousSymbolSing SAbortsWithTokSymbol)
    , ("address", SomeAnonymousSymbolSing SAddressTokSymbol)
    , ("apply", SomeAnonymousSymbolSing SApplyTokSymbol)
    , ("as", SomeAnonymousSymbolSing SAsTokSymbol)
    , ("assert", SomeAnonymousSymbolSing SAssertTokSymbol)
    , ("assume", SomeAnonymousSymbolSing SAssumeTokSymbol)
    , ("bool", SomeAnonymousSymbolSing SBoolTokSymbol)
    , ("break", SomeAnonymousSymbolSing SBreakTokSymbol)
    , ("bytearray", SomeAnonymousSymbolSing SBytearrayTokSymbol)
    , ("const", SomeAnonymousSymbolSing SConstTokSymbol)
    , ("continue", SomeAnonymousSymbolSing SContinueTokSymbol)
    , ("copy", SomeAnonymousSymbolSing SCopyTokSymbol)
    , ("decreases", SomeAnonymousSymbolSing SDecreasesTokSymbol)
    , ("drop", SomeAnonymousSymbolSing SDropTokSymbol)
    , ("else", SomeAnonymousSymbolSing SElseTokSymbol)
    , ("ensures", SomeAnonymousSymbolSing SEnsuresTokSymbol)
    , ("entry", SomeAnonymousSymbolSing SEntryTokSymbol)
    , ("enum", SomeAnonymousSymbolSing SEnumTokSymbol)
    , ("except", SomeAnonymousSymbolSing SExceptTokSymbol)
    , ("exists", SomeAnonymousSymbolSing SExistsTokSymbol)
    , ("false", SomeAnonymousSymbolSing SFalseTokSymbol)
    , ("forall", SomeAnonymousSymbolSing SForallTokSymbol)
    , ("friend", SomeAnonymousSymbolSing SFriendTokSymbol)
    , ("fun", SomeAnonymousSymbolSing SFunTokSymbol)
    , ("global", SomeAnonymousSymbolSing SGlobalTokSymbol)
    , ("has", SomeAnonymousSymbolSing SHasTokSymbol)
    , ("if", SomeAnonymousSymbolSing SIfTokSymbol)
    , ("in", SomeAnonymousSymbolSing SInTokSymbol)
    , ("include", SomeAnonymousSymbolSing SIncludeTokSymbol)
    , ("internal", SomeAnonymousSymbolSing SInternalTokSymbol)
    , ("invariant", SomeAnonymousSymbolSing SInvariantTokSymbol)
    , ("key", SomeAnonymousSymbolSing SKeyTokSymbol)
    , ("let", SomeAnonymousSymbolSing SLetTokSymbol)
    , ("local", SomeAnonymousSymbolSing SLocalTokSymbol)
    , ("loop", SomeAnonymousSymbolSing SLoopTokSymbol)
    , ("macro", SomeAnonymousSymbolSing SMacroTokSymbol)
    , ("match", SomeAnonymousSymbolSing SMatchTokSymbol)
    , ("modifies", SomeAnonymousSymbolSing SModifiesTokSymbol)
    , ("module", SomeAnonymousSymbolSing SModuleTokSymbol)
    , ("move", SomeAnonymousSymbolSing SMoveTokSymbol)
    , ("mut", SomeAnonymousSymbolSing SMutTokSymbol)
    , ("native", SomeAnonymousSymbolSing SNativeTokSymbol)
    , ("pack", SomeAnonymousSymbolSing SPackTokSymbol)
    , ("package", SomeAnonymousSymbolSing SPackageTokSymbol)
    , ("phantom", SomeAnonymousSymbolSing SPhantomTokSymbol)
    , ("post", SomeAnonymousSymbolSing SPostTokSymbol)
    , ("pragma", SomeAnonymousSymbolSing SPragmaTokSymbol)
    , ("public", SomeAnonymousSymbolSing SPublicTokSymbol)
    , ("requires", SomeAnonymousSymbolSing SRequiresTokSymbol)
    , ("return", SomeAnonymousSymbolSing SReturnTokSymbol)
    , ("schema", SomeAnonymousSymbolSing SSchemaTokSymbol)
    , ("signer", SomeAnonymousSymbolSing SSignerTokSymbol)
    , ("spec", SomeAnonymousSymbolSing SSpecTokSymbol)
    , ("store", SomeAnonymousSymbolSing SStoreTokSymbol)
    , ("struct", SomeAnonymousSymbolSing SStructTokSymbol)
    , ("succeeds_if", SomeAnonymousSymbolSing SSucceedsIfTokSymbol)
    , ("to", SomeAnonymousSymbolSing SToTokSymbol)
    , ("true", SomeAnonymousSymbolSing STrueTokSymbol)
    , ("u128", SomeAnonymousSymbolSing SU128TokSymbol)
    , ("u16", SomeAnonymousSymbolSing SU16TokSymbol)
    , ("u256", SomeAnonymousSymbolSing SU256TokSymbol)
    , ("u32", SomeAnonymousSymbolSing SU32TokSymbol)
    , ("u64", SomeAnonymousSymbolSing SU64TokSymbol)
    , ("u8", SomeAnonymousSymbolSing SU8TokSymbol)
    , ("unpack", SomeAnonymousSymbolSing SUnpackTokSymbol)
    , ("update", SomeAnonymousSymbolSing SUpdateTokSymbol)
    , ("use", SomeAnonymousSymbolSing SUseTokSymbol)
    , ("vector<", SomeAnonymousSymbolSing SVectorLessThanSignTokSymbol)
    , ("vector[", SomeAnonymousSymbolSing SVectorLeftSquareBracketTokSymbol)
    , ("where", SomeAnonymousSymbolSing SWhereTokSymbol)
    , ("while", SomeAnonymousSymbolSing SWhileTokSymbol)
    , ("with", SomeAnonymousSymbolSing SWithTokSymbol)
    , ("{", SomeAnonymousSymbolSing SLeftCurlyBracketTokSymbol)
    , ("|", SomeAnonymousSymbolSing SVerticalLineTokSymbol)
    , ("||", SomeAnonymousSymbolSing SVerticalLineVerticalLineTokSymbol)
    , ("}", SomeAnonymousSymbolSing SRightCurlyBracketTokSymbol)
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
