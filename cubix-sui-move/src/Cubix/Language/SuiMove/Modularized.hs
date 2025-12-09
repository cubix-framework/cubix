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
import TreeSitter qualified as TS

import Cubix.Language.Info (TermLab)
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax qualified as Syntax
import Cubix.ParsePretty (type RootSort)
import Data.Comp.Multi (Sort, Term)

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
data NumLiteralInternal1L
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
data WhileExpressionL

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
data AtTokL
data XorTokL
data AbortsIfTokL
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
data ExistsTokL
data FalseTokL
data ForallTokL
data FriendTokL
data GlobalTokL
data InternalTokL
data InvariantTokL
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
data RequiresTokL
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
  At :: Token e AtTokL
  Xor :: Token e XorTokL
  AbortsIf :: Token e AbortsIfTokL
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
  Exists :: Token e ExistsTokL
  False :: Token e FalseTokL
  Forall :: Token e ForallTokL
  Friend :: Token e FriendTokL
  Global :: Token e GlobalTokL
  Internal :: Token e InternalTokL
  Invariant :: Token e InvariantTokL
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
  Requires :: Token e RequiresTokL
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
    :: e [ModuleBodyInternal0L]
    -> ModuleBody e ModuleBodyL

data ModuleBodyInternal0 e l where
  ModuleBodyInternal0UseDeclaration
    :: e UseDeclarationL
    -> ModuleBodyInternal0 e ModuleBodyInternal0L
  ModuleBodyInternal0FriendDeclaration
    :: e FriendDeclarationL
    -> ModuleBodyInternal0 e ModuleBodyInternal0L
  ModuleBodyInternal0Constant
    :: e ConstantL
    -> ModuleBodyInternal0 e ModuleBodyInternal0L
  ModuleBodyInternal0FunctionItem
    :: e HiddenFunctionItemL
    -> ModuleBodyInternal0 e ModuleBodyInternal0L
  ModuleBodyInternal0StructItem
    :: e HiddenStructItemL
    -> ModuleBodyInternal0 e ModuleBodyInternal0L
  ModuleBodyInternal0EnumItem
    :: e EnumDefinitionL
    -> ModuleBodyInternal0 e ModuleBodyInternal0L
  ModuleBodyInternal0SpecBlock
    :: e SpecBlockL
    -> ModuleBodyInternal0 e ModuleBodyInternal0L

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
    :: e ((((((Maybe ModifierL, Maybe ModifierL), Maybe ModifierL), IdentifierL), Maybe TypeParametersL), FunctionParametersL), Maybe RetTypeL)
    -> e BlockL
    -> FunctionDefinition e FunctionDefinitionL

data Block e l where
  Block
    :: e [UseDeclarationL]
    -> e [BlockItemL]
    -> e (Maybe HiddenExpressionL)
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
    :: e HiddenUnaryExpressionInternal0L
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

data NumLiteral e l where
  NumLiteral
    :: e NumLiteralInternal0L
    -> e (Maybe NumLiteralInternal1L)
    -> NumLiteral e NumLiteralL

data NumLiteralInternal0 e l where
  NumLiteralInternal01
    :: Text
    -> NumLiteralInternal0 e NumLiteralInternal0L
  NumLiteralInternal02
    :: Text
    -> NumLiteralInternal0 e NumLiteralInternal0L

data NumLiteralInternal1 e l where
  NumLiteralInternal1U8
    :: e U8TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L
  NumLiteralInternal1U16
    :: e U16TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L
  NumLiteralInternal1U32
    :: e U32TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L
  NumLiteralInternal1U64
    :: e U64TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L
  NumLiteralInternal1U128
    :: e U128TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L
  NumLiteralInternal1U256
    :: e U256TokL
    -> NumLiteralInternal1 e NumLiteralInternal1L

data AnnotationExpression e l where
  AnnotationExpression
    :: e HiddenExpressionL
    -> e HiddenTypeL
    -> AnnotationExpression e AnnotationExpressionL

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
    :: e DollarTokL
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess2
    :: e AtTokL
    -> e IdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccessMember
    :: e HiddenReservedIdentifierL
    -> ModuleAccess e ModuleAccessL
  ModuleAccess4
    :: e IdentifierL
    -> e (Maybe TypeArgumentsL)
    -> ModuleAccess e ModuleAccessL
  ModuleAccess5
    :: e IdentifierL
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

data HiddenReservedIdentifier e l where
  HiddenReservedIdentifierForall
    :: e ForallTokL
    -> HiddenReservedIdentifier e HiddenReservedIdentifierL
  HiddenReservedIdentifierExists
    :: e ExistsTokL
    -> HiddenReservedIdentifier e HiddenReservedIdentifierL

data Identifier e l where
  Identifier
    :: Text
    -> Identifier e IdentifierL

data ModuleIdentity e l where
  ModuleIdentity
    :: e ModuleIdentityInternal0L
    -> e IdentifierL
    -> ModuleIdentity e ModuleIdentityL

data ModuleIdentityInternal0 e l where
  ModuleIdentityInternal0NumLiteral
    :: e NumLiteralL
    -> ModuleIdentityInternal0 e ModuleIdentityInternal0L
  ModuleIdentityInternal0ModuleIdentifier
    :: e IdentifierL
    -> ModuleIdentityInternal0 e ModuleIdentityInternal0L

data TypeArguments e l where
  TypeArguments
    :: e LtTokL
    -> e [HiddenTypeL]
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
    -> e ([HiddenTypeL], Maybe HiddenTypeL)
    -> e BitorTokL
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
    :: e BitandTokL
    -> ImmRef e ImmRefL

data MutRef e l where
  MutRef
    :: e BitandTokL
    -> MutRef e MutRefL

data TupleType e l where
  TupleType
    :: e [HiddenTypeL]
    -> e (Maybe HiddenTypeL)
    -> TupleType e TupleTypeL

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
    :: e (NameExpressionL, ArgListL)
    -> CallExpression e CallExpressionL

data ArgList e l where
  ArgList
    :: e [HiddenExpressionL]
    -> e (Maybe HiddenExpressionL)
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
    :: e (HiddenExpressionTermL, HiddenExpressionTermL)
    -> DotExpression e DotExpressionL

data ExpressionList e l where
  ExpressionList
    :: e [HiddenExpressionL]
    -> ExpressionList e ExpressionListL

data IfExpression e l where
  IfExpression
    :: e ((HiddenExpressionL, HiddenExpressionL), Maybe HiddenExpressionL)
    -> IfExpression e IfExpressionL

data IndexExpression e l where
  IndexExpression
    :: e (HiddenExpressionTermL, ([HiddenExpressionL], Maybe HiddenExpressionL))
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
    -> e ([MatchArmL], Maybe MatchArmL)
    -> MatchExpression e MatchExpressionL

data MatchArm e l where
  MatchArm
    :: e BindListL
    -> e (Maybe MatchConditionL)
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
    :: e IdentifierL
    -> HiddenBindInternal0 e HiddenBindInternal0L

data MutBindVar e l where
  MutBindVar
    :: e IdentifierL
    -> MutBindVar e MutBindVarL

data AtBind e l where
  AtBind
    :: e IdentifierL
    -> e AtTokL
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
    -> e (Maybe BindNamedFieldsInternal0L)
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
    -> e (Maybe BindListL)
    -> BindField e BindFieldL
  BindFieldSpreadOperator
    :: e RangeTokL
    -> BindField e BindFieldL

data MutBindField e l where
  MutBindField
    :: e BindFieldL
    -> MutBindField e MutBindFieldL

data BindPositionalFields e l where
  BindPositionalFields
    :: e [BindNamedFieldsInternal0L]
    -> e (Maybe BindNamedFieldsInternal0L)
    -> BindPositionalFields e BindPositionalFieldsL

data CommaBindList e l where
  CommaBindList
    :: e [HiddenBindL]
    -> e (Maybe HiddenBindL)
    -> CommaBindList e CommaBindListL

data OrBindList e l where
  OrBindList
    :: e HiddenBindL
    -> e [(BitorTokL, HiddenBindL)]
    -> e (Maybe BitorTokL)
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
    :: e [ExpFieldL]
    -> e (Maybe ExpFieldL)
    -> FieldInitializeList e FieldInitializeListL

data ExpField e l where
  ExpField
    :: e IdentifierL
    -> e (Maybe HiddenExpressionL)
    -> ExpField e ExpFieldL

data SpecBlock e l where
  SpecBlock1
    :: e (Maybe HiddenSpecBlockTargetL)
    -> e SpecBodyL
    -> SpecBlock e SpecBlockL
  SpecBlockSpecFunction
    :: e HiddenSpecFunctionL
    -> SpecBlock e SpecBlockL

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
    :: e IdentifierL
    -> e (Maybe TypeParametersL)
    -> SpecBlockTargetSchema e SpecBlockTargetSchemaL

data TypeParameters e l where
  TypeParameters
    :: e LtTokL
    -> e [TypeParameterL]
    -> e GtTokL
    -> TypeParameters e TypeParametersL

data TypeParameter e l where
  TypeParameter
    :: e (Maybe DollarTokL)
    -> e (Maybe PhantomTokL)
    -> e IdentifierL
    -> e (Maybe ((AbilityL, [(AddTokL, AbilityL)]), Maybe AddTokL))
    -> TypeParameter e TypeParameterL

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
    -> e (((IdentifierL, Maybe TypeParametersL), FunctionParametersL), RetTypeL)
    -> NativeSpecFunction e NativeSpecFunctionL

data FunctionParameters e l where
  FunctionParameters
    :: e [FunctionParametersInternal0L]
    -> e (Maybe FunctionParametersInternal0L)
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
    -> e HiddenTypeL
    -> FunctionParameter e FunctionParameterL

data FunctionParameterInternal0 e l where
  FunctionParameterInternal0Name
    :: e IdentifierL
    -> FunctionParameterInternal0 e FunctionParameterInternal0L
  FunctionParameterInternal02
    :: e DollarTokL
    -> e IdentifierL
    -> FunctionParameterInternal0 e FunctionParameterInternal0L

data MutFunctionParameter e l where
  MutFunctionParameter
    :: e FunctionParameterL
    -> MutFunctionParameter e MutFunctionParameterL

data RetType e l where
  RetType
    :: e HiddenTypeL
    -> RetType e RetTypeL

data UninterpretedSpecFunction e l where
  UninterpretedSpecFunction
    :: e (((IdentifierL, Maybe TypeParametersL), FunctionParametersL), RetTypeL)
    -> UninterpretedSpecFunction e UninterpretedSpecFunctionL

data UsualSpecFunction e l where
  UsualSpecFunction
    :: e (((IdentifierL, Maybe TypeParametersL), FunctionParametersL), RetTypeL)
    -> e BlockL
    -> UsualSpecFunction e UsualSpecFunctionL

data SpecBody e l where
  SpecBody
    :: e [UseDeclarationL]
    -> e [HiddenSpecBlockMemeberL]
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
    -> e (Maybe [SpecApplyPatternL])
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
    :: e ((HiddenSpecConditionInternal0L, Maybe ConditionPropertiesL), HiddenExpressionL)
    -> SpecCondition e SpecConditionL
  SpecConditionSpecAbortIf
    :: e (((AbortsIfTokL, Maybe ConditionPropertiesL), HiddenExpressionL), Maybe HiddenExpressionL)
    -> SpecCondition e SpecConditionL
  SpecConditionSpecAbortWithOrModifies
    :: e ((HiddenSpecAbortWithOrModifiesInternal0L, Maybe ConditionPropertiesL), [HiddenExpressionL])
    -> SpecCondition e SpecConditionL

data HiddenSpecAbortWithOrModifiesInternal0 e l where
  HiddenSpecAbortWithOrModifiesInternal0AbortsWith
    :: e AbortsWithTokL
    -> HiddenSpecAbortWithOrModifiesInternal0 e HiddenSpecAbortWithOrModifiesInternal0L
  HiddenSpecAbortWithOrModifiesInternal0Modifies
    :: e ModifiesTokL
    -> HiddenSpecAbortWithOrModifiesInternal0 e HiddenSpecAbortWithOrModifiesInternal0L

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

data ConditionProperties e l where
  ConditionProperties
    :: e [SpecPropertyL]
    -> e (Maybe SpecPropertyL)
    -> ConditionProperties e ConditionPropertiesL

data SpecProperty e l where
  SpecProperty
    :: e IdentifierL
    -> e (Maybe (AssignTokL, HiddenLiteralValueL))
    -> SpecProperty e SpecPropertyL

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
    :: e (Maybe PostTokL)
    -> e IdentifierL
    -> e AssignTokL
    -> e HiddenExpressionL
    -> SpecLet e SpecLetL

data SpecPragma e l where
  SpecPragma
    :: e [SpecPropertyL]
    -> e (Maybe SpecPropertyL)
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
    -> e (ModuleAccessL, IdentifierL)
    -> UseFun e UseFunL

data UseModule e l where
  UseModule
    :: e ModuleIdentityL
    -> e (Maybe IdentifierL)
    -> UseModule e UseModuleL

data UseModuleMember e l where
  UseModuleMember
    :: e ModuleIdentityL
    -> e UseMemberL
    -> UseModuleMember e UseModuleMemberL

data UseMember e l where
  UseMember1
    :: e IdentifierL
    -> e [UseMemberL]
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
    :: e ModuleIdentityInternal0L
    -> e [UseMemberL]
    -> UseModuleMembers e UseModuleMembersL
  UseModuleMembers2
    :: e ModuleIdentityL
    -> e [UseMemberL]
    -> UseModuleMembers e UseModuleMembersL

data UnitExpression e l where
  UnitExpression
    :: UnitExpression e UnitExpressionL

data VectorExpression e l where
  VectorExpression
    :: e (Maybe ([HiddenTypeL], GtTokL))
    -> e ([HiddenExpressionL], Maybe HiddenExpressionL)
    -> VectorExpression e VectorExpressionL

data BorrowExpression e l where
  BorrowExpression
    :: e (HiddenReferenceL, HiddenExpressionL)
    -> BorrowExpression e BorrowExpressionL

data DereferenceExpression e l where
  DereferenceExpression
    :: e (MulTokL, HiddenExpressionL)
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
    :: e BangTokL
    -> UnaryOp e UnaryOpL

data AbortExpression e l where
  AbortExpression
    :: e (Maybe HiddenExpressionL)
    -> AbortExpression e AbortExpressionL

data AssignExpression e l where
  AssignExpression
    :: e ((HiddenUnaryExpressionInternal0L, AssignTokL), HiddenExpressionL)
    -> AssignExpression e AssignExpressionL

data BinaryExpression e l where
  BinaryExpression1
    :: e HiddenExpressionL
    -> e ImpliesTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression2
    :: e HiddenExpressionL
    -> e OrTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression3
    :: e HiddenExpressionL
    -> e AndTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression4
    :: e HiddenExpressionL
    -> e EqTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression5
    :: e HiddenExpressionL
    -> e NeqTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression6
    :: e HiddenExpressionL
    -> e LtTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression7
    :: e HiddenExpressionL
    -> e GtTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression8
    :: e HiddenExpressionL
    -> e LeTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression9
    :: e HiddenExpressionL
    -> e GeTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression10
    :: e HiddenExpressionL
    -> e RangeTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression11
    :: e HiddenExpressionL
    -> e BitorTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression12
    :: e HiddenExpressionL
    -> e XorTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression13
    :: e HiddenExpressionL
    -> e BitandTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression14
    :: e HiddenExpressionL
    -> e ShlTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression15
    :: e HiddenExpressionL
    -> e ShrTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression16
    :: e HiddenExpressionL
    -> e AddTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression17
    :: e HiddenExpressionL
    -> e SubTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression18
    :: e HiddenExpressionL
    -> e MulTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression19
    :: e HiddenExpressionL
    -> e DivTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression20
    :: e HiddenExpressionL
    -> e ModTokL
    -> e HiddenExpressionL
    -> BinaryExpression e BinaryExpressionL

data CastExpression e l where
  CastExpression
    :: e (HiddenExpressionL, HiddenTypeL)
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
    -> e ([LambdaBindingL], Maybe LambdaBindingL)
    -> e BitorTokL
    -> LambdaBindings e LambdaBindingsL

data LambdaBinding e l where
  LambdaBindingCommaBindList
    :: e CommaBindListL
    -> LambdaBinding e LambdaBindingL
  LambdaBindingBind
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
    :: e (((HiddenReservedIdentifierL, QuantifierBindingsL), Maybe HiddenExpressionL), HiddenExpressionL)
    -> QuantifierExpression e QuantifierExpressionL

data QuantifierBindings e l where
  QuantifierBindings
    :: e [QuantifierBindingL]
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
  ReturnExpressionLabel
    :: e LabelL
    -> ReturnExpression e ReturnExpressionL

data WhileExpression e l where
  WhileExpression
    :: e HiddenExpressionL
    -> e HiddenExpressionL
    -> WhileExpression e WhileExpressionL

data BlockItem e l where
  BlockItemExpression
    :: e HiddenExpressionL
    -> BlockItem e BlockItemL
  BlockItemLetStatement
    :: e LetStatementL
    -> BlockItem e BlockItemL

data LetStatement e l where
  LetStatement
    :: e BindListL
    -> e (Maybe HiddenTypeL)
    -> e (Maybe (AssignTokL, HiddenExpressionL))
    -> LetStatement e LetStatementL

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

data MacroFunctionDefinition e l where
  MacroFunctionDefinition
    :: e (Maybe ModifierL)
    -> e ((((Maybe ModifierL, IdentifierL), Maybe TypeParametersL), FunctionParametersL), Maybe RetTypeL)
    -> e BlockL
    -> MacroFunctionDefinition e MacroFunctionDefinitionL

data NativeFunctionDefinition e l where
  NativeFunctionDefinition
    :: e ((((((Maybe ModifierL, Maybe ModifierL), Maybe ModifierL), IdentifierL), Maybe TypeParametersL), FunctionParametersL), Maybe RetTypeL)
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
    -> e NativeTokL
    -> e ((IdentifierL, Maybe TypeParametersL), Maybe AbilityDeclsL)
    -> NativeStructDefinition e NativeStructDefinitionL

data AbilityDecls e l where
  AbilityDecls
    :: e [AbilityL]
    -> e (Maybe AbilityL)
    -> AbilityDecls e AbilityDeclsL

data StructDefinition e l where
  StructDefinition
    :: e (Maybe PublicTokL)
    -> e ((IdentifierL, Maybe TypeParametersL), Maybe AbilityDeclsL)
    -> e DatatypeFieldsL
    -> e (Maybe PostfixAbilityDeclsL)
    -> StructDefinition e StructDefinitionL

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
    -> e (Maybe FieldAnnotationL)
    -> NamedFields e NamedFieldsL

data FieldAnnotation e l where
  FieldAnnotation
    :: e IdentifierL
    -> e HiddenTypeL
    -> FieldAnnotation e FieldAnnotationL

data PositionalFields e l where
  PositionalFields
    :: e [HiddenTypeL]
    -> e (Maybe HiddenTypeL)
    -> PositionalFields e PositionalFieldsL

data PostfixAbilityDecls e l where
  PostfixAbilityDecls
    :: e [AbilityL]
    -> e (Maybe AbilityL)
    -> PostfixAbilityDecls e PostfixAbilityDeclsL

data Constant e l where
  Constant
    :: e IdentifierL
    -> e HiddenTypeL
    -> e AssignTokL
    -> e HiddenExpressionL
    -> Constant e ConstantL

data EnumDefinition e l where
  EnumDefinition
    :: e (Maybe PublicTokL)
    -> e ((IdentifierL, Maybe TypeParametersL), Maybe AbilityDeclsL)
    -> e EnumVariantsL
    -> e (Maybe PostfixAbilityDeclsL)
    -> EnumDefinition e EnumDefinitionL

data EnumVariants e l where
  EnumVariants
    :: e [VariantL]
    -> e (Maybe VariantL)
    -> EnumVariants e EnumVariantsL

data Variant e l where
  Variant
    :: e IdentifierL
    -> e (Maybe DatatypeFieldsL)
    -> Variant e VariantL

data FriendDeclaration e l where
  FriendDeclaration
    :: e FriendTokL
    -> e FriendAccessL
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
  , ''HiddenFunctionItem
  , ''FunctionDefinition
  , ''Block
  , ''HiddenExpression
  , ''HiddenUnaryExpressionInternal0
  , ''HiddenExpressionTerm
  , ''HiddenLiteralValue
  , ''AddressLiteral
  , ''BoolLiteral
  , ''ByteStringLiteral
  , ''HexStringLiteral
  , ''NumLiteral
  , ''NumLiteralInternal0
  , ''NumLiteralInternal1
  , ''AnnotationExpression
  , ''HiddenType
  , ''ApplyType
  , ''ModuleAccess
  , ''HiddenReservedIdentifier
  , ''Identifier
  , ''ModuleIdentity
  , ''ModuleIdentityInternal0
  , ''TypeArguments
  , ''FunctionType
  , ''FunctionTypeParameters
  , ''PrimitiveType
  , ''RefType
  , ''HiddenReference
  , ''ImmRef
  , ''MutRef
  , ''TupleType
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
  , ''MutBindField
  , ''BindPositionalFields
  , ''CommaBindList
  , ''OrBindList
  , ''MatchCondition
  , ''PackExpression
  , ''FieldInitializeList
  , ''ExpField
  , ''SpecBlock
  , ''HiddenSpecBlockTarget
  , ''SpecBlockTargetSchema
  , ''TypeParameters
  , ''TypeParameter
  , ''Ability
  , ''HiddenSpecFunction
  , ''NativeSpecFunction
  , ''FunctionParameters
  , ''FunctionParametersInternal0
  , ''FunctionParameter
  , ''FunctionParameterInternal0
  , ''MutFunctionParameter
  , ''RetType
  , ''UninterpretedSpecFunction
  , ''UsualSpecFunction
  , ''SpecBody
  , ''HiddenSpecBlockMemeber
  , ''SpecApply
  , ''SpecApplyPattern
  , ''SpecApplyNamePattern
  , ''SpecApplyPatternInternal0
  , ''SpecCondition
  , ''HiddenSpecAbortWithOrModifiesInternal0
  , ''HiddenSpecConditionInternal0
  , ''HiddenSpecConditionKind
  , ''ConditionProperties
  , ''SpecProperty
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
  , ''LetStatement
  , ''Modifier
  , ''ModifierInternal0
  , ''MacroFunctionDefinition
  , ''NativeFunctionDefinition
  , ''HiddenStructItem
  , ''NativeStructDefinition
  , ''AbilityDecls
  , ''StructDefinition
  , ''DatatypeFields
  , ''NamedFields
  , ''FieldAnnotation
  , ''PositionalFields
  , ''PostfixAbilityDecls
  , ''Constant
  , ''EnumDefinition
  , ''EnumVariants
  , ''Variant
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
   , HiddenFunctionItem
   , FunctionDefinition
   , Block
   , HiddenExpression
   , HiddenUnaryExpressionInternal0
   , HiddenExpressionTerm
   , HiddenLiteralValue
   , AddressLiteral
   , BoolLiteral
   , ByteStringLiteral
   , HexStringLiteral
   , NumLiteral
   , NumLiteralInternal0
   , NumLiteralInternal1
   , AnnotationExpression
   , HiddenType
   , ApplyType
   , ModuleAccess
   , HiddenReservedIdentifier
   , Identifier
   , ModuleIdentity
   , ModuleIdentityInternal0
   , TypeArguments
   , FunctionType
   , FunctionTypeParameters
   , PrimitiveType
   , RefType
   , HiddenReference
   , ImmRef
   , MutRef
   , TupleType
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
   , MutBindField
   , BindPositionalFields
   , CommaBindList
   , OrBindList
   , MatchCondition
   , PackExpression
   , FieldInitializeList
   , ExpField
   , SpecBlock
   , HiddenSpecBlockTarget
   , SpecBlockTargetSchema
   , TypeParameters
   , TypeParameter
   , Ability
   , HiddenSpecFunction
   , NativeSpecFunction
   , FunctionParameters
   , FunctionParametersInternal0
   , FunctionParameter
   , FunctionParameterInternal0
   , MutFunctionParameter
   , RetType
   , UninterpretedSpecFunction
   , UsualSpecFunction
   , SpecBody
   , HiddenSpecBlockMemeber
   , SpecApply
   , SpecApplyPattern
   , SpecApplyNamePattern
   , SpecApplyPatternInternal0
   , SpecCondition
   , HiddenSpecAbortWithOrModifiesInternal0
   , HiddenSpecConditionInternal0
   , HiddenSpecConditionKind
   , ConditionProperties
   , SpecProperty
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
   , LetStatement
   , Modifier
   , ModifierInternal0
   , MacroFunctionDefinition
   , NativeFunctionDefinition
   , HiddenStructItem
   , NativeStructDefinition
   , AbilityDecls
   , StructDefinition
   , DatatypeFields
   , NamedFields
   , FieldAnnotation
   , PositionalFields
   , PostfixAbilityDecls
   , Constant
   , EnumDefinition
   , EnumVariants
   , Variant
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
  ModuleBodyInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenUnaryExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExpressionTermSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenLiteralValueSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AddressLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BoolLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ByteStringLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HexStringLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  AnnotationExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ApplyTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenReservedIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentityInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeArgumentsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PrimitiveTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RefTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenReferenceSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ImmRefSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutRefSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TupleTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  MutBindFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindPositionalFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CommaBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  OrBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PackExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldInitializeListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExpFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecBlockTargetSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockTargetSchemaSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParametersInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParameterInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MutFunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RetTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UninterpretedSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UsualSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBodySymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecBlockMemeberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplySymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyPatternSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyNamePatternSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyPatternInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortWithOrModifiesInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionKindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConditionPropertiesSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPropertySymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  LetStatementSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroFunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeFunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeStructDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilityDeclsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  StructDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DatatypeFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NamedFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldAnnotationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PositionalFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PostfixAbilityDeclsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConstantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumVariantsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VariantSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  AtTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  XorTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbortsIfTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  ExistsTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FalseTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ForallTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FriendTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  GlobalTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  InternalTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
  InvariantTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  RequiresTokSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  SHiddenFunctionItemSymbol :: SymbolSing Regular HiddenFunctionItemSymbol
  SFunctionDefinitionSymbol :: SymbolSing Regular FunctionDefinitionSymbol
  SBlockSymbol :: SymbolSing Regular BlockSymbol
  SHiddenExpressionSymbol :: SymbolSing Regular HiddenExpressionSymbol
  SHiddenUnaryExpressionInternal0Symbol :: SymbolSing Regular HiddenUnaryExpressionInternal0Symbol
  SHiddenExpressionTermSymbol :: SymbolSing Regular HiddenExpressionTermSymbol
  SHiddenLiteralValueSymbol :: SymbolSing Regular HiddenLiteralValueSymbol
  SAddressLiteralSymbol :: SymbolSing Regular AddressLiteralSymbol
  SBoolLiteralSymbol :: SymbolSing Regular BoolLiteralSymbol
  SByteStringLiteralSymbol :: SymbolSing Regular ByteStringLiteralSymbol
  SHexStringLiteralSymbol :: SymbolSing Regular HexStringLiteralSymbol
  SNumLiteralSymbol :: SymbolSing Regular NumLiteralSymbol
  SNumLiteralInternal0Symbol :: SymbolSing Regular NumLiteralInternal0Symbol
  SNumLiteralInternal1Symbol :: SymbolSing Regular NumLiteralInternal1Symbol
  SAnnotationExpressionSymbol :: SymbolSing Regular AnnotationExpressionSymbol
  SHiddenTypeSymbol :: SymbolSing Regular HiddenTypeSymbol
  SApplyTypeSymbol :: SymbolSing Regular ApplyTypeSymbol
  SModuleAccessSymbol :: SymbolSing Regular ModuleAccessSymbol
  SHiddenReservedIdentifierSymbol :: SymbolSing Regular HiddenReservedIdentifierSymbol
  SIdentifierSymbol :: SymbolSing Regular IdentifierSymbol
  SModuleIdentitySymbol :: SymbolSing Regular ModuleIdentitySymbol
  SModuleIdentityInternal0Symbol :: SymbolSing Regular ModuleIdentityInternal0Symbol
  STypeArgumentsSymbol :: SymbolSing Regular TypeArgumentsSymbol
  SFunctionTypeSymbol :: SymbolSing Regular FunctionTypeSymbol
  SFunctionTypeParametersSymbol :: SymbolSing Regular FunctionTypeParametersSymbol
  SPrimitiveTypeSymbol :: SymbolSing Regular PrimitiveTypeSymbol
  SRefTypeSymbol :: SymbolSing Regular RefTypeSymbol
  SHiddenReferenceSymbol :: SymbolSing Regular HiddenReferenceSymbol
  SImmRefSymbol :: SymbolSing Regular ImmRefSymbol
  SMutRefSymbol :: SymbolSing Regular MutRefSymbol
  STupleTypeSymbol :: SymbolSing Regular TupleTypeSymbol
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
  SMutBindFieldSymbol :: SymbolSing Regular MutBindFieldSymbol
  SBindPositionalFieldsSymbol :: SymbolSing Regular BindPositionalFieldsSymbol
  SCommaBindListSymbol :: SymbolSing Regular CommaBindListSymbol
  SOrBindListSymbol :: SymbolSing Regular OrBindListSymbol
  SMatchConditionSymbol :: SymbolSing Regular MatchConditionSymbol
  SPackExpressionSymbol :: SymbolSing Regular PackExpressionSymbol
  SFieldInitializeListSymbol :: SymbolSing Regular FieldInitializeListSymbol
  SExpFieldSymbol :: SymbolSing Regular ExpFieldSymbol
  SSpecBlockSymbol :: SymbolSing Regular SpecBlockSymbol
  SHiddenSpecBlockTargetSymbol :: SymbolSing Regular HiddenSpecBlockTargetSymbol
  SSpecBlockTargetSchemaSymbol :: SymbolSing Regular SpecBlockTargetSchemaSymbol
  STypeParametersSymbol :: SymbolSing Regular TypeParametersSymbol
  STypeParameterSymbol :: SymbolSing Regular TypeParameterSymbol
  SAbilitySymbol :: SymbolSing Regular AbilitySymbol
  SHiddenSpecFunctionSymbol :: SymbolSing Regular HiddenSpecFunctionSymbol
  SNativeSpecFunctionSymbol :: SymbolSing Regular NativeSpecFunctionSymbol
  SFunctionParametersSymbol :: SymbolSing Regular FunctionParametersSymbol
  SFunctionParametersInternal0Symbol :: SymbolSing Regular FunctionParametersInternal0Symbol
  SFunctionParameterSymbol :: SymbolSing Regular FunctionParameterSymbol
  SFunctionParameterInternal0Symbol :: SymbolSing Regular FunctionParameterInternal0Symbol
  SMutFunctionParameterSymbol :: SymbolSing Regular MutFunctionParameterSymbol
  SRetTypeSymbol :: SymbolSing Regular RetTypeSymbol
  SUninterpretedSpecFunctionSymbol :: SymbolSing Regular UninterpretedSpecFunctionSymbol
  SUsualSpecFunctionSymbol :: SymbolSing Regular UsualSpecFunctionSymbol
  SSpecBodySymbol :: SymbolSing Regular SpecBodySymbol
  SHiddenSpecBlockMemeberSymbol :: SymbolSing Regular HiddenSpecBlockMemeberSymbol
  SSpecApplySymbol :: SymbolSing Regular SpecApplySymbol
  SSpecApplyPatternSymbol :: SymbolSing Regular SpecApplyPatternSymbol
  SSpecApplyNamePatternSymbol :: SymbolSing Regular SpecApplyNamePatternSymbol
  SSpecApplyPatternInternal0Symbol :: SymbolSing Regular SpecApplyPatternInternal0Symbol
  SSpecConditionSymbol :: SymbolSing Regular SpecConditionSymbol
  SHiddenSpecAbortWithOrModifiesInternal0Symbol :: SymbolSing Regular HiddenSpecAbortWithOrModifiesInternal0Symbol
  SHiddenSpecConditionInternal0Symbol :: SymbolSing Regular HiddenSpecConditionInternal0Symbol
  SHiddenSpecConditionKindSymbol :: SymbolSing Regular HiddenSpecConditionKindSymbol
  SConditionPropertiesSymbol :: SymbolSing Regular ConditionPropertiesSymbol
  SSpecPropertySymbol :: SymbolSing Regular SpecPropertySymbol
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
  SLetStatementSymbol :: SymbolSing Regular LetStatementSymbol
  SModifierSymbol :: SymbolSing Regular ModifierSymbol
  SModifierInternal0Symbol :: SymbolSing Regular ModifierInternal0Symbol
  SMacroFunctionDefinitionSymbol :: SymbolSing Regular MacroFunctionDefinitionSymbol
  SNativeFunctionDefinitionSymbol :: SymbolSing Regular NativeFunctionDefinitionSymbol
  SHiddenStructItemSymbol :: SymbolSing Regular HiddenStructItemSymbol
  SNativeStructDefinitionSymbol :: SymbolSing Regular NativeStructDefinitionSymbol
  SAbilityDeclsSymbol :: SymbolSing Regular AbilityDeclsSymbol
  SStructDefinitionSymbol :: SymbolSing Regular StructDefinitionSymbol
  SDatatypeFieldsSymbol :: SymbolSing Regular DatatypeFieldsSymbol
  SNamedFieldsSymbol :: SymbolSing Regular NamedFieldsSymbol
  SFieldAnnotationSymbol :: SymbolSing Regular FieldAnnotationSymbol
  SPositionalFieldsSymbol :: SymbolSing Regular PositionalFieldsSymbol
  SPostfixAbilityDeclsSymbol :: SymbolSing Regular PostfixAbilityDeclsSymbol
  SConstantSymbol :: SymbolSing Regular ConstantSymbol
  SEnumDefinitionSymbol :: SymbolSing Regular EnumDefinitionSymbol
  SEnumVariantsSymbol :: SymbolSing Regular EnumVariantsSymbol
  SVariantSymbol :: SymbolSing Regular VariantSymbol
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
  SAtTokSymbol :: SymbolSing Anonymous AtTokSymbol
  SXorTokSymbol :: SymbolSing Anonymous XorTokSymbol
  SAbortsIfTokSymbol :: SymbolSing Anonymous AbortsIfTokSymbol
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
  SExistsTokSymbol :: SymbolSing Anonymous ExistsTokSymbol
  SFalseTokSymbol :: SymbolSing Anonymous FalseTokSymbol
  SForallTokSymbol :: SymbolSing Anonymous ForallTokSymbol
  SFriendTokSymbol :: SymbolSing Anonymous FriendTokSymbol
  SGlobalTokSymbol :: SymbolSing Anonymous GlobalTokSymbol
  SInternalTokSymbol :: SymbolSing Anonymous InternalTokSymbol
  SInvariantTokSymbol :: SymbolSing Anonymous InvariantTokSymbol
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
  SRequiresTokSymbol :: SymbolSing Anonymous RequiresTokSymbol
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
decSymbolSing SHiddenFunctionItemSymbol SHiddenFunctionItemSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionDefinitionSymbol SFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SBlockSymbol SBlockSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenExpressionSymbol SHiddenExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenUnaryExpressionInternal0Symbol SHiddenUnaryExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenExpressionTermSymbol SHiddenExpressionTermSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenLiteralValueSymbol SHiddenLiteralValueSymbol = Just (Refl, HRefl)
decSymbolSing SAddressLiteralSymbol SAddressLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SBoolLiteralSymbol SBoolLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SByteStringLiteralSymbol SByteStringLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SHexStringLiteralSymbol SHexStringLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralSymbol SNumLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralInternal0Symbol SNumLiteralInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralInternal1Symbol SNumLiteralInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SAnnotationExpressionSymbol SAnnotationExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeSymbol SHiddenTypeSymbol = Just (Refl, HRefl)
decSymbolSing SApplyTypeSymbol SApplyTypeSymbol = Just (Refl, HRefl)
decSymbolSing SModuleAccessSymbol SModuleAccessSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenReservedIdentifierSymbol SHiddenReservedIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SIdentifierSymbol SIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentitySymbol SModuleIdentitySymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentityInternal0Symbol SModuleIdentityInternal0Symbol = Just (Refl, HRefl)
decSymbolSing STypeArgumentsSymbol STypeArgumentsSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeSymbol SFunctionTypeSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeParametersSymbol SFunctionTypeParametersSymbol = Just (Refl, HRefl)
decSymbolSing SPrimitiveTypeSymbol SPrimitiveTypeSymbol = Just (Refl, HRefl)
decSymbolSing SRefTypeSymbol SRefTypeSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenReferenceSymbol SHiddenReferenceSymbol = Just (Refl, HRefl)
decSymbolSing SImmRefSymbol SImmRefSymbol = Just (Refl, HRefl)
decSymbolSing SMutRefSymbol SMutRefSymbol = Just (Refl, HRefl)
decSymbolSing STupleTypeSymbol STupleTypeSymbol = Just (Refl, HRefl)
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
decSymbolSing SMutBindFieldSymbol SMutBindFieldSymbol = Just (Refl, HRefl)
decSymbolSing SBindPositionalFieldsSymbol SBindPositionalFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SCommaBindListSymbol SCommaBindListSymbol = Just (Refl, HRefl)
decSymbolSing SOrBindListSymbol SOrBindListSymbol = Just (Refl, HRefl)
decSymbolSing SMatchConditionSymbol SMatchConditionSymbol = Just (Refl, HRefl)
decSymbolSing SPackExpressionSymbol SPackExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SFieldInitializeListSymbol SFieldInitializeListSymbol = Just (Refl, HRefl)
decSymbolSing SExpFieldSymbol SExpFieldSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockSymbol SSpecBlockSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecBlockTargetSymbol SHiddenSpecBlockTargetSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockTargetSchemaSymbol SSpecBlockTargetSchemaSymbol = Just (Refl, HRefl)
decSymbolSing STypeParametersSymbol STypeParametersSymbol = Just (Refl, HRefl)
decSymbolSing STypeParameterSymbol STypeParameterSymbol = Just (Refl, HRefl)
decSymbolSing SAbilitySymbol SAbilitySymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecFunctionSymbol SHiddenSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SNativeSpecFunctionSymbol SNativeSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParametersSymbol SFunctionParametersSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParametersInternal0Symbol SFunctionParametersInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SFunctionParameterSymbol SFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParameterInternal0Symbol SFunctionParameterInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMutFunctionParameterSymbol SMutFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SRetTypeSymbol SRetTypeSymbol = Just (Refl, HRefl)
decSymbolSing SUninterpretedSpecFunctionSymbol SUninterpretedSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SUsualSpecFunctionSymbol SUsualSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBodySymbol SSpecBodySymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecBlockMemeberSymbol SHiddenSpecBlockMemeberSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplySymbol SSpecApplySymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyPatternSymbol SSpecApplyPatternSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyNamePatternSymbol SSpecApplyNamePatternSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyPatternInternal0Symbol SSpecApplyPatternInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecConditionSymbol SSpecConditionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortWithOrModifiesInternal0Symbol SHiddenSpecAbortWithOrModifiesInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionInternal0Symbol SHiddenSpecConditionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionKindSymbol SHiddenSpecConditionKindSymbol = Just (Refl, HRefl)
decSymbolSing SConditionPropertiesSymbol SConditionPropertiesSymbol = Just (Refl, HRefl)
decSymbolSing SSpecPropertySymbol SSpecPropertySymbol = Just (Refl, HRefl)
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
decSymbolSing SLetStatementSymbol SLetStatementSymbol = Just (Refl, HRefl)
decSymbolSing SModifierSymbol SModifierSymbol = Just (Refl, HRefl)
decSymbolSing SModifierInternal0Symbol SModifierInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMacroFunctionDefinitionSymbol SMacroFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SNativeFunctionDefinitionSymbol SNativeFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructItemSymbol SHiddenStructItemSymbol = Just (Refl, HRefl)
decSymbolSing SNativeStructDefinitionSymbol SNativeStructDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SAbilityDeclsSymbol SAbilityDeclsSymbol = Just (Refl, HRefl)
decSymbolSing SStructDefinitionSymbol SStructDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SDatatypeFieldsSymbol SDatatypeFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SNamedFieldsSymbol SNamedFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SFieldAnnotationSymbol SFieldAnnotationSymbol = Just (Refl, HRefl)
decSymbolSing SPositionalFieldsSymbol SPositionalFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SPostfixAbilityDeclsSymbol SPostfixAbilityDeclsSymbol = Just (Refl, HRefl)
decSymbolSing SConstantSymbol SConstantSymbol = Just (Refl, HRefl)
decSymbolSing SEnumDefinitionSymbol SEnumDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SEnumVariantsSymbol SEnumVariantsSymbol = Just (Refl, HRefl)
decSymbolSing SVariantSymbol SVariantSymbol = Just (Refl, HRefl)
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
decSymbolSing SAtTokSymbol SAtTokSymbol = Just (Refl, HRefl)
decSymbolSing SXorTokSymbol SXorTokSymbol = Just (Refl, HRefl)
decSymbolSing SAbortsIfTokSymbol SAbortsIfTokSymbol = Just (Refl, HRefl)
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
decSymbolSing SExistsTokSymbol SExistsTokSymbol = Just (Refl, HRefl)
decSymbolSing SFalseTokSymbol SFalseTokSymbol = Just (Refl, HRefl)
decSymbolSing SForallTokSymbol SForallTokSymbol = Just (Refl, HRefl)
decSymbolSing SFriendTokSymbol SFriendTokSymbol = Just (Refl, HRefl)
decSymbolSing SGlobalTokSymbol SGlobalTokSymbol = Just (Refl, HRefl)
decSymbolSing SInternalTokSymbol SInternalTokSymbol = Just (Refl, HRefl)
decSymbolSing SInvariantTokSymbol SInvariantTokSymbol = Just (Refl, HRefl)
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
decSymbolSing SRequiresTokSymbol SRequiresTokSymbol = Just (Refl, HRefl)
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
    , ("_function_item", SomeRegularSymbolSing SHiddenFunctionItemSymbol)
    , ("function_definition", SomeRegularSymbolSing SFunctionDefinitionSymbol)
    , ("block", SomeRegularSymbolSing SBlockSymbol)
    , ("_expression", SomeRegularSymbolSing SHiddenExpressionSymbol)
    , ("_unary_expression_internal0", SomeRegularSymbolSing SHiddenUnaryExpressionInternal0Symbol)
    , ("_expression_term", SomeRegularSymbolSing SHiddenExpressionTermSymbol)
    , ("_literal_value", SomeRegularSymbolSing SHiddenLiteralValueSymbol)
    , ("address_literal", SomeRegularSymbolSing SAddressLiteralSymbol)
    , ("bool_literal", SomeRegularSymbolSing SBoolLiteralSymbol)
    , ("byte_string_literal", SomeRegularSymbolSing SByteStringLiteralSymbol)
    , ("hex_string_literal", SomeRegularSymbolSing SHexStringLiteralSymbol)
    , ("num_literal", SomeRegularSymbolSing SNumLiteralSymbol)
    , ("num_literal_internal0", SomeRegularSymbolSing SNumLiteralInternal0Symbol)
    , ("num_literal_internal1", SomeRegularSymbolSing SNumLiteralInternal1Symbol)
    , ("annotation_expression", SomeRegularSymbolSing SAnnotationExpressionSymbol)
    , ("_type", SomeRegularSymbolSing SHiddenTypeSymbol)
    , ("apply_type", SomeRegularSymbolSing SApplyTypeSymbol)
    , ("module_access", SomeRegularSymbolSing SModuleAccessSymbol)
    , ("_reserved_identifier", SomeRegularSymbolSing SHiddenReservedIdentifierSymbol)
    , ("identifier", SomeRegularSymbolSing SIdentifierSymbol)
    , ("module_identity", SomeRegularSymbolSing SModuleIdentitySymbol)
    , ("module_identity_internal0", SomeRegularSymbolSing SModuleIdentityInternal0Symbol)
    , ("type_arguments", SomeRegularSymbolSing STypeArgumentsSymbol)
    , ("function_type", SomeRegularSymbolSing SFunctionTypeSymbol)
    , ("function_type_parameters", SomeRegularSymbolSing SFunctionTypeParametersSymbol)
    , ("primitive_type", SomeRegularSymbolSing SPrimitiveTypeSymbol)
    , ("ref_type", SomeRegularSymbolSing SRefTypeSymbol)
    , ("_reference", SomeRegularSymbolSing SHiddenReferenceSymbol)
    , ("imm_ref", SomeRegularSymbolSing SImmRefSymbol)
    , ("mut_ref", SomeRegularSymbolSing SMutRefSymbol)
    , ("tuple_type", SomeRegularSymbolSing STupleTypeSymbol)
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
    , ("mut_bind_field", SomeRegularSymbolSing SMutBindFieldSymbol)
    , ("bind_positional_fields", SomeRegularSymbolSing SBindPositionalFieldsSymbol)
    , ("comma_bind_list", SomeRegularSymbolSing SCommaBindListSymbol)
    , ("or_bind_list", SomeRegularSymbolSing SOrBindListSymbol)
    , ("match_condition", SomeRegularSymbolSing SMatchConditionSymbol)
    , ("pack_expression", SomeRegularSymbolSing SPackExpressionSymbol)
    , ("field_initialize_list", SomeRegularSymbolSing SFieldInitializeListSymbol)
    , ("exp_field", SomeRegularSymbolSing SExpFieldSymbol)
    , ("spec_block", SomeRegularSymbolSing SSpecBlockSymbol)
    , ("_spec_block_target", SomeRegularSymbolSing SHiddenSpecBlockTargetSymbol)
    , ("spec_block_target_schema", SomeRegularSymbolSing SSpecBlockTargetSchemaSymbol)
    , ("type_parameters", SomeRegularSymbolSing STypeParametersSymbol)
    , ("type_parameter", SomeRegularSymbolSing STypeParameterSymbol)
    , ("ability", SomeRegularSymbolSing SAbilitySymbol)
    , ("_spec_function", SomeRegularSymbolSing SHiddenSpecFunctionSymbol)
    , ("native_spec_function", SomeRegularSymbolSing SNativeSpecFunctionSymbol)
    , ("function_parameters", SomeRegularSymbolSing SFunctionParametersSymbol)
    , ("function_parameters_internal0", SomeRegularSymbolSing SFunctionParametersInternal0Symbol)
    , ("function_parameter", SomeRegularSymbolSing SFunctionParameterSymbol)
    , ("function_parameter_internal0", SomeRegularSymbolSing SFunctionParameterInternal0Symbol)
    , ("mut_function_parameter", SomeRegularSymbolSing SMutFunctionParameterSymbol)
    , ("ret_type", SomeRegularSymbolSing SRetTypeSymbol)
    , ("uninterpreted_spec_function", SomeRegularSymbolSing SUninterpretedSpecFunctionSymbol)
    , ("usual_spec_function", SomeRegularSymbolSing SUsualSpecFunctionSymbol)
    , ("spec_body", SomeRegularSymbolSing SSpecBodySymbol)
    , ("_spec_block_memeber", SomeRegularSymbolSing SHiddenSpecBlockMemeberSymbol)
    , ("spec_apply", SomeRegularSymbolSing SSpecApplySymbol)
    , ("spec_apply_pattern", SomeRegularSymbolSing SSpecApplyPatternSymbol)
    , ("spec_apply_name_pattern", SomeRegularSymbolSing SSpecApplyNamePatternSymbol)
    , ("spec_apply_pattern_internal0", SomeRegularSymbolSing SSpecApplyPatternInternal0Symbol)
    , ("spec_condition", SomeRegularSymbolSing SSpecConditionSymbol)
    , ("_spec_abort_with_or_modifies_internal0", SomeRegularSymbolSing SHiddenSpecAbortWithOrModifiesInternal0Symbol)
    , ("_spec_condition_internal0", SomeRegularSymbolSing SHiddenSpecConditionInternal0Symbol)
    , ("_spec_condition_kind", SomeRegularSymbolSing SHiddenSpecConditionKindSymbol)
    , ("condition_properties", SomeRegularSymbolSing SConditionPropertiesSymbol)
    , ("spec_property", SomeRegularSymbolSing SSpecPropertySymbol)
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
    , ("let_statement", SomeRegularSymbolSing SLetStatementSymbol)
    , ("modifier", SomeRegularSymbolSing SModifierSymbol)
    , ("modifier_internal0", SomeRegularSymbolSing SModifierInternal0Symbol)
    , ("macro_function_definition", SomeRegularSymbolSing SMacroFunctionDefinitionSymbol)
    , ("native_function_definition", SomeRegularSymbolSing SNativeFunctionDefinitionSymbol)
    , ("_struct_item", SomeRegularSymbolSing SHiddenStructItemSymbol)
    , ("native_struct_definition", SomeRegularSymbolSing SNativeStructDefinitionSymbol)
    , ("ability_decls", SomeRegularSymbolSing SAbilityDeclsSymbol)
    , ("struct_definition", SomeRegularSymbolSing SStructDefinitionSymbol)
    , ("datatype_fields", SomeRegularSymbolSing SDatatypeFieldsSymbol)
    , ("named_fields", SomeRegularSymbolSing SNamedFieldsSymbol)
    , ("field_annotation", SomeRegularSymbolSing SFieldAnnotationSymbol)
    , ("positional_fields", SomeRegularSymbolSing SPositionalFieldsSymbol)
    , ("postfix_ability_decls", SomeRegularSymbolSing SPostfixAbilityDeclsSymbol)
    , ("constant", SomeRegularSymbolSing SConstantSymbol)
    , ("enum_definition", SomeRegularSymbolSing SEnumDefinitionSymbol)
    , ("enum_variants", SomeRegularSymbolSing SEnumVariantsSymbol)
    , ("variant", SomeRegularSymbolSing SVariantSymbol)
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
    , ("@", SomeAnonymousSymbolSing SAtTokSymbol)
    , ("^", SomeAnonymousSymbolSing SXorTokSymbol)
    , ("aborts_if", SomeAnonymousSymbolSing SAbortsIfTokSymbol)
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
    , ("exists", SomeAnonymousSymbolSing SExistsTokSymbol)
    , ("false", SomeAnonymousSymbolSing SFalseTokSymbol)
    , ("forall", SomeAnonymousSymbolSing SForallTokSymbol)
    , ("friend", SomeAnonymousSymbolSing SFriendTokSymbol)
    , ("global", SomeAnonymousSymbolSing SGlobalTokSymbol)
    , ("internal", SomeAnonymousSymbolSing SInternalTokSymbol)
    , ("invariant", SomeAnonymousSymbolSing SInvariantTokSymbol)
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
    , ("requires", SomeAnonymousSymbolSing SRequiresTokSymbol)
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
