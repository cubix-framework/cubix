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
data HidBindL
data HidBindInternal0L
data HidEnumIdentifierL
data HidEnumItemL
data HidEnumSignatureL
data HidExistsL
data HidExpressionL
data HidExpressionTermL
data HidFieldIdentifierL
data HidForallL
data HidFunctionIdentifierL
data HidFunctionItemL
data HidFunctionSignatureL
data HidLiteralValueL
data HidMacroSignatureL
data HidMatchBodyL
data HidModuleIdentifierL
data HidReferenceL
data HidReservedIdentifierL
data HidSpecAbortIfL
data HidSpecAbortWithOrModifiesL
data HidSpecAbortWithOrModifiesInternal0L
data HidSpecBlockMemeberL
data HidSpecBlockTargetL
data HidSpecConditionL
data HidSpecConditionInternal0L
data HidSpecConditionKindL
data HidSpecFunctionL
data HidSpecFunctionSignatureL
data HidSpreadOperatorL
data HidStructIdentifierL
data HidStructItemL
data HidStructSignatureL
data HidTypeL
data HidTypeIdentifierL
data HidTypeParameterIdentifierL
data HidUnaryExpressionL
data HidUnaryExpressionInternal0L
data HidVariableIdentifierL
data HidVariantIdentifierL
data HidWhitespaceL
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
  ModuleBodyInternal1HidFunctionItem
    :: e HidFunctionItemL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  ModuleBodyInternal1HidStructItem
    :: e HidStructItemL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  ModuleBodyInternal1HidEnumItem
    :: e HidEnumItemL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L
  ModuleBodyInternal1SpecBlock
    :: e SpecBlockL
    -> ModuleBodyInternal1 e ModuleBodyInternal1L

data Constant e l where
  Constant
    :: e IdentifierL
    -> e HidTypeL
    -> e HidExpressionL
    -> Constant e ConstantL

data HidExpression e l where
  HidExpressionCallExpression
    :: e CallExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionMacroCallExpression
    :: e MacroCallExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionLambdaExpression
    :: e LambdaExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionIfExpression
    :: e IfExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionWhileExpression
    :: e WhileExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionReturnExpression
    :: e ReturnExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionAbortExpression
    :: e AbortExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionAssignExpression
    :: e AssignExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionHidUnaryExpression
    :: e HidUnaryExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionBinaryExpression
    :: e BinaryExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionCastExpression
    :: e CastExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionQuantifierExpression
    :: e QuantifierExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionMatchExpression
    :: e MatchExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionVectorExpression
    :: e VectorExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionLoopExpression
    :: e LoopExpressionL
    -> HidExpression e HidExpressionL
  HidExpressionIdentifiedExpression
    :: e IdentifiedExpressionL
    -> HidExpression e HidExpressionL

data AbortExpression e l where
  AbortExpression
    :: e AbortTokL
    -> e (Maybe HidExpressionL)
    -> AbortExpression e AbortExpressionL

data AssignExpression e l where
  AssignExpression
    :: e ((HidUnaryExpressionL, EqualsSignTokL), HidExpressionL)
    -> AssignExpression e AssignExpressionL

data HidUnaryExpression e l where
  HidUnaryExpression
    :: e HidUnaryExpressionInternal0L
    -> HidUnaryExpression e HidUnaryExpressionL

data HidUnaryExpressionInternal0 e l where
  HidUnaryExpressionInternal0UnaryExpression
    :: e UnaryExpressionL
    -> HidUnaryExpressionInternal0 e HidUnaryExpressionInternal0L
  HidUnaryExpressionInternal0BorrowExpression
    :: e BorrowExpressionL
    -> HidUnaryExpressionInternal0 e HidUnaryExpressionInternal0L
  HidUnaryExpressionInternal0DereferenceExpression
    :: e DereferenceExpressionL
    -> HidUnaryExpressionInternal0 e HidUnaryExpressionInternal0L
  HidUnaryExpressionInternal0MoveOrCopyExpression
    :: e MoveOrCopyExpressionL
    -> HidUnaryExpressionInternal0 e HidUnaryExpressionInternal0L
  HidUnaryExpressionInternal0HidExpressionTerm
    :: e HidExpressionTermL
    -> HidUnaryExpressionInternal0 e HidUnaryExpressionInternal0L

data BorrowExpression e l where
  BorrowExpression
    :: e (HidReferenceL, HidExpressionL)
    -> BorrowExpression e BorrowExpressionL

data HidReference e l where
  HidReferenceImmRef
    :: e ImmRefL
    -> HidReference e HidReferenceL
  HidReferenceMutRef
    :: e MutRefL
    -> HidReference e HidReferenceL

data ImmRef e l where
  ImmRef
    :: e AmpersandTokL
    -> ImmRef e ImmRefL

data MutRef e l where
  MutRef
    :: e AmpersandTokL
    -> e MutTokL
    -> MutRef e MutRefL

data DereferenceExpression e l where
  DereferenceExpression
    :: e (AsteriskTokL, HidExpressionL)
    -> DereferenceExpression e DereferenceExpressionL

data HidExpressionTerm e l where
  HidExpressionTermCallExpression
    :: e CallExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermBreakExpression
    :: e BreakExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermContinueExpression
    :: e ContinueExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermNameExpression
    :: e NameExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermMacroCallExpression
    :: e MacroCallExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermPackExpression
    :: e PackExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermHidLiteralValue
    :: e HidLiteralValueL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermUnitExpression
    :: e UnitExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermExpressionList
    :: e ExpressionListL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermAnnotationExpression
    :: e AnnotationExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermBlock
    :: e BlockL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermSpecBlock
    :: e SpecBlockL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermIfExpression
    :: e IfExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermDotExpression
    :: e DotExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermIndexExpression
    :: e IndexExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermVectorExpression
    :: e VectorExpressionL
    -> HidExpressionTerm e HidExpressionTermL
  HidExpressionTermMatchExpression
    :: e MatchExpressionL
    -> HidExpressionTerm e HidExpressionTermL

data AnnotationExpression e l where
  AnnotationExpression
    :: e HidExpressionL
    -> e HidTypeL
    -> e RightParenthesisTokL
    -> AnnotationExpression e AnnotationExpressionL

data HidType e l where
  HidTypeApplyType
    :: e ApplyTypeL
    -> HidType e HidTypeL
  HidTypeRefType
    :: e RefTypeL
    -> HidType e HidTypeL
  HidTypeTupleType
    :: e TupleTypeL
    -> HidType e HidTypeL
  HidTypeFunctionType
    :: e FunctionTypeL
    -> HidType e HidTypeL
  HidTypePrimitiveType
    :: e PrimitiveTypeL
    -> HidType e HidTypeL

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
    :: e HidModuleIdentifierL
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
    :: e HidReservedIdentifierL
    -> ModuleAccess e ModuleAccessL

data HidModuleIdentifier e l where
  HidModuleIdentifier
    :: e IdentifierL
    -> HidModuleIdentifier e HidModuleIdentifierL

data Identifier e l where
  Identifier
    :: Text
    -> Identifier e IdentifierL

data HidReservedIdentifier e l where
  HidReservedIdentifierHidForall
    :: e HidForallL
    -> HidReservedIdentifier e HidReservedIdentifierL
  HidReservedIdentifierHidExists
    :: e HidExistsL
    -> HidReservedIdentifier e HidReservedIdentifierL

data HidExists e l where
  HidExists
    :: e ExistsTokL
    -> HidExists e HidExistsL

data HidForall e l where
  HidForall
    :: e ForallTokL
    -> HidForall e HidForallL

data ModuleIdentity e l where
  ModuleIdentity
    :: e ModuleIdentityInternal0L
    -> e ColonColonTokL
    -> e HidModuleIdentifierL
    -> ModuleIdentity e ModuleIdentityL

data ModuleIdentityInternal0 e l where
  ModuleIdentityInternal0NumLiteral
    :: e NumLiteralL
    -> ModuleIdentityInternal0 e ModuleIdentityInternal0L
  ModuleIdentityInternal0HidModuleIdentifier
    :: e HidModuleIdentifierL
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
    :: e [HidTypeL]
    -> TypeArguments e TypeArgumentsL

data FunctionType e l where
  FunctionType
    :: e FunctionTypeParametersL
    -> e (Maybe (HyphenMinusGreaterThanSignTokL, HidTypeL))
    -> FunctionType e FunctionTypeL

data FunctionTypeParameters e l where
  FunctionTypeParameters
    :: e [HidTypeL]
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
    :: e HidReferenceL
    -> e HidTypeL
    -> RefType e RefTypeL

data TupleType e l where
  TupleType
    :: e [HidTypeL]
    -> TupleType e TupleTypeL

data Block e l where
  Block
    :: e LeftCurlyBracketTokL
    -> e [UseDeclarationL]
    -> e [BlockItemL]
    -> e (Maybe HidExpressionL)
    -> e RightCurlyBracketTokL
    -> Block e BlockL

data BlockItem e l where
  BlockItem
    :: e BlockItemInternal0L
    -> e SemicolonTokL
    -> BlockItem e BlockItemL

data BlockItemInternal0 e l where
  BlockItemInternal0HidExpression
    :: e HidExpressionL
    -> BlockItemInternal0 e BlockItemInternal0L
  BlockItemInternal0LetStatement
    :: e LetStatementL
    -> BlockItemInternal0 e BlockItemInternal0L

data LetStatement e l where
  LetStatement
    :: e LetTokL
    -> e BindListL
    -> e (Maybe (ColonTokL, HidTypeL))
    -> e (Maybe (EqualsSignTokL, HidExpressionL))
    -> LetStatement e LetStatementL

data BindList e l where
  BindListHidBind
    :: e HidBindL
    -> BindList e BindListL
  BindListCommaBindList
    :: e CommaBindListL
    -> BindList e BindListL
  BindListOrBindList
    :: e OrBindListL
    -> BindList e BindListL

data CommaBindList e l where
  CommaBindList
    :: e [HidBindL]
    -> CommaBindList e CommaBindListL

data HidBind e l where
  HidBindHidBindInternal0
    :: e HidBindInternal0L
    -> HidBind e HidBindL
  HidBindBindUnpack
    :: e BindUnpackL
    -> HidBind e HidBindL
  HidBindAtBind
    :: e AtBindL
    -> HidBind e HidBindL
  HidBindHidLiteralValue
    :: e HidLiteralValueL
    -> HidBind e HidBindL

data AtBind e l where
  AtBind
    :: e HidVariableIdentifierL
    -> e CommercialAtTokL
    -> e BindListL
    -> AtBind e AtBindL

data HidVariableIdentifier e l where
  HidVariableIdentifier
    :: e IdentifierL
    -> HidVariableIdentifier e HidVariableIdentifierL

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
  BindFieldHidSpreadOperator
    :: e HidSpreadOperatorL
    -> BindField e BindFieldL

data HidSpreadOperator e l where
  HidSpreadOperator
    :: e FullStopFullStopTokL
    -> HidSpreadOperator e HidSpreadOperatorL

data MutBindField e l where
  MutBindField
    :: e MutTokL
    -> e BindFieldL
    -> MutBindField e MutBindFieldL

data BindPositionalFields e l where
  BindPositionalFields
    :: e [BindNamedFieldsInternal0L]
    -> BindPositionalFields e BindPositionalFieldsL

data NameExpression e l where
  NameExpression
    :: e (Maybe ColonColonTokL)
    -> e ModuleAccessL
    -> NameExpression e NameExpressionL

data HidBindInternal0 e l where
  HidBindInternal0MutBindVar
    :: e MutBindVarL
    -> HidBindInternal0 e HidBindInternal0L
  HidBindInternal0HidVariableIdentifier
    :: e HidVariableIdentifierL
    -> HidBindInternal0 e HidBindInternal0L

data MutBindVar e l where
  MutBindVar
    :: e MutTokL
    -> e HidVariableIdentifierL
    -> MutBindVar e MutBindVarL

data HidLiteralValue e l where
  HidLiteralValueAddressLiteral
    :: e AddressLiteralL
    -> HidLiteralValue e HidLiteralValueL
  HidLiteralValueBoolLiteral
    :: e BoolLiteralL
    -> HidLiteralValue e HidLiteralValueL
  HidLiteralValueNumLiteral
    :: e NumLiteralL
    -> HidLiteralValue e HidLiteralValueL
  HidLiteralValueHexStringLiteral
    :: e HexStringLiteralL
    -> HidLiteralValue e HidLiteralValueL
  HidLiteralValueByteStringLiteral
    :: e ByteStringLiteralL
    -> HidLiteralValue e HidLiteralValueL

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

data OrBindList e l where
  OrBindList
    :: e (Maybe LeftParenthesisTokL)
    -> e [((Maybe LeftParenthesisTokL, HidBindL), Maybe RightParenthesisTokL)]
    -> e (Maybe RightParenthesisTokL)
    -> OrBindList e OrBindListL

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
    -> e ((ModuleAccessL, FullStopTokL), HidFunctionIdentifierL)
    -> UseFun e UseFunL

data HidFunctionIdentifier e l where
  HidFunctionIdentifier
    :: e IdentifierL
    -> HidFunctionIdentifier e HidFunctionIdentifierL

data UseModule e l where
  UseModule
    :: e ModuleIdentityL
    -> e (Maybe (AsTokL, HidModuleIdentifierL))
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

data BreakExpression e l where
  BreakExpression
    :: e BreakTokL
    -> e (Maybe LabelL)
    -> e (Maybe HidExpressionL)
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
    :: e [HidExpressionL]
    -> ArgList e ArgListL

data ContinueExpression e l where
  ContinueExpression
    :: e ContinueTokL
    -> e (Maybe LabelL)
    -> ContinueExpression e ContinueExpressionL

data DotExpression e l where
  DotExpression
    :: e ((HidExpressionTermL, FullStopTokL), HidExpressionTermL)
    -> DotExpression e DotExpressionL

data ExpressionList e l where
  ExpressionList
    :: e [HidExpressionL]
    -> ExpressionList e ExpressionListL

data IfExpression e l where
  IfExpression
    :: e (((IfTokL, HidExpressionL), HidExpressionL), Maybe (ElseTokL, HidExpressionL))
    -> IfExpression e IfExpressionL

data IndexExpression e l where
  IndexExpression
    :: e (HidExpressionTermL, [HidExpressionL])
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
    -> e HidExpressionL
    -> e HidMatchBodyL
    -> MatchExpression e MatchExpressionL

data HidMatchBody e l where
  HidMatchBody
    :: e [MatchArmL]
    -> HidMatchBody e HidMatchBodyL

data MatchArm e l where
  MatchArm
    :: e BindListL
    -> e (Maybe MatchConditionL)
    -> e EqualsSignGreaterThanSignTokL
    -> e HidExpressionL
    -> MatchArm e MatchArmL

data MatchCondition e l where
  MatchCondition
    :: e IfTokL
    -> e HidExpressionL
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
    :: e HidFieldIdentifierL
    -> e (Maybe (ColonTokL, HidExpressionL))
    -> ExpField e ExpFieldL

data HidFieldIdentifier e l where
  HidFieldIdentifier
    :: e IdentifierL
    -> HidFieldIdentifier e HidFieldIdentifierL

data SpecBlock e l where
  SpecBlock
    :: e SpecTokL
    -> e SpecBlockInternal0L
    -> SpecBlock e SpecBlockL

data SpecBlockInternal0 e l where
  SpecBlockInternal01
    :: e (Maybe HidSpecBlockTargetL)
    -> e SpecBodyL
    -> SpecBlockInternal0 e SpecBlockInternal0L
  SpecBlockInternal0HidSpecFunction
    :: e HidSpecFunctionL
    -> SpecBlockInternal0 e SpecBlockInternal0L

data HidSpecBlockTarget e l where
  HidSpecBlockTargetIdentifier
    :: e IdentifierL
    -> HidSpecBlockTarget e HidSpecBlockTargetL
  HidSpecBlockTargetModule
    :: e ModuleTokL
    -> HidSpecBlockTarget e HidSpecBlockTargetL
  HidSpecBlockTargetSpecBlockTargetSchema
    :: e SpecBlockTargetSchemaL
    -> HidSpecBlockTarget e HidSpecBlockTargetL

data SpecBlockTargetSchema e l where
  SpecBlockTargetSchema
    :: e SchemaTokL
    -> e HidStructIdentifierL
    -> e (Maybe TypeParametersL)
    -> SpecBlockTargetSchema e SpecBlockTargetSchemaL

data HidStructIdentifier e l where
  HidStructIdentifier
    :: e IdentifierL
    -> HidStructIdentifier e HidStructIdentifierL

data TypeParameters e l where
  TypeParameters
    :: e [TypeParameterL]
    -> TypeParameters e TypeParametersL

data TypeParameter e l where
  TypeParameter
    :: e (Maybe DollarSignTokL)
    -> e (Maybe PhantomTokL)
    -> e HidTypeParameterIdentifierL
    -> e (Maybe (ColonTokL, [AbilityL]))
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

data HidTypeParameterIdentifier e l where
  HidTypeParameterIdentifier
    :: e IdentifierL
    -> HidTypeParameterIdentifier e HidTypeParameterIdentifierL

data HidSpecFunction e l where
  HidSpecFunctionNativeSpecFunction
    :: e NativeSpecFunctionL
    -> HidSpecFunction e HidSpecFunctionL
  HidSpecFunctionUsualSpecFunction
    :: e UsualSpecFunctionL
    -> HidSpecFunction e HidSpecFunctionL
  HidSpecFunctionUninterpretedSpecFunction
    :: e UninterpretedSpecFunctionL
    -> HidSpecFunction e HidSpecFunctionL

data NativeSpecFunction e l where
  NativeSpecFunction
    :: e NativeTokL
    -> e HidSpecFunctionSignatureL
    -> NativeSpecFunction e NativeSpecFunctionL

data HidSpecFunctionSignature e l where
  HidSpecFunctionSignature
    :: e HidFunctionIdentifierL
    -> e (Maybe TypeParametersL)
    -> e FunctionParametersL
    -> e RetTypeL
    -> HidSpecFunctionSignature e HidSpecFunctionSignatureL

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
    -> e HidTypeL
    -> FunctionParameter e FunctionParameterL

data FunctionParameterInternal0 e l where
  FunctionParameterInternal0Name
    :: e HidVariableIdentifierL
    -> FunctionParameterInternal0 e FunctionParameterInternal0L
  FunctionParameterInternal02
    :: e DollarSignTokL
    -> e HidVariableIdentifierL
    -> FunctionParameterInternal0 e FunctionParameterInternal0L

data MutFunctionParameter e l where
  MutFunctionParameter
    :: e MutTokL
    -> e FunctionParameterL
    -> MutFunctionParameter e MutFunctionParameterL

data RetType e l where
  RetType
    :: e ColonTokL
    -> e HidTypeL
    -> RetType e RetTypeL

data UninterpretedSpecFunction e l where
  UninterpretedSpecFunction
    :: e HidSpecFunctionSignatureL
    -> UninterpretedSpecFunction e UninterpretedSpecFunctionL

data UsualSpecFunction e l where
  UsualSpecFunction
    :: e FunTokL
    -> e HidSpecFunctionSignatureL
    -> e BlockL
    -> UsualSpecFunction e UsualSpecFunctionL

data SpecBody e l where
  SpecBody
    :: e LeftCurlyBracketTokL
    -> e [UseDeclarationL]
    -> e [HidSpecBlockMemeberL]
    -> e RightCurlyBracketTokL
    -> SpecBody e SpecBodyL

data HidSpecBlockMemeber e l where
  HidSpecBlockMemeberSpecInvariant
    :: e SpecInvariantL
    -> HidSpecBlockMemeber e HidSpecBlockMemeberL
  HidSpecBlockMemeberHidSpecFunction
    :: e HidSpecFunctionL
    -> HidSpecBlockMemeber e HidSpecBlockMemeberL
  HidSpecBlockMemeberSpecCondition
    :: e SpecConditionL
    -> HidSpecBlockMemeber e HidSpecBlockMemeberL
  HidSpecBlockMemeberSpecInclude
    :: e SpecIncludeL
    -> HidSpecBlockMemeber e HidSpecBlockMemeberL
  HidSpecBlockMemeberSpecApply
    :: e SpecApplyL
    -> HidSpecBlockMemeber e HidSpecBlockMemeberL
  HidSpecBlockMemeberSpecPragma
    :: e SpecPragmaL
    -> HidSpecBlockMemeber e HidSpecBlockMemeberL
  HidSpecBlockMemeberSpecVariable
    :: e SpecVariableL
    -> HidSpecBlockMemeber e HidSpecBlockMemeberL
  HidSpecBlockMemeberSpecLet
    :: e SpecLetL
    -> HidSpecBlockMemeber e HidSpecBlockMemeberL

data SpecApply e l where
  SpecApply
    :: e HidExpressionL
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
  SpecConditionHidSpecCondition
    :: e HidSpecConditionL
    -> SpecCondition e SpecConditionL
  SpecConditionHidSpecAbortIf
    :: e HidSpecAbortIfL
    -> SpecCondition e SpecConditionL
  SpecConditionHidSpecAbortWithOrModifies
    :: e HidSpecAbortWithOrModifiesL
    -> SpecCondition e SpecConditionL

data HidSpecAbortIf e l where
  HidSpecAbortIf
    :: e AbortsIfTokL
    -> e (Maybe ConditionPropertiesL)
    -> e HidExpressionL
    -> e (Maybe (WithTokL, HidExpressionL))
    -> e SemicolonTokL
    -> HidSpecAbortIf e HidSpecAbortIfL

data ConditionProperties e l where
  ConditionProperties
    :: e [SpecPropertyL]
    -> ConditionProperties e ConditionPropertiesL

data SpecProperty e l where
  SpecProperty
    :: e IdentifierL
    -> e (Maybe (EqualsSignTokL, HidLiteralValueL))
    -> SpecProperty e SpecPropertyL

data HidSpecAbortWithOrModifies e l where
  HidSpecAbortWithOrModifies
    :: e HidSpecAbortWithOrModifiesInternal0L
    -> e (Maybe ConditionPropertiesL)
    -> e [HidExpressionL]
    -> e SemicolonTokL
    -> HidSpecAbortWithOrModifies e HidSpecAbortWithOrModifiesL

data HidSpecAbortWithOrModifiesInternal0 e l where
  HidSpecAbortWithOrModifiesInternal0AbortsWith
    :: e AbortsWithTokL
    -> HidSpecAbortWithOrModifiesInternal0 e HidSpecAbortWithOrModifiesInternal0L
  HidSpecAbortWithOrModifiesInternal0Modifies
    :: e ModifiesTokL
    -> HidSpecAbortWithOrModifiesInternal0 e HidSpecAbortWithOrModifiesInternal0L

data HidSpecCondition e l where
  HidSpecCondition
    :: e HidSpecConditionInternal0L
    -> e (Maybe ConditionPropertiesL)
    -> e HidExpressionL
    -> e SemicolonTokL
    -> HidSpecCondition e HidSpecConditionL

data HidSpecConditionInternal0 e l where
  HidSpecConditionInternal0Kind
    :: e HidSpecConditionKindL
    -> HidSpecConditionInternal0 e HidSpecConditionInternal0L
  HidSpecConditionInternal02
    :: e RequiresTokL
    -> e (Maybe ModuleTokL)
    -> HidSpecConditionInternal0 e HidSpecConditionInternal0L

data HidSpecConditionKind e l where
  HidSpecConditionKindAssert
    :: e AssertTokL
    -> HidSpecConditionKind e HidSpecConditionKindL
  HidSpecConditionKindAssume
    :: e AssumeTokL
    -> HidSpecConditionKind e HidSpecConditionKindL
  HidSpecConditionKindDecreases
    :: e DecreasesTokL
    -> HidSpecConditionKind e HidSpecConditionKindL
  HidSpecConditionKindEnsures
    :: e EnsuresTokL
    -> HidSpecConditionKind e HidSpecConditionKindL
  HidSpecConditionKindSucceedsIf
    :: e SucceedsIfTokL
    -> HidSpecConditionKind e HidSpecConditionKindL

data SpecInclude e l where
  SpecInclude
    :: e HidExpressionL
    -> SpecInclude e SpecIncludeL

data SpecInvariant e l where
  SpecInvariant
    :: e InvariantTokL
    -> e (Maybe SpecInvariantInternal0L)
    -> e (Maybe ConditionPropertiesL)
    -> e HidExpressionL
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
    -> e HidExpressionL
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
    -> e HidTypeL
    -> SpecVariable e SpecVariableL

data SpecVariableInternal0 e l where
  SpecVariableInternal0Global
    :: e GlobalTokL
    -> SpecVariableInternal0 e SpecVariableInternal0L
  SpecVariableInternal0Local
    :: e LocalTokL
    -> SpecVariableInternal0 e SpecVariableInternal0L

data UnitExpression e l where
  UnitExpression
    :: e LeftParenthesisTokL
    -> e RightParenthesisTokL
    -> UnitExpression e UnitExpressionL

data VectorExpression e l where
  VectorExpression
    :: e VectorExpressionInternal0L
    -> e [HidExpressionL]
    -> e RightSquareBracketTokL
    -> VectorExpression e VectorExpressionL

data VectorExpressionInternal0 e l where
  VectorExpressionInternal0VectorLeftSquareBracket
    :: e VectorLeftSquareBracketTokL
    -> VectorExpressionInternal0 e VectorExpressionInternal0L
  VectorExpressionInternal02
    :: e [HidTypeL]
    -> e LeftSquareBracketTokL
    -> VectorExpressionInternal0 e VectorExpressionInternal0L

data MoveOrCopyExpression e l where
  MoveOrCopyExpression
    :: e (MoveOrCopyExpressionInternal0L, HidExpressionL)
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
    -> e HidExpressionL
    -> UnaryExpression e UnaryExpressionL

data UnaryOp e l where
  UnaryOp
    :: e ExclamationMarkTokL
    -> UnaryOp e UnaryOpL

data BinaryExpression e l where
  BinaryExpression1
    :: e HidExpressionL
    -> e EqualsSignEqualsSignGreaterThanSignTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression2
    :: e HidExpressionL
    -> e VerticalLineVerticalLineTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression3
    :: e HidExpressionL
    -> e AmpersandAmpersandTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression4
    :: e HidExpressionL
    -> e EqualsSignEqualsSignTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression5
    :: e HidExpressionL
    -> e ExclamationMarkEqualsSignTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression6
    :: e HidExpressionL
    -> e LessThanSignTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression7
    :: e HidExpressionL
    -> e GreaterThanSignTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression8
    :: e HidExpressionL
    -> e LessThanSignEqualsSignTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression9
    :: e HidExpressionL
    -> e GreaterThanSignEqualsSignTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression10
    :: e HidExpressionL
    -> e FullStopFullStopTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression11
    :: e HidExpressionL
    -> e VerticalLineTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression12
    :: e HidExpressionL
    -> e CircumflexAccentTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression13
    :: e HidExpressionL
    -> e AmpersandTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression14
    :: e HidExpressionL
    -> e LessThanSignLessThanSignTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression15
    :: e HidExpressionL
    -> e GreaterThanSignGreaterThanSignTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression16
    :: e HidExpressionL
    -> e PlusSignTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression17
    :: e HidExpressionL
    -> e HyphenMinusTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression18
    :: e HidExpressionL
    -> e AsteriskTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression19
    :: e HidExpressionL
    -> e SolidusTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL
  BinaryExpression20
    :: e HidExpressionL
    -> e PercentSignTokL
    -> e HidExpressionL
    -> BinaryExpression e BinaryExpressionL

data CastExpression e l where
  CastExpression
    :: e ((HidExpressionL, AsTokL), HidTypeL)
    -> CastExpression e CastExpressionL

data IdentifiedExpression e l where
  IdentifiedExpression
    :: e BlockIdentifierL
    -> e HidExpressionL
    -> IdentifiedExpression e IdentifiedExpressionL

data BlockIdentifier e l where
  BlockIdentifier
    :: e LabelL
    -> e ColonTokL
    -> BlockIdentifier e BlockIdentifierL

data LambdaExpression e l where
  LambdaExpression
    :: e LambdaBindingsL
    -> e (Maybe (HyphenMinusGreaterThanSignTokL, HidTypeL))
    -> e HidExpressionL
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
    :: e HidBindL
    -> e (Maybe (ColonTokL, HidTypeL))
    -> LambdaBinding e LambdaBindingL

data LoopExpression e l where
  LoopExpression
    :: e LoopTokL
    -> e HidExpressionL
    -> LoopExpression e LoopExpressionL

data QuantifierExpression e l where
  QuantifierExpression
    :: e ((((HidReservedIdentifierL, QuantifierBindingsL), Maybe (WhereTokL, HidExpressionL)), ColonTokL), HidExpressionL)
    -> QuantifierExpression e QuantifierExpressionL

data QuantifierBindings e l where
  QuantifierBindings
    :: e [QuantifierBindingL]
    -> QuantifierBindings e QuantifierBindingsL

data QuantifierBinding e l where
  QuantifierBinding1
    :: e IdentifierL
    -> e ColonTokL
    -> e HidTypeL
    -> QuantifierBinding e QuantifierBindingL
  QuantifierBinding2
    :: e IdentifierL
    -> e InTokL
    -> e HidExpressionL
    -> QuantifierBinding e QuantifierBindingL

data ReturnExpression e l where
  ReturnExpression1
    :: e ReturnTokL
    -> e (Maybe LabelL)
    -> e HidExpressionL
    -> ReturnExpression e ReturnExpressionL
  ReturnExpression2
    :: e ReturnTokL
    -> e (Maybe LabelL)
    -> ReturnExpression e ReturnExpressionL

data WhileExpression e l where
  WhileExpression
    :: e WhileTokL
    -> e HidExpressionL
    -> e HidExpressionL
    -> WhileExpression e WhileExpressionL

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

data HidEnumItem e l where
  HidEnumItem
    :: e EnumDefinitionL
    -> HidEnumItem e HidEnumItemL

data EnumDefinition e l where
  EnumDefinition
    :: e (Maybe PublicTokL)
    -> e HidEnumSignatureL
    -> e EnumVariantsL
    -> e (Maybe PostfixAbilityDeclsL)
    -> EnumDefinition e EnumDefinitionL

data EnumVariants e l where
  EnumVariants
    :: e [VariantL]
    -> EnumVariants e EnumVariantsL

data Variant e l where
  Variant
    :: e HidVariantIdentifierL
    -> e (Maybe DatatypeFieldsL)
    -> Variant e VariantL

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
    :: e HidFieldIdentifierL
    -> e ColonTokL
    -> e HidTypeL
    -> FieldAnnotation e FieldAnnotationL

data PositionalFields e l where
  PositionalFields
    :: e [HidTypeL]
    -> PositionalFields e PositionalFieldsL

data HidVariantIdentifier e l where
  HidVariantIdentifier
    :: e IdentifierL
    -> HidVariantIdentifier e HidVariantIdentifierL

data HidEnumSignature e l where
  HidEnumSignature
    :: e EnumTokL
    -> e HidEnumIdentifierL
    -> e (Maybe TypeParametersL)
    -> e (Maybe AbilityDeclsL)
    -> HidEnumSignature e HidEnumSignatureL

data AbilityDecls e l where
  AbilityDecls
    :: e HasTokL
    -> e [AbilityL]
    -> AbilityDecls e AbilityDeclsL

data HidEnumIdentifier e l where
  HidEnumIdentifier
    :: e IdentifierL
    -> HidEnumIdentifier e HidEnumIdentifierL

data PostfixAbilityDecls e l where
  PostfixAbilityDecls
    :: e [AbilityL]
    -> PostfixAbilityDecls e PostfixAbilityDeclsL

data HidFunctionItem e l where
  HidFunctionItemNativeFunctionDefinition
    :: e NativeFunctionDefinitionL
    -> HidFunctionItem e HidFunctionItemL
  HidFunctionItemMacroFunctionDefinition
    :: e MacroFunctionDefinitionL
    -> HidFunctionItem e HidFunctionItemL
  HidFunctionItemFunctionDefinition
    :: e FunctionDefinitionL
    -> HidFunctionItem e HidFunctionItemL

data FunctionDefinition e l where
  FunctionDefinition
    :: e HidFunctionSignatureL
    -> e BlockL
    -> FunctionDefinition e FunctionDefinitionL

data HidFunctionSignature e l where
  HidFunctionSignature
    :: e (Maybe ModifierL)
    -> e (Maybe ModifierL)
    -> e (Maybe ModifierL)
    -> e FunTokL
    -> e HidFunctionIdentifierL
    -> e (Maybe TypeParametersL)
    -> e FunctionParametersL
    -> e (Maybe RetTypeL)
    -> HidFunctionSignature e HidFunctionSignatureL

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
    -> e MacroTokL
    -> e HidMacroSignatureL
    -> e BlockL
    -> MacroFunctionDefinition e MacroFunctionDefinitionL

data HidMacroSignature e l where
  HidMacroSignature
    :: e (Maybe ModifierL)
    -> e FunTokL
    -> e HidFunctionIdentifierL
    -> e (Maybe TypeParametersL)
    -> e FunctionParametersL
    -> e (Maybe RetTypeL)
    -> HidMacroSignature e HidMacroSignatureL

data NativeFunctionDefinition e l where
  NativeFunctionDefinition
    :: e HidFunctionSignatureL
    -> e SemicolonTokL
    -> NativeFunctionDefinition e NativeFunctionDefinitionL

data HidStructItem e l where
  HidStructItemNativeStructDefinition
    :: e NativeStructDefinitionL
    -> HidStructItem e HidStructItemL
  HidStructItemStructDefinition
    :: e StructDefinitionL
    -> HidStructItem e HidStructItemL

data NativeStructDefinition e l where
  NativeStructDefinition
    :: e (Maybe PublicTokL)
    -> e HidStructSignatureL
    -> NativeStructDefinition e NativeStructDefinitionL

data HidStructSignature e l where
  HidStructSignature
    :: e StructTokL
    -> e HidStructIdentifierL
    -> e (Maybe TypeParametersL)
    -> e (Maybe AbilityDeclsL)
    -> HidStructSignature e HidStructSignatureL

data StructDefinition e l where
  StructDefinition
    :: e (Maybe PublicTokL)
    -> e HidStructSignatureL
    -> e DatatypeFieldsL
    -> e (Maybe PostfixAbilityDeclsL)
    -> StructDefinition e StructDefinitionL

deriveAll
  [ ''SourceFile
  , ''ModuleDefinition
  , ''ModuleBody
  , ''ModuleBodyInternal0
  , ''ModuleBodyInternal1
  , ''Constant
  , ''HidExpression
  , ''AbortExpression
  , ''AssignExpression
  , ''HidUnaryExpression
  , ''HidUnaryExpressionInternal0
  , ''BorrowExpression
  , ''HidReference
  , ''ImmRef
  , ''MutRef
  , ''DereferenceExpression
  , ''HidExpressionTerm
  , ''AnnotationExpression
  , ''HidType
  , ''ApplyType
  , ''ModuleAccess
  , ''HidModuleIdentifier
  , ''Identifier
  , ''HidReservedIdentifier
  , ''HidExists
  , ''HidForall
  , ''ModuleIdentity
  , ''ModuleIdentityInternal0
  , ''NumLiteral
  , ''NumLiteralInternal0
  , ''TypeArguments
  , ''FunctionType
  , ''FunctionTypeParameters
  , ''PrimitiveType
  , ''RefType
  , ''TupleType
  , ''Block
  , ''BlockItem
  , ''BlockItemInternal0
  , ''LetStatement
  , ''BindList
  , ''CommaBindList
  , ''HidBind
  , ''AtBind
  , ''HidVariableIdentifier
  , ''BindUnpack
  , ''BindFields
  , ''BindNamedFields
  , ''BindNamedFieldsInternal0
  , ''BindField
  , ''HidSpreadOperator
  , ''MutBindField
  , ''BindPositionalFields
  , ''NameExpression
  , ''HidBindInternal0
  , ''MutBindVar
  , ''HidLiteralValue
  , ''AddressLiteral
  , ''BoolLiteral
  , ''ByteStringLiteral
  , ''HexStringLiteral
  , ''OrBindList
  , ''UseDeclaration
  , ''UseDeclarationInternal0
  , ''UseFun
  , ''HidFunctionIdentifier
  , ''UseModule
  , ''UseModuleMember
  , ''UseMember
  , ''UseModuleMembers
  , ''BreakExpression
  , ''Label
  , ''CallExpression
  , ''ArgList
  , ''ContinueExpression
  , ''DotExpression
  , ''ExpressionList
  , ''IfExpression
  , ''IndexExpression
  , ''MacroCallExpression
  , ''MacroModuleAccess
  , ''MatchExpression
  , ''HidMatchBody
  , ''MatchArm
  , ''MatchCondition
  , ''PackExpression
  , ''FieldInitializeList
  , ''ExpField
  , ''HidFieldIdentifier
  , ''SpecBlock
  , ''SpecBlockInternal0
  , ''HidSpecBlockTarget
  , ''SpecBlockTargetSchema
  , ''HidStructIdentifier
  , ''TypeParameters
  , ''TypeParameter
  , ''Ability
  , ''HidTypeParameterIdentifier
  , ''HidSpecFunction
  , ''NativeSpecFunction
  , ''HidSpecFunctionSignature
  , ''FunctionParameters
  , ''FunctionParametersInternal0
  , ''FunctionParameter
  , ''FunctionParameterInternal0
  , ''MutFunctionParameter
  , ''RetType
  , ''UninterpretedSpecFunction
  , ''UsualSpecFunction
  , ''SpecBody
  , ''HidSpecBlockMemeber
  , ''SpecApply
  , ''SpecApplyPattern
  , ''SpecApplyNamePattern
  , ''SpecApplyPatternInternal0
  , ''SpecCondition
  , ''HidSpecAbortIf
  , ''ConditionProperties
  , ''SpecProperty
  , ''HidSpecAbortWithOrModifies
  , ''HidSpecAbortWithOrModifiesInternal0
  , ''HidSpecCondition
  , ''HidSpecConditionInternal0
  , ''HidSpecConditionKind
  , ''SpecInclude
  , ''SpecInvariant
  , ''SpecInvariantInternal0
  , ''SpecLet
  , ''SpecPragma
  , ''SpecVariable
  , ''SpecVariableInternal0
  , ''UnitExpression
  , ''VectorExpression
  , ''VectorExpressionInternal0
  , ''MoveOrCopyExpression
  , ''MoveOrCopyExpressionInternal0
  , ''UnaryExpression
  , ''UnaryOp
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
  , ''FriendDeclaration
  , ''FriendAccess
  , ''HidEnumItem
  , ''EnumDefinition
  , ''EnumVariants
  , ''Variant
  , ''DatatypeFields
  , ''NamedFields
  , ''FieldAnnotation
  , ''PositionalFields
  , ''HidVariantIdentifier
  , ''HidEnumSignature
  , ''AbilityDecls
  , ''HidEnumIdentifier
  , ''PostfixAbilityDecls
  , ''HidFunctionItem
  , ''FunctionDefinition
  , ''HidFunctionSignature
  , ''Modifier
  , ''ModifierInternal0
  , ''MacroFunctionDefinition
  , ''HidMacroSignature
  , ''NativeFunctionDefinition
  , ''HidStructItem
  , ''NativeStructDefinition
  , ''HidStructSignature
  , ''StructDefinition
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
   , Constant
   , HidExpression
   , AbortExpression
   , AssignExpression
   , HidUnaryExpression
   , HidUnaryExpressionInternal0
   , BorrowExpression
   , HidReference
   , ImmRef
   , MutRef
   , DereferenceExpression
   , HidExpressionTerm
   , AnnotationExpression
   , HidType
   , ApplyType
   , ModuleAccess
   , HidModuleIdentifier
   , Identifier
   , HidReservedIdentifier
   , HidExists
   , HidForall
   , ModuleIdentity
   , ModuleIdentityInternal0
   , NumLiteral
   , NumLiteralInternal0
   , TypeArguments
   , FunctionType
   , FunctionTypeParameters
   , PrimitiveType
   , RefType
   , TupleType
   , Block
   , BlockItem
   , BlockItemInternal0
   , LetStatement
   , BindList
   , CommaBindList
   , HidBind
   , AtBind
   , HidVariableIdentifier
   , BindUnpack
   , BindFields
   , BindNamedFields
   , BindNamedFieldsInternal0
   , BindField
   , HidSpreadOperator
   , MutBindField
   , BindPositionalFields
   , NameExpression
   , HidBindInternal0
   , MutBindVar
   , HidLiteralValue
   , AddressLiteral
   , BoolLiteral
   , ByteStringLiteral
   , HexStringLiteral
   , OrBindList
   , UseDeclaration
   , UseDeclarationInternal0
   , UseFun
   , HidFunctionIdentifier
   , UseModule
   , UseModuleMember
   , UseMember
   , UseModuleMembers
   , BreakExpression
   , Label
   , CallExpression
   , ArgList
   , ContinueExpression
   , DotExpression
   , ExpressionList
   , IfExpression
   , IndexExpression
   , MacroCallExpression
   , MacroModuleAccess
   , MatchExpression
   , HidMatchBody
   , MatchArm
   , MatchCondition
   , PackExpression
   , FieldInitializeList
   , ExpField
   , HidFieldIdentifier
   , SpecBlock
   , SpecBlockInternal0
   , HidSpecBlockTarget
   , SpecBlockTargetSchema
   , HidStructIdentifier
   , TypeParameters
   , TypeParameter
   , Ability
   , HidTypeParameterIdentifier
   , HidSpecFunction
   , NativeSpecFunction
   , HidSpecFunctionSignature
   , FunctionParameters
   , FunctionParametersInternal0
   , FunctionParameter
   , FunctionParameterInternal0
   , MutFunctionParameter
   , RetType
   , UninterpretedSpecFunction
   , UsualSpecFunction
   , SpecBody
   , HidSpecBlockMemeber
   , SpecApply
   , SpecApplyPattern
   , SpecApplyNamePattern
   , SpecApplyPatternInternal0
   , SpecCondition
   , HidSpecAbortIf
   , ConditionProperties
   , SpecProperty
   , HidSpecAbortWithOrModifies
   , HidSpecAbortWithOrModifiesInternal0
   , HidSpecCondition
   , HidSpecConditionInternal0
   , HidSpecConditionKind
   , SpecInclude
   , SpecInvariant
   , SpecInvariantInternal0
   , SpecLet
   , SpecPragma
   , SpecVariable
   , SpecVariableInternal0
   , UnitExpression
   , VectorExpression
   , VectorExpressionInternal0
   , MoveOrCopyExpression
   , MoveOrCopyExpressionInternal0
   , UnaryExpression
   , UnaryOp
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
   , FriendDeclaration
   , FriendAccess
   , HidEnumItem
   , EnumDefinition
   , EnumVariants
   , Variant
   , DatatypeFields
   , NamedFields
   , FieldAnnotation
   , PositionalFields
   , HidVariantIdentifier
   , HidEnumSignature
   , AbilityDecls
   , HidEnumIdentifier
   , PostfixAbilityDecls
   , HidFunctionItem
   , FunctionDefinition
   , HidFunctionSignature
   , Modifier
   , ModifierInternal0
   , MacroFunctionDefinition
   , HidMacroSignature
   , NativeFunctionDefinition
   , HidStructItem
   , NativeStructDefinition
   , HidStructSignature
   , StructDefinition
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
  ConstantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbortExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AssignExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidUnaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidUnaryExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  BorrowExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidReferenceSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ImmRefSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutRefSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DereferenceExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidExpressionTermSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AnnotationExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ApplyTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidModuleIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidReservedIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidExistsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidForallSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentityInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeArgumentsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PrimitiveTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RefTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TupleTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockItemInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  LetStatementSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CommaBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidBindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AtBindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidVariableIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindUnpackSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindNamedFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindNamedFieldsInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  BindFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidSpreadOperatorSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutBindFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindPositionalFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NameExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidBindInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MutBindVarSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidLiteralValueSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AddressLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BoolLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ByteStringLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HexStringLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  OrBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseDeclarationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseDeclarationInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  UseFunSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidFunctionIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleMemberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseMemberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleMembersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BreakExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LabelSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CallExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ArgListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ContinueExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DotExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExpressionListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IfExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IndexExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroCallExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroModuleAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidMatchBodySymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchArmSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PackExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldInitializeListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExpFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidFieldIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HidSpecBlockTargetSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockTargetSchemaSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidStructIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidTypeParameterIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidSpecFunctionSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParametersInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParameterInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MutFunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RetTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UninterpretedSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UsualSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBodySymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidSpecBlockMemeberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplySymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyPatternSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyNamePatternSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyPatternInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidSpecAbortIfSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConditionPropertiesSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPropertySymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidSpecAbortWithOrModifiesSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidSpecAbortWithOrModifiesInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HidSpecConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidSpecConditionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HidSpecConditionKindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecIncludeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecInvariantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecInvariantInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecLetSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPragmaSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecVariableSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecVariableInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  UnitExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VectorExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VectorExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MoveOrCopyExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MoveOrCopyExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  UnaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UnaryOpSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  FriendDeclarationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FriendAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidEnumItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumVariantsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VariantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DatatypeFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NamedFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldAnnotationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PositionalFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidVariantIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidEnumSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilityDeclsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidEnumIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PostfixAbilityDeclsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidFunctionItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidFunctionSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroFunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidMacroSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeFunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidStructItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeStructDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HidStructSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  StructDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  SConstantSymbol :: SymbolSing Regular ConstantSymbol
  SHidExpressionSymbol :: SymbolSing Regular HidExpressionSymbol
  SAbortExpressionSymbol :: SymbolSing Regular AbortExpressionSymbol
  SAssignExpressionSymbol :: SymbolSing Regular AssignExpressionSymbol
  SHidUnaryExpressionSymbol :: SymbolSing Regular HidUnaryExpressionSymbol
  SHidUnaryExpressionInternal0Symbol :: SymbolSing Regular HidUnaryExpressionInternal0Symbol
  SBorrowExpressionSymbol :: SymbolSing Regular BorrowExpressionSymbol
  SHidReferenceSymbol :: SymbolSing Regular HidReferenceSymbol
  SImmRefSymbol :: SymbolSing Regular ImmRefSymbol
  SMutRefSymbol :: SymbolSing Regular MutRefSymbol
  SDereferenceExpressionSymbol :: SymbolSing Regular DereferenceExpressionSymbol
  SHidExpressionTermSymbol :: SymbolSing Regular HidExpressionTermSymbol
  SAnnotationExpressionSymbol :: SymbolSing Regular AnnotationExpressionSymbol
  SHidTypeSymbol :: SymbolSing Regular HidTypeSymbol
  SApplyTypeSymbol :: SymbolSing Regular ApplyTypeSymbol
  SModuleAccessSymbol :: SymbolSing Regular ModuleAccessSymbol
  SHidModuleIdentifierSymbol :: SymbolSing Regular HidModuleIdentifierSymbol
  SIdentifierSymbol :: SymbolSing Regular IdentifierSymbol
  SHidReservedIdentifierSymbol :: SymbolSing Regular HidReservedIdentifierSymbol
  SHidExistsSymbol :: SymbolSing Regular HidExistsSymbol
  SHidForallSymbol :: SymbolSing Regular HidForallSymbol
  SModuleIdentitySymbol :: SymbolSing Regular ModuleIdentitySymbol
  SModuleIdentityInternal0Symbol :: SymbolSing Regular ModuleIdentityInternal0Symbol
  SNumLiteralSymbol :: SymbolSing Regular NumLiteralSymbol
  SNumLiteralInternal0Symbol :: SymbolSing Regular NumLiteralInternal0Symbol
  STypeArgumentsSymbol :: SymbolSing Regular TypeArgumentsSymbol
  SFunctionTypeSymbol :: SymbolSing Regular FunctionTypeSymbol
  SFunctionTypeParametersSymbol :: SymbolSing Regular FunctionTypeParametersSymbol
  SPrimitiveTypeSymbol :: SymbolSing Regular PrimitiveTypeSymbol
  SRefTypeSymbol :: SymbolSing Regular RefTypeSymbol
  STupleTypeSymbol :: SymbolSing Regular TupleTypeSymbol
  SBlockSymbol :: SymbolSing Regular BlockSymbol
  SBlockItemSymbol :: SymbolSing Regular BlockItemSymbol
  SBlockItemInternal0Symbol :: SymbolSing Regular BlockItemInternal0Symbol
  SLetStatementSymbol :: SymbolSing Regular LetStatementSymbol
  SBindListSymbol :: SymbolSing Regular BindListSymbol
  SCommaBindListSymbol :: SymbolSing Regular CommaBindListSymbol
  SHidBindSymbol :: SymbolSing Regular HidBindSymbol
  SAtBindSymbol :: SymbolSing Regular AtBindSymbol
  SHidVariableIdentifierSymbol :: SymbolSing Regular HidVariableIdentifierSymbol
  SBindUnpackSymbol :: SymbolSing Regular BindUnpackSymbol
  SBindFieldsSymbol :: SymbolSing Regular BindFieldsSymbol
  SBindNamedFieldsSymbol :: SymbolSing Regular BindNamedFieldsSymbol
  SBindNamedFieldsInternal0Symbol :: SymbolSing Regular BindNamedFieldsInternal0Symbol
  SBindFieldSymbol :: SymbolSing Regular BindFieldSymbol
  SHidSpreadOperatorSymbol :: SymbolSing Regular HidSpreadOperatorSymbol
  SMutBindFieldSymbol :: SymbolSing Regular MutBindFieldSymbol
  SBindPositionalFieldsSymbol :: SymbolSing Regular BindPositionalFieldsSymbol
  SNameExpressionSymbol :: SymbolSing Regular NameExpressionSymbol
  SHidBindInternal0Symbol :: SymbolSing Regular HidBindInternal0Symbol
  SMutBindVarSymbol :: SymbolSing Regular MutBindVarSymbol
  SHidLiteralValueSymbol :: SymbolSing Regular HidLiteralValueSymbol
  SAddressLiteralSymbol :: SymbolSing Regular AddressLiteralSymbol
  SBoolLiteralSymbol :: SymbolSing Regular BoolLiteralSymbol
  SByteStringLiteralSymbol :: SymbolSing Regular ByteStringLiteralSymbol
  SHexStringLiteralSymbol :: SymbolSing Regular HexStringLiteralSymbol
  SOrBindListSymbol :: SymbolSing Regular OrBindListSymbol
  SUseDeclarationSymbol :: SymbolSing Regular UseDeclarationSymbol
  SUseDeclarationInternal0Symbol :: SymbolSing Regular UseDeclarationInternal0Symbol
  SUseFunSymbol :: SymbolSing Regular UseFunSymbol
  SHidFunctionIdentifierSymbol :: SymbolSing Regular HidFunctionIdentifierSymbol
  SUseModuleSymbol :: SymbolSing Regular UseModuleSymbol
  SUseModuleMemberSymbol :: SymbolSing Regular UseModuleMemberSymbol
  SUseMemberSymbol :: SymbolSing Regular UseMemberSymbol
  SUseModuleMembersSymbol :: SymbolSing Regular UseModuleMembersSymbol
  SBreakExpressionSymbol :: SymbolSing Regular BreakExpressionSymbol
  SLabelSymbol :: SymbolSing Regular LabelSymbol
  SCallExpressionSymbol :: SymbolSing Regular CallExpressionSymbol
  SArgListSymbol :: SymbolSing Regular ArgListSymbol
  SContinueExpressionSymbol :: SymbolSing Regular ContinueExpressionSymbol
  SDotExpressionSymbol :: SymbolSing Regular DotExpressionSymbol
  SExpressionListSymbol :: SymbolSing Regular ExpressionListSymbol
  SIfExpressionSymbol :: SymbolSing Regular IfExpressionSymbol
  SIndexExpressionSymbol :: SymbolSing Regular IndexExpressionSymbol
  SMacroCallExpressionSymbol :: SymbolSing Regular MacroCallExpressionSymbol
  SMacroModuleAccessSymbol :: SymbolSing Regular MacroModuleAccessSymbol
  SMatchExpressionSymbol :: SymbolSing Regular MatchExpressionSymbol
  SHidMatchBodySymbol :: SymbolSing Regular HidMatchBodySymbol
  SMatchArmSymbol :: SymbolSing Regular MatchArmSymbol
  SMatchConditionSymbol :: SymbolSing Regular MatchConditionSymbol
  SPackExpressionSymbol :: SymbolSing Regular PackExpressionSymbol
  SFieldInitializeListSymbol :: SymbolSing Regular FieldInitializeListSymbol
  SExpFieldSymbol :: SymbolSing Regular ExpFieldSymbol
  SHidFieldIdentifierSymbol :: SymbolSing Regular HidFieldIdentifierSymbol
  SSpecBlockSymbol :: SymbolSing Regular SpecBlockSymbol
  SSpecBlockInternal0Symbol :: SymbolSing Regular SpecBlockInternal0Symbol
  SHidSpecBlockTargetSymbol :: SymbolSing Regular HidSpecBlockTargetSymbol
  SSpecBlockTargetSchemaSymbol :: SymbolSing Regular SpecBlockTargetSchemaSymbol
  SHidStructIdentifierSymbol :: SymbolSing Regular HidStructIdentifierSymbol
  STypeParametersSymbol :: SymbolSing Regular TypeParametersSymbol
  STypeParameterSymbol :: SymbolSing Regular TypeParameterSymbol
  SAbilitySymbol :: SymbolSing Regular AbilitySymbol
  SHidTypeParameterIdentifierSymbol :: SymbolSing Regular HidTypeParameterIdentifierSymbol
  SHidSpecFunctionSymbol :: SymbolSing Regular HidSpecFunctionSymbol
  SNativeSpecFunctionSymbol :: SymbolSing Regular NativeSpecFunctionSymbol
  SHidSpecFunctionSignatureSymbol :: SymbolSing Regular HidSpecFunctionSignatureSymbol
  SFunctionParametersSymbol :: SymbolSing Regular FunctionParametersSymbol
  SFunctionParametersInternal0Symbol :: SymbolSing Regular FunctionParametersInternal0Symbol
  SFunctionParameterSymbol :: SymbolSing Regular FunctionParameterSymbol
  SFunctionParameterInternal0Symbol :: SymbolSing Regular FunctionParameterInternal0Symbol
  SMutFunctionParameterSymbol :: SymbolSing Regular MutFunctionParameterSymbol
  SRetTypeSymbol :: SymbolSing Regular RetTypeSymbol
  SUninterpretedSpecFunctionSymbol :: SymbolSing Regular UninterpretedSpecFunctionSymbol
  SUsualSpecFunctionSymbol :: SymbolSing Regular UsualSpecFunctionSymbol
  SSpecBodySymbol :: SymbolSing Regular SpecBodySymbol
  SHidSpecBlockMemeberSymbol :: SymbolSing Regular HidSpecBlockMemeberSymbol
  SSpecApplySymbol :: SymbolSing Regular SpecApplySymbol
  SSpecApplyPatternSymbol :: SymbolSing Regular SpecApplyPatternSymbol
  SSpecApplyNamePatternSymbol :: SymbolSing Regular SpecApplyNamePatternSymbol
  SSpecApplyPatternInternal0Symbol :: SymbolSing Regular SpecApplyPatternInternal0Symbol
  SSpecConditionSymbol :: SymbolSing Regular SpecConditionSymbol
  SHidSpecAbortIfSymbol :: SymbolSing Regular HidSpecAbortIfSymbol
  SConditionPropertiesSymbol :: SymbolSing Regular ConditionPropertiesSymbol
  SSpecPropertySymbol :: SymbolSing Regular SpecPropertySymbol
  SHidSpecAbortWithOrModifiesSymbol :: SymbolSing Regular HidSpecAbortWithOrModifiesSymbol
  SHidSpecAbortWithOrModifiesInternal0Symbol :: SymbolSing Regular HidSpecAbortWithOrModifiesInternal0Symbol
  SHidSpecConditionSymbol :: SymbolSing Regular HidSpecConditionSymbol
  SHidSpecConditionInternal0Symbol :: SymbolSing Regular HidSpecConditionInternal0Symbol
  SHidSpecConditionKindSymbol :: SymbolSing Regular HidSpecConditionKindSymbol
  SSpecIncludeSymbol :: SymbolSing Regular SpecIncludeSymbol
  SSpecInvariantSymbol :: SymbolSing Regular SpecInvariantSymbol
  SSpecInvariantInternal0Symbol :: SymbolSing Regular SpecInvariantInternal0Symbol
  SSpecLetSymbol :: SymbolSing Regular SpecLetSymbol
  SSpecPragmaSymbol :: SymbolSing Regular SpecPragmaSymbol
  SSpecVariableSymbol :: SymbolSing Regular SpecVariableSymbol
  SSpecVariableInternal0Symbol :: SymbolSing Regular SpecVariableInternal0Symbol
  SUnitExpressionSymbol :: SymbolSing Regular UnitExpressionSymbol
  SVectorExpressionSymbol :: SymbolSing Regular VectorExpressionSymbol
  SVectorExpressionInternal0Symbol :: SymbolSing Regular VectorExpressionInternal0Symbol
  SMoveOrCopyExpressionSymbol :: SymbolSing Regular MoveOrCopyExpressionSymbol
  SMoveOrCopyExpressionInternal0Symbol :: SymbolSing Regular MoveOrCopyExpressionInternal0Symbol
  SUnaryExpressionSymbol :: SymbolSing Regular UnaryExpressionSymbol
  SUnaryOpSymbol :: SymbolSing Regular UnaryOpSymbol
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
  SFriendDeclarationSymbol :: SymbolSing Regular FriendDeclarationSymbol
  SFriendAccessSymbol :: SymbolSing Regular FriendAccessSymbol
  SHidEnumItemSymbol :: SymbolSing Regular HidEnumItemSymbol
  SEnumDefinitionSymbol :: SymbolSing Regular EnumDefinitionSymbol
  SEnumVariantsSymbol :: SymbolSing Regular EnumVariantsSymbol
  SVariantSymbol :: SymbolSing Regular VariantSymbol
  SDatatypeFieldsSymbol :: SymbolSing Regular DatatypeFieldsSymbol
  SNamedFieldsSymbol :: SymbolSing Regular NamedFieldsSymbol
  SFieldAnnotationSymbol :: SymbolSing Regular FieldAnnotationSymbol
  SPositionalFieldsSymbol :: SymbolSing Regular PositionalFieldsSymbol
  SHidVariantIdentifierSymbol :: SymbolSing Regular HidVariantIdentifierSymbol
  SHidEnumSignatureSymbol :: SymbolSing Regular HidEnumSignatureSymbol
  SAbilityDeclsSymbol :: SymbolSing Regular AbilityDeclsSymbol
  SHidEnumIdentifierSymbol :: SymbolSing Regular HidEnumIdentifierSymbol
  SPostfixAbilityDeclsSymbol :: SymbolSing Regular PostfixAbilityDeclsSymbol
  SHidFunctionItemSymbol :: SymbolSing Regular HidFunctionItemSymbol
  SFunctionDefinitionSymbol :: SymbolSing Regular FunctionDefinitionSymbol
  SHidFunctionSignatureSymbol :: SymbolSing Regular HidFunctionSignatureSymbol
  SModifierSymbol :: SymbolSing Regular ModifierSymbol
  SModifierInternal0Symbol :: SymbolSing Regular ModifierInternal0Symbol
  SMacroFunctionDefinitionSymbol :: SymbolSing Regular MacroFunctionDefinitionSymbol
  SHidMacroSignatureSymbol :: SymbolSing Regular HidMacroSignatureSymbol
  SNativeFunctionDefinitionSymbol :: SymbolSing Regular NativeFunctionDefinitionSymbol
  SHidStructItemSymbol :: SymbolSing Regular HidStructItemSymbol
  SNativeStructDefinitionSymbol :: SymbolSing Regular NativeStructDefinitionSymbol
  SHidStructSignatureSymbol :: SymbolSing Regular HidStructSignatureSymbol
  SStructDefinitionSymbol :: SymbolSing Regular StructDefinitionSymbol
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
decSymbolSing SConstantSymbol SConstantSymbol = Just (Refl, HRefl)
decSymbolSing SHidExpressionSymbol SHidExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SAbortExpressionSymbol SAbortExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SAssignExpressionSymbol SAssignExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHidUnaryExpressionSymbol SHidUnaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHidUnaryExpressionInternal0Symbol SHidUnaryExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SBorrowExpressionSymbol SBorrowExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHidReferenceSymbol SHidReferenceSymbol = Just (Refl, HRefl)
decSymbolSing SImmRefSymbol SImmRefSymbol = Just (Refl, HRefl)
decSymbolSing SMutRefSymbol SMutRefSymbol = Just (Refl, HRefl)
decSymbolSing SDereferenceExpressionSymbol SDereferenceExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHidExpressionTermSymbol SHidExpressionTermSymbol = Just (Refl, HRefl)
decSymbolSing SAnnotationExpressionSymbol SAnnotationExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHidTypeSymbol SHidTypeSymbol = Just (Refl, HRefl)
decSymbolSing SApplyTypeSymbol SApplyTypeSymbol = Just (Refl, HRefl)
decSymbolSing SModuleAccessSymbol SModuleAccessSymbol = Just (Refl, HRefl)
decSymbolSing SHidModuleIdentifierSymbol SHidModuleIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SIdentifierSymbol SIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHidReservedIdentifierSymbol SHidReservedIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHidExistsSymbol SHidExistsSymbol = Just (Refl, HRefl)
decSymbolSing SHidForallSymbol SHidForallSymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentitySymbol SModuleIdentitySymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentityInternal0Symbol SModuleIdentityInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralSymbol SNumLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralInternal0Symbol SNumLiteralInternal0Symbol = Just (Refl, HRefl)
decSymbolSing STypeArgumentsSymbol STypeArgumentsSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeSymbol SFunctionTypeSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeParametersSymbol SFunctionTypeParametersSymbol = Just (Refl, HRefl)
decSymbolSing SPrimitiveTypeSymbol SPrimitiveTypeSymbol = Just (Refl, HRefl)
decSymbolSing SRefTypeSymbol SRefTypeSymbol = Just (Refl, HRefl)
decSymbolSing STupleTypeSymbol STupleTypeSymbol = Just (Refl, HRefl)
decSymbolSing SBlockSymbol SBlockSymbol = Just (Refl, HRefl)
decSymbolSing SBlockItemSymbol SBlockItemSymbol = Just (Refl, HRefl)
decSymbolSing SBlockItemInternal0Symbol SBlockItemInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SLetStatementSymbol SLetStatementSymbol = Just (Refl, HRefl)
decSymbolSing SBindListSymbol SBindListSymbol = Just (Refl, HRefl)
decSymbolSing SCommaBindListSymbol SCommaBindListSymbol = Just (Refl, HRefl)
decSymbolSing SHidBindSymbol SHidBindSymbol = Just (Refl, HRefl)
decSymbolSing SAtBindSymbol SAtBindSymbol = Just (Refl, HRefl)
decSymbolSing SHidVariableIdentifierSymbol SHidVariableIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SBindUnpackSymbol SBindUnpackSymbol = Just (Refl, HRefl)
decSymbolSing SBindFieldsSymbol SBindFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SBindNamedFieldsSymbol SBindNamedFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SBindNamedFieldsInternal0Symbol SBindNamedFieldsInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SBindFieldSymbol SBindFieldSymbol = Just (Refl, HRefl)
decSymbolSing SHidSpreadOperatorSymbol SHidSpreadOperatorSymbol = Just (Refl, HRefl)
decSymbolSing SMutBindFieldSymbol SMutBindFieldSymbol = Just (Refl, HRefl)
decSymbolSing SBindPositionalFieldsSymbol SBindPositionalFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SNameExpressionSymbol SNameExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHidBindInternal0Symbol SHidBindInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMutBindVarSymbol SMutBindVarSymbol = Just (Refl, HRefl)
decSymbolSing SHidLiteralValueSymbol SHidLiteralValueSymbol = Just (Refl, HRefl)
decSymbolSing SAddressLiteralSymbol SAddressLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SBoolLiteralSymbol SBoolLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SByteStringLiteralSymbol SByteStringLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SHexStringLiteralSymbol SHexStringLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SOrBindListSymbol SOrBindListSymbol = Just (Refl, HRefl)
decSymbolSing SUseDeclarationSymbol SUseDeclarationSymbol = Just (Refl, HRefl)
decSymbolSing SUseDeclarationInternal0Symbol SUseDeclarationInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SUseFunSymbol SUseFunSymbol = Just (Refl, HRefl)
decSymbolSing SHidFunctionIdentifierSymbol SHidFunctionIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleSymbol SUseModuleSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleMemberSymbol SUseModuleMemberSymbol = Just (Refl, HRefl)
decSymbolSing SUseMemberSymbol SUseMemberSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleMembersSymbol SUseModuleMembersSymbol = Just (Refl, HRefl)
decSymbolSing SBreakExpressionSymbol SBreakExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SLabelSymbol SLabelSymbol = Just (Refl, HRefl)
decSymbolSing SCallExpressionSymbol SCallExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SArgListSymbol SArgListSymbol = Just (Refl, HRefl)
decSymbolSing SContinueExpressionSymbol SContinueExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SDotExpressionSymbol SDotExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SExpressionListSymbol SExpressionListSymbol = Just (Refl, HRefl)
decSymbolSing SIfExpressionSymbol SIfExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SIndexExpressionSymbol SIndexExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMacroCallExpressionSymbol SMacroCallExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMacroModuleAccessSymbol SMacroModuleAccessSymbol = Just (Refl, HRefl)
decSymbolSing SMatchExpressionSymbol SMatchExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHidMatchBodySymbol SHidMatchBodySymbol = Just (Refl, HRefl)
decSymbolSing SMatchArmSymbol SMatchArmSymbol = Just (Refl, HRefl)
decSymbolSing SMatchConditionSymbol SMatchConditionSymbol = Just (Refl, HRefl)
decSymbolSing SPackExpressionSymbol SPackExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SFieldInitializeListSymbol SFieldInitializeListSymbol = Just (Refl, HRefl)
decSymbolSing SExpFieldSymbol SExpFieldSymbol = Just (Refl, HRefl)
decSymbolSing SHidFieldIdentifierSymbol SHidFieldIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockSymbol SSpecBlockSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockInternal0Symbol SSpecBlockInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHidSpecBlockTargetSymbol SHidSpecBlockTargetSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockTargetSchemaSymbol SSpecBlockTargetSchemaSymbol = Just (Refl, HRefl)
decSymbolSing SHidStructIdentifierSymbol SHidStructIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing STypeParametersSymbol STypeParametersSymbol = Just (Refl, HRefl)
decSymbolSing STypeParameterSymbol STypeParameterSymbol = Just (Refl, HRefl)
decSymbolSing SAbilitySymbol SAbilitySymbol = Just (Refl, HRefl)
decSymbolSing SHidTypeParameterIdentifierSymbol SHidTypeParameterIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHidSpecFunctionSymbol SHidSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SNativeSpecFunctionSymbol SNativeSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SHidSpecFunctionSignatureSymbol SHidSpecFunctionSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParametersSymbol SFunctionParametersSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParametersInternal0Symbol SFunctionParametersInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SFunctionParameterSymbol SFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParameterInternal0Symbol SFunctionParameterInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMutFunctionParameterSymbol SMutFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SRetTypeSymbol SRetTypeSymbol = Just (Refl, HRefl)
decSymbolSing SUninterpretedSpecFunctionSymbol SUninterpretedSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SUsualSpecFunctionSymbol SUsualSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBodySymbol SSpecBodySymbol = Just (Refl, HRefl)
decSymbolSing SHidSpecBlockMemeberSymbol SHidSpecBlockMemeberSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplySymbol SSpecApplySymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyPatternSymbol SSpecApplyPatternSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyNamePatternSymbol SSpecApplyNamePatternSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyPatternInternal0Symbol SSpecApplyPatternInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecConditionSymbol SSpecConditionSymbol = Just (Refl, HRefl)
decSymbolSing SHidSpecAbortIfSymbol SHidSpecAbortIfSymbol = Just (Refl, HRefl)
decSymbolSing SConditionPropertiesSymbol SConditionPropertiesSymbol = Just (Refl, HRefl)
decSymbolSing SSpecPropertySymbol SSpecPropertySymbol = Just (Refl, HRefl)
decSymbolSing SHidSpecAbortWithOrModifiesSymbol SHidSpecAbortWithOrModifiesSymbol = Just (Refl, HRefl)
decSymbolSing SHidSpecAbortWithOrModifiesInternal0Symbol SHidSpecAbortWithOrModifiesInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHidSpecConditionSymbol SHidSpecConditionSymbol = Just (Refl, HRefl)
decSymbolSing SHidSpecConditionInternal0Symbol SHidSpecConditionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHidSpecConditionKindSymbol SHidSpecConditionKindSymbol = Just (Refl, HRefl)
decSymbolSing SSpecIncludeSymbol SSpecIncludeSymbol = Just (Refl, HRefl)
decSymbolSing SSpecInvariantSymbol SSpecInvariantSymbol = Just (Refl, HRefl)
decSymbolSing SSpecInvariantInternal0Symbol SSpecInvariantInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecLetSymbol SSpecLetSymbol = Just (Refl, HRefl)
decSymbolSing SSpecPragmaSymbol SSpecPragmaSymbol = Just (Refl, HRefl)
decSymbolSing SSpecVariableSymbol SSpecVariableSymbol = Just (Refl, HRefl)
decSymbolSing SSpecVariableInternal0Symbol SSpecVariableInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SUnitExpressionSymbol SUnitExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SVectorExpressionSymbol SVectorExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SVectorExpressionInternal0Symbol SVectorExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMoveOrCopyExpressionSymbol SMoveOrCopyExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMoveOrCopyExpressionInternal0Symbol SMoveOrCopyExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SUnaryExpressionSymbol SUnaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SUnaryOpSymbol SUnaryOpSymbol = Just (Refl, HRefl)
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
decSymbolSing SFriendDeclarationSymbol SFriendDeclarationSymbol = Just (Refl, HRefl)
decSymbolSing SFriendAccessSymbol SFriendAccessSymbol = Just (Refl, HRefl)
decSymbolSing SHidEnumItemSymbol SHidEnumItemSymbol = Just (Refl, HRefl)
decSymbolSing SEnumDefinitionSymbol SEnumDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SEnumVariantsSymbol SEnumVariantsSymbol = Just (Refl, HRefl)
decSymbolSing SVariantSymbol SVariantSymbol = Just (Refl, HRefl)
decSymbolSing SDatatypeFieldsSymbol SDatatypeFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SNamedFieldsSymbol SNamedFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SFieldAnnotationSymbol SFieldAnnotationSymbol = Just (Refl, HRefl)
decSymbolSing SPositionalFieldsSymbol SPositionalFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SHidVariantIdentifierSymbol SHidVariantIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHidEnumSignatureSymbol SHidEnumSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SAbilityDeclsSymbol SAbilityDeclsSymbol = Just (Refl, HRefl)
decSymbolSing SHidEnumIdentifierSymbol SHidEnumIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SPostfixAbilityDeclsSymbol SPostfixAbilityDeclsSymbol = Just (Refl, HRefl)
decSymbolSing SHidFunctionItemSymbol SHidFunctionItemSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionDefinitionSymbol SFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHidFunctionSignatureSymbol SHidFunctionSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SModifierSymbol SModifierSymbol = Just (Refl, HRefl)
decSymbolSing SModifierInternal0Symbol SModifierInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMacroFunctionDefinitionSymbol SMacroFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHidMacroSignatureSymbol SHidMacroSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SNativeFunctionDefinitionSymbol SNativeFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHidStructItemSymbol SHidStructItemSymbol = Just (Refl, HRefl)
decSymbolSing SNativeStructDefinitionSymbol SNativeStructDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SHidStructSignatureSymbol SHidStructSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SStructDefinitionSymbol SStructDefinitionSymbol = Just (Refl, HRefl)
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

-- newtype SymbolTable = SymbolTable {unSymbolTable :: IntMap SomeSymbolSing}
--   deriving Show

-- symbolMap :: Map String SomeSymbolSing
-- symbolMap = Map.fromList
--     [ ("source_file", SomeRegularSymbolSing SSourceFileSymbol)
--     , ("module_definition", SomeRegularSymbolSing SModuleDefinitionSymbol)
--     , ("module_body", SomeRegularSymbolSing SModuleBodySymbol)
--     , ("module_body_internal0", SomeRegularSymbolSing SModuleBodyInternal0Symbol)
--     , ("module_body_internal1", SomeRegularSymbolSing SModuleBodyInternal1Symbol)
--     , ("constant", SomeRegularSymbolSing SConstantSymbol)
--     , ("hid_expression", SomeRegularSymbolSing SHidExpressionSymbol)
--     , ("abort_expression", SomeRegularSymbolSing SAbortExpressionSymbol)
--     , ("assign_expression", SomeRegularSymbolSing SAssignExpressionSymbol)
--     , ("hid_unary_expression", SomeRegularSymbolSing SHidUnaryExpressionSymbol)
--     , ("hid_unary_expression_internal0", SomeRegularSymbolSing SHidUnaryExpressionInternal0Symbol)
--     , ("borrow_expression", SomeRegularSymbolSing SBorrowExpressionSymbol)
--     , ("hid_reference", SomeRegularSymbolSing SHidReferenceSymbol)
--     , ("imm_ref", SomeRegularSymbolSing SImmRefSymbol)
--     , ("mut_ref", SomeRegularSymbolSing SMutRefSymbol)
--     , ("dereference_expression", SomeRegularSymbolSing SDereferenceExpressionSymbol)
--     , ("hid_expression_term", SomeRegularSymbolSing SHidExpressionTermSymbol)
--     , ("annotation_expression", SomeRegularSymbolSing SAnnotationExpressionSymbol)
--     , ("hid_type", SomeRegularSymbolSing SHidTypeSymbol)
--     , ("apply_type", SomeRegularSymbolSing SApplyTypeSymbol)
--     , ("module_access", SomeRegularSymbolSing SModuleAccessSymbol)
--     , ("hid_module_identifier", SomeRegularSymbolSing SHidModuleIdentifierSymbol)
--     , ("identifier", SomeRegularSymbolSing SIdentifierSymbol)
--     , ("hid_reserved_identifier", SomeRegularSymbolSing SHidReservedIdentifierSymbol)
--     , ("hid_exists", SomeRegularSymbolSing SHidExistsSymbol)
--     , ("hid_forall", SomeRegularSymbolSing SHidForallSymbol)
--     , ("module_identity", SomeRegularSymbolSing SModuleIdentitySymbol)
--     , ("module_identity_internal0", SomeRegularSymbolSing SModuleIdentityInternal0Symbol)
--     , ("num_literal", SomeRegularSymbolSing SNumLiteralSymbol)
--     , ("num_literal_internal0", SomeRegularSymbolSing SNumLiteralInternal0Symbol)
--     , ("type_arguments", SomeRegularSymbolSing STypeArgumentsSymbol)
--     , ("function_type", SomeRegularSymbolSing SFunctionTypeSymbol)
--     , ("function_type_parameters", SomeRegularSymbolSing SFunctionTypeParametersSymbol)
--     , ("primitive_type", SomeRegularSymbolSing SPrimitiveTypeSymbol)
--     , ("ref_type", SomeRegularSymbolSing SRefTypeSymbol)
--     , ("tuple_type", SomeRegularSymbolSing STupleTypeSymbol)
--     , ("block", SomeRegularSymbolSing SBlockSymbol)
--     , ("block_item", SomeRegularSymbolSing SBlockItemSymbol)
--     , ("block_item_internal0", SomeRegularSymbolSing SBlockItemInternal0Symbol)
--     , ("let_statement", SomeRegularSymbolSing SLetStatementSymbol)
--     , ("bind_list", SomeRegularSymbolSing SBindListSymbol)
--     , ("comma_bind_list", SomeRegularSymbolSing SCommaBindListSymbol)
--     , ("hid_bind", SomeRegularSymbolSing SHidBindSymbol)
--     , ("at_bind", SomeRegularSymbolSing SAtBindSymbol)
--     , ("hid_variable_identifier", SomeRegularSymbolSing SHidVariableIdentifierSymbol)
--     , ("bind_unpack", SomeRegularSymbolSing SBindUnpackSymbol)
--     , ("bind_fields", SomeRegularSymbolSing SBindFieldsSymbol)
--     , ("bind_named_fields", SomeRegularSymbolSing SBindNamedFieldsSymbol)
--     , ("bind_named_fields_internal0", SomeRegularSymbolSing SBindNamedFieldsInternal0Symbol)
--     , ("bind_field", SomeRegularSymbolSing SBindFieldSymbol)
--     , ("hid_spread_operator", SomeRegularSymbolSing SHidSpreadOperatorSymbol)
--     , ("mut_bind_field", SomeRegularSymbolSing SMutBindFieldSymbol)
--     , ("bind_positional_fields", SomeRegularSymbolSing SBindPositionalFieldsSymbol)
--     , ("name_expression", SomeRegularSymbolSing SNameExpressionSymbol)
--     , ("hid_bind_internal0", SomeRegularSymbolSing SHidBindInternal0Symbol)
--     , ("mut_bind_var", SomeRegularSymbolSing SMutBindVarSymbol)
--     , ("hid_literal_value", SomeRegularSymbolSing SHidLiteralValueSymbol)
--     , ("address_literal", SomeRegularSymbolSing SAddressLiteralSymbol)
--     , ("bool_literal", SomeRegularSymbolSing SBoolLiteralSymbol)
--     , ("byte_string_literal", SomeRegularSymbolSing SByteStringLiteralSymbol)
--     , ("hex_string_literal", SomeRegularSymbolSing SHexStringLiteralSymbol)
--     , ("or_bind_list", SomeRegularSymbolSing SOrBindListSymbol)
--     , ("use_declaration", SomeRegularSymbolSing SUseDeclarationSymbol)
--     , ("use_declaration_internal0", SomeRegularSymbolSing SUseDeclarationInternal0Symbol)
--     , ("use_fun", SomeRegularSymbolSing SUseFunSymbol)
--     , ("hid_function_identifier", SomeRegularSymbolSing SHidFunctionIdentifierSymbol)
--     , ("use_module", SomeRegularSymbolSing SUseModuleSymbol)
--     , ("use_module_member", SomeRegularSymbolSing SUseModuleMemberSymbol)
--     , ("use_member", SomeRegularSymbolSing SUseMemberSymbol)
--     , ("use_module_members", SomeRegularSymbolSing SUseModuleMembersSymbol)
--     , ("break_expression", SomeRegularSymbolSing SBreakExpressionSymbol)
--     , ("label", SomeRegularSymbolSing SLabelSymbol)
--     , ("call_expression", SomeRegularSymbolSing SCallExpressionSymbol)
--     , ("arg_list", SomeRegularSymbolSing SArgListSymbol)
--     , ("continue_expression", SomeRegularSymbolSing SContinueExpressionSymbol)
--     , ("dot_expression", SomeRegularSymbolSing SDotExpressionSymbol)
--     , ("expression_list", SomeRegularSymbolSing SExpressionListSymbol)
--     , ("if_expression", SomeRegularSymbolSing SIfExpressionSymbol)
--     , ("index_expression", SomeRegularSymbolSing SIndexExpressionSymbol)
--     , ("macro_call_expression", SomeRegularSymbolSing SMacroCallExpressionSymbol)
--     , ("macro_module_access", SomeRegularSymbolSing SMacroModuleAccessSymbol)
--     , ("match_expression", SomeRegularSymbolSing SMatchExpressionSymbol)
--     , ("hid_match_body", SomeRegularSymbolSing SHidMatchBodySymbol)
--     , ("match_arm", SomeRegularSymbolSing SMatchArmSymbol)
--     , ("match_condition", SomeRegularSymbolSing SMatchConditionSymbol)
--     , ("pack_expression", SomeRegularSymbolSing SPackExpressionSymbol)
--     , ("field_initialize_list", SomeRegularSymbolSing SFieldInitializeListSymbol)
--     , ("exp_field", SomeRegularSymbolSing SExpFieldSymbol)
--     , ("hid_field_identifier", SomeRegularSymbolSing SHidFieldIdentifierSymbol)
--     , ("spec_block", SomeRegularSymbolSing SSpecBlockSymbol)
--     , ("spec_block_internal0", SomeRegularSymbolSing SSpecBlockInternal0Symbol)
--     , ("hid_spec_block_target", SomeRegularSymbolSing SHidSpecBlockTargetSymbol)
--     , ("spec_block_target_schema", SomeRegularSymbolSing SSpecBlockTargetSchemaSymbol)
--     , ("hid_struct_identifier", SomeRegularSymbolSing SHidStructIdentifierSymbol)
--     , ("type_parameters", SomeRegularSymbolSing STypeParametersSymbol)
--     , ("type_parameter", SomeRegularSymbolSing STypeParameterSymbol)
--     , ("ability", SomeRegularSymbolSing SAbilitySymbol)
--     , ("hid_type_parameter_identifier", SomeRegularSymbolSing SHidTypeParameterIdentifierSymbol)
--     , ("hid_spec_function", SomeRegularSymbolSing SHidSpecFunctionSymbol)
--     , ("native_spec_function", SomeRegularSymbolSing SNativeSpecFunctionSymbol)
--     , ("hid_spec_function_signature", SomeRegularSymbolSing SHidSpecFunctionSignatureSymbol)
--     , ("function_parameters", SomeRegularSymbolSing SFunctionParametersSymbol)
--     , ("function_parameters_internal0", SomeRegularSymbolSing SFunctionParametersInternal0Symbol)
--     , ("function_parameter", SomeRegularSymbolSing SFunctionParameterSymbol)
--     , ("function_parameter_internal0", SomeRegularSymbolSing SFunctionParameterInternal0Symbol)
--     , ("mut_function_parameter", SomeRegularSymbolSing SMutFunctionParameterSymbol)
--     , ("ret_type", SomeRegularSymbolSing SRetTypeSymbol)
--     , ("uninterpreted_spec_function", SomeRegularSymbolSing SUninterpretedSpecFunctionSymbol)
--     , ("usual_spec_function", SomeRegularSymbolSing SUsualSpecFunctionSymbol)
--     , ("spec_body", SomeRegularSymbolSing SSpecBodySymbol)
--     , ("hid_spec_block_memeber", SomeRegularSymbolSing SHidSpecBlockMemeberSymbol)
--     , ("spec_apply", SomeRegularSymbolSing SSpecApplySymbol)
--     , ("spec_apply_pattern", SomeRegularSymbolSing SSpecApplyPatternSymbol)
--     , ("spec_apply_name_pattern", SomeRegularSymbolSing SSpecApplyNamePatternSymbol)
--     , ("spec_apply_pattern_internal0", SomeRegularSymbolSing SSpecApplyPatternInternal0Symbol)
--     , ("spec_condition", SomeRegularSymbolSing SSpecConditionSymbol)
--     , ("hid_spec_abort_if", SomeRegularSymbolSing SHidSpecAbortIfSymbol)
--     , ("condition_properties", SomeRegularSymbolSing SConditionPropertiesSymbol)
--     , ("spec_property", SomeRegularSymbolSing SSpecPropertySymbol)
--     , ("hid_spec_abort_with_or_modifies", SomeRegularSymbolSing SHidSpecAbortWithOrModifiesSymbol)
--     , ("hid_spec_abort_with_or_modifies_internal0", SomeRegularSymbolSing SHidSpecAbortWithOrModifiesInternal0Symbol)
--     , ("hid_spec_condition", SomeRegularSymbolSing SHidSpecConditionSymbol)
--     , ("hid_spec_condition_internal0", SomeRegularSymbolSing SHidSpecConditionInternal0Symbol)
--     , ("hid_spec_condition_kind", SomeRegularSymbolSing SHidSpecConditionKindSymbol)
--     , ("spec_include", SomeRegularSymbolSing SSpecIncludeSymbol)
--     , ("spec_invariant", SomeRegularSymbolSing SSpecInvariantSymbol)
--     , ("spec_invariant_internal0", SomeRegularSymbolSing SSpecInvariantInternal0Symbol)
--     , ("spec_let", SomeRegularSymbolSing SSpecLetSymbol)
--     , ("spec_pragma", SomeRegularSymbolSing SSpecPragmaSymbol)
--     , ("spec_variable", SomeRegularSymbolSing SSpecVariableSymbol)
--     , ("spec_variable_internal0", SomeRegularSymbolSing SSpecVariableInternal0Symbol)
--     , ("unit_expression", SomeRegularSymbolSing SUnitExpressionSymbol)
--     , ("vector_expression", SomeRegularSymbolSing SVectorExpressionSymbol)
--     , ("vector_expression_internal0", SomeRegularSymbolSing SVectorExpressionInternal0Symbol)
--     , ("move_or_copy_expression", SomeRegularSymbolSing SMoveOrCopyExpressionSymbol)
--     , ("move_or_copy_expression_internal0", SomeRegularSymbolSing SMoveOrCopyExpressionInternal0Symbol)
--     , ("unary_expression", SomeRegularSymbolSing SUnaryExpressionSymbol)
--     , ("unary_op", SomeRegularSymbolSing SUnaryOpSymbol)
--     , ("binary_expression", SomeRegularSymbolSing SBinaryExpressionSymbol)
--     , ("cast_expression", SomeRegularSymbolSing SCastExpressionSymbol)
--     , ("identified_expression", SomeRegularSymbolSing SIdentifiedExpressionSymbol)
--     , ("block_identifier", SomeRegularSymbolSing SBlockIdentifierSymbol)
--     , ("lambda_expression", SomeRegularSymbolSing SLambdaExpressionSymbol)
--     , ("lambda_bindings", SomeRegularSymbolSing SLambdaBindingsSymbol)
--     , ("lambda_binding", SomeRegularSymbolSing SLambdaBindingSymbol)
--     , ("loop_expression", SomeRegularSymbolSing SLoopExpressionSymbol)
--     , ("quantifier_expression", SomeRegularSymbolSing SQuantifierExpressionSymbol)
--     , ("quantifier_bindings", SomeRegularSymbolSing SQuantifierBindingsSymbol)
--     , ("quantifier_binding", SomeRegularSymbolSing SQuantifierBindingSymbol)
--     , ("return_expression", SomeRegularSymbolSing SReturnExpressionSymbol)
--     , ("while_expression", SomeRegularSymbolSing SWhileExpressionSymbol)
--     , ("friend_declaration", SomeRegularSymbolSing SFriendDeclarationSymbol)
--     , ("friend_access", SomeRegularSymbolSing SFriendAccessSymbol)
--     , ("hid_enum_item", SomeRegularSymbolSing SHidEnumItemSymbol)
--     , ("enum_definition", SomeRegularSymbolSing SEnumDefinitionSymbol)
--     , ("enum_variants", SomeRegularSymbolSing SEnumVariantsSymbol)
--     , ("variant", SomeRegularSymbolSing SVariantSymbol)
--     , ("datatype_fields", SomeRegularSymbolSing SDatatypeFieldsSymbol)
--     , ("named_fields", SomeRegularSymbolSing SNamedFieldsSymbol)
--     , ("field_annotation", SomeRegularSymbolSing SFieldAnnotationSymbol)
--     , ("positional_fields", SomeRegularSymbolSing SPositionalFieldsSymbol)
--     , ("hid_variant_identifier", SomeRegularSymbolSing SHidVariantIdentifierSymbol)
--     , ("hid_enum_signature", SomeRegularSymbolSing SHidEnumSignatureSymbol)
--     , ("ability_decls", SomeRegularSymbolSing SAbilityDeclsSymbol)
--     , ("hid_enum_identifier", SomeRegularSymbolSing SHidEnumIdentifierSymbol)
--     , ("postfix_ability_decls", SomeRegularSymbolSing SPostfixAbilityDeclsSymbol)
--     , ("hid_function_item", SomeRegularSymbolSing SHidFunctionItemSymbol)
--     , ("function_definition", SomeRegularSymbolSing SFunctionDefinitionSymbol)
--     , ("hid_function_signature", SomeRegularSymbolSing SHidFunctionSignatureSymbol)
--     , ("modifier", SomeRegularSymbolSing SModifierSymbol)
--     , ("modifier_internal0", SomeRegularSymbolSing SModifierInternal0Symbol)
--     , ("macro_function_definition", SomeRegularSymbolSing SMacroFunctionDefinitionSymbol)
--     , ("hid_macro_signature", SomeRegularSymbolSing SHidMacroSignatureSymbol)
--     , ("native_function_definition", SomeRegularSymbolSing SNativeFunctionDefinitionSymbol)
--     , ("hid_struct_item", SomeRegularSymbolSing SHidStructItemSymbol)
--     , ("native_struct_definition", SomeRegularSymbolSing SNativeStructDefinitionSymbol)
--     , ("hid_struct_signature", SomeRegularSymbolSing SHidStructSignatureSymbol)
--     , ("struct_definition", SomeRegularSymbolSing SStructDefinitionSymbol)
--     , ("!", SomeAnonymousSymbolSing SExclamationMarkTokSymbol)
--     , ("!=", SomeAnonymousSymbolSing SExclamationMarkEqualsSignTokSymbol)
--     , ("#[", SomeAnonymousSymbolSing SNumberSignLeftSquareBracketTokSymbol)
--     , ("$", SomeAnonymousSymbolSing SDollarSignTokSymbol)
--     , ("%", SomeAnonymousSymbolSing SPercentSignTokSymbol)
--     , ("&", SomeAnonymousSymbolSing SAmpersandTokSymbol)
--     , ("&&", SomeAnonymousSymbolSing SAmpersandAmpersandTokSymbol)
--     , ("'", SomeAnonymousSymbolSing SApostropheTokSymbol)
--     , ("(", SomeAnonymousSymbolSing SLeftParenthesisTokSymbol)
--     , (")", SomeAnonymousSymbolSing SRightParenthesisTokSymbol)
--     , ("*", SomeAnonymousSymbolSing SAsteriskTokSymbol)
--     , ("+", SomeAnonymousSymbolSing SPlusSignTokSymbol)
--     , (",", SomeAnonymousSymbolSing SCommaTokSymbol)
--     , ("-", SomeAnonymousSymbolSing SHyphenMinusTokSymbol)
--     , ("->", SomeAnonymousSymbolSing SHyphenMinusGreaterThanSignTokSymbol)
--     , (".", SomeAnonymousSymbolSing SFullStopTokSymbol)
--     , ("..", SomeAnonymousSymbolSing SFullStopFullStopTokSymbol)
--     , ("/", SomeAnonymousSymbolSing SSolidusTokSymbol)
--     , ("/*", SomeAnonymousSymbolSing SSolidusAsteriskTokSymbol)
--     , ("//", SomeAnonymousSymbolSing SSolidusSolidusTokSymbol)
--     , (":", SomeAnonymousSymbolSing SColonTokSymbol)
--     , ("::", SomeAnonymousSymbolSing SColonColonTokSymbol)
--     , (";", SomeAnonymousSymbolSing SSemicolonTokSymbol)
--     , ("<", SomeAnonymousSymbolSing SLessThanSignTokSymbol)
--     , ("<<", SomeAnonymousSymbolSing SLessThanSignLessThanSignTokSymbol)
--     , ("<=", SomeAnonymousSymbolSing SLessThanSignEqualsSignTokSymbol)
--     , ("=", SomeAnonymousSymbolSing SEqualsSignTokSymbol)
--     , ("==", SomeAnonymousSymbolSing SEqualsSignEqualsSignTokSymbol)
--     , ("==>", SomeAnonymousSymbolSing SEqualsSignEqualsSignGreaterThanSignTokSymbol)
--     , ("=>", SomeAnonymousSymbolSing SEqualsSignGreaterThanSignTokSymbol)
--     , (">", SomeAnonymousSymbolSing SGreaterThanSignTokSymbol)
--     , (">=", SomeAnonymousSymbolSing SGreaterThanSignEqualsSignTokSymbol)
--     , (">>", SomeAnonymousSymbolSing SGreaterThanSignGreaterThanSignTokSymbol)
--     , ("@", SomeAnonymousSymbolSing SCommercialAtTokSymbol)
--     , ("[", SomeAnonymousSymbolSing SLeftSquareBracketTokSymbol)
--     , ("]", SomeAnonymousSymbolSing SRightSquareBracketTokSymbol)
--     , ("^", SomeAnonymousSymbolSing SCircumflexAccentTokSymbol)
--     , ("abort", SomeAnonymousSymbolSing SAbortTokSymbol)
--     , ("aborts_if", SomeAnonymousSymbolSing SAbortsIfTokSymbol)
--     , ("aborts_with", SomeAnonymousSymbolSing SAbortsWithTokSymbol)
--     , ("address", SomeAnonymousSymbolSing SAddressTokSymbol)
--     , ("apply", SomeAnonymousSymbolSing SApplyTokSymbol)
--     , ("as", SomeAnonymousSymbolSing SAsTokSymbol)
--     , ("assert", SomeAnonymousSymbolSing SAssertTokSymbol)
--     , ("assume", SomeAnonymousSymbolSing SAssumeTokSymbol)
--     , ("bool", SomeAnonymousSymbolSing SBoolTokSymbol)
--     , ("break", SomeAnonymousSymbolSing SBreakTokSymbol)
--     , ("bytearray", SomeAnonymousSymbolSing SBytearrayTokSymbol)
--     , ("const", SomeAnonymousSymbolSing SConstTokSymbol)
--     , ("continue", SomeAnonymousSymbolSing SContinueTokSymbol)
--     , ("copy", SomeAnonymousSymbolSing SCopyTokSymbol)
--     , ("decreases", SomeAnonymousSymbolSing SDecreasesTokSymbol)
--     , ("drop", SomeAnonymousSymbolSing SDropTokSymbol)
--     , ("else", SomeAnonymousSymbolSing SElseTokSymbol)
--     , ("ensures", SomeAnonymousSymbolSing SEnsuresTokSymbol)
--     , ("entry", SomeAnonymousSymbolSing SEntryTokSymbol)
--     , ("enum", SomeAnonymousSymbolSing SEnumTokSymbol)
--     , ("except", SomeAnonymousSymbolSing SExceptTokSymbol)
--     , ("exists", SomeAnonymousSymbolSing SExistsTokSymbol)
--     , ("false", SomeAnonymousSymbolSing SFalseTokSymbol)
--     , ("forall", SomeAnonymousSymbolSing SForallTokSymbol)
--     , ("friend", SomeAnonymousSymbolSing SFriendTokSymbol)
--     , ("fun", SomeAnonymousSymbolSing SFunTokSymbol)
--     , ("global", SomeAnonymousSymbolSing SGlobalTokSymbol)
--     , ("has", SomeAnonymousSymbolSing SHasTokSymbol)
--     , ("if", SomeAnonymousSymbolSing SIfTokSymbol)
--     , ("in", SomeAnonymousSymbolSing SInTokSymbol)
--     , ("include", SomeAnonymousSymbolSing SIncludeTokSymbol)
--     , ("internal", SomeAnonymousSymbolSing SInternalTokSymbol)
--     , ("invariant", SomeAnonymousSymbolSing SInvariantTokSymbol)
--     , ("key", SomeAnonymousSymbolSing SKeyTokSymbol)
--     , ("let", SomeAnonymousSymbolSing SLetTokSymbol)
--     , ("local", SomeAnonymousSymbolSing SLocalTokSymbol)
--     , ("loop", SomeAnonymousSymbolSing SLoopTokSymbol)
--     , ("macro", SomeAnonymousSymbolSing SMacroTokSymbol)
--     , ("match", SomeAnonymousSymbolSing SMatchTokSymbol)
--     , ("modifies", SomeAnonymousSymbolSing SModifiesTokSymbol)
--     , ("module", SomeAnonymousSymbolSing SModuleTokSymbol)
--     , ("move", SomeAnonymousSymbolSing SMoveTokSymbol)
--     , ("mut", SomeAnonymousSymbolSing SMutTokSymbol)
--     , ("native", SomeAnonymousSymbolSing SNativeTokSymbol)
--     , ("pack", SomeAnonymousSymbolSing SPackTokSymbol)
--     , ("package", SomeAnonymousSymbolSing SPackageTokSymbol)
--     , ("phantom", SomeAnonymousSymbolSing SPhantomTokSymbol)
--     , ("post", SomeAnonymousSymbolSing SPostTokSymbol)
--     , ("pragma", SomeAnonymousSymbolSing SPragmaTokSymbol)
--     , ("public", SomeAnonymousSymbolSing SPublicTokSymbol)
--     , ("requires", SomeAnonymousSymbolSing SRequiresTokSymbol)
--     , ("return", SomeAnonymousSymbolSing SReturnTokSymbol)
--     , ("schema", SomeAnonymousSymbolSing SSchemaTokSymbol)
--     , ("signer", SomeAnonymousSymbolSing SSignerTokSymbol)
--     , ("spec", SomeAnonymousSymbolSing SSpecTokSymbol)
--     , ("store", SomeAnonymousSymbolSing SStoreTokSymbol)
--     , ("struct", SomeAnonymousSymbolSing SStructTokSymbol)
--     , ("succeeds_if", SomeAnonymousSymbolSing SSucceedsIfTokSymbol)
--     , ("to", SomeAnonymousSymbolSing SToTokSymbol)
--     , ("true", SomeAnonymousSymbolSing STrueTokSymbol)
--     , ("u128", SomeAnonymousSymbolSing SU128TokSymbol)
--     , ("u16", SomeAnonymousSymbolSing SU16TokSymbol)
--     , ("u256", SomeAnonymousSymbolSing SU256TokSymbol)
--     , ("u32", SomeAnonymousSymbolSing SU32TokSymbol)
--     , ("u64", SomeAnonymousSymbolSing SU64TokSymbol)
--     , ("u8", SomeAnonymousSymbolSing SU8TokSymbol)
--     , ("unpack", SomeAnonymousSymbolSing SUnpackTokSymbol)
--     , ("update", SomeAnonymousSymbolSing SUpdateTokSymbol)
--     , ("use", SomeAnonymousSymbolSing SUseTokSymbol)
--     , ("vector<", SomeAnonymousSymbolSing SVectorLessThanSignTokSymbol)
--     , ("vector[", SomeAnonymousSymbolSing SVectorLeftSquareBracketTokSymbol)
--     , ("where", SomeAnonymousSymbolSing SWhereTokSymbol)
--     , ("while", SomeAnonymousSymbolSing SWhileTokSymbol)
--     , ("with", SomeAnonymousSymbolSing SWithTokSymbol)
--     , ("{", SomeAnonymousSymbolSing SLeftCurlyBracketTokSymbol)
--     , ("|", SomeAnonymousSymbolSing SVerticalLineTokSymbol)
--     , ("||", SomeAnonymousSymbolSing SVerticalLineVerticalLineTokSymbol)
--     , ("}", SomeAnonymousSymbolSing SRightCurlyBracketTokSymbol)
--     ]

-- mkSymbolTable :: TS.Language -> IO SymbolTable
-- mkSymbolTable lang = do
--   count <- fromIntegral <$> TS.languageSymbolCount lang
--   SymbolTable <$> foldrM
--     (\id acc -> do
--       symName <- TS.languageSymbolName lang id
--       let mSymSing = Map.lookup (BSC.unpack symName) symbolMap
--       pure (maybe acc (flip (IM.insert (fromIntegral id)) acc) mSymSing)
--     )
--     (IM.empty :: IntMap SomeSymbolSing)
--     [0..count-1]

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
   , ''Constant
   , ''HidExpression
   , ''AbortExpression
   , ''AssignExpression
   , ''HidUnaryExpression
   , ''HidUnaryExpressionInternal0
   , ''BorrowExpression
   , ''HidReference
   , ''ImmRef
   , ''MutRef
   , ''DereferenceExpression
   , ''HidExpressionTerm
   , ''AnnotationExpression
   , ''HidType
   , ''ApplyType
   , ''ModuleAccess
   , ''HidModuleIdentifier
   , ''Identifier
   , ''HidReservedIdentifier
   , ''HidExists
   , ''HidForall
   , ''ModuleIdentity
   , ''ModuleIdentityInternal0
   , ''NumLiteral
   , ''NumLiteralInternal0
   , ''TypeArguments
   , ''FunctionType
   , ''FunctionTypeParameters
   , ''PrimitiveType
   , ''RefType
   , ''TupleType
   , ''Block
   , ''BlockItem
   , ''BlockItemInternal0
   , ''LetStatement
   , ''BindList
   , ''CommaBindList
   , ''HidBind
   , ''AtBind
   , ''HidVariableIdentifier
   , ''BindUnpack
   , ''BindFields
   , ''BindNamedFields
   , ''BindNamedFieldsInternal0
   , ''BindField
   , ''HidSpreadOperator
   , ''MutBindField
   , ''BindPositionalFields
   , ''NameExpression
   , ''HidBindInternal0
   , ''MutBindVar
   , ''HidLiteralValue
   , ''AddressLiteral
   , ''BoolLiteral
   , ''ByteStringLiteral
   , ''HexStringLiteral
   , ''OrBindList
   , ''UseDeclaration
   , ''UseDeclarationInternal0
   , ''UseFun
   , ''HidFunctionIdentifier
   , ''UseModule
   , ''UseModuleMember
   , ''UseMember
   , ''UseModuleMembers
   , ''BreakExpression
   , ''Label
   , ''CallExpression
   , ''ArgList
   , ''ContinueExpression
   , ''DotExpression
   , ''ExpressionList
   , ''IfExpression
   , ''IndexExpression
   , ''MacroCallExpression
   , ''MacroModuleAccess
   , ''MatchExpression
   , ''HidMatchBody
   , ''MatchArm
   , ''MatchCondition
   , ''PackExpression
   , ''FieldInitializeList
   , ''ExpField
   , ''HidFieldIdentifier
   , ''SpecBlock
   , ''SpecBlockInternal0
   , ''HidSpecBlockTarget
   , ''SpecBlockTargetSchema
   , ''HidStructIdentifier
   , ''TypeParameters
   , ''TypeParameter
   , ''Ability
   , ''HidTypeParameterIdentifier
   , ''HidSpecFunction
   , ''NativeSpecFunction
   , ''HidSpecFunctionSignature
   , ''FunctionParameters
   , ''FunctionParametersInternal0
   , ''FunctionParameter
   , ''FunctionParameterInternal0
   , ''MutFunctionParameter
   , ''RetType
   , ''UninterpretedSpecFunction
   , ''UsualSpecFunction
   , ''SpecBody
   , ''HidSpecBlockMemeber
   , ''SpecApply
   , ''SpecApplyPattern
   , ''SpecApplyNamePattern
   , ''SpecApplyPatternInternal0
   , ''SpecCondition
   , ''HidSpecAbortIf
   , ''ConditionProperties
   , ''SpecProperty
   , ''HidSpecAbortWithOrModifies
   , ''HidSpecAbortWithOrModifiesInternal0
   , ''HidSpecCondition
   , ''HidSpecConditionInternal0
   , ''HidSpecConditionKind
   , ''SpecInclude
   , ''SpecInvariant
   , ''SpecInvariantInternal0
   , ''SpecLet
   , ''SpecPragma
   , ''SpecVariable
   , ''SpecVariableInternal0
   , ''UnitExpression
   , ''VectorExpression
   , ''VectorExpressionInternal0
   , ''MoveOrCopyExpression
   , ''MoveOrCopyExpressionInternal0
   , ''UnaryExpression
   , ''UnaryOp
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
   , ''FriendDeclaration
   , ''FriendAccess
   , ''HidEnumItem
   , ''EnumDefinition
   , ''EnumVariants
   , ''Variant
   , ''DatatypeFields
   , ''NamedFields
   , ''FieldAnnotation
   , ''PositionalFields
   , ''HidVariantIdentifier
   , ''HidEnumSignature
   , ''AbilityDecls
   , ''HidEnumIdentifier
   , ''PostfixAbilityDecls
   , ''HidFunctionItem
   , ''FunctionDefinition
   , ''HidFunctionSignature
   , ''Modifier
   , ''ModifierInternal0
   , ''MacroFunctionDefinition
   , ''HidMacroSignature
   , ''NativeFunctionDefinition
   , ''HidStructItem
   , ''NativeStructDefinition
   , ''HidStructSignature
   , ''StructDefinition
   , ''Token
   , ''Syntax.PairF
   , ''Syntax.MaybeF
   , ''Syntax.ListF
   ]
