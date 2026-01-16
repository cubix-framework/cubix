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

deriveAllButDynCase
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
