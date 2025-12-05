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
data BindVarL
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
data ConstantIdentifierL
data ContinueExpressionL
data DatatypeFieldsL
data DereferenceExpressionL
data DotExpressionL
data EnumDefinitionL
data EnumIdentifierL
data EnumVariantsL
data ExpFieldL
data ExpressionListL
data FieldAnnotationL
data FieldIdentifierL
data FieldInitializeListL
data FriendAccessL
data FriendDeclarationL
data FunctionDefinitionL
data FunctionIdentifierL
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
data InvariantModifierL
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
data ModuleIdentifierL
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
data SpecBlockTargetModuleL
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
data StructIdentifierL
data TupleTypeL
data TypeArgumentsL
data TypeIdentifierL
data TypeParameterL
data TypeParameterIdentifierL
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
data VariableIdentifierL
data VariantL
data VariantIdentifierL
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
  HiddenBindInternal0BindVarVariableIdentifier
    :: e IdentifierL
    -> HiddenBindInternal0 e HiddenBindInternal0L

data HiddenBindVarVariableIdentifier e l where
  HiddenBindVarVariableIdentifier
    :: e IdentifierL
    -> HiddenBindVarVariableIdentifier e BindVarL

data HiddenConstantIdentifierIdentifier e l where
  HiddenConstantIdentifierIdentifier
    :: e IdentifierL
    -> HiddenConstantIdentifierIdentifier e ConstantIdentifierL

data HiddenEnumIdentifier e l where
  HiddenEnumIdentifier
    :: e IdentifierL
    -> HiddenEnumIdentifier e HiddenEnumIdentifierL

data HiddenEnumIdentifierIdentifier e l where
  HiddenEnumIdentifierIdentifier
    :: e IdentifierL
    -> HiddenEnumIdentifierIdentifier e EnumIdentifierL

data HiddenEnumItem e l where
  HiddenEnumItem
    :: e EnumDefinitionL
    -> HiddenEnumItem e HiddenEnumItemL

data HiddenEnumSignature e l where
  HiddenEnumSignature
    :: e IdentifierL
    -> e (Maybe TypeParametersL)
    -> e (Maybe AbilityDeclsL)
    -> HiddenEnumSignature e HiddenEnumSignatureL

data HiddenExists e l where
  HiddenExists
    :: e ExistsTokL
    -> HiddenExists e HiddenExistsL

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

data HiddenFieldIdentifier e l where
  HiddenFieldIdentifier
    :: e IdentifierL
    -> HiddenFieldIdentifier e HiddenFieldIdentifierL

data HiddenFieldIdentifierIdentifier e l where
  HiddenFieldIdentifierIdentifier
    :: e IdentifierL
    -> HiddenFieldIdentifierIdentifier e FieldIdentifierL

data HiddenForall e l where
  HiddenForall
    :: e ForallTokL
    -> HiddenForall e HiddenForallL

data HiddenFunctionIdentifier e l where
  HiddenFunctionIdentifier
    :: e IdentifierL
    -> HiddenFunctionIdentifier e HiddenFunctionIdentifierL

data HiddenFunctionIdentifierIdentifier e l where
  HiddenFunctionIdentifierIdentifier
    :: e IdentifierL
    -> HiddenFunctionIdentifierIdentifier e FunctionIdentifierL

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

data HiddenFunctionSignature e l where
  HiddenFunctionSignature
    :: e (Maybe ModifierL)
    -> e (Maybe ModifierL)
    -> e (Maybe ModifierL)
    -> e IdentifierL
    -> e (Maybe TypeParametersL)
    -> e FunctionParametersL
    -> e (Maybe RetTypeL)
    -> HiddenFunctionSignature e HiddenFunctionSignatureL

data HiddenIdentifierReservedIdentifier e l where
  HiddenIdentifierReservedIdentifier
    :: e HiddenReservedIdentifierL
    -> HiddenIdentifierReservedIdentifier e IdentifierL

data HiddenInvariantModifier e l where
  HiddenInvariantModifier
    :: e SpecInvariantInternal0L
    -> HiddenInvariantModifier e InvariantModifierL

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

data HiddenMacroSignature e l where
  HiddenMacroSignature
    :: e (Maybe ModifierL)
    -> e IdentifierL
    -> e (Maybe TypeParametersL)
    -> e FunctionParametersL
    -> e (Maybe RetTypeL)
    -> HiddenMacroSignature e HiddenMacroSignatureL

data HiddenMatchBody e l where
  HiddenMatchBody
    :: e [MatchArmL]
    -> e (Maybe MatchArmL)
    -> HiddenMatchBody e HiddenMatchBodyL

data HiddenModuleIdentifier e l where
  HiddenModuleIdentifier
    :: e IdentifierL
    -> HiddenModuleIdentifier e HiddenModuleIdentifierL

data HiddenModuleIdentifierIdentifier e l where
  HiddenModuleIdentifierIdentifier
    :: e IdentifierL
    -> HiddenModuleIdentifierIdentifier e ModuleIdentifierL

data HiddenReference e l where
  HiddenReferenceImmRef
    :: e ImmRefL
    -> HiddenReference e HiddenReferenceL
  HiddenReferenceMutRef
    :: e MutRefL
    -> HiddenReference e HiddenReferenceL

data HiddenReservedIdentifier e l where
  HiddenReservedIdentifierForall
    :: e ForallTokL
    -> HiddenReservedIdentifier e HiddenReservedIdentifierL
  HiddenReservedIdentifierExists
    :: e ExistsTokL
    -> HiddenReservedIdentifier e HiddenReservedIdentifierL

data HiddenSpecAbortIf e l where
  HiddenSpecAbortIf
    :: e AbortsIfTokL
    -> e (Maybe ConditionPropertiesL)
    -> e HiddenExpressionL
    -> e (Maybe HiddenExpressionL)
    -> HiddenSpecAbortIf e HiddenSpecAbortIfL

data HiddenSpecAbortWithOrModifies e l where
  HiddenSpecAbortWithOrModifies
    :: e HiddenSpecAbortWithOrModifiesInternal0L
    -> e (Maybe ConditionPropertiesL)
    -> e [HiddenExpressionL]
    -> HiddenSpecAbortWithOrModifies e HiddenSpecAbortWithOrModifiesL

data HiddenSpecAbortWithOrModifiesInternal0 e l where
  HiddenSpecAbortWithOrModifiesInternal0AbortsWith
    :: e AbortsWithTokL
    -> HiddenSpecAbortWithOrModifiesInternal0 e HiddenSpecAbortWithOrModifiesInternal0L
  HiddenSpecAbortWithOrModifiesInternal0Modifies
    :: e ModifiesTokL
    -> HiddenSpecAbortWithOrModifiesInternal0 e HiddenSpecAbortWithOrModifiesInternal0L

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

data HiddenSpecBlockTarget e l where
  HiddenSpecBlockTargetIdentifier
    :: e IdentifierL
    -> HiddenSpecBlockTarget e HiddenSpecBlockTargetL
  HiddenSpecBlockTargetSpecBlockTargetModuleModule
    :: e ModuleTokL
    -> HiddenSpecBlockTarget e HiddenSpecBlockTargetL
  HiddenSpecBlockTargetSpecBlockTargetSchema
    :: e SpecBlockTargetSchemaL
    -> HiddenSpecBlockTarget e HiddenSpecBlockTargetL

data HiddenSpecBlockTargetModuleModule e l where
  HiddenSpecBlockTargetModuleModule
    :: e ModuleTokL
    -> HiddenSpecBlockTargetModuleModule e SpecBlockTargetModuleL

data HiddenSpecCondition e l where
  HiddenSpecCondition
    :: e HiddenSpecConditionInternal0L
    -> e (Maybe ConditionPropertiesL)
    -> e HiddenExpressionL
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

data HiddenSpecFunctionSignature e l where
  HiddenSpecFunctionSignature
    :: e IdentifierL
    -> e (Maybe TypeParametersL)
    -> e FunctionParametersL
    -> e RetTypeL
    -> HiddenSpecFunctionSignature e HiddenSpecFunctionSignatureL

data HiddenSpreadOperator e l where
  HiddenSpreadOperator
    :: e RangeTokL
    -> HiddenSpreadOperator e HiddenSpreadOperatorL

data HiddenStructIdentifier e l where
  HiddenStructIdentifier
    :: e IdentifierL
    -> HiddenStructIdentifier e HiddenStructIdentifierL

data HiddenStructIdentifierIdentifier e l where
  HiddenStructIdentifierIdentifier
    :: e IdentifierL
    -> HiddenStructIdentifierIdentifier e StructIdentifierL

data HiddenStructItem e l where
  HiddenStructItemNativeStructDefinition
    :: e NativeStructDefinitionL
    -> HiddenStructItem e HiddenStructItemL
  HiddenStructItemStructDefinition
    :: e StructDefinitionL
    -> HiddenStructItem e HiddenStructItemL

data HiddenStructSignature e l where
  HiddenStructSignature
    :: e IdentifierL
    -> e (Maybe TypeParametersL)
    -> e (Maybe AbilityDeclsL)
    -> HiddenStructSignature e HiddenStructSignatureL

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

data HiddenTypeIdentifier e l where
  HiddenTypeIdentifier
    :: e IdentifierL
    -> HiddenTypeIdentifier e HiddenTypeIdentifierL

data HiddenTypeIdentifierIdentifier e l where
  HiddenTypeIdentifierIdentifier
    :: e IdentifierL
    -> HiddenTypeIdentifierIdentifier e TypeIdentifierL

data HiddenTypeParameterIdentifier e l where
  HiddenTypeParameterIdentifier
    :: e IdentifierL
    -> HiddenTypeParameterIdentifier e HiddenTypeParameterIdentifierL

data HiddenTypeParameterIdentifierIdentifier e l where
  HiddenTypeParameterIdentifierIdentifier
    :: e IdentifierL
    -> HiddenTypeParameterIdentifierIdentifier e TypeParameterIdentifierL

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

data HiddenVariableIdentifier e l where
  HiddenVariableIdentifier
    :: e IdentifierL
    -> HiddenVariableIdentifier e HiddenVariableIdentifierL

data HiddenVariableIdentifierIdentifier e l where
  HiddenVariableIdentifierIdentifier
    :: e IdentifierL
    -> HiddenVariableIdentifierIdentifier e VariableIdentifierL

data HiddenVariantIdentifier e l where
  HiddenVariantIdentifier
    :: e IdentifierL
    -> HiddenVariantIdentifier e HiddenVariantIdentifierL

data HiddenVariantIdentifierIdentifier e l where
  HiddenVariantIdentifierIdentifier
    :: e IdentifierL
    -> HiddenVariantIdentifierIdentifier e VariantIdentifierL

data HiddenWhitespace e l where
  HiddenWhitespace
    :: Text
    -> HiddenWhitespace e HiddenWhitespaceL

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

data AbilityDecls e l where
  AbilityDecls
    :: e [AbilityL]
    -> e (Maybe AbilityL)
    -> AbilityDecls e AbilityDeclsL

data AbortExpression e l where
  AbortExpression
    :: e (Maybe HiddenExpressionL)
    -> AbortExpression e AbortExpressionL

data AddressLiteral e l where
  AddressLiteral
    :: Text
    -> AddressLiteral e AddressLiteralL

data Annotation e l where
  Annotation
    :: e [AnnotationItemL]
    -> Annotation e AnnotationL

data AnnotationExpr e l where
  AnnotationExprName
    :: e IdentifierL
    -> AnnotationExpr e AnnotationExprL
  AnnotationExpr2
    :: e IdentifierL
    -> e AssignTokL
    -> e AnnotationExprInternal0L
    -> AnnotationExpr e AnnotationExprL

data AnnotationExprInternal0 e l where
  AnnotationExprInternal0LocalConst
    :: e ModuleAccessL
    -> AnnotationExprInternal0 e AnnotationExprInternal0L
  AnnotationExprInternal0ModuleAccess
    :: e ModuleAccessL
    -> AnnotationExprInternal0 e AnnotationExprInternal0L
  AnnotationExprInternal0LiteralValue
    :: e HiddenLiteralValueL
    -> AnnotationExprInternal0 e AnnotationExprInternal0L

data AnnotationExpression e l where
  AnnotationExpression
    :: e HiddenExpressionL
    -> e HiddenTypeL
    -> AnnotationExpression e AnnotationExpressionL

data AnnotationItem e l where
  AnnotationItemAnnotationExpr
    :: e AnnotationExprL
    -> AnnotationItem e AnnotationItemL
  AnnotationItemAnnotationList
    :: e AnnotationListL
    -> AnnotationItem e AnnotationItemL

data AnnotationList e l where
  AnnotationList
    :: e IdentifierL
    -> e [AnnotationListInternal0L]
    -> AnnotationList e AnnotationListL

data AnnotationListInternal0 e l where
  AnnotationListInternal0LiteralValue
    :: e HiddenLiteralValueL
    -> AnnotationListInternal0 e AnnotationListInternal0L
  AnnotationListInternal0AnnotationItem
    :: e AnnotationItemL
    -> AnnotationListInternal0 e AnnotationListInternal0L
  AnnotationListInternal0ModuleAccess
    :: e ModuleAccessL
    -> AnnotationListInternal0 e AnnotationListInternal0L
  AnnotationListInternal0LocalConst
    :: e ModuleAccessL
    -> AnnotationListInternal0 e AnnotationListInternal0L

data ApplyType e l where
  ApplyType
    :: e (ModuleAccessL, Maybe TypeArgumentsL)
    -> ApplyType e ApplyTypeL

data ArgList e l where
  ArgList
    :: e [HiddenExpressionL]
    -> e (Maybe HiddenExpressionL)
    -> ArgList e ArgListL

data AssignExpression e l where
  AssignExpression
    :: e ((HiddenUnaryExpressionInternal0L, AssignTokL), HiddenExpressionL)
    -> AssignExpression e AssignExpressionL

data AtBind e l where
  AtBind
    :: e IdentifierL
    -> e AtTokL
    -> e BindListL
    -> AtBind e AtBindL

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

data BindField e l where
  BindField1
    :: e BindListL
    -> e (Maybe BindListL)
    -> BindField e BindFieldL
  BindFieldSpreadOperator
    :: e RangeTokL
    -> BindField e BindFieldL

data BindFields e l where
  BindFieldsBindPositionalFields
    :: e BindPositionalFieldsL
    -> BindFields e BindFieldsL
  BindFieldsBindNamedFields
    :: e BindNamedFieldsL
    -> BindFields e BindFieldsL

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

data BindPositionalFields e l where
  BindPositionalFields
    :: e [BindNamedFieldsInternal0L]
    -> e (Maybe BindNamedFieldsInternal0L)
    -> BindPositionalFields e BindPositionalFieldsL

data BindUnpack e l where
  BindUnpack
    :: e NameExpressionL
    -> e (Maybe BindFieldsL)
    -> BindUnpack e BindUnpackL

data Block e l where
  Block
    :: e [UseDeclarationL]
    -> e [BlockItemL]
    -> e (Maybe HiddenExpressionL)
    -> Block e BlockL

data BlockComment e l where
  BlockComment
    :: e (Text, DivTokL)
    -> BlockComment e BlockCommentL

data BlockIdentifier e l where
  BlockIdentifier
    :: e LabelL
    -> BlockIdentifier e BlockIdentifierL

data BlockItem e l where
  BlockItemExpression
    :: e HiddenExpressionL
    -> BlockItem e BlockItemL
  BlockItemLetStatement
    :: e LetStatementL
    -> BlockItem e BlockItemL

data BoolLiteral e l where
  BoolLiteralTrue
    :: e TrueTokL
    -> BoolLiteral e BoolLiteralL
  BoolLiteralFalse
    :: e FalseTokL
    -> BoolLiteral e BoolLiteralL

data BorrowExpression e l where
  BorrowExpression
    :: e (HiddenReferenceL, HiddenExpressionL)
    -> BorrowExpression e BorrowExpressionL

data BreakExpression e l where
  BreakExpression
    :: e (Maybe LabelL)
    -> e (Maybe HiddenExpressionL)
    -> BreakExpression e BreakExpressionL

data ByteStringLiteral e l where
  ByteStringLiteral
    :: Text
    -> ByteStringLiteral e ByteStringLiteralL

data CallExpression e l where
  CallExpression
    :: e (NameExpressionL, ArgListL)
    -> CallExpression e CallExpressionL

data CastExpression e l where
  CastExpression
    :: e (HiddenExpressionL, HiddenTypeL)
    -> CastExpression e CastExpressionL

data CommaBindList e l where
  CommaBindList
    :: e [HiddenBindL]
    -> e (Maybe HiddenBindL)
    -> CommaBindList e CommaBindListL

data ConditionProperties e l where
  ConditionProperties
    :: e [SpecPropertyL]
    -> e (Maybe SpecPropertyL)
    -> ConditionProperties e ConditionPropertiesL

data Constant e l where
  Constant
    :: e IdentifierL
    -> e HiddenTypeL
    -> e AssignTokL
    -> e HiddenExpressionL
    -> Constant e ConstantL

data ContinueExpression e l where
  ContinueExpression
    :: e (Maybe LabelL)
    -> ContinueExpression e ContinueExpressionL

data DatatypeFields e l where
  DatatypeFieldsPositionalFields
    :: e PositionalFieldsL
    -> DatatypeFields e DatatypeFieldsL
  DatatypeFieldsNamedFields
    :: e NamedFieldsL
    -> DatatypeFields e DatatypeFieldsL

data DereferenceExpression e l where
  DereferenceExpression
    :: e (MulTokL, HiddenExpressionL)
    -> DereferenceExpression e DereferenceExpressionL

data DotExpression e l where
  DotExpression
    :: e (HiddenExpressionTermL, HiddenExpressionTermL)
    -> DotExpression e DotExpressionL

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

data ExpField e l where
  ExpField
    :: e IdentifierL
    -> e (Maybe HiddenExpressionL)
    -> ExpField e ExpFieldL

data ExpressionList e l where
  ExpressionList
    :: e [HiddenExpressionL]
    -> ExpressionList e ExpressionListL

data FieldAnnotation e l where
  FieldAnnotation
    :: e IdentifierL
    -> e HiddenTypeL
    -> FieldAnnotation e FieldAnnotationL

data FieldInitializeList e l where
  FieldInitializeList
    :: e [ExpFieldL]
    -> e (Maybe ExpFieldL)
    -> FieldInitializeList e FieldInitializeListL

data FriendAccess e l where
  FriendAccessLocalModule
    :: e IdentifierL
    -> FriendAccess e FriendAccessL
  FriendAccessFullyQualifiedModule
    :: e ModuleIdentityL
    -> FriendAccess e FriendAccessL

data FriendDeclaration e l where
  FriendDeclaration
    :: e FriendTokL
    -> e FriendAccessL
    -> FriendDeclaration e FriendDeclarationL

data FunctionDefinition e l where
  FunctionDefinition
    :: e ((((((Maybe ModifierL, Maybe ModifierL), Maybe ModifierL), IdentifierL), Maybe TypeParametersL), FunctionParametersL), Maybe RetTypeL)
    -> e BlockL
    -> FunctionDefinition e FunctionDefinitionL

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

data HexStringLiteral e l where
  HexStringLiteral
    :: Text
    -> HexStringLiteral e HexStringLiteralL

data IdentifiedExpression e l where
  IdentifiedExpression
    :: e BlockIdentifierL
    -> e HiddenExpressionL
    -> IdentifiedExpression e IdentifiedExpressionL

data Identifier e l where
  Identifier
    :: Text
    -> Identifier e IdentifierL

data IfExpression e l where
  IfExpression
    :: e ((HiddenExpressionL, HiddenExpressionL), Maybe HiddenExpressionL)
    -> IfExpression e IfExpressionL

data ImmRef e l where
  ImmRef
    :: e BitandTokL
    -> ImmRef e ImmRefL

data IndexExpression e l where
  IndexExpression
    :: e (HiddenExpressionTermL, ([HiddenExpressionL], Maybe HiddenExpressionL))
    -> IndexExpression e IndexExpressionL

data Label e l where
  Label
    :: e IdentifierL
    -> Label e LabelL

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

data LambdaBindings e l where
  LambdaBindings
    :: e BitorTokL
    -> e ([LambdaBindingL], Maybe LambdaBindingL)
    -> e BitorTokL
    -> LambdaBindings e LambdaBindingsL

data LambdaExpression e l where
  LambdaExpression
    :: e LambdaBindingsL
    -> e (Maybe HiddenTypeL)
    -> e HiddenExpressionL
    -> LambdaExpression e LambdaExpressionL

data LetStatement e l where
  LetStatement
    :: e BindListL
    -> e (Maybe HiddenTypeL)
    -> e (Maybe (AssignTokL, HiddenExpressionL))
    -> LetStatement e LetStatementL

data LineComment e l where
  LineComment
    :: Text
    -> LineComment e LineCommentL

data LoopExpression e l where
  LoopExpression
    :: e HiddenExpressionL
    -> LoopExpression e LoopExpressionL

data MacroCallExpression e l where
  MacroCallExpression
    :: e MacroModuleAccessL
    -> e (Maybe TypeArgumentsL)
    -> e ArgListL
    -> MacroCallExpression e MacroCallExpressionL

data MacroFunctionDefinition e l where
  MacroFunctionDefinition
    :: e (Maybe ModifierL)
    -> e ((((Maybe ModifierL, IdentifierL), Maybe TypeParametersL), FunctionParametersL), Maybe RetTypeL)
    -> e BlockL
    -> MacroFunctionDefinition e MacroFunctionDefinitionL

data MacroIdentifier e l where
  MacroIdentifier
    :: Text
    -> MacroIdentifier e MacroIdentifierL

data MacroModuleAccess e l where
  MacroModuleAccess
    :: e ModuleAccessL
    -> e BangTokL
    -> MacroModuleAccess e MacroModuleAccessL

data MatchArm e l where
  MatchArm
    :: e BindListL
    -> e (Maybe MatchConditionL)
    -> e HiddenExpressionL
    -> MatchArm e MatchArmL

data MatchCondition e l where
  MatchCondition
    :: e HiddenExpressionL
    -> MatchCondition e MatchConditionL

data MatchExpression e l where
  MatchExpression
    :: e HiddenExpressionL
    -> e ([MatchArmL], Maybe MatchArmL)
    -> MatchExpression e MatchExpressionL

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

data ModuleDefinition e l where
  ModuleDefinition
    :: e ModuleTokL
    -> e ModuleIdentityL
    -> e ModuleBodyL
    -> ModuleDefinition e ModuleDefinitionL

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

data MutBindField e l where
  MutBindField
    :: e BindFieldL
    -> MutBindField e MutBindFieldL

data MutBindVar e l where
  MutBindVar
    :: e IdentifierL
    -> MutBindVar e MutBindVarL

data MutFunctionParameter e l where
  MutFunctionParameter
    :: e FunctionParameterL
    -> MutFunctionParameter e MutFunctionParameterL

data MutRef e l where
  MutRef
    :: e BitandTokL
    -> MutRef e MutRefL

data NameExpression e l where
  NameExpression
    :: e ModuleAccessL
    -> NameExpression e NameExpressionL

data NamedFields e l where
  NamedFields
    :: e [FieldAnnotationL]
    -> e (Maybe FieldAnnotationL)
    -> NamedFields e NamedFieldsL

data NativeFunctionDefinition e l where
  NativeFunctionDefinition
    :: e ((((((Maybe ModifierL, Maybe ModifierL), Maybe ModifierL), IdentifierL), Maybe TypeParametersL), FunctionParametersL), Maybe RetTypeL)
    -> NativeFunctionDefinition e NativeFunctionDefinitionL

data NativeSpecFunction e l where
  NativeSpecFunction
    :: e NativeTokL
    -> e (((IdentifierL, Maybe TypeParametersL), FunctionParametersL), RetTypeL)
    -> NativeSpecFunction e NativeSpecFunctionL

data NativeStructDefinition e l where
  NativeStructDefinition
    :: e (Maybe PublicTokL)
    -> e NativeTokL
    -> e ((IdentifierL, Maybe TypeParametersL), Maybe AbilityDeclsL)
    -> NativeStructDefinition e NativeStructDefinitionL

data Newline e l where
  Newline
    :: Text
    -> Newline e NewlineL

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

data OrBindList e l where
  OrBindList
    :: e HiddenBindL
    -> e [(BitorTokL, HiddenBindL)]
    -> e (Maybe BitorTokL)
    -> OrBindList e OrBindListL

data PackExpression e l where
  PackExpression
    :: e NameExpressionL
    -> e FieldInitializeListL
    -> PackExpression e PackExpressionL

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

data QuantifierBinding e l where
  QuantifierBinding1
    :: e IdentifierL
    -> e HiddenTypeL
    -> QuantifierBinding e QuantifierBindingL
  QuantifierBinding2
    :: e IdentifierL
    -> e HiddenExpressionL
    -> QuantifierBinding e QuantifierBindingL

data QuantifierBindings e l where
  QuantifierBindings
    :: e [QuantifierBindingL]
    -> QuantifierBindings e QuantifierBindingsL

data QuantifierExpression e l where
  QuantifierExpression
    :: e (((HiddenReservedIdentifierL, QuantifierBindingsL), Maybe HiddenExpressionL), HiddenExpressionL)
    -> QuantifierExpression e QuantifierExpressionL

data RefType e l where
  RefType
    :: e HiddenReferenceL
    -> e HiddenTypeL
    -> RefType e RefTypeL

data RetType e l where
  RetType
    :: e HiddenTypeL
    -> RetType e RetTypeL

data ReturnExpression e l where
  ReturnExpression1
    :: e (Maybe LabelL)
    -> e HiddenExpressionL
    -> ReturnExpression e ReturnExpressionL
  ReturnExpressionLabel
    :: e LabelL
    -> ReturnExpression e ReturnExpressionL

data SourceFile e l where
  SourceFile
    :: e [ModuleDefinitionL]
    -> SourceFile e SourceFileL

data SpecApply e l where
  SpecApply
    :: e HiddenExpressionL
    -> e [SpecApplyPatternL]
    -> e (Maybe [SpecApplyPatternL])
    -> SpecApply e SpecApplyL

data SpecApplyNamePattern e l where
  SpecApplyNamePattern
    :: Text
    -> SpecApplyNamePattern e SpecApplyNamePatternL

data SpecApplyPattern e l where
  SpecApplyPattern
    :: e (Maybe SpecApplyPatternInternal0L)
    -> e SpecApplyNamePatternL
    -> e (Maybe TypeParametersL)
    -> SpecApplyPattern e SpecApplyPatternL

data SpecApplyPatternInternal0 e l where
  SpecApplyPatternInternal0Public
    :: e PublicTokL
    -> SpecApplyPatternInternal0 e SpecApplyPatternInternal0L
  SpecApplyPatternInternal0Internal
    :: e InternalTokL
    -> SpecApplyPatternInternal0 e SpecApplyPatternInternal0L

data SpecBlock e l where
  SpecBlock1
    :: e (Maybe HiddenSpecBlockTargetL)
    -> e SpecBodyL
    -> SpecBlock e SpecBlockL
  SpecBlockSpecFunction
    :: e HiddenSpecFunctionL
    -> SpecBlock e SpecBlockL

data SpecBlockTargetFun e l where
  SpecBlockTargetFun
    :: e IdentifierL
    -> SpecBlockTargetFun e SpecBlockTargetFunL

data SpecBlockTargetSchema e l where
  SpecBlockTargetSchema
    :: e IdentifierL
    -> e (Maybe TypeParametersL)
    -> SpecBlockTargetSchema e SpecBlockTargetSchemaL

data SpecBlockTargetStruct e l where
  SpecBlockTargetStruct
    :: e IdentifierL
    -> SpecBlockTargetStruct e SpecBlockTargetStructL

data SpecBody e l where
  SpecBody
    :: e [UseDeclarationL]
    -> e [HiddenSpecBlockMemeberL]
    -> SpecBody e SpecBodyL

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

data SpecProperty e l where
  SpecProperty
    :: e IdentifierL
    -> e (Maybe (AssignTokL, HiddenLiteralValueL))
    -> SpecProperty e SpecPropertyL

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

data StructDefinition e l where
  StructDefinition
    :: e (Maybe PublicTokL)
    -> e ((IdentifierL, Maybe TypeParametersL), Maybe AbilityDeclsL)
    -> e DatatypeFieldsL
    -> e (Maybe PostfixAbilityDeclsL)
    -> StructDefinition e StructDefinitionL

data TupleType e l where
  TupleType
    :: e [HiddenTypeL]
    -> e (Maybe HiddenTypeL)
    -> TupleType e TupleTypeL

data TypeArguments e l where
  TypeArguments
    :: e LtTokL
    -> e [HiddenTypeL]
    -> e GtTokL
    -> TypeArguments e TypeArgumentsL

data TypeParameter e l where
  TypeParameter
    :: e (Maybe DollarTokL)
    -> e (Maybe PhantomTokL)
    -> e IdentifierL
    -> e (Maybe ((AbilityL, [(AddTokL, AbilityL)]), Maybe AddTokL))
    -> TypeParameter e TypeParameterL

data TypeParameters e l where
  TypeParameters
    :: e LtTokL
    -> e [TypeParameterL]
    -> e GtTokL
    -> TypeParameters e TypeParametersL

data UnaryExpression e l where
  UnaryExpression
    :: e UnaryOpL
    -> e HiddenExpressionL
    -> UnaryExpression e UnaryExpressionL

data UnaryOp e l where
  UnaryOp
    :: e BangTokL
    -> UnaryOp e UnaryOpL

data UninterpretedSpecFunction e l where
  UninterpretedSpecFunction
    :: e (((IdentifierL, Maybe TypeParametersL), FunctionParametersL), RetTypeL)
    -> UninterpretedSpecFunction e UninterpretedSpecFunctionL

data UnitExpression e l where
  UnitExpression
    :: UnitExpression e UnitExpressionL

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

data UseModuleMembers e l where
  UseModuleMembers1
    :: e ModuleIdentityInternal0L
    -> e [UseMemberL]
    -> UseModuleMembers e UseModuleMembersL
  UseModuleMembers2
    :: e ModuleIdentityL
    -> e [UseMemberL]
    -> UseModuleMembers e UseModuleMembersL

data UsualSpecFunction e l where
  UsualSpecFunction
    :: e (((IdentifierL, Maybe TypeParametersL), FunctionParametersL), RetTypeL)
    -> e BlockL
    -> UsualSpecFunction e UsualSpecFunctionL

data Variant e l where
  Variant
    :: e IdentifierL
    -> e (Maybe DatatypeFieldsL)
    -> Variant e VariantL

data VectorExpression e l where
  VectorExpression
    :: e (Maybe ([HiddenTypeL], GtTokL))
    -> e ([HiddenExpressionL], Maybe HiddenExpressionL)
    -> VectorExpression e VectorExpressionL

data WhileExpression e l where
  WhileExpression
    :: e HiddenExpressionL
    -> e HiddenExpressionL
    -> WhileExpression e WhileExpressionL

deriveAll
  [ ''HiddenBind
  , ''HiddenBindInternal0
  , ''HiddenBindVarVariableIdentifier
  , ''HiddenConstantIdentifierIdentifier
  , ''HiddenEnumIdentifier
  , ''HiddenEnumIdentifierIdentifier
  , ''HiddenEnumItem
  , ''HiddenEnumSignature
  , ''HiddenExists
  , ''HiddenExpression
  , ''HiddenExpressionTerm
  , ''HiddenFieldIdentifier
  , ''HiddenFieldIdentifierIdentifier
  , ''HiddenForall
  , ''HiddenFunctionIdentifier
  , ''HiddenFunctionIdentifierIdentifier
  , ''HiddenFunctionItem
  , ''HiddenFunctionSignature
  , ''HiddenIdentifierReservedIdentifier
  , ''HiddenInvariantModifier
  , ''HiddenLiteralValue
  , ''HiddenMacroSignature
  , ''HiddenMatchBody
  , ''HiddenModuleIdentifier
  , ''HiddenModuleIdentifierIdentifier
  , ''HiddenReference
  , ''HiddenReservedIdentifier
  , ''HiddenSpecAbortIf
  , ''HiddenSpecAbortWithOrModifies
  , ''HiddenSpecAbortWithOrModifiesInternal0
  , ''HiddenSpecBlockMemeber
  , ''HiddenSpecBlockTarget
  , ''HiddenSpecBlockTargetModuleModule
  , ''HiddenSpecCondition
  , ''HiddenSpecConditionInternal0
  , ''HiddenSpecConditionKind
  , ''HiddenSpecFunction
  , ''HiddenSpecFunctionSignature
  , ''HiddenSpreadOperator
  , ''HiddenStructIdentifier
  , ''HiddenStructIdentifierIdentifier
  , ''HiddenStructItem
  , ''HiddenStructSignature
  , ''HiddenType
  , ''HiddenTypeIdentifier
  , ''HiddenTypeIdentifierIdentifier
  , ''HiddenTypeParameterIdentifier
  , ''HiddenTypeParameterIdentifierIdentifier
  , ''HiddenUnaryExpression
  , ''HiddenUnaryExpressionInternal0
  , ''HiddenVariableIdentifier
  , ''HiddenVariableIdentifierIdentifier
  , ''HiddenVariantIdentifier
  , ''HiddenVariantIdentifierIdentifier
  , ''HiddenWhitespace
  , ''Ability
  , ''AbilityDecls
  , ''AbortExpression
  , ''AddressLiteral
  , ''Annotation
  , ''AnnotationExpr
  , ''AnnotationExprInternal0
  , ''AnnotationExpression
  , ''AnnotationItem
  , ''AnnotationList
  , ''AnnotationListInternal0
  , ''ApplyType
  , ''ArgList
  , ''AssignExpression
  , ''AtBind
  , ''BinaryExpression
  , ''BindField
  , ''BindFields
  , ''BindList
  , ''BindNamedFields
  , ''BindNamedFieldsInternal0
  , ''BindPositionalFields
  , ''BindUnpack
  , ''Block
  , ''BlockComment
  , ''BlockIdentifier
  , ''BlockItem
  , ''BoolLiteral
  , ''BorrowExpression
  , ''BreakExpression
  , ''ByteStringLiteral
  , ''CallExpression
  , ''CastExpression
  , ''CommaBindList
  , ''ConditionProperties
  , ''Constant
  , ''ContinueExpression
  , ''DatatypeFields
  , ''DereferenceExpression
  , ''DotExpression
  , ''EnumDefinition
  , ''EnumVariants
  , ''ExpField
  , ''ExpressionList
  , ''FieldAnnotation
  , ''FieldInitializeList
  , ''FriendAccess
  , ''FriendDeclaration
  , ''FunctionDefinition
  , ''FunctionParameter
  , ''FunctionParameterInternal0
  , ''FunctionParameters
  , ''FunctionParametersInternal0
  , ''FunctionType
  , ''FunctionTypeParameters
  , ''HexStringLiteral
  , ''IdentifiedExpression
  , ''Identifier
  , ''IfExpression
  , ''ImmRef
  , ''IndexExpression
  , ''Label
  , ''LambdaBinding
  , ''LambdaBindings
  , ''LambdaExpression
  , ''LetStatement
  , ''LineComment
  , ''LoopExpression
  , ''MacroCallExpression
  , ''MacroFunctionDefinition
  , ''MacroIdentifier
  , ''MacroModuleAccess
  , ''MatchArm
  , ''MatchCondition
  , ''MatchExpression
  , ''Modifier
  , ''ModifierInternal0
  , ''ModuleAccess
  , ''ModuleBody
  , ''ModuleBodyInternal0
  , ''ModuleDefinition
  , ''ModuleIdentity
  , ''ModuleIdentityInternal0
  , ''MoveOrCopyExpression
  , ''MoveOrCopyExpressionInternal0
  , ''MutBindField
  , ''MutBindVar
  , ''MutFunctionParameter
  , ''MutRef
  , ''NameExpression
  , ''NamedFields
  , ''NativeFunctionDefinition
  , ''NativeSpecFunction
  , ''NativeStructDefinition
  , ''Newline
  , ''NumLiteral
  , ''NumLiteralInternal0
  , ''NumLiteralInternal1
  , ''OrBindList
  , ''PackExpression
  , ''PositionalFields
  , ''PostfixAbilityDecls
  , ''PrimitiveType
  , ''QuantifierBinding
  , ''QuantifierBindings
  , ''QuantifierExpression
  , ''RefType
  , ''RetType
  , ''ReturnExpression
  , ''SourceFile
  , ''SpecApply
  , ''SpecApplyNamePattern
  , ''SpecApplyPattern
  , ''SpecApplyPatternInternal0
  , ''SpecBlock
  , ''SpecBlockTargetFun
  , ''SpecBlockTargetSchema
  , ''SpecBlockTargetStruct
  , ''SpecBody
  , ''SpecCondition
  , ''SpecInclude
  , ''SpecInvariant
  , ''SpecInvariantInternal0
  , ''SpecLet
  , ''SpecPragma
  , ''SpecProperty
  , ''SpecVariable
  , ''SpecVariableInternal0
  , ''StructDefinition
  , ''TupleType
  , ''TypeArguments
  , ''TypeParameter
  , ''TypeParameters
  , ''UnaryExpression
  , ''UnaryOp
  , ''UninterpretedSpecFunction
  , ''UnitExpression
  , ''UseDeclaration
  , ''UseDeclarationInternal0
  , ''UseFun
  , ''UseMember
  , ''UseModule
  , ''UseModuleMember
  , ''UseModuleMembers
  , ''UsualSpecFunction
  , ''Variant
  , ''VectorExpression
  , ''WhileExpression
  ]

--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

type MoveSig =
  '[ HiddenBind
   , HiddenBindInternal0
   , HiddenBindVarVariableIdentifier
   , HiddenConstantIdentifierIdentifier
   , HiddenEnumIdentifier
   , HiddenEnumIdentifierIdentifier
   , HiddenEnumItem
   , HiddenEnumSignature
   , HiddenExists
   , HiddenExpression
   , HiddenExpressionTerm
   , HiddenFieldIdentifier
   , HiddenFieldIdentifierIdentifier
   , HiddenForall
   , HiddenFunctionIdentifier
   , HiddenFunctionIdentifierIdentifier
   , HiddenFunctionItem
   , HiddenFunctionSignature
   , HiddenIdentifierReservedIdentifier
   , HiddenInvariantModifier
   , HiddenLiteralValue
   , HiddenMacroSignature
   , HiddenMatchBody
   , HiddenModuleIdentifier
   , HiddenModuleIdentifierIdentifier
   , HiddenReference
   , HiddenReservedIdentifier
   , HiddenSpecAbortIf
   , HiddenSpecAbortWithOrModifies
   , HiddenSpecAbortWithOrModifiesInternal0
   , HiddenSpecBlockMemeber
   , HiddenSpecBlockTarget
   , HiddenSpecBlockTargetModuleModule
   , HiddenSpecCondition
   , HiddenSpecConditionInternal0
   , HiddenSpecConditionKind
   , HiddenSpecFunction
   , HiddenSpecFunctionSignature
   , HiddenSpreadOperator
   , HiddenStructIdentifier
   , HiddenStructIdentifierIdentifier
   , HiddenStructItem
   , HiddenStructSignature
   , HiddenType
   , HiddenTypeIdentifier
   , HiddenTypeIdentifierIdentifier
   , HiddenTypeParameterIdentifier
   , HiddenTypeParameterIdentifierIdentifier
   , HiddenUnaryExpression
   , HiddenUnaryExpressionInternal0
   , HiddenVariableIdentifier
   , HiddenVariableIdentifierIdentifier
   , HiddenVariantIdentifier
   , HiddenVariantIdentifierIdentifier
   , HiddenWhitespace
   , Ability
   , AbilityDecls
   , AbortExpression
   , AddressLiteral
   , Annotation
   , AnnotationExpr
   , AnnotationExprInternal0
   , AnnotationExpression
   , AnnotationItem
   , AnnotationList
   , AnnotationListInternal0
   , ApplyType
   , ArgList
   , AssignExpression
   , AtBind
   , BinaryExpression
   , BindField
   , BindFields
   , BindList
   , BindNamedFields
   , BindNamedFieldsInternal0
   , BindPositionalFields
   , BindUnpack
   , Block
   , BlockComment
   , BlockIdentifier
   , BlockItem
   , BoolLiteral
   , BorrowExpression
   , BreakExpression
   , ByteStringLiteral
   , CallExpression
   , CastExpression
   , CommaBindList
   , ConditionProperties
   , Constant
   , ContinueExpression
   , DatatypeFields
   , DereferenceExpression
   , DotExpression
   , EnumDefinition
   , EnumVariants
   , ExpField
   , ExpressionList
   , FieldAnnotation
   , FieldInitializeList
   , FriendAccess
   , FriendDeclaration
   , FunctionDefinition
   , FunctionParameter
   , FunctionParameterInternal0
   , FunctionParameters
   , FunctionParametersInternal0
   , FunctionType
   , FunctionTypeParameters
   , HexStringLiteral
   , IdentifiedExpression
   , Identifier
   , IfExpression
   , ImmRef
   , IndexExpression
   , Label
   , LambdaBinding
   , LambdaBindings
   , LambdaExpression
   , LetStatement
   , LineComment
   , LoopExpression
   , MacroCallExpression
   , MacroFunctionDefinition
   , MacroIdentifier
   , MacroModuleAccess
   , MatchArm
   , MatchCondition
   , MatchExpression
   , Modifier
   , ModifierInternal0
   , ModuleAccess
   , ModuleBody
   , ModuleBodyInternal0
   , ModuleDefinition
   , ModuleIdentity
   , ModuleIdentityInternal0
   , MoveOrCopyExpression
   , MoveOrCopyExpressionInternal0
   , MutBindField
   , MutBindVar
   , MutFunctionParameter
   , MutRef
   , NameExpression
   , NamedFields
   , NativeFunctionDefinition
   , NativeSpecFunction
   , NativeStructDefinition
   , Newline
   , NumLiteral
   , NumLiteralInternal0
   , NumLiteralInternal1
   , OrBindList
   , PackExpression
   , PositionalFields
   , PostfixAbilityDecls
   , PrimitiveType
   , QuantifierBinding
   , QuantifierBindings
   , QuantifierExpression
   , RefType
   , RetType
   , ReturnExpression
   , SourceFile
   , SpecApply
   , SpecApplyNamePattern
   , SpecApplyPattern
   , SpecApplyPatternInternal0
   , SpecBlock
   , SpecBlockTargetFun
   , SpecBlockTargetSchema
   , SpecBlockTargetStruct
   , SpecBody
   , SpecCondition
   , SpecInclude
   , SpecInvariant
   , SpecInvariantInternal0
   , SpecLet
   , SpecPragma
   , SpecProperty
   , SpecVariable
   , SpecVariableInternal0
   , StructDefinition
   , TupleType
   , TypeArguments
   , TypeParameter
   , TypeParameters
   , UnaryExpression
   , UnaryOp
   , UninterpretedSpecFunction
   , UnitExpression
   , UseDeclaration
   , UseDeclarationInternal0
   , UseFun
   , UseMember
   , UseModule
   , UseModuleMember
   , UseModuleMembers
   , UsualSpecFunction
   , Variant
   , VectorExpression
   , WhileExpression
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

type instance RootSort MoveSig = HiddenBindL

--------------------------------------------------------------------------------
-- Symbol
--------------------------------------------------------------------------------

type data Symbol (symbolType :: SymbolType) where
  HiddenBindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenBindInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenBindVarVariableIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenConstantIdentifierIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenEnumIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenEnumIdentifierIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenEnumItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenEnumSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExistsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenExpressionTermSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFieldIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFieldIdentifierIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenForallSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionIdentifierIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenFunctionSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenIdentifierReservedIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenInvariantModifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenLiteralValueSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenMacroSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenMatchBodySymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenModuleIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenModuleIdentifierIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenReferenceSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenReservedIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortIfSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortWithOrModifiesSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecAbortWithOrModifiesInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecBlockMemeberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecBlockTargetSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecBlockTargetModuleModuleSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecConditionKindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpecFunctionSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenSpreadOperatorSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructIdentifierIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenStructSignatureSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeIdentifierIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeParameterIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenTypeParameterIdentifierIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenUnaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenUnaryExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenVariableIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenVariableIdentifierIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenVariantIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenVariantIdentifierIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HiddenWhitespaceSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbilityDeclsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AbortExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AddressLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AnnotationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AnnotationExprSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AnnotationExprInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  AnnotationExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AnnotationItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AnnotationListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AnnotationListInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  ApplyTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ArgListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AssignExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  AtBindSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BinaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindNamedFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindNamedFieldsInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  BindPositionalFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BindUnpackSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockCommentSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BlockItemSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BoolLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BorrowExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  BreakExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ByteStringLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CallExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CastExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  CommaBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConditionPropertiesSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ConstantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ContinueExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DatatypeFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DereferenceExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  DotExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  EnumVariantsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExpFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ExpressionListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldAnnotationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FieldInitializeListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FriendAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FriendDeclarationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParameterInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionParametersInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  FunctionTypeParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  HexStringLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IdentifiedExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IfExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ImmRefSymbol :: (symbolType ~ Regular) => Symbol symbolType
  IndexExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LabelSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaBindingSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaBindingsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LambdaExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LetStatementSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LineCommentSymbol :: (symbolType ~ Regular) => Symbol symbolType
  LoopExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroCallExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroFunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroIdentifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MacroModuleAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchArmSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MatchExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModifierInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleAccessSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleBodySymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleBodyInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentitySymbol :: (symbolType ~ Regular) => Symbol symbolType
  ModuleIdentityInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MoveOrCopyExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MoveOrCopyExpressionInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  MutBindFieldSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutBindVarSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutFunctionParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  MutRefSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NameExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NamedFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeFunctionDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NativeStructDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NewlineSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralSymbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  NumLiteralInternal1Symbol :: (symbolType ~ Regular) => Symbol symbolType
  OrBindListSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PackExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PositionalFieldsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PostfixAbilityDeclsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  PrimitiveTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierBindingSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierBindingsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  QuantifierExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RefTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  RetTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  ReturnExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SourceFileSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplySymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyNamePatternSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyPatternSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecApplyPatternInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockTargetFunSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockTargetSchemaSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBlockTargetStructSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecBodySymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecConditionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecIncludeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecInvariantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecInvariantInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecLetSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPragmaSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecPropertySymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecVariableSymbol :: (symbolType ~ Regular) => Symbol symbolType
  SpecVariableInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  StructDefinitionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TupleTypeSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeArgumentsSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParameterSymbol :: (symbolType ~ Regular) => Symbol symbolType
  TypeParametersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UnaryExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UnaryOpSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UninterpretedSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UnitExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseDeclarationSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseDeclarationInternal0Symbol :: (symbolType ~ Regular) => Symbol symbolType
  UseFunSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseMemberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleMemberSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UseModuleMembersSymbol :: (symbolType ~ Regular) => Symbol symbolType
  UsualSpecFunctionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VariantSymbol :: (symbolType ~ Regular) => Symbol symbolType
  VectorExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
  WhileExpressionSymbol :: (symbolType ~ Regular) => Symbol symbolType
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
  SHiddenBindSymbol :: SymbolSing Regular HiddenBindSymbol
  SHiddenBindInternal0Symbol :: SymbolSing Regular HiddenBindInternal0Symbol
  SHiddenBindVarVariableIdentifierSymbol :: SymbolSing Regular HiddenBindVarVariableIdentifierSymbol
  SHiddenConstantIdentifierIdentifierSymbol :: SymbolSing Regular HiddenConstantIdentifierIdentifierSymbol
  SHiddenEnumIdentifierSymbol :: SymbolSing Regular HiddenEnumIdentifierSymbol
  SHiddenEnumIdentifierIdentifierSymbol :: SymbolSing Regular HiddenEnumIdentifierIdentifierSymbol
  SHiddenEnumItemSymbol :: SymbolSing Regular HiddenEnumItemSymbol
  SHiddenEnumSignatureSymbol :: SymbolSing Regular HiddenEnumSignatureSymbol
  SHiddenExistsSymbol :: SymbolSing Regular HiddenExistsSymbol
  SHiddenExpressionSymbol :: SymbolSing Regular HiddenExpressionSymbol
  SHiddenExpressionTermSymbol :: SymbolSing Regular HiddenExpressionTermSymbol
  SHiddenFieldIdentifierSymbol :: SymbolSing Regular HiddenFieldIdentifierSymbol
  SHiddenFieldIdentifierIdentifierSymbol :: SymbolSing Regular HiddenFieldIdentifierIdentifierSymbol
  SHiddenForallSymbol :: SymbolSing Regular HiddenForallSymbol
  SHiddenFunctionIdentifierSymbol :: SymbolSing Regular HiddenFunctionIdentifierSymbol
  SHiddenFunctionIdentifierIdentifierSymbol :: SymbolSing Regular HiddenFunctionIdentifierIdentifierSymbol
  SHiddenFunctionItemSymbol :: SymbolSing Regular HiddenFunctionItemSymbol
  SHiddenFunctionSignatureSymbol :: SymbolSing Regular HiddenFunctionSignatureSymbol
  SHiddenIdentifierReservedIdentifierSymbol :: SymbolSing Regular HiddenIdentifierReservedIdentifierSymbol
  SHiddenInvariantModifierSymbol :: SymbolSing Regular HiddenInvariantModifierSymbol
  SHiddenLiteralValueSymbol :: SymbolSing Regular HiddenLiteralValueSymbol
  SHiddenMacroSignatureSymbol :: SymbolSing Regular HiddenMacroSignatureSymbol
  SHiddenMatchBodySymbol :: SymbolSing Regular HiddenMatchBodySymbol
  SHiddenModuleIdentifierSymbol :: SymbolSing Regular HiddenModuleIdentifierSymbol
  SHiddenModuleIdentifierIdentifierSymbol :: SymbolSing Regular HiddenModuleIdentifierIdentifierSymbol
  SHiddenReferenceSymbol :: SymbolSing Regular HiddenReferenceSymbol
  SHiddenReservedIdentifierSymbol :: SymbolSing Regular HiddenReservedIdentifierSymbol
  SHiddenSpecAbortIfSymbol :: SymbolSing Regular HiddenSpecAbortIfSymbol
  SHiddenSpecAbortWithOrModifiesSymbol :: SymbolSing Regular HiddenSpecAbortWithOrModifiesSymbol
  SHiddenSpecAbortWithOrModifiesInternal0Symbol :: SymbolSing Regular HiddenSpecAbortWithOrModifiesInternal0Symbol
  SHiddenSpecBlockMemeberSymbol :: SymbolSing Regular HiddenSpecBlockMemeberSymbol
  SHiddenSpecBlockTargetSymbol :: SymbolSing Regular HiddenSpecBlockTargetSymbol
  SHiddenSpecBlockTargetModuleModuleSymbol :: SymbolSing Regular HiddenSpecBlockTargetModuleModuleSymbol
  SHiddenSpecConditionSymbol :: SymbolSing Regular HiddenSpecConditionSymbol
  SHiddenSpecConditionInternal0Symbol :: SymbolSing Regular HiddenSpecConditionInternal0Symbol
  SHiddenSpecConditionKindSymbol :: SymbolSing Regular HiddenSpecConditionKindSymbol
  SHiddenSpecFunctionSymbol :: SymbolSing Regular HiddenSpecFunctionSymbol
  SHiddenSpecFunctionSignatureSymbol :: SymbolSing Regular HiddenSpecFunctionSignatureSymbol
  SHiddenSpreadOperatorSymbol :: SymbolSing Regular HiddenSpreadOperatorSymbol
  SHiddenStructIdentifierSymbol :: SymbolSing Regular HiddenStructIdentifierSymbol
  SHiddenStructIdentifierIdentifierSymbol :: SymbolSing Regular HiddenStructIdentifierIdentifierSymbol
  SHiddenStructItemSymbol :: SymbolSing Regular HiddenStructItemSymbol
  SHiddenStructSignatureSymbol :: SymbolSing Regular HiddenStructSignatureSymbol
  SHiddenTypeSymbol :: SymbolSing Regular HiddenTypeSymbol
  SHiddenTypeIdentifierSymbol :: SymbolSing Regular HiddenTypeIdentifierSymbol
  SHiddenTypeIdentifierIdentifierSymbol :: SymbolSing Regular HiddenTypeIdentifierIdentifierSymbol
  SHiddenTypeParameterIdentifierSymbol :: SymbolSing Regular HiddenTypeParameterIdentifierSymbol
  SHiddenTypeParameterIdentifierIdentifierSymbol :: SymbolSing Regular HiddenTypeParameterIdentifierIdentifierSymbol
  SHiddenUnaryExpressionSymbol :: SymbolSing Regular HiddenUnaryExpressionSymbol
  SHiddenUnaryExpressionInternal0Symbol :: SymbolSing Regular HiddenUnaryExpressionInternal0Symbol
  SHiddenVariableIdentifierSymbol :: SymbolSing Regular HiddenVariableIdentifierSymbol
  SHiddenVariableIdentifierIdentifierSymbol :: SymbolSing Regular HiddenVariableIdentifierIdentifierSymbol
  SHiddenVariantIdentifierSymbol :: SymbolSing Regular HiddenVariantIdentifierSymbol
  SHiddenVariantIdentifierIdentifierSymbol :: SymbolSing Regular HiddenVariantIdentifierIdentifierSymbol
  SHiddenWhitespaceSymbol :: SymbolSing Regular HiddenWhitespaceSymbol
  SAbilitySymbol :: SymbolSing Regular AbilitySymbol
  SAbilityDeclsSymbol :: SymbolSing Regular AbilityDeclsSymbol
  SAbortExpressionSymbol :: SymbolSing Regular AbortExpressionSymbol
  SAddressLiteralSymbol :: SymbolSing Regular AddressLiteralSymbol
  SAnnotationSymbol :: SymbolSing Regular AnnotationSymbol
  SAnnotationExprSymbol :: SymbolSing Regular AnnotationExprSymbol
  SAnnotationExprInternal0Symbol :: SymbolSing Regular AnnotationExprInternal0Symbol
  SAnnotationExpressionSymbol :: SymbolSing Regular AnnotationExpressionSymbol
  SAnnotationItemSymbol :: SymbolSing Regular AnnotationItemSymbol
  SAnnotationListSymbol :: SymbolSing Regular AnnotationListSymbol
  SAnnotationListInternal0Symbol :: SymbolSing Regular AnnotationListInternal0Symbol
  SApplyTypeSymbol :: SymbolSing Regular ApplyTypeSymbol
  SArgListSymbol :: SymbolSing Regular ArgListSymbol
  SAssignExpressionSymbol :: SymbolSing Regular AssignExpressionSymbol
  SAtBindSymbol :: SymbolSing Regular AtBindSymbol
  SBinaryExpressionSymbol :: SymbolSing Regular BinaryExpressionSymbol
  SBindFieldSymbol :: SymbolSing Regular BindFieldSymbol
  SBindFieldsSymbol :: SymbolSing Regular BindFieldsSymbol
  SBindListSymbol :: SymbolSing Regular BindListSymbol
  SBindNamedFieldsSymbol :: SymbolSing Regular BindNamedFieldsSymbol
  SBindNamedFieldsInternal0Symbol :: SymbolSing Regular BindNamedFieldsInternal0Symbol
  SBindPositionalFieldsSymbol :: SymbolSing Regular BindPositionalFieldsSymbol
  SBindUnpackSymbol :: SymbolSing Regular BindUnpackSymbol
  SBlockSymbol :: SymbolSing Regular BlockSymbol
  SBlockCommentSymbol :: SymbolSing Regular BlockCommentSymbol
  SBlockIdentifierSymbol :: SymbolSing Regular BlockIdentifierSymbol
  SBlockItemSymbol :: SymbolSing Regular BlockItemSymbol
  SBoolLiteralSymbol :: SymbolSing Regular BoolLiteralSymbol
  SBorrowExpressionSymbol :: SymbolSing Regular BorrowExpressionSymbol
  SBreakExpressionSymbol :: SymbolSing Regular BreakExpressionSymbol
  SByteStringLiteralSymbol :: SymbolSing Regular ByteStringLiteralSymbol
  SCallExpressionSymbol :: SymbolSing Regular CallExpressionSymbol
  SCastExpressionSymbol :: SymbolSing Regular CastExpressionSymbol
  SCommaBindListSymbol :: SymbolSing Regular CommaBindListSymbol
  SConditionPropertiesSymbol :: SymbolSing Regular ConditionPropertiesSymbol
  SConstantSymbol :: SymbolSing Regular ConstantSymbol
  SContinueExpressionSymbol :: SymbolSing Regular ContinueExpressionSymbol
  SDatatypeFieldsSymbol :: SymbolSing Regular DatatypeFieldsSymbol
  SDereferenceExpressionSymbol :: SymbolSing Regular DereferenceExpressionSymbol
  SDotExpressionSymbol :: SymbolSing Regular DotExpressionSymbol
  SEnumDefinitionSymbol :: SymbolSing Regular EnumDefinitionSymbol
  SEnumVariantsSymbol :: SymbolSing Regular EnumVariantsSymbol
  SExpFieldSymbol :: SymbolSing Regular ExpFieldSymbol
  SExpressionListSymbol :: SymbolSing Regular ExpressionListSymbol
  SFieldAnnotationSymbol :: SymbolSing Regular FieldAnnotationSymbol
  SFieldInitializeListSymbol :: SymbolSing Regular FieldInitializeListSymbol
  SFriendAccessSymbol :: SymbolSing Regular FriendAccessSymbol
  SFriendDeclarationSymbol :: SymbolSing Regular FriendDeclarationSymbol
  SFunctionDefinitionSymbol :: SymbolSing Regular FunctionDefinitionSymbol
  SFunctionParameterSymbol :: SymbolSing Regular FunctionParameterSymbol
  SFunctionParameterInternal0Symbol :: SymbolSing Regular FunctionParameterInternal0Symbol
  SFunctionParametersSymbol :: SymbolSing Regular FunctionParametersSymbol
  SFunctionParametersInternal0Symbol :: SymbolSing Regular FunctionParametersInternal0Symbol
  SFunctionTypeSymbol :: SymbolSing Regular FunctionTypeSymbol
  SFunctionTypeParametersSymbol :: SymbolSing Regular FunctionTypeParametersSymbol
  SHexStringLiteralSymbol :: SymbolSing Regular HexStringLiteralSymbol
  SIdentifiedExpressionSymbol :: SymbolSing Regular IdentifiedExpressionSymbol
  SIdentifierSymbol :: SymbolSing Regular IdentifierSymbol
  SIfExpressionSymbol :: SymbolSing Regular IfExpressionSymbol
  SImmRefSymbol :: SymbolSing Regular ImmRefSymbol
  SIndexExpressionSymbol :: SymbolSing Regular IndexExpressionSymbol
  SLabelSymbol :: SymbolSing Regular LabelSymbol
  SLambdaBindingSymbol :: SymbolSing Regular LambdaBindingSymbol
  SLambdaBindingsSymbol :: SymbolSing Regular LambdaBindingsSymbol
  SLambdaExpressionSymbol :: SymbolSing Regular LambdaExpressionSymbol
  SLetStatementSymbol :: SymbolSing Regular LetStatementSymbol
  SLineCommentSymbol :: SymbolSing Regular LineCommentSymbol
  SLoopExpressionSymbol :: SymbolSing Regular LoopExpressionSymbol
  SMacroCallExpressionSymbol :: SymbolSing Regular MacroCallExpressionSymbol
  SMacroFunctionDefinitionSymbol :: SymbolSing Regular MacroFunctionDefinitionSymbol
  SMacroIdentifierSymbol :: SymbolSing Regular MacroIdentifierSymbol
  SMacroModuleAccessSymbol :: SymbolSing Regular MacroModuleAccessSymbol
  SMatchArmSymbol :: SymbolSing Regular MatchArmSymbol
  SMatchConditionSymbol :: SymbolSing Regular MatchConditionSymbol
  SMatchExpressionSymbol :: SymbolSing Regular MatchExpressionSymbol
  SModifierSymbol :: SymbolSing Regular ModifierSymbol
  SModifierInternal0Symbol :: SymbolSing Regular ModifierInternal0Symbol
  SModuleAccessSymbol :: SymbolSing Regular ModuleAccessSymbol
  SModuleBodySymbol :: SymbolSing Regular ModuleBodySymbol
  SModuleBodyInternal0Symbol :: SymbolSing Regular ModuleBodyInternal0Symbol
  SModuleDefinitionSymbol :: SymbolSing Regular ModuleDefinitionSymbol
  SModuleIdentitySymbol :: SymbolSing Regular ModuleIdentitySymbol
  SModuleIdentityInternal0Symbol :: SymbolSing Regular ModuleIdentityInternal0Symbol
  SMoveOrCopyExpressionSymbol :: SymbolSing Regular MoveOrCopyExpressionSymbol
  SMoveOrCopyExpressionInternal0Symbol :: SymbolSing Regular MoveOrCopyExpressionInternal0Symbol
  SMutBindFieldSymbol :: SymbolSing Regular MutBindFieldSymbol
  SMutBindVarSymbol :: SymbolSing Regular MutBindVarSymbol
  SMutFunctionParameterSymbol :: SymbolSing Regular MutFunctionParameterSymbol
  SMutRefSymbol :: SymbolSing Regular MutRefSymbol
  SNameExpressionSymbol :: SymbolSing Regular NameExpressionSymbol
  SNamedFieldsSymbol :: SymbolSing Regular NamedFieldsSymbol
  SNativeFunctionDefinitionSymbol :: SymbolSing Regular NativeFunctionDefinitionSymbol
  SNativeSpecFunctionSymbol :: SymbolSing Regular NativeSpecFunctionSymbol
  SNativeStructDefinitionSymbol :: SymbolSing Regular NativeStructDefinitionSymbol
  SNewlineSymbol :: SymbolSing Regular NewlineSymbol
  SNumLiteralSymbol :: SymbolSing Regular NumLiteralSymbol
  SNumLiteralInternal0Symbol :: SymbolSing Regular NumLiteralInternal0Symbol
  SNumLiteralInternal1Symbol :: SymbolSing Regular NumLiteralInternal1Symbol
  SOrBindListSymbol :: SymbolSing Regular OrBindListSymbol
  SPackExpressionSymbol :: SymbolSing Regular PackExpressionSymbol
  SPositionalFieldsSymbol :: SymbolSing Regular PositionalFieldsSymbol
  SPostfixAbilityDeclsSymbol :: SymbolSing Regular PostfixAbilityDeclsSymbol
  SPrimitiveTypeSymbol :: SymbolSing Regular PrimitiveTypeSymbol
  SQuantifierBindingSymbol :: SymbolSing Regular QuantifierBindingSymbol
  SQuantifierBindingsSymbol :: SymbolSing Regular QuantifierBindingsSymbol
  SQuantifierExpressionSymbol :: SymbolSing Regular QuantifierExpressionSymbol
  SRefTypeSymbol :: SymbolSing Regular RefTypeSymbol
  SRetTypeSymbol :: SymbolSing Regular RetTypeSymbol
  SReturnExpressionSymbol :: SymbolSing Regular ReturnExpressionSymbol
  SSourceFileSymbol :: SymbolSing Regular SourceFileSymbol
  SSpecApplySymbol :: SymbolSing Regular SpecApplySymbol
  SSpecApplyNamePatternSymbol :: SymbolSing Regular SpecApplyNamePatternSymbol
  SSpecApplyPatternSymbol :: SymbolSing Regular SpecApplyPatternSymbol
  SSpecApplyPatternInternal0Symbol :: SymbolSing Regular SpecApplyPatternInternal0Symbol
  SSpecBlockSymbol :: SymbolSing Regular SpecBlockSymbol
  SSpecBlockTargetFunSymbol :: SymbolSing Regular SpecBlockTargetFunSymbol
  SSpecBlockTargetSchemaSymbol :: SymbolSing Regular SpecBlockTargetSchemaSymbol
  SSpecBlockTargetStructSymbol :: SymbolSing Regular SpecBlockTargetStructSymbol
  SSpecBodySymbol :: SymbolSing Regular SpecBodySymbol
  SSpecConditionSymbol :: SymbolSing Regular SpecConditionSymbol
  SSpecIncludeSymbol :: SymbolSing Regular SpecIncludeSymbol
  SSpecInvariantSymbol :: SymbolSing Regular SpecInvariantSymbol
  SSpecInvariantInternal0Symbol :: SymbolSing Regular SpecInvariantInternal0Symbol
  SSpecLetSymbol :: SymbolSing Regular SpecLetSymbol
  SSpecPragmaSymbol :: SymbolSing Regular SpecPragmaSymbol
  SSpecPropertySymbol :: SymbolSing Regular SpecPropertySymbol
  SSpecVariableSymbol :: SymbolSing Regular SpecVariableSymbol
  SSpecVariableInternal0Symbol :: SymbolSing Regular SpecVariableInternal0Symbol
  SStructDefinitionSymbol :: SymbolSing Regular StructDefinitionSymbol
  STupleTypeSymbol :: SymbolSing Regular TupleTypeSymbol
  STypeArgumentsSymbol :: SymbolSing Regular TypeArgumentsSymbol
  STypeParameterSymbol :: SymbolSing Regular TypeParameterSymbol
  STypeParametersSymbol :: SymbolSing Regular TypeParametersSymbol
  SUnaryExpressionSymbol :: SymbolSing Regular UnaryExpressionSymbol
  SUnaryOpSymbol :: SymbolSing Regular UnaryOpSymbol
  SUninterpretedSpecFunctionSymbol :: SymbolSing Regular UninterpretedSpecFunctionSymbol
  SUnitExpressionSymbol :: SymbolSing Regular UnitExpressionSymbol
  SUseDeclarationSymbol :: SymbolSing Regular UseDeclarationSymbol
  SUseDeclarationInternal0Symbol :: SymbolSing Regular UseDeclarationInternal0Symbol
  SUseFunSymbol :: SymbolSing Regular UseFunSymbol
  SUseMemberSymbol :: SymbolSing Regular UseMemberSymbol
  SUseModuleSymbol :: SymbolSing Regular UseModuleSymbol
  SUseModuleMemberSymbol :: SymbolSing Regular UseModuleMemberSymbol
  SUseModuleMembersSymbol :: SymbolSing Regular UseModuleMembersSymbol
  SUsualSpecFunctionSymbol :: SymbolSing Regular UsualSpecFunctionSymbol
  SVariantSymbol :: SymbolSing Regular VariantSymbol
  SVectorExpressionSymbol :: SymbolSing Regular VectorExpressionSymbol
  SWhileExpressionSymbol :: SymbolSing Regular WhileExpressionSymbol
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
decSymbolSing SHiddenBindSymbol SHiddenBindSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenBindInternal0Symbol SHiddenBindInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenBindVarVariableIdentifierSymbol SHiddenBindVarVariableIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenConstantIdentifierIdentifierSymbol SHiddenConstantIdentifierIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenEnumIdentifierSymbol SHiddenEnumIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenEnumIdentifierIdentifierSymbol SHiddenEnumIdentifierIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenEnumItemSymbol SHiddenEnumItemSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenEnumSignatureSymbol SHiddenEnumSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenExistsSymbol SHiddenExistsSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenExpressionSymbol SHiddenExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenExpressionTermSymbol SHiddenExpressionTermSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFieldIdentifierSymbol SHiddenFieldIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFieldIdentifierIdentifierSymbol SHiddenFieldIdentifierIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenForallSymbol SHiddenForallSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionIdentifierSymbol SHiddenFunctionIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionIdentifierIdentifierSymbol SHiddenFunctionIdentifierIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionItemSymbol SHiddenFunctionItemSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenFunctionSignatureSymbol SHiddenFunctionSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenIdentifierReservedIdentifierSymbol SHiddenIdentifierReservedIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenInvariantModifierSymbol SHiddenInvariantModifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenLiteralValueSymbol SHiddenLiteralValueSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenMacroSignatureSymbol SHiddenMacroSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenMatchBodySymbol SHiddenMatchBodySymbol = Just (Refl, HRefl)
decSymbolSing SHiddenModuleIdentifierSymbol SHiddenModuleIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenModuleIdentifierIdentifierSymbol SHiddenModuleIdentifierIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenReferenceSymbol SHiddenReferenceSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenReservedIdentifierSymbol SHiddenReservedIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortIfSymbol SHiddenSpecAbortIfSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortWithOrModifiesSymbol SHiddenSpecAbortWithOrModifiesSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecAbortWithOrModifiesInternal0Symbol SHiddenSpecAbortWithOrModifiesInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecBlockMemeberSymbol SHiddenSpecBlockMemeberSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecBlockTargetSymbol SHiddenSpecBlockTargetSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecBlockTargetModuleModuleSymbol SHiddenSpecBlockTargetModuleModuleSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionSymbol SHiddenSpecConditionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionInternal0Symbol SHiddenSpecConditionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecConditionKindSymbol SHiddenSpecConditionKindSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecFunctionSymbol SHiddenSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpecFunctionSignatureSymbol SHiddenSpecFunctionSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenSpreadOperatorSymbol SHiddenSpreadOperatorSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructIdentifierSymbol SHiddenStructIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructIdentifierIdentifierSymbol SHiddenStructIdentifierIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructItemSymbol SHiddenStructItemSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenStructSignatureSymbol SHiddenStructSignatureSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeSymbol SHiddenTypeSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeIdentifierSymbol SHiddenTypeIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeIdentifierIdentifierSymbol SHiddenTypeIdentifierIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeParameterIdentifierSymbol SHiddenTypeParameterIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenTypeParameterIdentifierIdentifierSymbol SHiddenTypeParameterIdentifierIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenUnaryExpressionSymbol SHiddenUnaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenUnaryExpressionInternal0Symbol SHiddenUnaryExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SHiddenVariableIdentifierSymbol SHiddenVariableIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenVariableIdentifierIdentifierSymbol SHiddenVariableIdentifierIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenVariantIdentifierSymbol SHiddenVariantIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenVariantIdentifierIdentifierSymbol SHiddenVariantIdentifierIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SHiddenWhitespaceSymbol SHiddenWhitespaceSymbol = Just (Refl, HRefl)
decSymbolSing SAbilitySymbol SAbilitySymbol = Just (Refl, HRefl)
decSymbolSing SAbilityDeclsSymbol SAbilityDeclsSymbol = Just (Refl, HRefl)
decSymbolSing SAbortExpressionSymbol SAbortExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SAddressLiteralSymbol SAddressLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SAnnotationSymbol SAnnotationSymbol = Just (Refl, HRefl)
decSymbolSing SAnnotationExprSymbol SAnnotationExprSymbol = Just (Refl, HRefl)
decSymbolSing SAnnotationExprInternal0Symbol SAnnotationExprInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SAnnotationExpressionSymbol SAnnotationExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SAnnotationItemSymbol SAnnotationItemSymbol = Just (Refl, HRefl)
decSymbolSing SAnnotationListSymbol SAnnotationListSymbol = Just (Refl, HRefl)
decSymbolSing SAnnotationListInternal0Symbol SAnnotationListInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SApplyTypeSymbol SApplyTypeSymbol = Just (Refl, HRefl)
decSymbolSing SArgListSymbol SArgListSymbol = Just (Refl, HRefl)
decSymbolSing SAssignExpressionSymbol SAssignExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SAtBindSymbol SAtBindSymbol = Just (Refl, HRefl)
decSymbolSing SBinaryExpressionSymbol SBinaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBindFieldSymbol SBindFieldSymbol = Just (Refl, HRefl)
decSymbolSing SBindFieldsSymbol SBindFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SBindListSymbol SBindListSymbol = Just (Refl, HRefl)
decSymbolSing SBindNamedFieldsSymbol SBindNamedFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SBindNamedFieldsInternal0Symbol SBindNamedFieldsInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SBindPositionalFieldsSymbol SBindPositionalFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SBindUnpackSymbol SBindUnpackSymbol = Just (Refl, HRefl)
decSymbolSing SBlockSymbol SBlockSymbol = Just (Refl, HRefl)
decSymbolSing SBlockCommentSymbol SBlockCommentSymbol = Just (Refl, HRefl)
decSymbolSing SBlockIdentifierSymbol SBlockIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SBlockItemSymbol SBlockItemSymbol = Just (Refl, HRefl)
decSymbolSing SBoolLiteralSymbol SBoolLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SBorrowExpressionSymbol SBorrowExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SBreakExpressionSymbol SBreakExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SByteStringLiteralSymbol SByteStringLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SCallExpressionSymbol SCallExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SCastExpressionSymbol SCastExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SCommaBindListSymbol SCommaBindListSymbol = Just (Refl, HRefl)
decSymbolSing SConditionPropertiesSymbol SConditionPropertiesSymbol = Just (Refl, HRefl)
decSymbolSing SConstantSymbol SConstantSymbol = Just (Refl, HRefl)
decSymbolSing SContinueExpressionSymbol SContinueExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SDatatypeFieldsSymbol SDatatypeFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SDereferenceExpressionSymbol SDereferenceExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SDotExpressionSymbol SDotExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SEnumDefinitionSymbol SEnumDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SEnumVariantsSymbol SEnumVariantsSymbol = Just (Refl, HRefl)
decSymbolSing SExpFieldSymbol SExpFieldSymbol = Just (Refl, HRefl)
decSymbolSing SExpressionListSymbol SExpressionListSymbol = Just (Refl, HRefl)
decSymbolSing SFieldAnnotationSymbol SFieldAnnotationSymbol = Just (Refl, HRefl)
decSymbolSing SFieldInitializeListSymbol SFieldInitializeListSymbol = Just (Refl, HRefl)
decSymbolSing SFriendAccessSymbol SFriendAccessSymbol = Just (Refl, HRefl)
decSymbolSing SFriendDeclarationSymbol SFriendDeclarationSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionDefinitionSymbol SFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParameterSymbol SFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParameterInternal0Symbol SFunctionParameterInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SFunctionParametersSymbol SFunctionParametersSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionParametersInternal0Symbol SFunctionParametersInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeSymbol SFunctionTypeSymbol = Just (Refl, HRefl)
decSymbolSing SFunctionTypeParametersSymbol SFunctionTypeParametersSymbol = Just (Refl, HRefl)
decSymbolSing SHexStringLiteralSymbol SHexStringLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SIdentifiedExpressionSymbol SIdentifiedExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SIdentifierSymbol SIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SIfExpressionSymbol SIfExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SImmRefSymbol SImmRefSymbol = Just (Refl, HRefl)
decSymbolSing SIndexExpressionSymbol SIndexExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SLabelSymbol SLabelSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaBindingSymbol SLambdaBindingSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaBindingsSymbol SLambdaBindingsSymbol = Just (Refl, HRefl)
decSymbolSing SLambdaExpressionSymbol SLambdaExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SLetStatementSymbol SLetStatementSymbol = Just (Refl, HRefl)
decSymbolSing SLineCommentSymbol SLineCommentSymbol = Just (Refl, HRefl)
decSymbolSing SLoopExpressionSymbol SLoopExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMacroCallExpressionSymbol SMacroCallExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMacroFunctionDefinitionSymbol SMacroFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SMacroIdentifierSymbol SMacroIdentifierSymbol = Just (Refl, HRefl)
decSymbolSing SMacroModuleAccessSymbol SMacroModuleAccessSymbol = Just (Refl, HRefl)
decSymbolSing SMatchArmSymbol SMatchArmSymbol = Just (Refl, HRefl)
decSymbolSing SMatchConditionSymbol SMatchConditionSymbol = Just (Refl, HRefl)
decSymbolSing SMatchExpressionSymbol SMatchExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SModifierSymbol SModifierSymbol = Just (Refl, HRefl)
decSymbolSing SModifierInternal0Symbol SModifierInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SModuleAccessSymbol SModuleAccessSymbol = Just (Refl, HRefl)
decSymbolSing SModuleBodySymbol SModuleBodySymbol = Just (Refl, HRefl)
decSymbolSing SModuleBodyInternal0Symbol SModuleBodyInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SModuleDefinitionSymbol SModuleDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentitySymbol SModuleIdentitySymbol = Just (Refl, HRefl)
decSymbolSing SModuleIdentityInternal0Symbol SModuleIdentityInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMoveOrCopyExpressionSymbol SMoveOrCopyExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SMoveOrCopyExpressionInternal0Symbol SMoveOrCopyExpressionInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SMutBindFieldSymbol SMutBindFieldSymbol = Just (Refl, HRefl)
decSymbolSing SMutBindVarSymbol SMutBindVarSymbol = Just (Refl, HRefl)
decSymbolSing SMutFunctionParameterSymbol SMutFunctionParameterSymbol = Just (Refl, HRefl)
decSymbolSing SMutRefSymbol SMutRefSymbol = Just (Refl, HRefl)
decSymbolSing SNameExpressionSymbol SNameExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SNamedFieldsSymbol SNamedFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SNativeFunctionDefinitionSymbol SNativeFunctionDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SNativeSpecFunctionSymbol SNativeSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SNativeStructDefinitionSymbol SNativeStructDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing SNewlineSymbol SNewlineSymbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralSymbol SNumLiteralSymbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralInternal0Symbol SNumLiteralInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SNumLiteralInternal1Symbol SNumLiteralInternal1Symbol = Just (Refl, HRefl)
decSymbolSing SOrBindListSymbol SOrBindListSymbol = Just (Refl, HRefl)
decSymbolSing SPackExpressionSymbol SPackExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SPositionalFieldsSymbol SPositionalFieldsSymbol = Just (Refl, HRefl)
decSymbolSing SPostfixAbilityDeclsSymbol SPostfixAbilityDeclsSymbol = Just (Refl, HRefl)
decSymbolSing SPrimitiveTypeSymbol SPrimitiveTypeSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierBindingSymbol SQuantifierBindingSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierBindingsSymbol SQuantifierBindingsSymbol = Just (Refl, HRefl)
decSymbolSing SQuantifierExpressionSymbol SQuantifierExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SRefTypeSymbol SRefTypeSymbol = Just (Refl, HRefl)
decSymbolSing SRetTypeSymbol SRetTypeSymbol = Just (Refl, HRefl)
decSymbolSing SReturnExpressionSymbol SReturnExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SSourceFileSymbol SSourceFileSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplySymbol SSpecApplySymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyNamePatternSymbol SSpecApplyNamePatternSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyPatternSymbol SSpecApplyPatternSymbol = Just (Refl, HRefl)
decSymbolSing SSpecApplyPatternInternal0Symbol SSpecApplyPatternInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockSymbol SSpecBlockSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockTargetFunSymbol SSpecBlockTargetFunSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockTargetSchemaSymbol SSpecBlockTargetSchemaSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBlockTargetStructSymbol SSpecBlockTargetStructSymbol = Just (Refl, HRefl)
decSymbolSing SSpecBodySymbol SSpecBodySymbol = Just (Refl, HRefl)
decSymbolSing SSpecConditionSymbol SSpecConditionSymbol = Just (Refl, HRefl)
decSymbolSing SSpecIncludeSymbol SSpecIncludeSymbol = Just (Refl, HRefl)
decSymbolSing SSpecInvariantSymbol SSpecInvariantSymbol = Just (Refl, HRefl)
decSymbolSing SSpecInvariantInternal0Symbol SSpecInvariantInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SSpecLetSymbol SSpecLetSymbol = Just (Refl, HRefl)
decSymbolSing SSpecPragmaSymbol SSpecPragmaSymbol = Just (Refl, HRefl)
decSymbolSing SSpecPropertySymbol SSpecPropertySymbol = Just (Refl, HRefl)
decSymbolSing SSpecVariableSymbol SSpecVariableSymbol = Just (Refl, HRefl)
decSymbolSing SSpecVariableInternal0Symbol SSpecVariableInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SStructDefinitionSymbol SStructDefinitionSymbol = Just (Refl, HRefl)
decSymbolSing STupleTypeSymbol STupleTypeSymbol = Just (Refl, HRefl)
decSymbolSing STypeArgumentsSymbol STypeArgumentsSymbol = Just (Refl, HRefl)
decSymbolSing STypeParameterSymbol STypeParameterSymbol = Just (Refl, HRefl)
decSymbolSing STypeParametersSymbol STypeParametersSymbol = Just (Refl, HRefl)
decSymbolSing SUnaryExpressionSymbol SUnaryExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SUnaryOpSymbol SUnaryOpSymbol = Just (Refl, HRefl)
decSymbolSing SUninterpretedSpecFunctionSymbol SUninterpretedSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SUnitExpressionSymbol SUnitExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SUseDeclarationSymbol SUseDeclarationSymbol = Just (Refl, HRefl)
decSymbolSing SUseDeclarationInternal0Symbol SUseDeclarationInternal0Symbol = Just (Refl, HRefl)
decSymbolSing SUseFunSymbol SUseFunSymbol = Just (Refl, HRefl)
decSymbolSing SUseMemberSymbol SUseMemberSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleSymbol SUseModuleSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleMemberSymbol SUseModuleMemberSymbol = Just (Refl, HRefl)
decSymbolSing SUseModuleMembersSymbol SUseModuleMembersSymbol = Just (Refl, HRefl)
decSymbolSing SUsualSpecFunctionSymbol SUsualSpecFunctionSymbol = Just (Refl, HRefl)
decSymbolSing SVariantSymbol SVariantSymbol = Just (Refl, HRefl)
decSymbolSing SVectorExpressionSymbol SVectorExpressionSymbol = Just (Refl, HRefl)
decSymbolSing SWhileExpressionSymbol SWhileExpressionSymbol = Just (Refl, HRefl)
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
    [ ("_bind", SomeRegularSymbolSing SHiddenBindSymbol)
    , ("_bind_internal0", SomeRegularSymbolSing SHiddenBindInternal0Symbol)
    , ("_bind_var__variable_identifier", SomeRegularSymbolSing SHiddenBindVarVariableIdentifierSymbol)
    , ("_constant_identifier_identifier", SomeRegularSymbolSing SHiddenConstantIdentifierIdentifierSymbol)
    , ("_enum_identifier", SomeRegularSymbolSing SHiddenEnumIdentifierSymbol)
    , ("_enum_identifier_identifier", SomeRegularSymbolSing SHiddenEnumIdentifierIdentifierSymbol)
    , ("_enum_item", SomeRegularSymbolSing SHiddenEnumItemSymbol)
    , ("_enum_signature", SomeRegularSymbolSing SHiddenEnumSignatureSymbol)
    , ("_exists", SomeRegularSymbolSing SHiddenExistsSymbol)
    , ("_expression", SomeRegularSymbolSing SHiddenExpressionSymbol)
    , ("_expression_term", SomeRegularSymbolSing SHiddenExpressionTermSymbol)
    , ("_field_identifier", SomeRegularSymbolSing SHiddenFieldIdentifierSymbol)
    , ("_field_identifier_identifier", SomeRegularSymbolSing SHiddenFieldIdentifierIdentifierSymbol)
    , ("_forall", SomeRegularSymbolSing SHiddenForallSymbol)
    , ("_function_identifier", SomeRegularSymbolSing SHiddenFunctionIdentifierSymbol)
    , ("_function_identifier_identifier", SomeRegularSymbolSing SHiddenFunctionIdentifierIdentifierSymbol)
    , ("_function_item", SomeRegularSymbolSing SHiddenFunctionItemSymbol)
    , ("_function_signature", SomeRegularSymbolSing SHiddenFunctionSignatureSymbol)
    , ("_identifier__reserved_identifier", SomeRegularSymbolSing SHiddenIdentifierReservedIdentifierSymbol)
    , ("_invariant_modifier", SomeRegularSymbolSing SHiddenInvariantModifierSymbol)
    , ("_literal_value", SomeRegularSymbolSing SHiddenLiteralValueSymbol)
    , ("_macro_signature", SomeRegularSymbolSing SHiddenMacroSignatureSymbol)
    , ("_match_body", SomeRegularSymbolSing SHiddenMatchBodySymbol)
    , ("_module_identifier", SomeRegularSymbolSing SHiddenModuleIdentifierSymbol)
    , ("_module_identifier_identifier", SomeRegularSymbolSing SHiddenModuleIdentifierIdentifierSymbol)
    , ("_reference", SomeRegularSymbolSing SHiddenReferenceSymbol)
    , ("_reserved_identifier", SomeRegularSymbolSing SHiddenReservedIdentifierSymbol)
    , ("_spec_abort_if", SomeRegularSymbolSing SHiddenSpecAbortIfSymbol)
    , ("_spec_abort_with_or_modifies", SomeRegularSymbolSing SHiddenSpecAbortWithOrModifiesSymbol)
    , ("_spec_abort_with_or_modifies_internal0", SomeRegularSymbolSing SHiddenSpecAbortWithOrModifiesInternal0Symbol)
    , ("_spec_block_memeber", SomeRegularSymbolSing SHiddenSpecBlockMemeberSymbol)
    , ("_spec_block_target", SomeRegularSymbolSing SHiddenSpecBlockTargetSymbol)
    , ("_spec_block_target_module_module", SomeRegularSymbolSing SHiddenSpecBlockTargetModuleModuleSymbol)
    , ("_spec_condition", SomeRegularSymbolSing SHiddenSpecConditionSymbol)
    , ("_spec_condition_internal0", SomeRegularSymbolSing SHiddenSpecConditionInternal0Symbol)
    , ("_spec_condition_kind", SomeRegularSymbolSing SHiddenSpecConditionKindSymbol)
    , ("_spec_function", SomeRegularSymbolSing SHiddenSpecFunctionSymbol)
    , ("_spec_function_signature", SomeRegularSymbolSing SHiddenSpecFunctionSignatureSymbol)
    , ("_spread_operator", SomeRegularSymbolSing SHiddenSpreadOperatorSymbol)
    , ("_struct_identifier", SomeRegularSymbolSing SHiddenStructIdentifierSymbol)
    , ("_struct_identifier_identifier", SomeRegularSymbolSing SHiddenStructIdentifierIdentifierSymbol)
    , ("_struct_item", SomeRegularSymbolSing SHiddenStructItemSymbol)
    , ("_struct_signature", SomeRegularSymbolSing SHiddenStructSignatureSymbol)
    , ("_type", SomeRegularSymbolSing SHiddenTypeSymbol)
    , ("_type_identifier", SomeRegularSymbolSing SHiddenTypeIdentifierSymbol)
    , ("_type_identifier_identifier", SomeRegularSymbolSing SHiddenTypeIdentifierIdentifierSymbol)
    , ("_type_parameter_identifier", SomeRegularSymbolSing SHiddenTypeParameterIdentifierSymbol)
    , ("_type_parameter_identifier_identifier", SomeRegularSymbolSing SHiddenTypeParameterIdentifierIdentifierSymbol)
    , ("_unary_expression", SomeRegularSymbolSing SHiddenUnaryExpressionSymbol)
    , ("_unary_expression_internal0", SomeRegularSymbolSing SHiddenUnaryExpressionInternal0Symbol)
    , ("_variable_identifier", SomeRegularSymbolSing SHiddenVariableIdentifierSymbol)
    , ("_variable_identifier_identifier", SomeRegularSymbolSing SHiddenVariableIdentifierIdentifierSymbol)
    , ("_variant_identifier", SomeRegularSymbolSing SHiddenVariantIdentifierSymbol)
    , ("_variant_identifier_identifier", SomeRegularSymbolSing SHiddenVariantIdentifierIdentifierSymbol)
    , ("_whitespace", SomeRegularSymbolSing SHiddenWhitespaceSymbol)
    , ("ability", SomeRegularSymbolSing SAbilitySymbol)
    , ("ability_decls", SomeRegularSymbolSing SAbilityDeclsSymbol)
    , ("abort_expression", SomeRegularSymbolSing SAbortExpressionSymbol)
    , ("address_literal", SomeRegularSymbolSing SAddressLiteralSymbol)
    , ("annotation", SomeRegularSymbolSing SAnnotationSymbol)
    , ("annotation_expr", SomeRegularSymbolSing SAnnotationExprSymbol)
    , ("annotation_expr_internal0", SomeRegularSymbolSing SAnnotationExprInternal0Symbol)
    , ("annotation_expression", SomeRegularSymbolSing SAnnotationExpressionSymbol)
    , ("annotation_item", SomeRegularSymbolSing SAnnotationItemSymbol)
    , ("annotation_list", SomeRegularSymbolSing SAnnotationListSymbol)
    , ("annotation_list_internal0", SomeRegularSymbolSing SAnnotationListInternal0Symbol)
    , ("apply_type", SomeRegularSymbolSing SApplyTypeSymbol)
    , ("arg_list", SomeRegularSymbolSing SArgListSymbol)
    , ("assign_expression", SomeRegularSymbolSing SAssignExpressionSymbol)
    , ("at_bind", SomeRegularSymbolSing SAtBindSymbol)
    , ("binary_expression", SomeRegularSymbolSing SBinaryExpressionSymbol)
    , ("bind_field", SomeRegularSymbolSing SBindFieldSymbol)
    , ("bind_fields", SomeRegularSymbolSing SBindFieldsSymbol)
    , ("bind_list", SomeRegularSymbolSing SBindListSymbol)
    , ("bind_named_fields", SomeRegularSymbolSing SBindNamedFieldsSymbol)
    , ("bind_named_fields_internal0", SomeRegularSymbolSing SBindNamedFieldsInternal0Symbol)
    , ("bind_positional_fields", SomeRegularSymbolSing SBindPositionalFieldsSymbol)
    , ("bind_unpack", SomeRegularSymbolSing SBindUnpackSymbol)
    , ("block", SomeRegularSymbolSing SBlockSymbol)
    , ("block_comment", SomeRegularSymbolSing SBlockCommentSymbol)
    , ("block_identifier", SomeRegularSymbolSing SBlockIdentifierSymbol)
    , ("block_item", SomeRegularSymbolSing SBlockItemSymbol)
    , ("bool_literal", SomeRegularSymbolSing SBoolLiteralSymbol)
    , ("borrow_expression", SomeRegularSymbolSing SBorrowExpressionSymbol)
    , ("break_expression", SomeRegularSymbolSing SBreakExpressionSymbol)
    , ("byte_string_literal", SomeRegularSymbolSing SByteStringLiteralSymbol)
    , ("call_expression", SomeRegularSymbolSing SCallExpressionSymbol)
    , ("cast_expression", SomeRegularSymbolSing SCastExpressionSymbol)
    , ("comma_bind_list", SomeRegularSymbolSing SCommaBindListSymbol)
    , ("condition_properties", SomeRegularSymbolSing SConditionPropertiesSymbol)
    , ("constant", SomeRegularSymbolSing SConstantSymbol)
    , ("continue_expression", SomeRegularSymbolSing SContinueExpressionSymbol)
    , ("datatype_fields", SomeRegularSymbolSing SDatatypeFieldsSymbol)
    , ("dereference_expression", SomeRegularSymbolSing SDereferenceExpressionSymbol)
    , ("dot_expression", SomeRegularSymbolSing SDotExpressionSymbol)
    , ("enum_definition", SomeRegularSymbolSing SEnumDefinitionSymbol)
    , ("enum_variants", SomeRegularSymbolSing SEnumVariantsSymbol)
    , ("exp_field", SomeRegularSymbolSing SExpFieldSymbol)
    , ("expression_list", SomeRegularSymbolSing SExpressionListSymbol)
    , ("field_annotation", SomeRegularSymbolSing SFieldAnnotationSymbol)
    , ("field_initialize_list", SomeRegularSymbolSing SFieldInitializeListSymbol)
    , ("friend_access", SomeRegularSymbolSing SFriendAccessSymbol)
    , ("friend_declaration", SomeRegularSymbolSing SFriendDeclarationSymbol)
    , ("function_definition", SomeRegularSymbolSing SFunctionDefinitionSymbol)
    , ("function_parameter", SomeRegularSymbolSing SFunctionParameterSymbol)
    , ("function_parameter_internal0", SomeRegularSymbolSing SFunctionParameterInternal0Symbol)
    , ("function_parameters", SomeRegularSymbolSing SFunctionParametersSymbol)
    , ("function_parameters_internal0", SomeRegularSymbolSing SFunctionParametersInternal0Symbol)
    , ("function_type", SomeRegularSymbolSing SFunctionTypeSymbol)
    , ("function_type_parameters", SomeRegularSymbolSing SFunctionTypeParametersSymbol)
    , ("hex_string_literal", SomeRegularSymbolSing SHexStringLiteralSymbol)
    , ("identified_expression", SomeRegularSymbolSing SIdentifiedExpressionSymbol)
    , ("identifier", SomeRegularSymbolSing SIdentifierSymbol)
    , ("if_expression", SomeRegularSymbolSing SIfExpressionSymbol)
    , ("imm_ref", SomeRegularSymbolSing SImmRefSymbol)
    , ("index_expression", SomeRegularSymbolSing SIndexExpressionSymbol)
    , ("label", SomeRegularSymbolSing SLabelSymbol)
    , ("lambda_binding", SomeRegularSymbolSing SLambdaBindingSymbol)
    , ("lambda_bindings", SomeRegularSymbolSing SLambdaBindingsSymbol)
    , ("lambda_expression", SomeRegularSymbolSing SLambdaExpressionSymbol)
    , ("let_statement", SomeRegularSymbolSing SLetStatementSymbol)
    , ("line_comment", SomeRegularSymbolSing SLineCommentSymbol)
    , ("loop_expression", SomeRegularSymbolSing SLoopExpressionSymbol)
    , ("macro_call_expression", SomeRegularSymbolSing SMacroCallExpressionSymbol)
    , ("macro_function_definition", SomeRegularSymbolSing SMacroFunctionDefinitionSymbol)
    , ("macro_identifier", SomeRegularSymbolSing SMacroIdentifierSymbol)
    , ("macro_module_access", SomeRegularSymbolSing SMacroModuleAccessSymbol)
    , ("match_arm", SomeRegularSymbolSing SMatchArmSymbol)
    , ("match_condition", SomeRegularSymbolSing SMatchConditionSymbol)
    , ("match_expression", SomeRegularSymbolSing SMatchExpressionSymbol)
    , ("modifier", SomeRegularSymbolSing SModifierSymbol)
    , ("modifier_internal0", SomeRegularSymbolSing SModifierInternal0Symbol)
    , ("module_access", SomeRegularSymbolSing SModuleAccessSymbol)
    , ("module_body", SomeRegularSymbolSing SModuleBodySymbol)
    , ("module_body_internal0", SomeRegularSymbolSing SModuleBodyInternal0Symbol)
    , ("module_definition", SomeRegularSymbolSing SModuleDefinitionSymbol)
    , ("module_identity", SomeRegularSymbolSing SModuleIdentitySymbol)
    , ("module_identity_internal0", SomeRegularSymbolSing SModuleIdentityInternal0Symbol)
    , ("move_or_copy_expression", SomeRegularSymbolSing SMoveOrCopyExpressionSymbol)
    , ("move_or_copy_expression_internal0", SomeRegularSymbolSing SMoveOrCopyExpressionInternal0Symbol)
    , ("mut_bind_field", SomeRegularSymbolSing SMutBindFieldSymbol)
    , ("mut_bind_var", SomeRegularSymbolSing SMutBindVarSymbol)
    , ("mut_function_parameter", SomeRegularSymbolSing SMutFunctionParameterSymbol)
    , ("mut_ref", SomeRegularSymbolSing SMutRefSymbol)
    , ("name_expression", SomeRegularSymbolSing SNameExpressionSymbol)
    , ("named_fields", SomeRegularSymbolSing SNamedFieldsSymbol)
    , ("native_function_definition", SomeRegularSymbolSing SNativeFunctionDefinitionSymbol)
    , ("native_spec_function", SomeRegularSymbolSing SNativeSpecFunctionSymbol)
    , ("native_struct_definition", SomeRegularSymbolSing SNativeStructDefinitionSymbol)
    , ("newline", SomeRegularSymbolSing SNewlineSymbol)
    , ("num_literal", SomeRegularSymbolSing SNumLiteralSymbol)
    , ("num_literal_internal0", SomeRegularSymbolSing SNumLiteralInternal0Symbol)
    , ("num_literal_internal1", SomeRegularSymbolSing SNumLiteralInternal1Symbol)
    , ("or_bind_list", SomeRegularSymbolSing SOrBindListSymbol)
    , ("pack_expression", SomeRegularSymbolSing SPackExpressionSymbol)
    , ("positional_fields", SomeRegularSymbolSing SPositionalFieldsSymbol)
    , ("postfix_ability_decls", SomeRegularSymbolSing SPostfixAbilityDeclsSymbol)
    , ("primitive_type", SomeRegularSymbolSing SPrimitiveTypeSymbol)
    , ("quantifier_binding", SomeRegularSymbolSing SQuantifierBindingSymbol)
    , ("quantifier_bindings", SomeRegularSymbolSing SQuantifierBindingsSymbol)
    , ("quantifier_expression", SomeRegularSymbolSing SQuantifierExpressionSymbol)
    , ("ref_type", SomeRegularSymbolSing SRefTypeSymbol)
    , ("ret_type", SomeRegularSymbolSing SRetTypeSymbol)
    , ("return_expression", SomeRegularSymbolSing SReturnExpressionSymbol)
    , ("source_file", SomeRegularSymbolSing SSourceFileSymbol)
    , ("spec_apply", SomeRegularSymbolSing SSpecApplySymbol)
    , ("spec_apply_name_pattern", SomeRegularSymbolSing SSpecApplyNamePatternSymbol)
    , ("spec_apply_pattern", SomeRegularSymbolSing SSpecApplyPatternSymbol)
    , ("spec_apply_pattern_internal0", SomeRegularSymbolSing SSpecApplyPatternInternal0Symbol)
    , ("spec_block", SomeRegularSymbolSing SSpecBlockSymbol)
    , ("spec_block_target_fun", SomeRegularSymbolSing SSpecBlockTargetFunSymbol)
    , ("spec_block_target_schema", SomeRegularSymbolSing SSpecBlockTargetSchemaSymbol)
    , ("spec_block_target_struct", SomeRegularSymbolSing SSpecBlockTargetStructSymbol)
    , ("spec_body", SomeRegularSymbolSing SSpecBodySymbol)
    , ("spec_condition", SomeRegularSymbolSing SSpecConditionSymbol)
    , ("spec_include", SomeRegularSymbolSing SSpecIncludeSymbol)
    , ("spec_invariant", SomeRegularSymbolSing SSpecInvariantSymbol)
    , ("spec_invariant_internal0", SomeRegularSymbolSing SSpecInvariantInternal0Symbol)
    , ("spec_let", SomeRegularSymbolSing SSpecLetSymbol)
    , ("spec_pragma", SomeRegularSymbolSing SSpecPragmaSymbol)
    , ("spec_property", SomeRegularSymbolSing SSpecPropertySymbol)
    , ("spec_variable", SomeRegularSymbolSing SSpecVariableSymbol)
    , ("spec_variable_internal0", SomeRegularSymbolSing SSpecVariableInternal0Symbol)
    , ("struct_definition", SomeRegularSymbolSing SStructDefinitionSymbol)
    , ("tuple_type", SomeRegularSymbolSing STupleTypeSymbol)
    , ("type_arguments", SomeRegularSymbolSing STypeArgumentsSymbol)
    , ("type_parameter", SomeRegularSymbolSing STypeParameterSymbol)
    , ("type_parameters", SomeRegularSymbolSing STypeParametersSymbol)
    , ("unary_expression", SomeRegularSymbolSing SUnaryExpressionSymbol)
    , ("unary_op", SomeRegularSymbolSing SUnaryOpSymbol)
    , ("uninterpreted_spec_function", SomeRegularSymbolSing SUninterpretedSpecFunctionSymbol)
    , ("unit_expression", SomeRegularSymbolSing SUnitExpressionSymbol)
    , ("use_declaration", SomeRegularSymbolSing SUseDeclarationSymbol)
    , ("use_declaration_internal0", SomeRegularSymbolSing SUseDeclarationInternal0Symbol)
    , ("use_fun", SomeRegularSymbolSing SUseFunSymbol)
    , ("use_member", SomeRegularSymbolSing SUseMemberSymbol)
    , ("use_module", SomeRegularSymbolSing SUseModuleSymbol)
    , ("use_module_member", SomeRegularSymbolSing SUseModuleMemberSymbol)
    , ("use_module_members", SomeRegularSymbolSing SUseModuleMembersSymbol)
    , ("usual_spec_function", SomeRegularSymbolSing SUsualSpecFunctionSymbol)
    , ("variant", SomeRegularSymbolSing SVariantSymbol)
    , ("vector_expression", SomeRegularSymbolSing SVectorExpressionSymbol)
    , ("while_expression", SomeRegularSymbolSing SWhileExpressionSymbol)
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
