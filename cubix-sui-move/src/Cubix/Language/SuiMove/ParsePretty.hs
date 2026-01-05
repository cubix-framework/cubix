{-# LANGUAGE OverloadedStrings #-}
module Cubix.Language.SuiMove.ParsePretty where

import Control.Applicative.Combinators
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString (..))
import Data.Text
import Data.Type.Equality (type (:~:) (..), type (:~~:) (..))
import Data.Typeable (Typeable)
import Streaming.Prelude qualified as Streaming

import TreeSitter qualified as TS
import TreeSitter.SuiMove (tree_sitter_sui_move)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.TreeSitter qualified as Megaparsec.TreeSitter
import Text.Megaparsec.Cubix qualified as Megaparsec.Cubix

import Cubix.ParsePretty
import Cubix.Language.SuiMove.Modularized
import Cubix.TreeSitter

parse' :: ReaderT (TreeSitterEnv SomeSymbolSing) IO (Maybe (MoveTerm (RootSort MoveSig)))
parse' = do
  filepath <- getFilePath
  rootNode <- liftIO . TS.treeRootNode =<< getTree
  toks <- Streaming.toList_
    $ Streaming.mapMaybeM
        (\tok ->
           fmap (\sym -> tok { tokenValue = sym }) <$> getSymbol (tokenValue tok))
    $ symbols filepath rootNode

  source <- getSource
  let lexed = Megaparsec.TreeSitter.Lexed source toks
  case Megaparsec.parse pRoot filepath lexed of
    Right ast -> pure $ Just ast
    Left err -> do
      liftIO . putStrLn $ Megaparsec.errorBundlePretty err
      pure Nothing

parse :: FilePath -> IO (Maybe (MoveTerm (RootSort MoveSig)))
parse path =
  runReaderT parse' =<<
    newTreeSitterEnv path tree_sitter_sui_move (fmap unSymbolTable . mkSymbolTable)

-- --------------------------------------------------------------------------------
-- -- Parser 
-- --------------------------------------------------------------------------------
type Parser = Megaparsec.TreeSitter.Parser SomeSymbolSing

instance IsString (NonEmpty Char) where
  fromString [] = error "NonEmpty.fromString: empty string"
  fromString (s:ss) = s :| ss

pSymbol
  :: forall (symbolType :: SymbolType) (symbol :: Symbol symbolType)
   . NonEmpty Char
  -> SymbolSing symbolType symbol
  -> Parser (Cubix.TreeSitter.Token (SymbolSing symbolType symbol))
pSymbol expected sym = Megaparsec.TreeSitter.pToken expected $ \case
  SomeSymbolSing _isReal symSing -> case decSymbolSing sym symSing of
    Just (Refl, HRefl) -> Just symSing
    Nothing -> Nothing

pContent :: Cubix.TreeSitter.Token a -> Parser Text
pContent = Megaparsec.TreeSitter.pContent

-- reify the types to aid inference, that might get broken
pMaybe :: Typeable a => Parser (MoveTerm a) -> Parser (MoveTerm (Maybe a))
pMaybe = Megaparsec.Cubix.pMaybe

pPair :: (Typeable a, Typeable b) => Parser (MoveTerm a) -> Parser (MoveTerm b) -> Parser (MoveTerm (a , b))
pPair = Megaparsec.Cubix.pPair

pSome :: Typeable a => Parser (MoveTerm a) -> Parser (MoveTerm [a])
pSome = Megaparsec.Cubix.pSome

pMany :: Typeable a => Parser (MoveTerm a) -> Parser (MoveTerm [a])
pMany = Megaparsec.Cubix.pMany

pRoot :: Parser (MoveTerm (RootSort MoveSig))
pRoot = pSourceFile

pBang :: Parser (MoveTerm BangTokL)
pBang = pSymbol "bang" SBangTokSymbol $> iBang

pNeq :: Parser (MoveTerm NeqTokL)
pNeq = pSymbol "neq" SNeqTokSymbol $> iNeq

pDollar :: Parser (MoveTerm DollarTokL)
pDollar = pSymbol "dollar" SDollarTokSymbol $> iDollar

pMod :: Parser (MoveTerm ModTokL)
pMod = pSymbol "mod" SModTokSymbol $> iMod

pBitand :: Parser (MoveTerm BitandTokL)
pBitand = pSymbol "bitand" SBitandTokSymbol $> iBitand

pAnd :: Parser (MoveTerm AndTokL)
pAnd = pSymbol "and" SAndTokSymbol $> iAnd

pMul :: Parser (MoveTerm MulTokL)
pMul = pSymbol "mul" SMulTokSymbol $> iMul

pAdd :: Parser (MoveTerm AddTokL)
pAdd = pSymbol "add" SAddTokSymbol $> iAdd

pComma :: Parser (MoveTerm CommaTokL)
pComma = pSymbol "comma" SCommaTokSymbol $> iComma

pSub :: Parser (MoveTerm SubTokL)
pSub = pSymbol "sub" SSubTokSymbol $> iSub

pRange :: Parser (MoveTerm RangeTokL)
pRange = pSymbol "range" SRangeTokSymbol $> iRange

pDiv :: Parser (MoveTerm DivTokL)
pDiv = pSymbol "div" SDivTokSymbol $> iDiv

pAccess :: Parser (MoveTerm AccessTokL)
pAccess = pSymbol "access" SAccessTokSymbol $> iAccess

pLt :: Parser (MoveTerm LtTokL)
pLt = pSymbol "lt" SLtTokSymbol $> iLt

pShl :: Parser (MoveTerm ShlTokL)
pShl = pSymbol "shl" SShlTokSymbol $> iShl

pLe :: Parser (MoveTerm LeTokL)
pLe = pSymbol "le" SLeTokSymbol $> iLe

pAssign :: Parser (MoveTerm AssignTokL)
pAssign = pSymbol "assign" SAssignTokSymbol $> iAssign

pEq :: Parser (MoveTerm EqTokL)
pEq = pSymbol "eq" SEqTokSymbol $> iEq

pImplies :: Parser (MoveTerm ImpliesTokL)
pImplies = pSymbol "implies" SImpliesTokSymbol $> iImplies

pGt :: Parser (MoveTerm GtTokL)
pGt = pSymbol "gt" SGtTokSymbol $> iGt

pGe :: Parser (MoveTerm GeTokL)
pGe = pSymbol "ge" SGeTokSymbol $> iGe

pShr :: Parser (MoveTerm ShrTokL)
pShr = pSymbol "shr" SShrTokSymbol $> iShr

pAt :: Parser (MoveTerm AtTokL)
pAt = pSymbol "at" SAtTokSymbol $> iAt

pXor :: Parser (MoveTerm XorTokL)
pXor = pSymbol "xor" SXorTokSymbol $> iXor

pAbortsIf :: Parser (MoveTerm AbortsIfTokL)
pAbortsIf = pSymbol "aborts_if" SAbortsIfTokSymbol $> iAbortsIf

pAbortsWith :: Parser (MoveTerm AbortsWithTokL)
pAbortsWith = pSymbol "aborts_with" SAbortsWithTokSymbol $> iAbortsWith

pAddress :: Parser (MoveTerm AddressTokL)
pAddress = pSymbol "address" SAddressTokSymbol $> iAddress

pAssert :: Parser (MoveTerm AssertTokL)
pAssert = pSymbol "assert" SAssertTokSymbol $> iAssert

pAssume :: Parser (MoveTerm AssumeTokL)
pAssume = pSymbol "assume" SAssumeTokSymbol $> iAssume

pBool :: Parser (MoveTerm BoolTokL)
pBool = pSymbol "bool" SBoolTokSymbol $> iBool

pBytearray :: Parser (MoveTerm BytearrayTokL)
pBytearray = pSymbol "bytearray" SBytearrayTokSymbol $> iBytearray

pCopy :: Parser (MoveTerm CopyTokL)
pCopy = pSymbol "copy" SCopyTokSymbol $> iCopy

pDecreases :: Parser (MoveTerm DecreasesTokL)
pDecreases = pSymbol "decreases" SDecreasesTokSymbol $> iDecreases

pDrop :: Parser (MoveTerm DropTokL)
pDrop = pSymbol "drop" SDropTokSymbol $> iDrop

pEnsures :: Parser (MoveTerm EnsuresTokL)
pEnsures = pSymbol "ensures" SEnsuresTokSymbol $> iEnsures

pEntry :: Parser (MoveTerm EntryTokL)
pEntry = pSymbol "entry" SEntryTokSymbol $> iEntry

pExists :: Parser (MoveTerm ExistsTokL)
pExists = pSymbol "exists" SExistsTokSymbol $> iExists

pFalse :: Parser (MoveTerm FalseTokL)
pFalse = pSymbol "false" SFalseTokSymbol $> iFalse

pForall :: Parser (MoveTerm ForallTokL)
pForall = pSymbol "forall" SForallTokSymbol $> iForall

pFriend :: Parser (MoveTerm FriendTokL)
pFriend = pSymbol "friend" SFriendTokSymbol $> iFriend

pGlobal :: Parser (MoveTerm GlobalTokL)
pGlobal = pSymbol "global" SGlobalTokSymbol $> iGlobal

pInternal :: Parser (MoveTerm InternalTokL)
pInternal = pSymbol "internal" SInternalTokSymbol $> iInternal

pInvariant :: Parser (MoveTerm InvariantTokL)
pInvariant = pSymbol "invariant" SInvariantTokSymbol $> iInvariant

pKey :: Parser (MoveTerm KeyTokL)
pKey = pSymbol "key" SKeyTokSymbol $> iKey

pLocal :: Parser (MoveTerm LocalTokL)
pLocal = pSymbol "local" SLocalTokSymbol $> iLocal

pModifies :: Parser (MoveTerm ModifiesTokL)
pModifies = pSymbol "modifies" SModifiesTokSymbol $> iModifies

pModule :: Parser (MoveTerm ModuleTokL)
pModule = pSymbol "module" SModuleTokSymbol $> iModule

pMove :: Parser (MoveTerm MoveTokL)
pMove = pSymbol "move" SMoveTokSymbol $> iMove

pNative :: Parser (MoveTerm NativeTokL)
pNative = pSymbol "native" SNativeTokSymbol $> iNative

pPack :: Parser (MoveTerm PackTokL)
pPack = pSymbol "pack" SPackTokSymbol $> iPack

pPackage :: Parser (MoveTerm PackageTokL)
pPackage = pSymbol "package" SPackageTokSymbol $> iPackage

pPhantom :: Parser (MoveTerm PhantomTokL)
pPhantom = pSymbol "phantom" SPhantomTokSymbol $> iPhantom

pPost :: Parser (MoveTerm PostTokL)
pPost = pSymbol "post" SPostTokSymbol $> iPost

pPublic :: Parser (MoveTerm PublicTokL)
pPublic = pSymbol "public" SPublicTokSymbol $> iPublic

pRequires :: Parser (MoveTerm RequiresTokL)
pRequires = pSymbol "requires" SRequiresTokSymbol $> iRequires

pSigner :: Parser (MoveTerm SignerTokL)
pSigner = pSymbol "signer" SSignerTokSymbol $> iSigner

pStore :: Parser (MoveTerm StoreTokL)
pStore = pSymbol "store" SStoreTokSymbol $> iStore

pSucceedsIf :: Parser (MoveTerm SucceedsIfTokL)
pSucceedsIf = pSymbol "succeeds_if" SSucceedsIfTokSymbol $> iSucceedsIf

pTrue :: Parser (MoveTerm TrueTokL)
pTrue = pSymbol "true" STrueTokSymbol $> iTrue

pU128 :: Parser (MoveTerm U128TokL)
pU128 = pSymbol "u128" SU128TokSymbol $> iU128

pU16 :: Parser (MoveTerm U16TokL)
pU16 = pSymbol "u16" SU16TokSymbol $> iU16

pU256 :: Parser (MoveTerm U256TokL)
pU256 = pSymbol "u256" SU256TokSymbol $> iU256

pU32 :: Parser (MoveTerm U32TokL)
pU32 = pSymbol "u32" SU32TokSymbol $> iU32

pU64 :: Parser (MoveTerm U64TokL)
pU64 = pSymbol "u64" SU64TokSymbol $> iU64

pU8 :: Parser (MoveTerm U8TokL)
pU8 = pSymbol "u8" SU8TokSymbol $> iU8

pUnpack :: Parser (MoveTerm UnpackTokL)
pUnpack = pSymbol "unpack" SUnpackTokSymbol $> iUnpack

pUpdate :: Parser (MoveTerm UpdateTokL)
pUpdate = pSymbol "update" SUpdateTokSymbol $> iUpdate

pBitor :: Parser (MoveTerm BitorTokL)
pBitor = pSymbol "bitor" SBitorTokSymbol $> iBitor

pOr :: Parser (MoveTerm OrTokL)
pOr = pSymbol "or" SOrTokSymbol $> iOr

pSourceFile :: Parser (MoveTerm SourceFileL)
pSourceFile = do
  _sym <- pSymbol "source_file" SSourceFileSymbol
  iSourceFile
    <$> pMany pModuleDefinition

pModuleDefinition :: Parser (MoveTerm ModuleDefinitionL)
pModuleDefinition = do
  _sym <- pSymbol "module_definition" SModuleDefinitionSymbol
  iModuleDefinition
    <$> pModule
    <*> pModuleIdentity
    <*> pModuleBody

pModuleBody :: Parser (MoveTerm ModuleBodyL)
pModuleBody = do
  _sym <- pSymbol "module_body" SModuleBodySymbol
  iModuleBody
    <$> (pModuleBodyInternal0 _sym)
    <*> pMany (pModuleBodyInternal1 _sym)
    <*> pMaybe pRightCurlyBracket

pModuleBodyInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal0L)
pModuleBodyInternal0 _sym = do
  choice [ Megaparsec.try (pModuleBodyInternal0Semicolon _sym)
         , Megaparsec.try (pModuleBodyInternal0LeftCurlyBracket _sym)
         ]
  where
    pModuleBodyInternal0Semicolon :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal0L)
    pModuleBodyInternal0Semicolon _sym =
      iModuleBodyInternal0Semicolon
        <$> pSemicolon
    pModuleBodyInternal0LeftCurlyBracket :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal0L)
    pModuleBodyInternal0LeftCurlyBracket _sym =
      iModuleBodyInternal0LeftCurlyBracket
        <$> pLeftCurlyBracket

pModuleBodyInternal1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
pModuleBodyInternal1 _sym = do
  choice [ Megaparsec.try (pModuleBodyInternal1UseDeclaration _sym)
         , Megaparsec.try (pModuleBodyInternal1FriendDeclaration _sym)
         , Megaparsec.try (pModuleBodyInternal1Constant _sym)
         , Megaparsec.try (pModuleBodyInternal1FunctionItem _sym)
         , Megaparsec.try (pModuleBodyInternal1StructItem _sym)
         , Megaparsec.try (pModuleBodyInternal1EnumItem _sym)
         , Megaparsec.try (pModuleBodyInternal1SpecBlock _sym)
         ]
  where
    pModuleBodyInternal1UseDeclaration :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1UseDeclaration _sym =
      iModuleBodyInternal1UseDeclaration
        <$> pUseDeclaration
    pModuleBodyInternal1FriendDeclaration :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1FriendDeclaration _sym =
      iModuleBodyInternal1FriendDeclaration
        <$> pFriendDeclaration
    pModuleBodyInternal1Constant :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1Constant _sym =
      iModuleBodyInternal1Constant
        <$> pConstant
    pModuleBodyInternal1FunctionItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1FunctionItem _sym =
      iModuleBodyInternal1FunctionItem
        <$> (pHiddenFunctionItem _sym)
    pModuleBodyInternal1StructItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1StructItem _sym =
      iModuleBodyInternal1StructItem
        <$> (pHiddenStructItem _sym)
    pModuleBodyInternal1EnumItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1EnumItem _sym =
      iModuleBodyInternal1EnumItem
        <$> (pHiddenEnumItem _sym)
    pModuleBodyInternal1SpecBlock :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1SpecBlock _sym =
      iModuleBodyInternal1SpecBlock
        <$> pSpecBlock

pHiddenEnumItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenEnumItemL)
pHiddenEnumItem _sym = do
  iHiddenEnumItem
    <$> pEnumDefinition

pEnumDefinition :: Parser (MoveTerm EnumDefinitionL)
pEnumDefinition = do
  _sym <- pSymbol "enum_definition" SEnumDefinitionSymbol
  iEnumDefinition
    <$> pMaybe pPublic
    <*> (pHiddenEnumSignature _sym)
    <*> pEnumVariants
    <*> pMaybe pPostfixAbilityDecls

pHiddenEnumSignature :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenEnumSignatureL)
pHiddenEnumSignature _sym = do
  iHiddenEnumSignature
    <$> pEnum
    <*> (pHiddenEnumIdentifier _sym)
    <*> pMaybe pTypeParameters
    <*> pMaybe pAbilityDecls

pHiddenEnumIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenEnumIdentifierL)
pHiddenEnumIdentifier _sym = do
  iHiddenEnumIdentifier
    <$> pIdentifier

pIdentifier :: Parser (MoveTerm IdentifierL)
pIdentifier = do
  _sym <- pSymbol "identifier" SIdentifierSymbol
  iIdentifier
    <$> pContent _sym

pAbilityDecls :: Parser (MoveTerm AbilityDeclsL)
pAbilityDecls = do
  _sym <- pSymbol "ability_decls" SAbilityDeclsSymbol
  iAbilityDecls
    <$> pHas
    <*> pMany pAbility

pAbility :: Parser (MoveTerm AbilityL)
pAbility = do
  _sym <- pSymbol "ability" SAbilitySymbol
  choice [ Megaparsec.try (pAbilityCopy _sym)
         , Megaparsec.try (pAbilityDrop _sym)
         , Megaparsec.try (pAbilityStore _sym)
         , Megaparsec.try (pAbilityKey _sym)
         ]
  where
    pAbilityCopy :: Cubix.TreeSitter.Token a -> Parser (MoveTerm AbilityL)
    pAbilityCopy _sym =
      iAbilityCopy
        <$> pCopy
    pAbilityDrop :: Cubix.TreeSitter.Token a -> Parser (MoveTerm AbilityL)
    pAbilityDrop _sym =
      iAbilityDrop
        <$> pDrop
    pAbilityStore :: Cubix.TreeSitter.Token a -> Parser (MoveTerm AbilityL)
    pAbilityStore _sym =
      iAbilityStore
        <$> pStore
    pAbilityKey :: Cubix.TreeSitter.Token a -> Parser (MoveTerm AbilityL)
    pAbilityKey _sym =
      iAbilityKey
        <$> pKey

pTypeParameters :: Parser (MoveTerm TypeParametersL)
pTypeParameters = do
  _sym <- pSymbol "type_parameters" STypeParametersSymbol
  iTypeParameters
    <$> pSome pTypeParameter

pTypeParameter :: Parser (MoveTerm TypeParameterL)
pTypeParameter = do
  _sym <- pSymbol "type_parameter" STypeParameterSymbol
  iTypeParameter
    <$> pMaybe pDollarSign
    <*> pMaybe pPhantom
    <*> (pHiddenTypeParameterIdentifier _sym)
    <*> pMaybe (pPair pColon (pSome pAbility))

pHiddenTypeParameterIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeParameterIdentifierL)
pHiddenTypeParameterIdentifier _sym = do
  iHiddenTypeParameterIdentifier
    <$> pIdentifier

pEnumVariants :: Parser (MoveTerm EnumVariantsL)
pEnumVariants = do
  _sym <- pSymbol "enum_variants" SEnumVariantsSymbol
  iEnumVariants
    <$> pMany pVariant

pVariant :: Parser (MoveTerm VariantL)
pVariant = do
  _sym <- pSymbol "variant" SVariantSymbol
  iVariant
    <$> (pHiddenVariantIdentifier _sym)
    <*> pMaybe pDatatypeFields

pHiddenVariantIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenVariantIdentifierL)
pHiddenVariantIdentifier _sym = do
  iHiddenVariantIdentifier
    <$> pIdentifier

pDatatypeFields :: Parser (MoveTerm DatatypeFieldsL)
pDatatypeFields = do
  _sym <- pSymbol "datatype_fields" SDatatypeFieldsSymbol
  choice [ Megaparsec.try (pDatatypeFieldsPositionalFields _sym)
         , Megaparsec.try (pDatatypeFieldsNamedFields _sym)
         ]
  where
    pDatatypeFieldsPositionalFields :: Cubix.TreeSitter.Token a -> Parser (MoveTerm DatatypeFieldsL)
    pDatatypeFieldsPositionalFields _sym =
      iDatatypeFieldsPositionalFields
        <$> pPositionalFields
    pDatatypeFieldsNamedFields :: Cubix.TreeSitter.Token a -> Parser (MoveTerm DatatypeFieldsL)
    pDatatypeFieldsNamedFields _sym =
      iDatatypeFieldsNamedFields
        <$> pNamedFields

pNamedFields :: Parser (MoveTerm NamedFieldsL)
pNamedFields = do
  _sym <- pSymbol "named_fields" SNamedFieldsSymbol
  iNamedFields
    <$> pMany pFieldAnnotation

pFieldAnnotation :: Parser (MoveTerm FieldAnnotationL)
pFieldAnnotation = do
  _sym <- pSymbol "field_annotation" SFieldAnnotationSymbol
  iFieldAnnotation
    <$> (pHiddenFieldIdentifier _sym)
    <*> pColon
    <*> (pHiddenType _sym)

pHiddenFieldIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFieldIdentifierL)
pHiddenFieldIdentifier _sym = do
  iHiddenFieldIdentifier
    <$> pIdentifier

pHiddenType :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeL)
pHiddenType _sym = do
  choice [ Megaparsec.try (pHiddenTypeApplyType _sym)
         , Megaparsec.try (pHiddenTypeRefType _sym)
         , Megaparsec.try (pHiddenTypeTupleType _sym)
         , Megaparsec.try (pHiddenTypeFunctionType _sym)
         , Megaparsec.try (pHiddenTypePrimitiveType _sym)
         ]
  where
    pHiddenTypeApplyType :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeL)
    pHiddenTypeApplyType _sym =
      iHiddenTypeApplyType
        <$> pApplyType
    pHiddenTypeRefType :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeL)
    pHiddenTypeRefType _sym =
      iHiddenTypeRefType
        <$> pRefType
    pHiddenTypeTupleType :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeL)
    pHiddenTypeTupleType _sym =
      iHiddenTypeTupleType
        <$> pTupleType
    pHiddenTypeFunctionType :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeL)
    pHiddenTypeFunctionType _sym =
      iHiddenTypeFunctionType
        <$> pFunctionType
    pHiddenTypePrimitiveType :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeL)
    pHiddenTypePrimitiveType _sym =
      iHiddenTypePrimitiveType
        <$> pPrimitiveType

pApplyType :: Parser (MoveTerm ApplyTypeL)
pApplyType = do
  _sym <- pSymbol "apply_type" SApplyTypeSymbol
  iApplyType
    <$> pPair pModuleAccess (pMaybe pTypeArguments)

pModuleAccess :: Parser (MoveTerm ModuleAccessL)
pModuleAccess = do
  _sym <- pSymbol "module_access" SModuleAccessSymbol
  choice [ Megaparsec.try (pModuleAccess1 _sym)
         , Megaparsec.try (pModuleAccess2 _sym)
         , Megaparsec.try (pModuleAccess3 _sym)
         , Megaparsec.try (pModuleAccess4 _sym)
         , Megaparsec.try (pModuleAccess5 _sym)
         , Megaparsec.try (pModuleAccess6 _sym)
         , Megaparsec.try (pModuleAccess7 _sym)
         , Megaparsec.try (pModuleAccess8 _sym)
         , Megaparsec.try (pModuleAccessMember _sym)
         ]
  where
    pModuleAccess1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess1 _sym =
      iModuleAccess1
        <$> pDollarSign
        <*> pIdentifier
    pModuleAccess2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess2 _sym =
      iModuleAccess2
        <$> pCommercialAt
        <*> pIdentifier
    pModuleAccess3 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess3 _sym =
      iModuleAccess3
        <$> pModuleIdentity
        <*> pColoncolon
        <*> pIdentifier
        <*> pMaybe pTypeArguments
        <*> pColoncolon
        <*> pIdentifier
    pModuleAccess4 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess4 _sym =
      iModuleAccess4
        <$> pModuleIdentity
        <*> pColoncolon
        <*> pIdentifier
        <*> pTypeArguments
    pModuleAccess5 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess5 _sym =
      iModuleAccess5
        <$> pModuleIdentity
        <*> pMaybe pTypeArguments
        <*> pColoncolon
        <*> pIdentifier
    pModuleAccess6 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess6 _sym =
      iModuleAccess6
        <$> (pHiddenModuleIdentifier _sym)
        <*> pMaybe pTypeArguments
        <*> pColoncolon
        <*> pIdentifier
    pModuleAccess7 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess7 _sym =
      iModuleAccess7
        <$> pModuleIdentity
        <*> pMaybe pTypeArguments
    pModuleAccess8 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess8 _sym =
      iModuleAccess8
        <$> pIdentifier
        <*> pMaybe pTypeArguments
    pModuleAccessMember :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccessMember _sym =
      iModuleAccessMember
        <$> (pHiddenReservedIdentifier _sym)

pHiddenModuleIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenModuleIdentifierL)
pHiddenModuleIdentifier _sym = do
  iHiddenModuleIdentifier
    <$> pIdentifier

pHiddenReservedIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReservedIdentifierL)
pHiddenReservedIdentifier _sym = do
  choice [ Megaparsec.try (pHiddenReservedIdentifierForall _sym)
         , Megaparsec.try (pHiddenReservedIdentifierExists _sym)
         ]
  where
    pHiddenReservedIdentifierForall :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReservedIdentifierL)
    pHiddenReservedIdentifierForall _sym =
      iHiddenReservedIdentifierForall
        <$> (pHiddenForall _sym)
    pHiddenReservedIdentifierExists :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReservedIdentifierL)
    pHiddenReservedIdentifierExists _sym =
      iHiddenReservedIdentifierExists
        <$> (pHiddenExists _sym)

pHiddenExists :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExistsL)
pHiddenExists _sym = do
  iHiddenExists
    <$> pExists

pHiddenForall :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenForallL)
pHiddenForall _sym = do
  iHiddenForall
    <$> pForall

pModuleIdentity :: Parser (MoveTerm ModuleIdentityL)
pModuleIdentity = do
  _sym <- pSymbol "module_identity" SModuleIdentitySymbol
  iModuleIdentity
    <$> (pModuleIdentityInternal0 _sym)
    <*> pColoncolon
    <*> (pHiddenModuleIdentifier _sym)

pModuleIdentityInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleIdentityInternal0L)
pModuleIdentityInternal0 _sym = do
  choice [ Megaparsec.try (pModuleIdentityInternal0NumLiteral _sym)
         , Megaparsec.try (pModuleIdentityInternal0ModuleIdentifier _sym)
         ]
  where
    pModuleIdentityInternal0NumLiteral :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleIdentityInternal0L)
    pModuleIdentityInternal0NumLiteral _sym =
      iModuleIdentityInternal0NumLiteral
        <$> pNumLiteral
    pModuleIdentityInternal0ModuleIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleIdentityInternal0L)
    pModuleIdentityInternal0ModuleIdentifier _sym =
      iModuleIdentityInternal0ModuleIdentifier
        <$> (pHiddenModuleIdentifier _sym)

pNumLiteral :: Parser (MoveTerm NumLiteralL)
pNumLiteral = do
  _sym <- pSymbol "num_literal" SNumLiteralSymbol
  iNumLiteral
    <$> pContent _sym
    <*> pMaybe (pNumLiteralInternal0 _sym)

pNumLiteralInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
pNumLiteralInternal0 _sym = do
  choice [ Megaparsec.try (pNumLiteralInternal0U8 _sym)
         , Megaparsec.try (pNumLiteralInternal0U16 _sym)
         , Megaparsec.try (pNumLiteralInternal0U32 _sym)
         , Megaparsec.try (pNumLiteralInternal0U64 _sym)
         , Megaparsec.try (pNumLiteralInternal0U128 _sym)
         , Megaparsec.try (pNumLiteralInternal0U256 _sym)
         ]
  where
    pNumLiteralInternal0U8 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U8 _sym =
      iNumLiteralInternal0U8
        <$> pU8
    pNumLiteralInternal0U16 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U16 _sym =
      iNumLiteralInternal0U16
        <$> pU16
    pNumLiteralInternal0U32 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U32 _sym =
      iNumLiteralInternal0U32
        <$> pU32
    pNumLiteralInternal0U64 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U64 _sym =
      iNumLiteralInternal0U64
        <$> pU64
    pNumLiteralInternal0U128 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U128 _sym =
      iNumLiteralInternal0U128
        <$> pU128
    pNumLiteralInternal0U256 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U256 _sym =
      iNumLiteralInternal0U256
        <$> pU256

pTypeArguments :: Parser (MoveTerm TypeArgumentsL)
pTypeArguments = do
  _sym <- pSymbol "type_arguments" STypeArgumentsSymbol
  iTypeArguments
    <$> pSome (pHiddenType _sym)

pFunctionType :: Parser (MoveTerm FunctionTypeL)
pFunctionType = do
  _sym <- pSymbol "function_type" SFunctionTypeSymbol
  iFunctionType
    <$> pFunctionTypeParameters
    <*> pMaybe (pPair pGreaterThanSign (pHiddenType _sym))

pFunctionTypeParameters :: Parser (MoveTerm FunctionTypeParametersL)
pFunctionTypeParameters = do
  _sym <- pSymbol "function_type_parameters" SFunctionTypeParametersSymbol
  iFunctionTypeParameters
    <$> pMany (pHiddenType _sym)

pPrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
pPrimitiveType = do
  _sym <- pSymbol "primitive_type" SPrimitiveTypeSymbol
  choice [ Megaparsec.try (pPrimitiveTypeU8 _sym)
         , Megaparsec.try (pPrimitiveTypeU16 _sym)
         , Megaparsec.try (pPrimitiveTypeU32 _sym)
         , Megaparsec.try (pPrimitiveTypeU64 _sym)
         , Megaparsec.try (pPrimitiveTypeU128 _sym)
         , Megaparsec.try (pPrimitiveTypeU256 _sym)
         , Megaparsec.try (pPrimitiveTypeBool _sym)
         , Megaparsec.try (pPrimitiveTypeAddress _sym)
         , Megaparsec.try (pPrimitiveTypeSigner _sym)
         , Megaparsec.try (pPrimitiveTypeBytearray _sym)
         ]
  where
    pPrimitiveTypeU8 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU8 _sym =
      iPrimitiveTypeU8
        <$> pU8
    pPrimitiveTypeU16 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU16 _sym =
      iPrimitiveTypeU16
        <$> pU16
    pPrimitiveTypeU32 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU32 _sym =
      iPrimitiveTypeU32
        <$> pU32
    pPrimitiveTypeU64 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU64 _sym =
      iPrimitiveTypeU64
        <$> pU64
    pPrimitiveTypeU128 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU128 _sym =
      iPrimitiveTypeU128
        <$> pU128
    pPrimitiveTypeU256 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU256 _sym =
      iPrimitiveTypeU256
        <$> pU256
    pPrimitiveTypeBool :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeBool _sym =
      iPrimitiveTypeBool
        <$> pBool
    pPrimitiveTypeAddress :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeAddress _sym =
      iPrimitiveTypeAddress
        <$> pAddress
    pPrimitiveTypeSigner :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeSigner _sym =
      iPrimitiveTypeSigner
        <$> pSigner
    pPrimitiveTypeBytearray :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeBytearray _sym =
      iPrimitiveTypeBytearray
        <$> pBytearray

pRefType :: Parser (MoveTerm RefTypeL)
pRefType = do
  _sym <- pSymbol "ref_type" SRefTypeSymbol
  iRefType
    <$> (pHiddenReference _sym)
    <*> (pHiddenType _sym)

pHiddenReference :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReferenceL)
pHiddenReference _sym = do
  choice [ Megaparsec.try (pHiddenReferenceImmRef _sym)
         , Megaparsec.try (pHiddenReferenceMutRef _sym)
         ]
  where
    pHiddenReferenceImmRef :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReferenceL)
    pHiddenReferenceImmRef _sym =
      iHiddenReferenceImmRef
        <$> pImmRef
    pHiddenReferenceMutRef :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReferenceL)
    pHiddenReferenceMutRef _sym =
      iHiddenReferenceMutRef
        <$> pMutRef

pImmRef :: Parser (MoveTerm ImmRefL)
pImmRef = do
  _sym <- pSymbol "imm_ref" SImmRefSymbol
  iImmRef
    <$> pAmpersand

pMutRef :: Parser (MoveTerm MutRefL)
pMutRef = do
  _sym <- pSymbol "mut_ref" SMutRefSymbol
  iMutRef
    <$> pAmpersand
    <*> pMut

pTupleType :: Parser (MoveTerm TupleTypeL)
pTupleType = do
  _sym <- pSymbol "tuple_type" STupleTypeSymbol
  iTupleType
    <$> pMany (pHiddenType _sym)

pPositionalFields :: Parser (MoveTerm PositionalFieldsL)
pPositionalFields = do
  _sym <- pSymbol "positional_fields" SPositionalFieldsSymbol
  iPositionalFields
    <$> pMany (pHiddenType _sym)

pPostfixAbilityDecls :: Parser (MoveTerm PostfixAbilityDeclsL)
pPostfixAbilityDecls = do
  _sym <- pSymbol "postfix_ability_decls" SPostfixAbilityDeclsSymbol
  iPostfixAbilityDecls
    <$> pMany pAbility

pHiddenFunctionItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionItemL)
pHiddenFunctionItem _sym = do
  choice [ Megaparsec.try (pHiddenFunctionItemNativeFunctionDefinition _sym)
         , Megaparsec.try (pHiddenFunctionItemMacroFunctionDefinition _sym)
         , Megaparsec.try (pHiddenFunctionItemFunctionDefinition _sym)
         ]
  where
    pHiddenFunctionItemNativeFunctionDefinition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionItemL)
    pHiddenFunctionItemNativeFunctionDefinition _sym =
      iHiddenFunctionItemNativeFunctionDefinition
        <$> pNativeFunctionDefinition
    pHiddenFunctionItemMacroFunctionDefinition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionItemL)
    pHiddenFunctionItemMacroFunctionDefinition _sym =
      iHiddenFunctionItemMacroFunctionDefinition
        <$> pMacroFunctionDefinition
    pHiddenFunctionItemFunctionDefinition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionItemL)
    pHiddenFunctionItemFunctionDefinition _sym =
      iHiddenFunctionItemFunctionDefinition
        <$> pFunctionDefinition

pFunctionDefinition :: Parser (MoveTerm FunctionDefinitionL)
pFunctionDefinition = do
  _sym <- pSymbol "function_definition" SFunctionDefinitionSymbol
  iFunctionDefinition
    <$> (pHiddenFunctionSignature _sym)
    <*> pBlock

pHiddenFunctionSignature :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionSignatureL)
pHiddenFunctionSignature _sym = do
  iHiddenFunctionSignature
    <$> pMaybe pModifier
    <*> pMaybe pModifier
    <*> pMaybe pModifier
    <*> pFun
    <*> (pHiddenFunctionIdentifier _sym)
    <*> pMaybe pTypeParameters
    <*> pFunctionParameters
    <*> pMaybe pRetType

pHiddenFunctionIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionIdentifierL)
pHiddenFunctionIdentifier _sym = do
  iHiddenFunctionIdentifier
    <$> pIdentifier

pFunctionParameters :: Parser (MoveTerm FunctionParametersL)
pFunctionParameters = do
  _sym <- pSymbol "function_parameters" SFunctionParametersSymbol
  iFunctionParameters
    <$> pMany (pFunctionParametersInternal0 _sym)

pFunctionParametersInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParametersInternal0L)
pFunctionParametersInternal0 _sym = do
  choice [ Megaparsec.try (pFunctionParametersInternal0MutFunctionParameter _sym)
         , Megaparsec.try (pFunctionParametersInternal0FunctionParameter _sym)
         ]
  where
    pFunctionParametersInternal0MutFunctionParameter :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParametersInternal0L)
    pFunctionParametersInternal0MutFunctionParameter _sym =
      iFunctionParametersInternal0MutFunctionParameter
        <$> pMutFunctionParameter
    pFunctionParametersInternal0FunctionParameter :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParametersInternal0L)
    pFunctionParametersInternal0FunctionParameter _sym =
      iFunctionParametersInternal0FunctionParameter
        <$> pFunctionParameter

pFunctionParameter :: Parser (MoveTerm FunctionParameterL)
pFunctionParameter = do
  _sym <- pSymbol "function_parameter" SFunctionParameterSymbol
  iFunctionParameter
    <$> (pFunctionParameterInternal0 _sym)
    <*> pColon
    <*> (pHiddenType _sym)

pFunctionParameterInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParameterInternal0L)
pFunctionParameterInternal0 _sym = do
  choice [ Megaparsec.try (pFunctionParameterInternal0Name _sym)
         , Megaparsec.try (pFunctionParameterInternal02 _sym)
         ]
  where
    pFunctionParameterInternal0Name :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParameterInternal0L)
    pFunctionParameterInternal0Name _sym =
      iFunctionParameterInternal0Name
        <$> (pHiddenVariableIdentifier _sym)
    pFunctionParameterInternal02 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParameterInternal0L)
    pFunctionParameterInternal02 _sym =
      iFunctionParameterInternal02
        <$> pDollarSign
        <*> (pHiddenVariableIdentifier _sym)

pHiddenVariableIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenVariableIdentifierL)
pHiddenVariableIdentifier _sym = do
  iHiddenVariableIdentifier
    <$> pIdentifier

pMutFunctionParameter :: Parser (MoveTerm MutFunctionParameterL)
pMutFunctionParameter = do
  _sym <- pSymbol "mut_function_parameter" SMutFunctionParameterSymbol
  iMutFunctionParameter
    <$> pMut
    <*> pFunctionParameter

pModifier :: Parser (MoveTerm ModifierL)
pModifier = do
  _sym <- pSymbol "modifier" SModifierSymbol
  choice [ Megaparsec.try (pModifier1 _sym)
         , Megaparsec.try (pModifierEntry _sym)
         , Megaparsec.try (pModifierNative _sym)
         ]
  where
    pModifier1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModifierL)
    pModifier1 _sym =
      iModifier1
        <$> pPublic
        <*> pMaybe (pModifierInternal0 _sym)
    pModifierEntry :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModifierL)
    pModifierEntry _sym =
      iModifierEntry
        <$> pEntry
    pModifierNative :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModifierL)
    pModifierNative _sym =
      iModifierNative
        <$> pNative

pModifierInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModifierInternal0L)
pModifierInternal0 _sym = do
  choice [ Megaparsec.try (pModifierInternal0Package _sym)
         , Megaparsec.try (pModifierInternal0Friend _sym)
         ]
  where
    pModifierInternal0Package :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModifierInternal0L)
    pModifierInternal0Package _sym =
      iModifierInternal0Package
        <$> pPackage
    pModifierInternal0Friend :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModifierInternal0L)
    pModifierInternal0Friend _sym =
      iModifierInternal0Friend
        <$> pFriend

pRetType :: Parser (MoveTerm RetTypeL)
pRetType = do
  _sym <- pSymbol "ret_type" SRetTypeSymbol
  iRetType
    <$> pColon
    <*> (pHiddenType _sym)

pBlock :: Parser (MoveTerm BlockL)
pBlock = do
  _sym <- pSymbol "block" SBlockSymbol
  iBlock
    <$> pLeftCurlyBracket
    <*> pMany pUseDeclaration
    <*> pMany pBlockItem
    <*> pMaybe (pHiddenExpression _sym)
    <*> pRightCurlyBracket

pHiddenExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
pHiddenExpression _sym = do
  choice [ Megaparsec.try (pHiddenExpressionCallExpression _sym)
         , Megaparsec.try (pHiddenExpressionMacroCallExpression _sym)
         , Megaparsec.try (pHiddenExpressionLambdaExpression _sym)
         , Megaparsec.try (pHiddenExpressionIfExpression _sym)
         , Megaparsec.try (pHiddenExpressionWhileExpression _sym)
         , Megaparsec.try (pHiddenExpressionReturnExpression _sym)
         , Megaparsec.try (pHiddenExpressionAbortExpression _sym)
         , Megaparsec.try (pHiddenExpressionAssignExpression _sym)
         , Megaparsec.try (pHiddenExpressionUnaryExpression _sym)
         , Megaparsec.try (pHiddenExpressionBinaryExpression _sym)
         , Megaparsec.try (pHiddenExpressionCastExpression _sym)
         , Megaparsec.try (pHiddenExpressionQuantifierExpression _sym)
         , Megaparsec.try (pHiddenExpressionMatchExpression _sym)
         , Megaparsec.try (pHiddenExpressionVectorExpression _sym)
         , Megaparsec.try (pHiddenExpressionLoopExpression _sym)
         , Megaparsec.try (pHiddenExpressionIdentifiedExpression _sym)
         ]
  where
    pHiddenExpressionCallExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionCallExpression _sym =
      iHiddenExpressionCallExpression
        <$> pCallExpression
    pHiddenExpressionMacroCallExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionMacroCallExpression _sym =
      iHiddenExpressionMacroCallExpression
        <$> pMacroCallExpression
    pHiddenExpressionLambdaExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionLambdaExpression _sym =
      iHiddenExpressionLambdaExpression
        <$> pLambdaExpression
    pHiddenExpressionIfExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionIfExpression _sym =
      iHiddenExpressionIfExpression
        <$> pIfExpression
    pHiddenExpressionWhileExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionWhileExpression _sym =
      iHiddenExpressionWhileExpression
        <$> pWhileExpression
    pHiddenExpressionReturnExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionReturnExpression _sym =
      iHiddenExpressionReturnExpression
        <$> pReturnExpression
    pHiddenExpressionAbortExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionAbortExpression _sym =
      iHiddenExpressionAbortExpression
        <$> pAbortExpression
    pHiddenExpressionAssignExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionAssignExpression _sym =
      iHiddenExpressionAssignExpression
        <$> pAssignExpression
    pHiddenExpressionUnaryExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionUnaryExpression _sym =
      iHiddenExpressionUnaryExpression
        <$> (pHiddenUnaryExpression _sym)
    pHiddenExpressionBinaryExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionBinaryExpression _sym =
      iHiddenExpressionBinaryExpression
        <$> pBinaryExpression
    pHiddenExpressionCastExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionCastExpression _sym =
      iHiddenExpressionCastExpression
        <$> pCastExpression
    pHiddenExpressionQuantifierExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionQuantifierExpression _sym =
      iHiddenExpressionQuantifierExpression
        <$> pQuantifierExpression
    pHiddenExpressionMatchExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionMatchExpression _sym =
      iHiddenExpressionMatchExpression
        <$> pMatchExpression
    pHiddenExpressionVectorExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionVectorExpression _sym =
      iHiddenExpressionVectorExpression
        <$> pVectorExpression
    pHiddenExpressionLoopExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionLoopExpression _sym =
      iHiddenExpressionLoopExpression
        <$> pLoopExpression
    pHiddenExpressionIdentifiedExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionIdentifiedExpression _sym =
      iHiddenExpressionIdentifiedExpression
        <$> pIdentifiedExpression

pHiddenUnaryExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionL)
pHiddenUnaryExpression _sym = do
  iHiddenUnaryExpression
    <$> (pHiddenUnaryExpressionInternal0 _sym)

pHiddenUnaryExpressionInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionInternal0L)
pHiddenUnaryExpressionInternal0 _sym = do
  choice [ Megaparsec.try (pHiddenUnaryExpressionInternal0UnaryExpression _sym)
         , Megaparsec.try (pHiddenUnaryExpressionInternal0BorrowExpression _sym)
         , Megaparsec.try (pHiddenUnaryExpressionInternal0DereferenceExpression _sym)
         , Megaparsec.try (pHiddenUnaryExpressionInternal0MoveOrCopyExpression _sym)
         , Megaparsec.try (pHiddenUnaryExpressionInternal0ExpressionTerm _sym)
         ]
  where
    pHiddenUnaryExpressionInternal0UnaryExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0UnaryExpression _sym =
      iHiddenUnaryExpressionInternal0UnaryExpression
        <$> pUnaryExpression
    pHiddenUnaryExpressionInternal0BorrowExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0BorrowExpression _sym =
      iHiddenUnaryExpressionInternal0BorrowExpression
        <$> pBorrowExpression
    pHiddenUnaryExpressionInternal0DereferenceExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0DereferenceExpression _sym =
      iHiddenUnaryExpressionInternal0DereferenceExpression
        <$> pDereferenceExpression
    pHiddenUnaryExpressionInternal0MoveOrCopyExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0MoveOrCopyExpression _sym =
      iHiddenUnaryExpressionInternal0MoveOrCopyExpression
        <$> pMoveOrCopyExpression
    pHiddenUnaryExpressionInternal0ExpressionTerm :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0ExpressionTerm _sym =
      iHiddenUnaryExpressionInternal0ExpressionTerm
        <$> (pHiddenExpressionTerm _sym)

pHiddenExpressionTerm :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
pHiddenExpressionTerm _sym = do
  choice [ Megaparsec.try (pHiddenExpressionTermCallExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermBreakExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermContinueExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermNameExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermMacroCallExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermPackExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermLiteralValue _sym)
         , Megaparsec.try (pHiddenExpressionTermUnitExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermExpressionList _sym)
         , Megaparsec.try (pHiddenExpressionTermAnnotationExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermBlock _sym)
         , Megaparsec.try (pHiddenExpressionTermSpecBlock _sym)
         , Megaparsec.try (pHiddenExpressionTermIfExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermDotExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermIndexExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermVectorExpression _sym)
         , Megaparsec.try (pHiddenExpressionTermMatchExpression _sym)
         ]
  where
    pHiddenExpressionTermCallExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermCallExpression _sym =
      iHiddenExpressionTermCallExpression
        <$> pCallExpression
    pHiddenExpressionTermBreakExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermBreakExpression _sym =
      iHiddenExpressionTermBreakExpression
        <$> pBreakExpression
    pHiddenExpressionTermContinueExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermContinueExpression _sym =
      iHiddenExpressionTermContinueExpression
        <$> pContinueExpression
    pHiddenExpressionTermNameExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermNameExpression _sym =
      iHiddenExpressionTermNameExpression
        <$> pNameExpression
    pHiddenExpressionTermMacroCallExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermMacroCallExpression _sym =
      iHiddenExpressionTermMacroCallExpression
        <$> pMacroCallExpression
    pHiddenExpressionTermPackExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermPackExpression _sym =
      iHiddenExpressionTermPackExpression
        <$> pPackExpression
    pHiddenExpressionTermLiteralValue :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermLiteralValue _sym =
      iHiddenExpressionTermLiteralValue
        <$> (pHiddenLiteralValue _sym)
    pHiddenExpressionTermUnitExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermUnitExpression _sym =
      iHiddenExpressionTermUnitExpression
        <$> pUnitExpression
    pHiddenExpressionTermExpressionList :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermExpressionList _sym =
      iHiddenExpressionTermExpressionList
        <$> pExpressionList
    pHiddenExpressionTermAnnotationExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermAnnotationExpression _sym =
      iHiddenExpressionTermAnnotationExpression
        <$> pAnnotationExpression
    pHiddenExpressionTermBlock :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermBlock _sym =
      iHiddenExpressionTermBlock
        <$> pBlock
    pHiddenExpressionTermSpecBlock :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermSpecBlock _sym =
      iHiddenExpressionTermSpecBlock
        <$> pSpecBlock
    pHiddenExpressionTermIfExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermIfExpression _sym =
      iHiddenExpressionTermIfExpression
        <$> pIfExpression
    pHiddenExpressionTermDotExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermDotExpression _sym =
      iHiddenExpressionTermDotExpression
        <$> pDotExpression
    pHiddenExpressionTermIndexExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermIndexExpression _sym =
      iHiddenExpressionTermIndexExpression
        <$> pIndexExpression
    pHiddenExpressionTermVectorExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermVectorExpression _sym =
      iHiddenExpressionTermVectorExpression
        <$> pVectorExpression
    pHiddenExpressionTermMatchExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermMatchExpression _sym =
      iHiddenExpressionTermMatchExpression
        <$> pMatchExpression

pHiddenLiteralValue :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenLiteralValueL)
pHiddenLiteralValue _sym = do
  choice [ Megaparsec.try (pHiddenLiteralValueAddressLiteral _sym)
         , Megaparsec.try (pHiddenLiteralValueBoolLiteral _sym)
         , Megaparsec.try (pHiddenLiteralValueNumLiteral _sym)
         , Megaparsec.try (pHiddenLiteralValueHexStringLiteral _sym)
         , Megaparsec.try (pHiddenLiteralValueByteStringLiteral _sym)
         ]
  where
    pHiddenLiteralValueAddressLiteral :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueAddressLiteral _sym =
      iHiddenLiteralValueAddressLiteral
        <$> pAddressLiteral
    pHiddenLiteralValueBoolLiteral :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueBoolLiteral _sym =
      iHiddenLiteralValueBoolLiteral
        <$> pBoolLiteral
    pHiddenLiteralValueNumLiteral :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueNumLiteral _sym =
      iHiddenLiteralValueNumLiteral
        <$> pNumLiteral
    pHiddenLiteralValueHexStringLiteral :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueHexStringLiteral _sym =
      iHiddenLiteralValueHexStringLiteral
        <$> pHexStringLiteral
    pHiddenLiteralValueByteStringLiteral :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueByteStringLiteral _sym =
      iHiddenLiteralValueByteStringLiteral
        <$> pByteStringLiteral

pAddressLiteral :: Parser (MoveTerm AddressLiteralL)
pAddressLiteral = do
  _sym <- pSymbol "address_literal" SAddressLiteralSymbol
  iAddressLiteral
    <$> pContent _sym

pBoolLiteral :: Parser (MoveTerm BoolLiteralL)
pBoolLiteral = do
  _sym <- pSymbol "bool_literal" SBoolLiteralSymbol
  choice [ Megaparsec.try (pBoolLiteralTrue _sym)
         , Megaparsec.try (pBoolLiteralFalse _sym)
         ]
  where
    pBoolLiteralTrue :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BoolLiteralL)
    pBoolLiteralTrue _sym =
      iBoolLiteralTrue
        <$> pTrue
    pBoolLiteralFalse :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BoolLiteralL)
    pBoolLiteralFalse _sym =
      iBoolLiteralFalse
        <$> pFalse

pByteStringLiteral :: Parser (MoveTerm ByteStringLiteralL)
pByteStringLiteral = do
  _sym <- pSymbol "byte_string_literal" SByteStringLiteralSymbol
  iByteStringLiteral
    <$> pContent _sym

pHexStringLiteral :: Parser (MoveTerm HexStringLiteralL)
pHexStringLiteral = do
  _sym <- pSymbol "hex_string_literal" SHexStringLiteralSymbol
  iHexStringLiteral
    <$> pContent _sym

pAnnotationExpression :: Parser (MoveTerm AnnotationExpressionL)
pAnnotationExpression = do
  _sym <- pSymbol "annotation_expression" SAnnotationExpressionSymbol
  iAnnotationExpression
    <$> (pHiddenExpression _sym)
    <*> (pHiddenType _sym)
    <*> pRightParenthesis

pBreakExpression :: Parser (MoveTerm BreakExpressionL)
pBreakExpression = do
  _sym <- pSymbol "break_expression" SBreakExpressionSymbol
  iBreakExpression
    <$> pBreak
    <*> pMaybe pLabel
    <*> pMaybe (pHiddenExpression _sym)

pLabel :: Parser (MoveTerm LabelL)
pLabel = do
  _sym <- pSymbol "label" SLabelSymbol
  iLabel
    <$> pApostrophe
    <*> pIdentifier

pCallExpression :: Parser (MoveTerm CallExpressionL)
pCallExpression = do
  _sym <- pSymbol "call_expression" SCallExpressionSymbol
  iCallExpression
    <$> pPair pNameExpression pArgList

pArgList :: Parser (MoveTerm ArgListL)
pArgList = do
  _sym <- pSymbol "arg_list" SArgListSymbol
  iArgList
    <$> pMany (pHiddenExpression _sym)

pNameExpression :: Parser (MoveTerm NameExpressionL)
pNameExpression = do
  _sym <- pSymbol "name_expression" SNameExpressionSymbol
  iNameExpression
    <$> pMaybe pColoncolon
    <*> pModuleAccess

pContinueExpression :: Parser (MoveTerm ContinueExpressionL)
pContinueExpression = do
  _sym <- pSymbol "continue_expression" SContinueExpressionSymbol
  iContinueExpression
    <$> pContinue
    <*> pMaybe pLabel

pDotExpression :: Parser (MoveTerm DotExpressionL)
pDotExpression = do
  _sym <- pSymbol "dot_expression" SDotExpressionSymbol
  iDotExpression
    <$> pPair (pPair (pHiddenExpressionTerm _sym) pFullStop) (pHiddenExpressionTerm _sym)

pExpressionList :: Parser (MoveTerm ExpressionListL)
pExpressionList = do
  _sym <- pSymbol "expression_list" SExpressionListSymbol
  iExpressionList
    <$> pSome (pHiddenExpression _sym)

pIfExpression :: Parser (MoveTerm IfExpressionL)
pIfExpression = do
  _sym <- pSymbol "if_expression" SIfExpressionSymbol
  iIfExpression
    <$> pPair (pPair (pPair pIf (pHiddenExpression _sym)) (pHiddenExpression _sym)) (pMaybe (pPair pElse (pHiddenExpression _sym)))

pIndexExpression :: Parser (MoveTerm IndexExpressionL)
pIndexExpression = do
  _sym <- pSymbol "index_expression" SIndexExpressionSymbol
  iIndexExpression
    <$> pPair (pHiddenExpressionTerm _sym) (pMany (pHiddenExpression _sym))

pMacroCallExpression :: Parser (MoveTerm MacroCallExpressionL)
pMacroCallExpression = do
  _sym <- pSymbol "macro_call_expression" SMacroCallExpressionSymbol
  iMacroCallExpression
    <$> pMacroModuleAccess
    <*> pMaybe pTypeArguments
    <*> pArgList

pMacroModuleAccess :: Parser (MoveTerm MacroModuleAccessL)
pMacroModuleAccess = do
  _sym <- pSymbol "macro_module_access" SMacroModuleAccessSymbol
  iMacroModuleAccess
    <$> pModuleAccess
    <*> pExclamationMark

pMatchExpression :: Parser (MoveTerm MatchExpressionL)
pMatchExpression = do
  _sym <- pSymbol "match_expression" SMatchExpressionSymbol
  iMatchExpression
    <$> pMatch
    <*> (pHiddenExpression _sym)
    <*> (pHiddenMatchBody _sym)

pHiddenMatchBody :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenMatchBodyL)
pHiddenMatchBody _sym = do
  iHiddenMatchBody
    <$> pMany pMatchArm

pMatchArm :: Parser (MoveTerm MatchArmL)
pMatchArm = do
  _sym <- pSymbol "match_arm" SMatchArmSymbol
  iMatchArm
    <$> pBindList
    <*> pMaybe pMatchCondition
    <*> pEqualsSigngreaterThanSign
    <*> (pHiddenExpression _sym)

pBindList :: Parser (MoveTerm BindListL)
pBindList = do
  _sym <- pSymbol "bind_list" SBindListSymbol
  choice [ Megaparsec.try (pBindListBind _sym)
         , Megaparsec.try (pBindListCommaBindList _sym)
         , Megaparsec.try (pBindListOrBindList _sym)
         ]
  where
    pBindListBind :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindListL)
    pBindListBind _sym =
      iBindListBind
        <$> (pHiddenBind _sym)
    pBindListCommaBindList :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindListL)
    pBindListCommaBindList _sym =
      iBindListCommaBindList
        <$> pCommaBindList
    pBindListOrBindList :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindListL)
    pBindListOrBindList _sym =
      iBindListOrBindList
        <$> pOrBindList

pHiddenBind :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindL)
pHiddenBind _sym = do
  choice [ Megaparsec.try (pHiddenBindBindInternal0 _sym)
         , Megaparsec.try (pHiddenBindBindUnpack _sym)
         , Megaparsec.try (pHiddenBindAtBind _sym)
         , Megaparsec.try (pHiddenBindLiteralValue _sym)
         ]
  where
    pHiddenBindBindInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindL)
    pHiddenBindBindInternal0 _sym =
      iHiddenBindBindInternal0
        <$> (pHiddenBindInternal0 _sym)
    pHiddenBindBindUnpack :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindL)
    pHiddenBindBindUnpack _sym =
      iHiddenBindBindUnpack
        <$> pBindUnpack
    pHiddenBindAtBind :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindL)
    pHiddenBindAtBind _sym =
      iHiddenBindAtBind
        <$> pAtBind
    pHiddenBindLiteralValue :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindL)
    pHiddenBindLiteralValue _sym =
      iHiddenBindLiteralValue
        <$> (pHiddenLiteralValue _sym)

pHiddenBindInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindInternal0L)
pHiddenBindInternal0 _sym = do
  choice [ Megaparsec.try (pHiddenBindInternal0MutBindVar _sym)
         , Megaparsec.try (pHiddenBindInternal0VariableIdentifier _sym)
         ]
  where
    pHiddenBindInternal0MutBindVar :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindInternal0L)
    pHiddenBindInternal0MutBindVar _sym =
      iHiddenBindInternal0MutBindVar
        <$> pMutBindVar
    pHiddenBindInternal0VariableIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindInternal0L)
    pHiddenBindInternal0VariableIdentifier _sym =
      iHiddenBindInternal0VariableIdentifier
        <$> (pHiddenVariableIdentifier _sym)

pMutBindVar :: Parser (MoveTerm MutBindVarL)
pMutBindVar = do
  _sym <- pSymbol "mut_bind_var" SMutBindVarSymbol
  iMutBindVar
    <$> pMut
    <*> (pHiddenVariableIdentifier _sym)

pAtBind :: Parser (MoveTerm AtBindL)
pAtBind = do
  _sym <- pSymbol "at_bind" SAtBindSymbol
  iAtBind
    <$> (pHiddenVariableIdentifier _sym)
    <*> pCommercialAt
    <*> pBindList

pBindUnpack :: Parser (MoveTerm BindUnpackL)
pBindUnpack = do
  _sym <- pSymbol "bind_unpack" SBindUnpackSymbol
  iBindUnpack
    <$> pNameExpression
    <*> pMaybe pBindFields

pBindFields :: Parser (MoveTerm BindFieldsL)
pBindFields = do
  _sym <- pSymbol "bind_fields" SBindFieldsSymbol
  choice [ Megaparsec.try (pBindFieldsBindPositionalFields _sym)
         , Megaparsec.try (pBindFieldsBindNamedFields _sym)
         ]
  where
    pBindFieldsBindPositionalFields :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindFieldsL)
    pBindFieldsBindPositionalFields _sym =
      iBindFieldsBindPositionalFields
        <$> pBindPositionalFields
    pBindFieldsBindNamedFields :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindFieldsL)
    pBindFieldsBindNamedFields _sym =
      iBindFieldsBindNamedFields
        <$> pBindNamedFields

pBindNamedFields :: Parser (MoveTerm BindNamedFieldsL)
pBindNamedFields = do
  _sym <- pSymbol "bind_named_fields" SBindNamedFieldsSymbol
  iBindNamedFields
    <$> pMany (pBindNamedFieldsInternal0 _sym)

pBindNamedFieldsInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindNamedFieldsInternal0L)
pBindNamedFieldsInternal0 _sym = do
  choice [ Megaparsec.try (pBindNamedFieldsInternal0BindField _sym)
         , Megaparsec.try (pBindNamedFieldsInternal0MutBindField _sym)
         ]
  where
    pBindNamedFieldsInternal0BindField :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindNamedFieldsInternal0L)
    pBindNamedFieldsInternal0BindField _sym =
      iBindNamedFieldsInternal0BindField
        <$> pBindField
    pBindNamedFieldsInternal0MutBindField :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindNamedFieldsInternal0L)
    pBindNamedFieldsInternal0MutBindField _sym =
      iBindNamedFieldsInternal0MutBindField
        <$> pMutBindField

pBindField :: Parser (MoveTerm BindFieldL)
pBindField = do
  _sym <- pSymbol "bind_field" SBindFieldSymbol
  choice [ Megaparsec.try (pBindField1 _sym)
         , Megaparsec.try (pBindFieldSpreadOperator _sym)
         ]
  where
    pBindField1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindFieldL)
    pBindField1 _sym =
      iBindField1
        <$> pBindList
        <*> pMaybe (pPair pColon pBindList)
    pBindFieldSpreadOperator :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindFieldL)
    pBindFieldSpreadOperator _sym =
      iBindFieldSpreadOperator
        <$> (pHiddenSpreadOperator _sym)

pHiddenSpreadOperator :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpreadOperatorL)
pHiddenSpreadOperator _sym = do
  iHiddenSpreadOperator
    <$> pFullStopfullStop

pMutBindField :: Parser (MoveTerm MutBindFieldL)
pMutBindField = do
  _sym <- pSymbol "mut_bind_field" SMutBindFieldSymbol
  iMutBindField
    <$> pMut
    <*> pBindField

pBindPositionalFields :: Parser (MoveTerm BindPositionalFieldsL)
pBindPositionalFields = do
  _sym <- pSymbol "bind_positional_fields" SBindPositionalFieldsSymbol
  iBindPositionalFields
    <$> pMany (pBindNamedFieldsInternal0 _sym)

pCommaBindList :: Parser (MoveTerm CommaBindListL)
pCommaBindList = do
  _sym <- pSymbol "comma_bind_list" SCommaBindListSymbol
  iCommaBindList
    <$> pMany (pHiddenBind _sym)

pOrBindList :: Parser (MoveTerm OrBindListL)
pOrBindList = do
  _sym <- pSymbol "or_bind_list" SOrBindListSymbol
  iOrBindList
    <$> pMaybe pLeftParenthesis
    <*> pSome (pPair (pPair (pMaybe pLeftParenthesis) (pHiddenBind _sym)) (pMaybe pRightParenthesis))
    <*> pMaybe pRightParenthesis

pMatchCondition :: Parser (MoveTerm MatchConditionL)
pMatchCondition = do
  _sym <- pSymbol "match_condition" SMatchConditionSymbol
  iMatchCondition
    <$> pIf
    <*> (pHiddenExpression _sym)

pPackExpression :: Parser (MoveTerm PackExpressionL)
pPackExpression = do
  _sym <- pSymbol "pack_expression" SPackExpressionSymbol
  iPackExpression
    <$> pNameExpression
    <*> pFieldInitializeList

pFieldInitializeList :: Parser (MoveTerm FieldInitializeListL)
pFieldInitializeList = do
  _sym <- pSymbol "field_initialize_list" SFieldInitializeListSymbol
  iFieldInitializeList
    <$> pMany pExpField

pExpField :: Parser (MoveTerm ExpFieldL)
pExpField = do
  _sym <- pSymbol "exp_field" SExpFieldSymbol
  iExpField
    <$> (pHiddenFieldIdentifier _sym)
    <*> pMaybe (pPair pColon (pHiddenExpression _sym))

pSpecBlock :: Parser (MoveTerm SpecBlockL)
pSpecBlock = do
  _sym <- pSymbol "spec_block" SSpecBlockSymbol
  iSpecBlock
    <$> pSpec
    <*> (pSpecBlockInternal0 _sym)

pSpecBlockInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecBlockInternal0L)
pSpecBlockInternal0 _sym = do
  choice [ Megaparsec.try (pSpecBlockInternal01 _sym)
         , Megaparsec.try (pSpecBlockInternal0SpecFunction _sym)
         ]
  where
    pSpecBlockInternal01 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecBlockInternal0L)
    pSpecBlockInternal01 _sym =
      iSpecBlockInternal01
        <$> pMaybe (pHiddenSpecBlockTarget _sym)
        <*> pSpecBody
    pSpecBlockInternal0SpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecBlockInternal0L)
    pSpecBlockInternal0SpecFunction _sym =
      iSpecBlockInternal0SpecFunction
        <$> (pHiddenSpecFunction _sym)

pHiddenSpecBlockTarget :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockTargetL)
pHiddenSpecBlockTarget _sym = do
  choice [ Megaparsec.try (pHiddenSpecBlockTargetIdentifier _sym)
         , Megaparsec.try (pHiddenSpecBlockTargetModule _sym)
         , Megaparsec.try (pHiddenSpecBlockTargetSpecBlockTargetSchema _sym)
         ]
  where
    pHiddenSpecBlockTargetIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockTargetL)
    pHiddenSpecBlockTargetIdentifier _sym =
      iHiddenSpecBlockTargetIdentifier
        <$> pIdentifier
    pHiddenSpecBlockTargetModule :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockTargetL)
    pHiddenSpecBlockTargetModule _sym =
      iHiddenSpecBlockTargetModule
        <$> pModule
    pHiddenSpecBlockTargetSpecBlockTargetSchema :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockTargetL)
    pHiddenSpecBlockTargetSpecBlockTargetSchema _sym =
      iHiddenSpecBlockTargetSpecBlockTargetSchema
        <$> pSpecBlockTargetSchema

pSpecBlockTargetSchema :: Parser (MoveTerm SpecBlockTargetSchemaL)
pSpecBlockTargetSchema = do
  _sym <- pSymbol "spec_block_target_schema" SSpecBlockTargetSchemaSymbol
  iSpecBlockTargetSchema
    <$> pSchema
    <*> (pHiddenStructIdentifier _sym)
    <*> pMaybe pTypeParameters

pHiddenStructIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenStructIdentifierL)
pHiddenStructIdentifier _sym = do
  iHiddenStructIdentifier
    <$> pIdentifier

pHiddenSpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecFunctionL)
pHiddenSpecFunction _sym = do
  choice [ Megaparsec.try (pHiddenSpecFunctionNativeSpecFunction _sym)
         , Megaparsec.try (pHiddenSpecFunctionUsualSpecFunction _sym)
         , Megaparsec.try (pHiddenSpecFunctionUninterpretedSpecFunction _sym)
         ]
  where
    pHiddenSpecFunctionNativeSpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecFunctionL)
    pHiddenSpecFunctionNativeSpecFunction _sym =
      iHiddenSpecFunctionNativeSpecFunction
        <$> pNativeSpecFunction
    pHiddenSpecFunctionUsualSpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecFunctionL)
    pHiddenSpecFunctionUsualSpecFunction _sym =
      iHiddenSpecFunctionUsualSpecFunction
        <$> pUsualSpecFunction
    pHiddenSpecFunctionUninterpretedSpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecFunctionL)
    pHiddenSpecFunctionUninterpretedSpecFunction _sym =
      iHiddenSpecFunctionUninterpretedSpecFunction
        <$> pUninterpretedSpecFunction

pNativeSpecFunction :: Parser (MoveTerm NativeSpecFunctionL)
pNativeSpecFunction = do
  _sym <- pSymbol "native_spec_function" SNativeSpecFunctionSymbol
  iNativeSpecFunction
    <$> pNative
    <*> (pHiddenSpecFunctionSignature _sym)

pHiddenSpecFunctionSignature :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecFunctionSignatureL)
pHiddenSpecFunctionSignature _sym = do
  iHiddenSpecFunctionSignature
    <$> (pHiddenFunctionIdentifier _sym)
    <*> pMaybe pTypeParameters
    <*> pFunctionParameters
    <*> pRetType

pUninterpretedSpecFunction :: Parser (MoveTerm UninterpretedSpecFunctionL)
pUninterpretedSpecFunction = do
  _sym <- pSymbol "uninterpreted_spec_function" SUninterpretedSpecFunctionSymbol
  iUninterpretedSpecFunction
    <$> (pHiddenSpecFunctionSignature _sym)

pUsualSpecFunction :: Parser (MoveTerm UsualSpecFunctionL)
pUsualSpecFunction = do
  _sym <- pSymbol "usual_spec_function" SUsualSpecFunctionSymbol
  iUsualSpecFunction
    <$> pFun
    <*> (pHiddenSpecFunctionSignature _sym)
    <*> pBlock

pSpecBody :: Parser (MoveTerm SpecBodyL)
pSpecBody = do
  _sym <- pSymbol "spec_body" SSpecBodySymbol
  iSpecBody
    <$> pLeftCurlyBracket
    <*> pMany pUseDeclaration
    <*> pMany (pHiddenSpecBlockMemeber _sym)
    <*> pRightCurlyBracket

pHiddenSpecBlockMemeber :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
pHiddenSpecBlockMemeber _sym = do
  choice [ Megaparsec.try (pHiddenSpecBlockMemeberSpecInvariant _sym)
         , Megaparsec.try (pHiddenSpecBlockMemeberSpecFunction _sym)
         , Megaparsec.try (pHiddenSpecBlockMemeberSpecCondition _sym)
         , Megaparsec.try (pHiddenSpecBlockMemeberSpecInclude _sym)
         , Megaparsec.try (pHiddenSpecBlockMemeberSpecApply _sym)
         , Megaparsec.try (pHiddenSpecBlockMemeberSpecPragma _sym)
         , Megaparsec.try (pHiddenSpecBlockMemeberSpecVariable _sym)
         , Megaparsec.try (pHiddenSpecBlockMemeberSpecLet _sym)
         ]
  where
    pHiddenSpecBlockMemeberSpecInvariant :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecInvariant _sym =
      iHiddenSpecBlockMemeberSpecInvariant
        <$> pSpecInvariant
    pHiddenSpecBlockMemeberSpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecFunction _sym =
      iHiddenSpecBlockMemeberSpecFunction
        <$> (pHiddenSpecFunction _sym)
    pHiddenSpecBlockMemeberSpecCondition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecCondition _sym =
      iHiddenSpecBlockMemeberSpecCondition
        <$> pSpecCondition
    pHiddenSpecBlockMemeberSpecInclude :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecInclude _sym =
      iHiddenSpecBlockMemeberSpecInclude
        <$> pSpecInclude
    pHiddenSpecBlockMemeberSpecApply :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecApply _sym =
      iHiddenSpecBlockMemeberSpecApply
        <$> pSpecApply
    pHiddenSpecBlockMemeberSpecPragma :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecPragma _sym =
      iHiddenSpecBlockMemeberSpecPragma
        <$> pSpecPragma
    pHiddenSpecBlockMemeberSpecVariable :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecVariable _sym =
      iHiddenSpecBlockMemeberSpecVariable
        <$> pSpecVariable
    pHiddenSpecBlockMemeberSpecLet :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecLet _sym =
      iHiddenSpecBlockMemeberSpecLet
        <$> pSpecLet

pSpecApply :: Parser (MoveTerm SpecApplyL)
pSpecApply = do
  _sym <- pSymbol "spec_apply" SSpecApplySymbol
  iSpecApply
    <$> (pHiddenExpression _sym)
    <*> pSome pSpecApplyPattern
    <*> pMaybe (pPair pExcept (pSome pSpecApplyPattern))
    <*> pSemicolon

pSpecApplyPattern :: Parser (MoveTerm SpecApplyPatternL)
pSpecApplyPattern = do
  _sym <- pSymbol "spec_apply_pattern" SSpecApplyPatternSymbol
  iSpecApplyPattern
    <$> pMaybe (pSpecApplyPatternInternal0 _sym)
    <*> pSpecApplyNamePattern
    <*> pMaybe pTypeParameters

pSpecApplyNamePattern :: Parser (MoveTerm SpecApplyNamePatternL)
pSpecApplyNamePattern = do
  _sym <- pSymbol "spec_apply_name_pattern" SSpecApplyNamePatternSymbol
  iSpecApplyNamePattern
    <$> pContent _sym

pSpecApplyPatternInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecApplyPatternInternal0L)
pSpecApplyPatternInternal0 _sym = do
  choice [ Megaparsec.try (pSpecApplyPatternInternal0Public _sym)
         , Megaparsec.try (pSpecApplyPatternInternal0Internal _sym)
         ]
  where
    pSpecApplyPatternInternal0Public :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecApplyPatternInternal0L)
    pSpecApplyPatternInternal0Public _sym =
      iSpecApplyPatternInternal0Public
        <$> pPublic
    pSpecApplyPatternInternal0Internal :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecApplyPatternInternal0L)
    pSpecApplyPatternInternal0Internal _sym =
      iSpecApplyPatternInternal0Internal
        <$> pInternal

pSpecCondition :: Parser (MoveTerm SpecConditionL)
pSpecCondition = do
  _sym <- pSymbol "spec_condition" SSpecConditionSymbol
  choice [ Megaparsec.try (pSpecConditionSpecCondition _sym)
         , Megaparsec.try (pSpecConditionSpecAbortIf _sym)
         , Megaparsec.try (pSpecConditionSpecAbortWithOrModifies _sym)
         ]
  where
    pSpecConditionSpecCondition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecConditionL)
    pSpecConditionSpecCondition _sym =
      iSpecConditionSpecCondition
        <$> (pHiddenSpecCondition _sym)
    pSpecConditionSpecAbortIf :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecConditionL)
    pSpecConditionSpecAbortIf _sym =
      iSpecConditionSpecAbortIf
        <$> (pHiddenSpecAbortIf _sym)
    pSpecConditionSpecAbortWithOrModifies :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecConditionL)
    pSpecConditionSpecAbortWithOrModifies _sym =
      iSpecConditionSpecAbortWithOrModifies
        <$> (pHiddenSpecAbortWithOrModifies _sym)

pHiddenSpecAbortIf :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecAbortIfL)
pHiddenSpecAbortIf _sym = do
  iHiddenSpecAbortIf
    <$> pAbortsIf
    <*> pMaybe pConditionProperties
    <*> (pHiddenExpression _sym)
    <*> pMaybe (pPair pWith (pHiddenExpression _sym))
    <*> pSemicolon

pConditionProperties :: Parser (MoveTerm ConditionPropertiesL)
pConditionProperties = do
  _sym <- pSymbol "condition_properties" SConditionPropertiesSymbol
  iConditionProperties
    <$> pMany pSpecProperty

pSpecProperty :: Parser (MoveTerm SpecPropertyL)
pSpecProperty = do
  _sym <- pSymbol "spec_property" SSpecPropertySymbol
  iSpecProperty
    <$> pIdentifier
    <*> pMaybe (pPair pEqualsSign (pHiddenLiteralValue _sym))

pHiddenSpecAbortWithOrModifies :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecAbortWithOrModifiesL)
pHiddenSpecAbortWithOrModifies _sym = do
  iHiddenSpecAbortWithOrModifies
    <$> (pHiddenSpecAbortWithOrModifiesInternal0 _sym)
    <*> pMaybe pConditionProperties
    <*> pSome (pHiddenExpression _sym)
    <*> pSemicolon

pHiddenSpecAbortWithOrModifiesInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecAbortWithOrModifiesInternal0L)
pHiddenSpecAbortWithOrModifiesInternal0 _sym = do
  choice [ Megaparsec.try (pHiddenSpecAbortWithOrModifiesInternal0AbortsWith _sym)
         , Megaparsec.try (pHiddenSpecAbortWithOrModifiesInternal0Modifies _sym)
         ]
  where
    pHiddenSpecAbortWithOrModifiesInternal0AbortsWith :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecAbortWithOrModifiesInternal0L)
    pHiddenSpecAbortWithOrModifiesInternal0AbortsWith _sym =
      iHiddenSpecAbortWithOrModifiesInternal0AbortsWith
        <$> pAbortsWith
    pHiddenSpecAbortWithOrModifiesInternal0Modifies :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecAbortWithOrModifiesInternal0L)
    pHiddenSpecAbortWithOrModifiesInternal0Modifies _sym =
      iHiddenSpecAbortWithOrModifiesInternal0Modifies
        <$> pModifies

pHiddenSpecCondition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionL)
pHiddenSpecCondition _sym = do
  iHiddenSpecCondition
    <$> (pHiddenSpecConditionInternal0 _sym)
    <*> pMaybe pConditionProperties
    <*> (pHiddenExpression _sym)
    <*> pSemicolon

pHiddenSpecConditionInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionInternal0L)
pHiddenSpecConditionInternal0 _sym = do
  choice [ Megaparsec.try (pHiddenSpecConditionInternal0Kind _sym)
         , Megaparsec.try (pHiddenSpecConditionInternal02 _sym)
         ]
  where
    pHiddenSpecConditionInternal0Kind :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionInternal0L)
    pHiddenSpecConditionInternal0Kind _sym =
      iHiddenSpecConditionInternal0Kind
        <$> (pHiddenSpecConditionKind _sym)
    pHiddenSpecConditionInternal02 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionInternal0L)
    pHiddenSpecConditionInternal02 _sym =
      iHiddenSpecConditionInternal02
        <$> pRequires
        <*> pMaybe pModule

pHiddenSpecConditionKind :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionKindL)
pHiddenSpecConditionKind _sym = do
  choice [ Megaparsec.try (pHiddenSpecConditionKindAssert _sym)
         , Megaparsec.try (pHiddenSpecConditionKindAssume _sym)
         , Megaparsec.try (pHiddenSpecConditionKindDecreases _sym)
         , Megaparsec.try (pHiddenSpecConditionKindEnsures _sym)
         , Megaparsec.try (pHiddenSpecConditionKindSucceedsIf _sym)
         ]
  where
    pHiddenSpecConditionKindAssert :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindAssert _sym =
      iHiddenSpecConditionKindAssert
        <$> pAssert
    pHiddenSpecConditionKindAssume :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindAssume _sym =
      iHiddenSpecConditionKindAssume
        <$> pAssume
    pHiddenSpecConditionKindDecreases :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindDecreases _sym =
      iHiddenSpecConditionKindDecreases
        <$> pDecreases
    pHiddenSpecConditionKindEnsures :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindEnsures _sym =
      iHiddenSpecConditionKindEnsures
        <$> pEnsures
    pHiddenSpecConditionKindSucceedsIf :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindSucceedsIf _sym =
      iHiddenSpecConditionKindSucceedsIf
        <$> pSucceedsIf

pSpecInclude :: Parser (MoveTerm SpecIncludeL)
pSpecInclude = do
  _sym <- pSymbol "spec_include" SSpecIncludeSymbol
  iSpecInclude
    <$> (pHiddenExpression _sym)

pSpecInvariant :: Parser (MoveTerm SpecInvariantL)
pSpecInvariant = do
  _sym <- pSymbol "spec_invariant" SSpecInvariantSymbol
  iSpecInvariant
    <$> pInvariant
    <*> pMaybe (pSpecInvariantInternal0 _sym)
    <*> pMaybe pConditionProperties
    <*> (pHiddenExpression _sym)
    <*> pSemicolon

pSpecInvariantInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecInvariantInternal0L)
pSpecInvariantInternal0 _sym = do
  choice [ Megaparsec.try (pSpecInvariantInternal0Update _sym)
         , Megaparsec.try (pSpecInvariantInternal0Pack _sym)
         , Megaparsec.try (pSpecInvariantInternal0Unpack _sym)
         , Megaparsec.try (pSpecInvariantInternal0Module _sym)
         ]
  where
    pSpecInvariantInternal0Update :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Update _sym =
      iSpecInvariantInternal0Update
        <$> pUpdate
    pSpecInvariantInternal0Pack :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Pack _sym =
      iSpecInvariantInternal0Pack
        <$> pPack
    pSpecInvariantInternal0Unpack :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Unpack _sym =
      iSpecInvariantInternal0Unpack
        <$> pUnpack
    pSpecInvariantInternal0Module :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Module _sym =
      iSpecInvariantInternal0Module
        <$> pModule

pSpecLet :: Parser (MoveTerm SpecLetL)
pSpecLet = do
  _sym <- pSymbol "spec_let" SSpecLetSymbol
  iSpecLet
    <$> pLet
    <*> pMaybe pPost
    <*> pIdentifier
    <*> (pHiddenExpression _sym)

pSpecPragma :: Parser (MoveTerm SpecPragmaL)
pSpecPragma = do
  _sym <- pSymbol "spec_pragma" SSpecPragmaSymbol
  iSpecPragma
    <$> pMany pSpecProperty

pSpecVariable :: Parser (MoveTerm SpecVariableL)
pSpecVariable = do
  _sym <- pSymbol "spec_variable" SSpecVariableSymbol
  iSpecVariable
    <$> pMaybe (pSpecVariableInternal0 _sym)
    <*> pIdentifier
    <*> pMaybe pTypeParameters
    <*> (pHiddenType _sym)

pSpecVariableInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecVariableInternal0L)
pSpecVariableInternal0 _sym = do
  choice [ Megaparsec.try (pSpecVariableInternal0Global _sym)
         , Megaparsec.try (pSpecVariableInternal0Local _sym)
         ]
  where
    pSpecVariableInternal0Global :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecVariableInternal0L)
    pSpecVariableInternal0Global _sym =
      iSpecVariableInternal0Global
        <$> pGlobal
    pSpecVariableInternal0Local :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecVariableInternal0L)
    pSpecVariableInternal0Local _sym =
      iSpecVariableInternal0Local
        <$> pLocal

pUseDeclaration :: Parser (MoveTerm UseDeclarationL)
pUseDeclaration = do
  _sym <- pSymbol "use_declaration" SUseDeclarationSymbol
  iUseDeclaration
    <$> pMaybe pPublic
    <*> (pUseDeclarationInternal0 _sym)

pUseDeclarationInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseDeclarationInternal0L)
pUseDeclarationInternal0 _sym = do
  choice [ Megaparsec.try (pUseDeclarationInternal0UseFun _sym)
         , Megaparsec.try (pUseDeclarationInternal0UseModule _sym)
         , Megaparsec.try (pUseDeclarationInternal0UseModuleMember _sym)
         , Megaparsec.try (pUseDeclarationInternal0UseModuleMembers _sym)
         ]
  where
    pUseDeclarationInternal0UseFun :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseFun _sym =
      iUseDeclarationInternal0UseFun
        <$> pUseFun
    pUseDeclarationInternal0UseModule :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseModule _sym =
      iUseDeclarationInternal0UseModule
        <$> pUseModule
    pUseDeclarationInternal0UseModuleMember :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseModuleMember _sym =
      iUseDeclarationInternal0UseModuleMember
        <$> pUseModuleMember
    pUseDeclarationInternal0UseModuleMembers :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseModuleMembers _sym =
      iUseDeclarationInternal0UseModuleMembers
        <$> pUseModuleMembers

pUseFun :: Parser (MoveTerm UseFunL)
pUseFun = do
  _sym <- pSymbol "use_fun" SUseFunSymbol
  iUseFun
    <$> pModuleAccess
    <*> pPair (pPair pModuleAccess pFullStop) (pHiddenFunctionIdentifier _sym)

pUseModule :: Parser (MoveTerm UseModuleL)
pUseModule = do
  _sym <- pSymbol "use_module" SUseModuleSymbol
  iUseModule
    <$> pModuleIdentity
    <*> pMaybe (pPair pAs (pHiddenModuleIdentifier _sym))

pUseModuleMember :: Parser (MoveTerm UseModuleMemberL)
pUseModuleMember = do
  _sym <- pSymbol "use_module_member" SUseModuleMemberSymbol
  iUseModuleMember
    <$> pModuleIdentity
    <*> pColoncolon
    <*> pUseMember

pUseMember :: Parser (MoveTerm UseMemberL)
pUseMember = do
  _sym <- pSymbol "use_member" SUseMemberSymbol
  choice [ Megaparsec.try (pUseMember1 _sym)
         , Megaparsec.try (pUseMember2 _sym)
         , Megaparsec.try (pUseMember3 _sym)
         ]
  where
    pUseMember1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseMemberL)
    pUseMember1 _sym =
      iUseMember1
        <$> pIdentifier
        <*> pColoncolon
        <*> pSome pUseMember
    pUseMember2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseMemberL)
    pUseMember2 _sym =
      iUseMember2
        <$> pIdentifier
        <*> pColoncolon
        <*> pIdentifier
        <*> pMaybe (pPair pAs pIdentifier)
    pUseMember3 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseMemberL)
    pUseMember3 _sym =
      iUseMember3
        <$> pIdentifier
        <*> pMaybe (pPair pAs pIdentifier)

pUseModuleMembers :: Parser (MoveTerm UseModuleMembersL)
pUseModuleMembers = do
  _sym <- pSymbol "use_module_members" SUseModuleMembersSymbol
  choice [ Megaparsec.try (pUseModuleMembers1 _sym)
         , Megaparsec.try (pUseModuleMembers2 _sym)
         ]
  where
    pUseModuleMembers1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseModuleMembersL)
    pUseModuleMembers1 _sym =
      iUseModuleMembers1
        <$> (pModuleIdentityInternal0 _sym)
        <*> pColoncolon
        <*> pSome pUseMember
    pUseModuleMembers2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseModuleMembersL)
    pUseModuleMembers2 _sym =
      iUseModuleMembers2
        <$> pModuleIdentity
        <*> pColoncolon
        <*> pSome pUseMember

pUnitExpression :: Parser (MoveTerm UnitExpressionL)
pUnitExpression = do
  _sym <- pSymbol "unit_expression" SUnitExpressionSymbol
  iUnitExpression
    <$> pLeftParenthesis
    <*> pRightParenthesis

pVectorExpression :: Parser (MoveTerm VectorExpressionL)
pVectorExpression = do
  _sym <- pSymbol "vector_expression" SVectorExpressionSymbol
  iVectorExpression
    <$> (pVectorExpressionInternal0 _sym)
    <*> pMany (pHiddenExpression _sym)
    <*> pRightSquareBracket

pVectorExpressionInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm VectorExpressionInternal0L)
pVectorExpressionInternal0 _sym = do
  choice [ Megaparsec.try (pVectorExpressionInternal0VectorleftSquareBracket _sym)
         , Megaparsec.try (pVectorExpressionInternal02 _sym)
         ]
  where
    pVectorExpressionInternal0VectorleftSquareBracket :: Cubix.TreeSitter.Token a -> Parser (MoveTerm VectorExpressionInternal0L)
    pVectorExpressionInternal0VectorleftSquareBracket _sym =
      iVectorExpressionInternal0VectorleftSquareBracket
        <$> pVectorleftSquareBracket
    pVectorExpressionInternal02 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm VectorExpressionInternal0L)
    pVectorExpressionInternal02 _sym =
      iVectorExpressionInternal02
        <$> pSome (pHiddenType _sym)
        <*> pLeftSquareBracket

pBorrowExpression :: Parser (MoveTerm BorrowExpressionL)
pBorrowExpression = do
  _sym <- pSymbol "borrow_expression" SBorrowExpressionSymbol
  iBorrowExpression
    <$> pPair (pHiddenReference _sym) (pHiddenExpression _sym)

pDereferenceExpression :: Parser (MoveTerm DereferenceExpressionL)
pDereferenceExpression = do
  _sym <- pSymbol "dereference_expression" SDereferenceExpressionSymbol
  iDereferenceExpression
    <$> pPair pAsterisk (pHiddenExpression _sym)

pMoveOrCopyExpression :: Parser (MoveTerm MoveOrCopyExpressionL)
pMoveOrCopyExpression = do
  _sym <- pSymbol "move_or_copy_expression" SMoveOrCopyExpressionSymbol
  iMoveOrCopyExpression
    <$> pPair (pMoveOrCopyExpressionInternal0 _sym) (pHiddenExpression _sym)

pMoveOrCopyExpressionInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm MoveOrCopyExpressionInternal0L)
pMoveOrCopyExpressionInternal0 _sym = do
  choice [ Megaparsec.try (pMoveOrCopyExpressionInternal0Move _sym)
         , Megaparsec.try (pMoveOrCopyExpressionInternal0Copy _sym)
         ]
  where
    pMoveOrCopyExpressionInternal0Move :: Cubix.TreeSitter.Token a -> Parser (MoveTerm MoveOrCopyExpressionInternal0L)
    pMoveOrCopyExpressionInternal0Move _sym =
      iMoveOrCopyExpressionInternal0Move
        <$> pMove
    pMoveOrCopyExpressionInternal0Copy :: Cubix.TreeSitter.Token a -> Parser (MoveTerm MoveOrCopyExpressionInternal0L)
    pMoveOrCopyExpressionInternal0Copy _sym =
      iMoveOrCopyExpressionInternal0Copy
        <$> pCopy

pUnaryExpression :: Parser (MoveTerm UnaryExpressionL)
pUnaryExpression = do
  _sym <- pSymbol "unary_expression" SUnaryExpressionSymbol
  iUnaryExpression
    <$> pUnaryOp
    <*> (pHiddenExpression _sym)

pUnaryOp :: Parser (MoveTerm UnaryOpL)
pUnaryOp = do
  _sym <- pSymbol "unary_op" SUnaryOpSymbol
  iUnaryOp
    <$> pExclamationMark

pAbortExpression :: Parser (MoveTerm AbortExpressionL)
pAbortExpression = do
  _sym <- pSymbol "abort_expression" SAbortExpressionSymbol
  iAbortExpression
    <$> pAbort
    <*> pMaybe (pHiddenExpression _sym)

pAssignExpression :: Parser (MoveTerm AssignExpressionL)
pAssignExpression = do
  _sym <- pSymbol "assign_expression" SAssignExpressionSymbol
  iAssignExpression
    <$> pPair (pPair (pHiddenUnaryExpression _sym) pEqualsSign) (pHiddenExpression _sym)

pBinaryExpression :: Parser (MoveTerm BinaryExpressionL)
pBinaryExpression = do
  _sym <- pSymbol "binary_expression" SBinaryExpressionSymbol
  choice [ Megaparsec.try (pBinaryExpression1 _sym)
         , Megaparsec.try (pBinaryExpression2 _sym)
         , Megaparsec.try (pBinaryExpression3 _sym)
         , Megaparsec.try (pBinaryExpression4 _sym)
         , Megaparsec.try (pBinaryExpression5 _sym)
         , Megaparsec.try (pBinaryExpression6 _sym)
         , Megaparsec.try (pBinaryExpression7 _sym)
         , Megaparsec.try (pBinaryExpression8 _sym)
         , Megaparsec.try (pBinaryExpression9 _sym)
         , Megaparsec.try (pBinaryExpression10 _sym)
         , Megaparsec.try (pBinaryExpression11 _sym)
         , Megaparsec.try (pBinaryExpression12 _sym)
         , Megaparsec.try (pBinaryExpression13 _sym)
         , Megaparsec.try (pBinaryExpression14 _sym)
         , Megaparsec.try (pBinaryExpression15 _sym)
         , Megaparsec.try (pBinaryExpression16 _sym)
         , Megaparsec.try (pBinaryExpression17 _sym)
         , Megaparsec.try (pBinaryExpression18 _sym)
         , Megaparsec.try (pBinaryExpression19 _sym)
         , Megaparsec.try (pBinaryExpression20 _sym)
         ]
  where
    pBinaryExpression1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression1 _sym =
      iBinaryExpression1
        <$> (pHiddenExpression _sym)
        <*> pEqualsSignequalsSigngreaterThanSign
        <*> (pHiddenExpression _sym)
    pBinaryExpression2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression2 _sym =
      iBinaryExpression2
        <$> (pHiddenExpression _sym)
        <*> pVerticalLineverticalLine
        <*> (pHiddenExpression _sym)
    pBinaryExpression3 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression3 _sym =
      iBinaryExpression3
        <$> (pHiddenExpression _sym)
        <*> pAmpersandampersand
        <*> (pHiddenExpression _sym)
    pBinaryExpression4 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression4 _sym =
      iBinaryExpression4
        <$> (pHiddenExpression _sym)
        <*> pEqualsSignequalsSign
        <*> (pHiddenExpression _sym)
    pBinaryExpression5 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression5 _sym =
      iBinaryExpression5
        <$> (pHiddenExpression _sym)
        <*> pExclamationMarkequalsSign
        <*> (pHiddenExpression _sym)
    pBinaryExpression6 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression6 _sym =
      iBinaryExpression6
        <$> (pHiddenExpression _sym)
        <*> pLessThanSign
        <*> (pHiddenExpression _sym)
    pBinaryExpression7 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression7 _sym =
      iBinaryExpression7
        <$> (pHiddenExpression _sym)
        <*> pGreaterThanSign
        <*> (pHiddenExpression _sym)
    pBinaryExpression8 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression8 _sym =
      iBinaryExpression8
        <$> (pHiddenExpression _sym)
        <*> pLessThanSignequalsSign
        <*> (pHiddenExpression _sym)
    pBinaryExpression9 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression9 _sym =
      iBinaryExpression9
        <$> (pHiddenExpression _sym)
        <*> pGreaterThanSignequalsSign
        <*> (pHiddenExpression _sym)
    pBinaryExpression10 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression10 _sym =
      iBinaryExpression10
        <$> (pHiddenExpression _sym)
        <*> pFullStopfullStop
        <*> (pHiddenExpression _sym)
    pBinaryExpression11 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression11 _sym =
      iBinaryExpression11
        <$> (pHiddenExpression _sym)
        <*> pVerticalLine
        <*> (pHiddenExpression _sym)
    pBinaryExpression12 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression12 _sym =
      iBinaryExpression12
        <$> (pHiddenExpression _sym)
        <*> pCircumflexAccent
        <*> (pHiddenExpression _sym)
    pBinaryExpression13 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression13 _sym =
      iBinaryExpression13
        <$> (pHiddenExpression _sym)
        <*> pAmpersand
        <*> (pHiddenExpression _sym)
    pBinaryExpression14 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression14 _sym =
      iBinaryExpression14
        <$> (pHiddenExpression _sym)
        <*> pLessThanSignlessThanSign
        <*> (pHiddenExpression _sym)
    pBinaryExpression15 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression15 _sym =
      iBinaryExpression15
        <$> (pHiddenExpression _sym)
        <*> pGreaterThanSigngreaterThanSign
        <*> (pHiddenExpression _sym)
    pBinaryExpression16 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression16 _sym =
      iBinaryExpression16
        <$> (pHiddenExpression _sym)
        <*> pPlusSign
        <*> (pHiddenExpression _sym)
    pBinaryExpression17 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression17 _sym =
      iBinaryExpression17
        <$> (pHiddenExpression _sym)
        <*> p
        <*> (pHiddenExpression _sym)
    pBinaryExpression18 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression18 _sym =
      iBinaryExpression18
        <$> (pHiddenExpression _sym)
        <*> pAsterisk
        <*> (pHiddenExpression _sym)
    pBinaryExpression19 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression19 _sym =
      iBinaryExpression19
        <$> (pHiddenExpression _sym)
        <*> pSolidus
        <*> (pHiddenExpression _sym)
    pBinaryExpression20 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression20 _sym =
      iBinaryExpression20
        <$> (pHiddenExpression _sym)
        <*> pPercentSign
        <*> (pHiddenExpression _sym)

pCastExpression :: Parser (MoveTerm CastExpressionL)
pCastExpression = do
  _sym <- pSymbol "cast_expression" SCastExpressionSymbol
  iCastExpression
    <$> pPair (pPair (pHiddenExpression _sym) pAs) (pHiddenType _sym)

pIdentifiedExpression :: Parser (MoveTerm IdentifiedExpressionL)
pIdentifiedExpression = do
  _sym <- pSymbol "identified_expression" SIdentifiedExpressionSymbol
  iIdentifiedExpression
    <$> pBlockIdentifier
    <*> (pHiddenExpression _sym)

pBlockIdentifier :: Parser (MoveTerm BlockIdentifierL)
pBlockIdentifier = do
  _sym <- pSymbol "block_identifier" SBlockIdentifierSymbol
  iBlockIdentifier
    <$> pLabel
    <*> pColon

pLambdaExpression :: Parser (MoveTerm LambdaExpressionL)
pLambdaExpression = do
  _sym <- pSymbol "lambda_expression" SLambdaExpressionSymbol
  iLambdaExpression
    <$> pLambdaBindings
    <*> pMaybe (pPair pGreaterThanSign (pHiddenType _sym))
    <*> (pHiddenExpression _sym)

pLambdaBindings :: Parser (MoveTerm LambdaBindingsL)
pLambdaBindings = do
  _sym <- pSymbol "lambda_bindings" SLambdaBindingsSymbol
  iLambdaBindings
    <$> pMany pLambdaBinding

pLambdaBinding :: Parser (MoveTerm LambdaBindingL)
pLambdaBinding = do
  _sym <- pSymbol "lambda_binding" SLambdaBindingSymbol
  choice [ Megaparsec.try (pLambdaBindingCommaBindList _sym)
         , Megaparsec.try (pLambdaBindingBind _sym)
         , Megaparsec.try (pLambdaBinding3 _sym)
         ]
  where
    pLambdaBindingCommaBindList :: Cubix.TreeSitter.Token a -> Parser (MoveTerm LambdaBindingL)
    pLambdaBindingCommaBindList _sym =
      iLambdaBindingCommaBindList
        <$> pCommaBindList
    pLambdaBindingBind :: Cubix.TreeSitter.Token a -> Parser (MoveTerm LambdaBindingL)
    pLambdaBindingBind _sym =
      iLambdaBindingBind
        <$> (pHiddenBind _sym)
    pLambdaBinding3 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm LambdaBindingL)
    pLambdaBinding3 _sym =
      iLambdaBinding3
        <$> (pHiddenBind _sym)
        <*> pMaybe (pPair pColon (pHiddenType _sym))

pLoopExpression :: Parser (MoveTerm LoopExpressionL)
pLoopExpression = do
  _sym <- pSymbol "loop_expression" SLoopExpressionSymbol
  iLoopExpression
    <$> pLoop
    <*> (pHiddenExpression _sym)

pQuantifierExpression :: Parser (MoveTerm QuantifierExpressionL)
pQuantifierExpression = do
  _sym <- pSymbol "quantifier_expression" SQuantifierExpressionSymbol
  iQuantifierExpression
    <$> pPair (pPair (pPair (pPair (pHiddenReservedIdentifier _sym) pQuantifierBindings) (pMaybe (pPair pWhere (pHiddenExpression _sym)))) pColon) (pHiddenExpression _sym)

pQuantifierBindings :: Parser (MoveTerm QuantifierBindingsL)
pQuantifierBindings = do
  _sym <- pSymbol "quantifier_bindings" SQuantifierBindingsSymbol
  iQuantifierBindings
    <$> pSome pQuantifierBinding

pQuantifierBinding :: Parser (MoveTerm QuantifierBindingL)
pQuantifierBinding = do
  _sym <- pSymbol "quantifier_binding" SQuantifierBindingSymbol
  choice [ Megaparsec.try (pQuantifierBinding1 _sym)
         , Megaparsec.try (pQuantifierBinding2 _sym)
         ]
  where
    pQuantifierBinding1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding1 _sym =
      iQuantifierBinding1
        <$> pIdentifier
        <*> pColon
        <*> (pHiddenType _sym)
    pQuantifierBinding2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding2 _sym =
      iQuantifierBinding2
        <$> pIdentifier
        <*> pIn
        <*> (pHiddenExpression _sym)

pReturnExpression :: Parser (MoveTerm ReturnExpressionL)
pReturnExpression = do
  _sym <- pSymbol "return_expression" SReturnExpressionSymbol
  choice [ Megaparsec.try (pReturnExpression1 _sym)
         , Megaparsec.try (pReturnExpression2 _sym)
         ]
  where
    pReturnExpression1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ReturnExpressionL)
    pReturnExpression1 _sym =
      iReturnExpression1
        <$> pReturn
        <*> pMaybe pLabel
        <*> (pHiddenExpression _sym)
    pReturnExpression2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ReturnExpressionL)
    pReturnExpression2 _sym =
      iReturnExpression2
        <$> pReturn
        <*> pMaybe pLabel

pWhileExpression :: Parser (MoveTerm WhileExpressionL)
pWhileExpression = do
  _sym <- pSymbol "while_expression" SWhileExpressionSymbol
  iWhileExpression
    <$> pWhile
    <*> (pHiddenExpression _sym)
    <*> (pHiddenExpression _sym)

pBlockItem :: Parser (MoveTerm BlockItemL)
pBlockItem = do
  _sym <- pSymbol "block_item" SBlockItemSymbol
  iBlockItem
    <$> (pBlockItemInternal0 _sym)
    <*> pSemicolon

pBlockItemInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BlockItemInternal0L)
pBlockItemInternal0 _sym = do
  choice [ Megaparsec.try (pBlockItemInternal0Expression _sym)
         , Megaparsec.try (pBlockItemInternal0LetStatement _sym)
         ]
  where
    pBlockItemInternal0Expression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BlockItemInternal0L)
    pBlockItemInternal0Expression _sym =
      iBlockItemInternal0Expression
        <$> (pHiddenExpression _sym)
    pBlockItemInternal0LetStatement :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BlockItemInternal0L)
    pBlockItemInternal0LetStatement _sym =
      iBlockItemInternal0LetStatement
        <$> pLetStatement

pLetStatement :: Parser (MoveTerm LetStatementL)
pLetStatement = do
  _sym <- pSymbol "let_statement" SLetStatementSymbol
  iLetStatement
    <$> pLet
    <*> pBindList
    <*> pMaybe (pPair pColon (pHiddenType _sym))
    <*> pMaybe (pPair pEqualsSign (pHiddenExpression _sym))

pMacroFunctionDefinition :: Parser (MoveTerm MacroFunctionDefinitionL)
pMacroFunctionDefinition = do
  _sym <- pSymbol "macro_function_definition" SMacroFunctionDefinitionSymbol
  iMacroFunctionDefinition
    <$> pMaybe pModifier
    <*> pMacro
    <*> (pHiddenMacroSignature _sym)
    <*> pBlock

pHiddenMacroSignature :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenMacroSignatureL)
pHiddenMacroSignature _sym = do
  iHiddenMacroSignature
    <$> pMaybe pModifier
    <*> pFun
    <*> (pHiddenFunctionIdentifier _sym)
    <*> pMaybe pTypeParameters
    <*> pFunctionParameters
    <*> pMaybe pRetType

pNativeFunctionDefinition :: Parser (MoveTerm NativeFunctionDefinitionL)
pNativeFunctionDefinition = do
  _sym <- pSymbol "native_function_definition" SNativeFunctionDefinitionSymbol
  iNativeFunctionDefinition
    <$> (pHiddenFunctionSignature _sym)
    <*> pSemicolon

pHiddenStructItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenStructItemL)
pHiddenStructItem _sym = do
  choice [ Megaparsec.try (pHiddenStructItemNativeStructDefinition _sym)
         , Megaparsec.try (pHiddenStructItemStructDefinition _sym)
         ]
  where
    pHiddenStructItemNativeStructDefinition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenStructItemL)
    pHiddenStructItemNativeStructDefinition _sym =
      iHiddenStructItemNativeStructDefinition
        <$> pNativeStructDefinition
    pHiddenStructItemStructDefinition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenStructItemL)
    pHiddenStructItemStructDefinition _sym =
      iHiddenStructItemStructDefinition
        <$> pStructDefinition

pNativeStructDefinition :: Parser (MoveTerm NativeStructDefinitionL)
pNativeStructDefinition = do
  _sym <- pSymbol "native_struct_definition" SNativeStructDefinitionSymbol
  iNativeStructDefinition
    <$> pMaybe pPublic
    <*> (pHiddenStructSignature _sym)

pHiddenStructSignature :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenStructSignatureL)
pHiddenStructSignature _sym = do
  iHiddenStructSignature
    <$> pStruct
    <*> (pHiddenStructIdentifier _sym)
    <*> pMaybe pTypeParameters
    <*> pMaybe pAbilityDecls

pStructDefinition :: Parser (MoveTerm StructDefinitionL)
pStructDefinition = do
  _sym <- pSymbol "struct_definition" SStructDefinitionSymbol
  iStructDefinition
    <$> pMaybe pPublic
    <*> (pHiddenStructSignature _sym)
    <*> pDatatypeFields
    <*> pMaybe pPostfixAbilityDecls

pConstant :: Parser (MoveTerm ConstantL)
pConstant = do
  _sym <- pSymbol "constant" SConstantSymbol
  iConstant
    <$> pIdentifier
    <*> (pHiddenType _sym)
    <*> (pHiddenExpression _sym)

pFriendDeclaration :: Parser (MoveTerm FriendDeclarationL)
pFriendDeclaration = do
  _sym <- pSymbol "friend_declaration" SFriendDeclarationSymbol
  iFriendDeclaration
    <$> pFriendAccess

pFriendAccess :: Parser (MoveTerm FriendAccessL)
pFriendAccess = do
  _sym <- pSymbol "friend_access" SFriendAccessSymbol
  choice [ Megaparsec.try (pFriendAccessLocalModule _sym)
         , Megaparsec.try (pFriendAccessFullyQualifiedModule _sym)
         ]
  where
    pFriendAccessLocalModule :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FriendAccessL)
    pFriendAccessLocalModule _sym =
      iFriendAccessLocalModule
        <$> pIdentifier
    pFriendAccessFullyQualifiedModule :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FriendAccessL)
    pFriendAccessFullyQualifiedModule _sym =
      iFriendAccessFullyQualifiedModule
        <$> pModuleIdentity
