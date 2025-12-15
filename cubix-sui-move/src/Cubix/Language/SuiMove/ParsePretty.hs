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

pText :: Parser Text
pText = pure mempty

-- reify the types to aid inference, that might get broken
pMaybe :: Typeable a => Parser (MoveTerm a) -> Parser (MoveTerm (Maybe a))
pMaybe = Megaparsec.Cubix.pMaybe

pPair :: (Typeable a, Typeable b) => Parser (MoveTerm a) -> Parser (MoveTerm b) -> Parser (MoveTerm (a , b))
pPair = Megaparsec.Cubix.pPair

pEither :: (Typeable a, Typeable b) => Parser (MoveTerm a) -> Parser (MoveTerm b) -> Parser (MoveTerm (Either a b))
pEither = Megaparsec.Cubix.pEither

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

pSub :: Parser (MoveTerm SubTokL)
pSub = pSymbol "sub" SSubTokSymbol $> iSub

pRange :: Parser (MoveTerm RangeTokL)
pRange = pSymbol "range" SRangeTokSymbol $> iRange

pDiv :: Parser (MoveTerm DivTokL)
pDiv = pSymbol "div" SDivTokSymbol $> iDiv

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
    <$> pModuleBodyInternal0
    <*> pMany pModuleBodyInternal1
    <*> pMaybe pure ()

pModuleBodyInternal0 :: Parser (MoveTerm ModuleBodyInternal0L)
pModuleBodyInternal0 = do
  choice [ Megaparsec.try pModuleBodyInternal0Semicolon
         , Megaparsec.try pModuleBodyInternal0Leftspacecurlyspacebracket
         ]
  where
    pModuleBodyInternal0Semicolon :: Parser (MoveTerm ModuleBodyInternal0L)
    pModuleBodyInternal0Semicolon =
      pure iModuleBodyInternal0Semicolon
        <$> pure ()
    pModuleBodyInternal0Leftspacecurlyspacebracket :: Parser (MoveTerm ModuleBodyInternal0L)
    pModuleBodyInternal0Leftspacecurlyspacebracket =
      pure iModuleBodyInternal0Leftspacecurlyspacebracket
        <$> pure ()

pModuleBodyInternal1 :: Parser (MoveTerm ModuleBodyInternal1L)
pModuleBodyInternal1 = do
  choice [ Megaparsec.try pModuleBodyInternal1UseDeclaration
         , Megaparsec.try pModuleBodyInternal1FriendDeclaration
         , Megaparsec.try pModuleBodyInternal1Constant
         , Megaparsec.try pModuleBodyInternal1FunctionItem
         , Megaparsec.try pModuleBodyInternal1StructItem
         , Megaparsec.try pModuleBodyInternal1EnumItem
         , Megaparsec.try pModuleBodyInternal1SpecBlock
         ]
  where
    pModuleBodyInternal1UseDeclaration :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1UseDeclaration =
      iModuleBodyInternal1UseDeclaration
        <$> pUseDeclaration
    pModuleBodyInternal1FriendDeclaration :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1FriendDeclaration =
      iModuleBodyInternal1FriendDeclaration
        <$> pFriendDeclaration
    pModuleBodyInternal1Constant :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1Constant =
      iModuleBodyInternal1Constant
        <$> pConstant
    pModuleBodyInternal1FunctionItem :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1FunctionItem =
      iModuleBodyInternal1FunctionItem
        <$> pHiddenFunctionItem
    pModuleBodyInternal1StructItem :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1StructItem =
      iModuleBodyInternal1StructItem
        <$> pHiddenStructItem
    pModuleBodyInternal1EnumItem :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1EnumItem =
      iModuleBodyInternal1EnumItem
        <$> pHiddenEnumItem
    pModuleBodyInternal1SpecBlock :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1SpecBlock =
      iModuleBodyInternal1SpecBlock
        <$> pSpecBlock

pHiddenEnumItem :: Parser (MoveTerm HiddenEnumItemL)
pHiddenEnumItem = do
  iHiddenEnumItem
    <$> pEnumDefinition

pEnumDefinition :: Parser (MoveTerm EnumDefinitionL)
pEnumDefinition = do
  _sym <- pSymbol "enum_definition" SEnumDefinitionSymbol
  iEnumDefinition
    <$> pMaybe pPublic
    <*> pHiddenEnumSignature
    <*> pEnumVariants
    <*> pMaybe pPostfixAbilityDecls

pHiddenEnumSignature :: Parser (MoveTerm HiddenEnumSignatureL)
pHiddenEnumSignature = do
  iHiddenEnumSignature
    <$> pure ()
    <*> pHiddenEnumIdentifier
    <*> pMaybe pTypeParameters
    <*> pMaybe pAbilityDecls

pHiddenEnumIdentifier :: Parser (MoveTerm HiddenEnumIdentifierL)
pHiddenEnumIdentifier = do
  iHiddenEnumIdentifier
    <$> pIdentifier

pIdentifier :: Parser (MoveTerm IdentifierL)
pIdentifier = do
  _sym <- pSymbol "identifier" SIdentifierSymbol
  iIdentifier
    <$> pText

pAbilityDecls :: Parser (MoveTerm AbilityDeclsL)
pAbilityDecls = do
  _sym <- pSymbol "ability_decls" SAbilityDeclsSymbol
  iAbilityDecls
    <$> pure ()
    <*> pMany pAbility

pAbility :: Parser (MoveTerm AbilityL)
pAbility = do
  _sym <- pSymbol "ability" SAbilitySymbol
  choice [ Megaparsec.try pAbilityCopy
         , Megaparsec.try pAbilityDrop
         , Megaparsec.try pAbilityStore
         , Megaparsec.try pAbilityKey
         ]
  where
    pAbilityCopy :: Parser (MoveTerm AbilityL)
    pAbilityCopy =
      iAbilityCopy
        <$> pCopy
    pAbilityDrop :: Parser (MoveTerm AbilityL)
    pAbilityDrop =
      iAbilityDrop
        <$> pDrop
    pAbilityStore :: Parser (MoveTerm AbilityL)
    pAbilityStore =
      iAbilityStore
        <$> pStore
    pAbilityKey :: Parser (MoveTerm AbilityL)
    pAbilityKey =
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
    <$> pMaybe pDollar
    <*> pMaybe pPhantom
    <*> pHiddenTypeParameterIdentifier
    <*> pMaybe (pSome pAbility)

pHiddenTypeParameterIdentifier :: Parser (MoveTerm HiddenTypeParameterIdentifierL)
pHiddenTypeParameterIdentifier = do
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
    <$> pHiddenVariantIdentifier
    <*> pMaybe pDatatypeFields

pHiddenVariantIdentifier :: Parser (MoveTerm HiddenVariantIdentifierL)
pHiddenVariantIdentifier = do
  iHiddenVariantIdentifier
    <$> pIdentifier

pDatatypeFields :: Parser (MoveTerm DatatypeFieldsL)
pDatatypeFields = do
  _sym <- pSymbol "datatype_fields" SDatatypeFieldsSymbol
  choice [ Megaparsec.try pDatatypeFieldsPositionalFields
         , Megaparsec.try pDatatypeFieldsNamedFields
         ]
  where
    pDatatypeFieldsPositionalFields :: Parser (MoveTerm DatatypeFieldsL)
    pDatatypeFieldsPositionalFields =
      iDatatypeFieldsPositionalFields
        <$> pPositionalFields
    pDatatypeFieldsNamedFields :: Parser (MoveTerm DatatypeFieldsL)
    pDatatypeFieldsNamedFields =
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
    <$> pHiddenFieldIdentifier
    <*> pure ()
    <*> pHiddenType

pHiddenFieldIdentifier :: Parser (MoveTerm HiddenFieldIdentifierL)
pHiddenFieldIdentifier = do
  iHiddenFieldIdentifier
    <$> pIdentifier

pHiddenType :: Parser (MoveTerm HiddenTypeL)
pHiddenType = do
  choice [ Megaparsec.try pHiddenTypeApplyType
         , Megaparsec.try pHiddenTypeRefType
         , Megaparsec.try pHiddenTypeTupleType
         , Megaparsec.try pHiddenTypeFunctionType
         , Megaparsec.try pHiddenTypePrimitiveType
         ]
  where
    pHiddenTypeApplyType :: Parser (MoveTerm HiddenTypeL)
    pHiddenTypeApplyType =
      iHiddenTypeApplyType
        <$> pApplyType
    pHiddenTypeRefType :: Parser (MoveTerm HiddenTypeL)
    pHiddenTypeRefType =
      iHiddenTypeRefType
        <$> pRefType
    pHiddenTypeTupleType :: Parser (MoveTerm HiddenTypeL)
    pHiddenTypeTupleType =
      iHiddenTypeTupleType
        <$> pTupleType
    pHiddenTypeFunctionType :: Parser (MoveTerm HiddenTypeL)
    pHiddenTypeFunctionType =
      iHiddenTypeFunctionType
        <$> pFunctionType
    pHiddenTypePrimitiveType :: Parser (MoveTerm HiddenTypeL)
    pHiddenTypePrimitiveType =
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
  choice [ Megaparsec.try pModuleAccess1
         , Megaparsec.try pModuleAccess2
         , Megaparsec.try pModuleAccessMember
         , Megaparsec.try pModuleAccess4
         , Megaparsec.try pModuleAccess5
         , Megaparsec.try pModuleAccess6
         , Megaparsec.try pModuleAccess7
         , Megaparsec.try pModuleAccess8
         , Megaparsec.try pModuleAccess9
         ]
  where
    pModuleAccess1 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess1 =
      iModuleAccess1
        <$> pDollar
        <*> pIdentifier
    pModuleAccess2 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess2 =
      iModuleAccess2
        <$> pAt
        <*> pIdentifier
    pModuleAccessMember :: Parser (MoveTerm ModuleAccessL)
    pModuleAccessMember =
      iModuleAccessMember
        <$> pHiddenReservedIdentifier
    pModuleAccess4 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess4 =
      iModuleAccess4
        <$> pIdentifier
        <*> pMaybe pTypeArguments
    pModuleAccess5 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess5 =
      iModuleAccess5
        <$> pHiddenModuleIdentifier
        <*> pMaybe pTypeArguments
        <*> pure ()
        <*> pIdentifier
    pModuleAccess6 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess6 =
      iModuleAccess6
        <$> pModuleIdentity
        <*> pure ()
        <*> pIdentifier
        <*> pTypeArguments
    pModuleAccess7 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess7 =
      iModuleAccess7
        <$> pModuleIdentity
        <*> pMaybe pTypeArguments
    pModuleAccess8 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess8 =
      iModuleAccess8
        <$> pModuleIdentity
        <*> pMaybe pTypeArguments
        <*> pure ()
        <*> pIdentifier
    pModuleAccess9 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess9 =
      iModuleAccess9
        <$> pModuleIdentity
        <*> pure ()
        <*> pIdentifier
        <*> pMaybe pTypeArguments
        <*> pure ()
        <*> pIdentifier

pHiddenModuleIdentifier :: Parser (MoveTerm HiddenModuleIdentifierL)
pHiddenModuleIdentifier = do
  iHiddenModuleIdentifier
    <$> pIdentifier

pHiddenReservedIdentifier :: Parser (MoveTerm HiddenReservedIdentifierL)
pHiddenReservedIdentifier = do
  choice [ Megaparsec.try pHiddenReservedIdentifierForall
         , Megaparsec.try pHiddenReservedIdentifierExists
         ]
  where
    pHiddenReservedIdentifierForall :: Parser (MoveTerm HiddenReservedIdentifierL)
    pHiddenReservedIdentifierForall =
      iHiddenReservedIdentifierForall
        <$> pHiddenForall
    pHiddenReservedIdentifierExists :: Parser (MoveTerm HiddenReservedIdentifierL)
    pHiddenReservedIdentifierExists =
      iHiddenReservedIdentifierExists
        <$> pHiddenExists

pHiddenExists :: Parser (MoveTerm HiddenExistsL)
pHiddenExists = do
  iHiddenExists
    <$> pExists

pHiddenForall :: Parser (MoveTerm HiddenForallL)
pHiddenForall = do
  iHiddenForall
    <$> pForall

pModuleIdentity :: Parser (MoveTerm ModuleIdentityL)
pModuleIdentity = do
  _sym <- pSymbol "module_identity" SModuleIdentitySymbol
  iModuleIdentity
    <$> pModuleIdentityInternal0
    <*> pure ()
    <*> pHiddenModuleIdentifier

pModuleIdentityInternal0 :: Parser (MoveTerm ModuleIdentityInternal0L)
pModuleIdentityInternal0 = do
  choice [ Megaparsec.try pModuleIdentityInternal0NumLiteral
         , Megaparsec.try pModuleIdentityInternal0ModuleIdentifier
         ]
  where
    pModuleIdentityInternal0NumLiteral :: Parser (MoveTerm ModuleIdentityInternal0L)
    pModuleIdentityInternal0NumLiteral =
      iModuleIdentityInternal0NumLiteral
        <$> pNumLiteral
    pModuleIdentityInternal0ModuleIdentifier :: Parser (MoveTerm ModuleIdentityInternal0L)
    pModuleIdentityInternal0ModuleIdentifier =
      iModuleIdentityInternal0ModuleIdentifier
        <$> pHiddenModuleIdentifier

pNumLiteral :: Parser (MoveTerm NumLiteralL)
pNumLiteral = do
  _sym <- pSymbol "num_literal" SNumLiteralSymbol
  iNumLiteral
    <$> pNumLiteralInternal0
    <*> pMaybe pNumLiteralInternal1

pNumLiteralInternal0 :: Parser (MoveTerm NumLiteralInternal0L)
pNumLiteralInternal0 = do
  choice [ Megaparsec.try pNumLiteralInternal01
         , Megaparsec.try pNumLiteralInternal02
         ]
  where
    pNumLiteralInternal01 :: Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal01 =
      iNumLiteralInternal01
        <$> pText
    pNumLiteralInternal02 :: Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal02 =
      iNumLiteralInternal02
        <$> pText

pNumLiteralInternal1 :: Parser (MoveTerm NumLiteralInternal1L)
pNumLiteralInternal1 = do
  choice [ Megaparsec.try pNumLiteralInternal1U8
         , Megaparsec.try pNumLiteralInternal1U16
         , Megaparsec.try pNumLiteralInternal1U32
         , Megaparsec.try pNumLiteralInternal1U64
         , Megaparsec.try pNumLiteralInternal1U128
         , Megaparsec.try pNumLiteralInternal1U256
         ]
  where
    pNumLiteralInternal1U8 :: Parser (MoveTerm NumLiteralInternal1L)
    pNumLiteralInternal1U8 =
      iNumLiteralInternal1U8
        <$> pU8
    pNumLiteralInternal1U16 :: Parser (MoveTerm NumLiteralInternal1L)
    pNumLiteralInternal1U16 =
      iNumLiteralInternal1U16
        <$> pU16
    pNumLiteralInternal1U32 :: Parser (MoveTerm NumLiteralInternal1L)
    pNumLiteralInternal1U32 =
      iNumLiteralInternal1U32
        <$> pU32
    pNumLiteralInternal1U64 :: Parser (MoveTerm NumLiteralInternal1L)
    pNumLiteralInternal1U64 =
      iNumLiteralInternal1U64
        <$> pU64
    pNumLiteralInternal1U128 :: Parser (MoveTerm NumLiteralInternal1L)
    pNumLiteralInternal1U128 =
      iNumLiteralInternal1U128
        <$> pU128
    pNumLiteralInternal1U256 :: Parser (MoveTerm NumLiteralInternal1L)
    pNumLiteralInternal1U256 =
      iNumLiteralInternal1U256
        <$> pU256

pTypeArguments :: Parser (MoveTerm TypeArgumentsL)
pTypeArguments = do
  _sym <- pSymbol "type_arguments" STypeArgumentsSymbol
  iTypeArguments
    <$> pSome pHiddenType

pFunctionType :: Parser (MoveTerm FunctionTypeL)
pFunctionType = do
  _sym <- pSymbol "function_type" SFunctionTypeSymbol
  iFunctionType
    <$> pFunctionTypeParameters
    <*> pMaybe pHiddenType

pFunctionTypeParameters :: Parser (MoveTerm FunctionTypeParametersL)
pFunctionTypeParameters = do
  _sym <- pSymbol "function_type_parameters" SFunctionTypeParametersSymbol
  iFunctionTypeParameters
    <$> pMany pHiddenType

pPrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
pPrimitiveType = do
  _sym <- pSymbol "primitive_type" SPrimitiveTypeSymbol
  choice [ Megaparsec.try pPrimitiveTypeU8
         , Megaparsec.try pPrimitiveTypeU16
         , Megaparsec.try pPrimitiveTypeU32
         , Megaparsec.try pPrimitiveTypeU64
         , Megaparsec.try pPrimitiveTypeU128
         , Megaparsec.try pPrimitiveTypeU256
         , Megaparsec.try pPrimitiveTypeBool
         , Megaparsec.try pPrimitiveTypeAddress
         , Megaparsec.try pPrimitiveTypeSigner
         , Megaparsec.try pPrimitiveTypeBytearray
         ]
  where
    pPrimitiveTypeU8 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU8 =
      iPrimitiveTypeU8
        <$> pU8
    pPrimitiveTypeU16 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU16 =
      iPrimitiveTypeU16
        <$> pU16
    pPrimitiveTypeU32 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU32 =
      iPrimitiveTypeU32
        <$> pU32
    pPrimitiveTypeU64 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU64 =
      iPrimitiveTypeU64
        <$> pU64
    pPrimitiveTypeU128 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU128 =
      iPrimitiveTypeU128
        <$> pU128
    pPrimitiveTypeU256 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU256 =
      iPrimitiveTypeU256
        <$> pU256
    pPrimitiveTypeBool :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeBool =
      iPrimitiveTypeBool
        <$> pBool
    pPrimitiveTypeAddress :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeAddress =
      iPrimitiveTypeAddress
        <$> pAddress
    pPrimitiveTypeSigner :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeSigner =
      iPrimitiveTypeSigner
        <$> pSigner
    pPrimitiveTypeBytearray :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeBytearray =
      iPrimitiveTypeBytearray
        <$> pBytearray

pRefType :: Parser (MoveTerm RefTypeL)
pRefType = do
  _sym <- pSymbol "ref_type" SRefTypeSymbol
  iRefType
    <$> pHiddenReference
    <*> pHiddenType

pHiddenReference :: Parser (MoveTerm HiddenReferenceL)
pHiddenReference = do
  choice [ Megaparsec.try pHiddenReferenceImmRef
         , Megaparsec.try pHiddenReferenceMutRef
         ]
  where
    pHiddenReferenceImmRef :: Parser (MoveTerm HiddenReferenceL)
    pHiddenReferenceImmRef =
      iHiddenReferenceImmRef
        <$> pImmRef
    pHiddenReferenceMutRef :: Parser (MoveTerm HiddenReferenceL)
    pHiddenReferenceMutRef =
      iHiddenReferenceMutRef
        <$> pMutRef

pImmRef :: Parser (MoveTerm ImmRefL)
pImmRef = do
  _sym <- pSymbol "imm_ref" SImmRefSymbol
  iImmRef
    <$> pBitand

pMutRef :: Parser (MoveTerm MutRefL)
pMutRef = do
  _sym <- pSymbol "mut_ref" SMutRefSymbol
  iMutRef
    <$> pBitand
    <*> pure ()

pTupleType :: Parser (MoveTerm TupleTypeL)
pTupleType = do
  _sym <- pSymbol "tuple_type" STupleTypeSymbol
  iTupleType
    <$> pMany pHiddenType

pPositionalFields :: Parser (MoveTerm PositionalFieldsL)
pPositionalFields = do
  _sym <- pSymbol "positional_fields" SPositionalFieldsSymbol
  iPositionalFields
    <$> pMany pHiddenType

pPostfixAbilityDecls :: Parser (MoveTerm PostfixAbilityDeclsL)
pPostfixAbilityDecls = do
  _sym <- pSymbol "postfix_ability_decls" SPostfixAbilityDeclsSymbol
  iPostfixAbilityDecls
    <$> pMany pAbility

pHiddenFunctionItem :: Parser (MoveTerm HiddenFunctionItemL)
pHiddenFunctionItem = do
  choice [ Megaparsec.try pHiddenFunctionItemNativeFunctionDefinition
         , Megaparsec.try pHiddenFunctionItemMacroFunctionDefinition
         , Megaparsec.try pHiddenFunctionItemFunctionDefinition
         ]
  where
    pHiddenFunctionItemNativeFunctionDefinition :: Parser (MoveTerm HiddenFunctionItemL)
    pHiddenFunctionItemNativeFunctionDefinition =
      iHiddenFunctionItemNativeFunctionDefinition
        <$> pNativeFunctionDefinition
    pHiddenFunctionItemMacroFunctionDefinition :: Parser (MoveTerm HiddenFunctionItemL)
    pHiddenFunctionItemMacroFunctionDefinition =
      iHiddenFunctionItemMacroFunctionDefinition
        <$> pMacroFunctionDefinition
    pHiddenFunctionItemFunctionDefinition :: Parser (MoveTerm HiddenFunctionItemL)
    pHiddenFunctionItemFunctionDefinition =
      iHiddenFunctionItemFunctionDefinition
        <$> pFunctionDefinition

pFunctionDefinition :: Parser (MoveTerm FunctionDefinitionL)
pFunctionDefinition = do
  _sym <- pSymbol "function_definition" SFunctionDefinitionSymbol
  iFunctionDefinition
    <$> pHiddenFunctionSignature
    <*> pBlock

pHiddenFunctionSignature :: Parser (MoveTerm HiddenFunctionSignatureL)
pHiddenFunctionSignature = do
  iHiddenFunctionSignature
    <$> pMaybe pModifier
    <*> pMaybe pModifier
    <*> pMaybe pModifier
    <*> pure ()
    <*> pHiddenFunctionIdentifier
    <*> pMaybe pTypeParameters
    <*> pFunctionParameters
    <*> pMaybe pRetType

pHiddenFunctionIdentifier :: Parser (MoveTerm HiddenFunctionIdentifierL)
pHiddenFunctionIdentifier = do
  iHiddenFunctionIdentifier
    <$> pIdentifier

pFunctionParameters :: Parser (MoveTerm FunctionParametersL)
pFunctionParameters = do
  _sym <- pSymbol "function_parameters" SFunctionParametersSymbol
  iFunctionParameters
    <$> pMany pFunctionParametersInternal0

pFunctionParametersInternal0 :: Parser (MoveTerm FunctionParametersInternal0L)
pFunctionParametersInternal0 = do
  choice [ Megaparsec.try pFunctionParametersInternal0MutFunctionParameter
         , Megaparsec.try pFunctionParametersInternal0FunctionParameter
         ]
  where
    pFunctionParametersInternal0MutFunctionParameter :: Parser (MoveTerm FunctionParametersInternal0L)
    pFunctionParametersInternal0MutFunctionParameter =
      iFunctionParametersInternal0MutFunctionParameter
        <$> pMutFunctionParameter
    pFunctionParametersInternal0FunctionParameter :: Parser (MoveTerm FunctionParametersInternal0L)
    pFunctionParametersInternal0FunctionParameter =
      iFunctionParametersInternal0FunctionParameter
        <$> pFunctionParameter

pFunctionParameter :: Parser (MoveTerm FunctionParameterL)
pFunctionParameter = do
  _sym <- pSymbol "function_parameter" SFunctionParameterSymbol
  iFunctionParameter
    <$> pFunctionParameterInternal0
    <*> pure ()
    <*> pHiddenType

pFunctionParameterInternal0 :: Parser (MoveTerm FunctionParameterInternal0L)
pFunctionParameterInternal0 = do
  choice [ Megaparsec.try pFunctionParameterInternal0Name
         , Megaparsec.try pFunctionParameterInternal02
         ]
  where
    pFunctionParameterInternal0Name :: Parser (MoveTerm FunctionParameterInternal0L)
    pFunctionParameterInternal0Name =
      iFunctionParameterInternal0Name
        <$> pHiddenVariableIdentifier
    pFunctionParameterInternal02 :: Parser (MoveTerm FunctionParameterInternal0L)
    pFunctionParameterInternal02 =
      iFunctionParameterInternal02
        <$> pDollar
        <*> pHiddenVariableIdentifier

pHiddenVariableIdentifier :: Parser (MoveTerm HiddenVariableIdentifierL)
pHiddenVariableIdentifier = do
  iHiddenVariableIdentifier
    <$> pIdentifier

pMutFunctionParameter :: Parser (MoveTerm MutFunctionParameterL)
pMutFunctionParameter = do
  _sym <- pSymbol "mut_function_parameter" SMutFunctionParameterSymbol
  iMutFunctionParameter
    <$> pure ()
    <*> pFunctionParameter

pModifier :: Parser (MoveTerm ModifierL)
pModifier = do
  _sym <- pSymbol "modifier" SModifierSymbol
  choice [ Megaparsec.try pModifier1
         , Megaparsec.try pModifierEntry
         , Megaparsec.try pModifierNative
         ]
  where
    pModifier1 :: Parser (MoveTerm ModifierL)
    pModifier1 =
      iModifier1
        <$> pPublic
        <*> pMaybe pModifierInternal0
    pModifierEntry :: Parser (MoveTerm ModifierL)
    pModifierEntry =
      iModifierEntry
        <$> pEntry
    pModifierNative :: Parser (MoveTerm ModifierL)
    pModifierNative =
      iModifierNative
        <$> pNative

pModifierInternal0 :: Parser (MoveTerm ModifierInternal0L)
pModifierInternal0 = do
  choice [ Megaparsec.try pModifierInternal0Package
         , Megaparsec.try pModifierInternal0Friend
         ]
  where
    pModifierInternal0Package :: Parser (MoveTerm ModifierInternal0L)
    pModifierInternal0Package =
      iModifierInternal0Package
        <$> pPackage
    pModifierInternal0Friend :: Parser (MoveTerm ModifierInternal0L)
    pModifierInternal0Friend =
      iModifierInternal0Friend
        <$> pFriend

pRetType :: Parser (MoveTerm RetTypeL)
pRetType = do
  _sym <- pSymbol "ret_type" SRetTypeSymbol
  iRetType
    <$> pure ()
    <*> pHiddenType

pBlock :: Parser (MoveTerm BlockL)
pBlock = do
  _sym <- pSymbol "block" SBlockSymbol
  iBlock
    <$> pPair (pPair (pMany pUseDeclaration) (pMany pBlockItem)) (pMaybe pHiddenExpression)

pHiddenExpression :: Parser (MoveTerm HiddenExpressionL)
pHiddenExpression = do
  choice [ Megaparsec.try pHiddenExpressionCallExpression
         , Megaparsec.try pHiddenExpressionMacroCallExpression
         , Megaparsec.try pHiddenExpressionLambdaExpression
         , Megaparsec.try pHiddenExpressionIfExpression
         , Megaparsec.try pHiddenExpressionWhileExpression
         , Megaparsec.try pHiddenExpressionReturnExpression
         , Megaparsec.try pHiddenExpressionAbortExpression
         , Megaparsec.try pHiddenExpressionAssignExpression
         , Megaparsec.try pHiddenExpressionUnaryExpression
         , Megaparsec.try pHiddenExpressionBinaryExpression
         , Megaparsec.try pHiddenExpressionCastExpression
         , Megaparsec.try pHiddenExpressionQuantifierExpression
         , Megaparsec.try pHiddenExpressionMatchExpression
         , Megaparsec.try pHiddenExpressionVectorExpression
         , Megaparsec.try pHiddenExpressionLoopExpression
         , Megaparsec.try pHiddenExpressionIdentifiedExpression
         ]
  where
    pHiddenExpressionCallExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionCallExpression =
      iHiddenExpressionCallExpression
        <$> pCallExpression
    pHiddenExpressionMacroCallExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionMacroCallExpression =
      iHiddenExpressionMacroCallExpression
        <$> pMacroCallExpression
    pHiddenExpressionLambdaExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionLambdaExpression =
      iHiddenExpressionLambdaExpression
        <$> pLambdaExpression
    pHiddenExpressionIfExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionIfExpression =
      iHiddenExpressionIfExpression
        <$> pIfExpression
    pHiddenExpressionWhileExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionWhileExpression =
      iHiddenExpressionWhileExpression
        <$> pWhileExpression
    pHiddenExpressionReturnExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionReturnExpression =
      iHiddenExpressionReturnExpression
        <$> pReturnExpression
    pHiddenExpressionAbortExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionAbortExpression =
      iHiddenExpressionAbortExpression
        <$> pAbortExpression
    pHiddenExpressionAssignExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionAssignExpression =
      iHiddenExpressionAssignExpression
        <$> pAssignExpression
    pHiddenExpressionUnaryExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionUnaryExpression =
      iHiddenExpressionUnaryExpression
        <$> pHiddenUnaryExpression
    pHiddenExpressionBinaryExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionBinaryExpression =
      iHiddenExpressionBinaryExpression
        <$> pBinaryExpression
    pHiddenExpressionCastExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionCastExpression =
      iHiddenExpressionCastExpression
        <$> pCastExpression
    pHiddenExpressionQuantifierExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionQuantifierExpression =
      iHiddenExpressionQuantifierExpression
        <$> pQuantifierExpression
    pHiddenExpressionMatchExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionMatchExpression =
      iHiddenExpressionMatchExpression
        <$> pMatchExpression
    pHiddenExpressionVectorExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionVectorExpression =
      iHiddenExpressionVectorExpression
        <$> pVectorExpression
    pHiddenExpressionLoopExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionLoopExpression =
      iHiddenExpressionLoopExpression
        <$> pLoopExpression
    pHiddenExpressionIdentifiedExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionIdentifiedExpression =
      iHiddenExpressionIdentifiedExpression
        <$> pIdentifiedExpression

pHiddenUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
pHiddenUnaryExpression = do
  iHiddenUnaryExpression
    <$> pHiddenUnaryExpressionInternal0

pHiddenUnaryExpressionInternal0 :: Parser (MoveTerm HiddenUnaryExpressionInternal0L)
pHiddenUnaryExpressionInternal0 = do
  choice [ Megaparsec.try pHiddenUnaryExpressionInternal0UnaryExpression
         , Megaparsec.try pHiddenUnaryExpressionInternal0BorrowExpression
         , Megaparsec.try pHiddenUnaryExpressionInternal0DereferenceExpression
         , Megaparsec.try pHiddenUnaryExpressionInternal0MoveOrCopyExpression
         , Megaparsec.try pHiddenUnaryExpressionInternal0ExpressionTerm
         ]
  where
    pHiddenUnaryExpressionInternal0UnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0UnaryExpression =
      iHiddenUnaryExpressionInternal0UnaryExpression
        <$> pUnaryExpression
    pHiddenUnaryExpressionInternal0BorrowExpression :: Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0BorrowExpression =
      iHiddenUnaryExpressionInternal0BorrowExpression
        <$> pBorrowExpression
    pHiddenUnaryExpressionInternal0DereferenceExpression :: Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0DereferenceExpression =
      iHiddenUnaryExpressionInternal0DereferenceExpression
        <$> pDereferenceExpression
    pHiddenUnaryExpressionInternal0MoveOrCopyExpression :: Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0MoveOrCopyExpression =
      iHiddenUnaryExpressionInternal0MoveOrCopyExpression
        <$> pMoveOrCopyExpression
    pHiddenUnaryExpressionInternal0ExpressionTerm :: Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0ExpressionTerm =
      iHiddenUnaryExpressionInternal0ExpressionTerm
        <$> pHiddenExpressionTerm

pHiddenExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
pHiddenExpressionTerm = do
  choice [ Megaparsec.try pHiddenExpressionTermCallExpression
         , Megaparsec.try pHiddenExpressionTermBreakExpression
         , Megaparsec.try pHiddenExpressionTermContinueExpression
         , Megaparsec.try pHiddenExpressionTermNameExpression
         , Megaparsec.try pHiddenExpressionTermMacroCallExpression
         , Megaparsec.try pHiddenExpressionTermPackExpression
         , Megaparsec.try pHiddenExpressionTermLiteralValue
         , Megaparsec.try pHiddenExpressionTermUnitExpression
         , Megaparsec.try pHiddenExpressionTermExpressionList
         , Megaparsec.try pHiddenExpressionTermAnnotationExpression
         , Megaparsec.try pHiddenExpressionTermBlock
         , Megaparsec.try pHiddenExpressionTermSpecBlock
         , Megaparsec.try pHiddenExpressionTermIfExpression
         , Megaparsec.try pHiddenExpressionTermDotExpression
         , Megaparsec.try pHiddenExpressionTermIndexExpression
         , Megaparsec.try pHiddenExpressionTermVectorExpression
         , Megaparsec.try pHiddenExpressionTermMatchExpression
         ]
  where
    pHiddenExpressionTermCallExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermCallExpression =
      iHiddenExpressionTermCallExpression
        <$> pCallExpression
    pHiddenExpressionTermBreakExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermBreakExpression =
      iHiddenExpressionTermBreakExpression
        <$> pBreakExpression
    pHiddenExpressionTermContinueExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermContinueExpression =
      iHiddenExpressionTermContinueExpression
        <$> pContinueExpression
    pHiddenExpressionTermNameExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermNameExpression =
      iHiddenExpressionTermNameExpression
        <$> pNameExpression
    pHiddenExpressionTermMacroCallExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermMacroCallExpression =
      iHiddenExpressionTermMacroCallExpression
        <$> pMacroCallExpression
    pHiddenExpressionTermPackExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermPackExpression =
      iHiddenExpressionTermPackExpression
        <$> pPackExpression
    pHiddenExpressionTermLiteralValue :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermLiteralValue =
      iHiddenExpressionTermLiteralValue
        <$> pHiddenLiteralValue
    pHiddenExpressionTermUnitExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermUnitExpression =
      iHiddenExpressionTermUnitExpression
        <$> pUnitExpression
    pHiddenExpressionTermExpressionList :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermExpressionList =
      iHiddenExpressionTermExpressionList
        <$> pExpressionList
    pHiddenExpressionTermAnnotationExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermAnnotationExpression =
      iHiddenExpressionTermAnnotationExpression
        <$> pAnnotationExpression
    pHiddenExpressionTermBlock :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermBlock =
      iHiddenExpressionTermBlock
        <$> pBlock
    pHiddenExpressionTermSpecBlock :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermSpecBlock =
      iHiddenExpressionTermSpecBlock
        <$> pSpecBlock
    pHiddenExpressionTermIfExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermIfExpression =
      iHiddenExpressionTermIfExpression
        <$> pIfExpression
    pHiddenExpressionTermDotExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermDotExpression =
      iHiddenExpressionTermDotExpression
        <$> pDotExpression
    pHiddenExpressionTermIndexExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermIndexExpression =
      iHiddenExpressionTermIndexExpression
        <$> pIndexExpression
    pHiddenExpressionTermVectorExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermVectorExpression =
      iHiddenExpressionTermVectorExpression
        <$> pVectorExpression
    pHiddenExpressionTermMatchExpression :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermMatchExpression =
      iHiddenExpressionTermMatchExpression
        <$> pMatchExpression

pHiddenLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
pHiddenLiteralValue = do
  choice [ Megaparsec.try pHiddenLiteralValueAddressLiteral
         , Megaparsec.try pHiddenLiteralValueBoolLiteral
         , Megaparsec.try pHiddenLiteralValueNumLiteral
         , Megaparsec.try pHiddenLiteralValueHexStringLiteral
         , Megaparsec.try pHiddenLiteralValueByteStringLiteral
         ]
  where
    pHiddenLiteralValueAddressLiteral :: Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueAddressLiteral =
      iHiddenLiteralValueAddressLiteral
        <$> pAddressLiteral
    pHiddenLiteralValueBoolLiteral :: Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueBoolLiteral =
      iHiddenLiteralValueBoolLiteral
        <$> pBoolLiteral
    pHiddenLiteralValueNumLiteral :: Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueNumLiteral =
      iHiddenLiteralValueNumLiteral
        <$> pNumLiteral
    pHiddenLiteralValueHexStringLiteral :: Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueHexStringLiteral =
      iHiddenLiteralValueHexStringLiteral
        <$> pHexStringLiteral
    pHiddenLiteralValueByteStringLiteral :: Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueByteStringLiteral =
      iHiddenLiteralValueByteStringLiteral
        <$> pByteStringLiteral

pAddressLiteral :: Parser (MoveTerm AddressLiteralL)
pAddressLiteral = do
  _sym <- pSymbol "address_literal" SAddressLiteralSymbol
  iAddressLiteral
    <$> pText

pBoolLiteral :: Parser (MoveTerm BoolLiteralL)
pBoolLiteral = do
  _sym <- pSymbol "bool_literal" SBoolLiteralSymbol
  choice [ Megaparsec.try pBoolLiteralTrue
         , Megaparsec.try pBoolLiteralFalse
         ]
  where
    pBoolLiteralTrue :: Parser (MoveTerm BoolLiteralL)
    pBoolLiteralTrue =
      iBoolLiteralTrue
        <$> pTrue
    pBoolLiteralFalse :: Parser (MoveTerm BoolLiteralL)
    pBoolLiteralFalse =
      iBoolLiteralFalse
        <$> pFalse

pByteStringLiteral :: Parser (MoveTerm ByteStringLiteralL)
pByteStringLiteral = do
  _sym <- pSymbol "byte_string_literal" SByteStringLiteralSymbol
  iByteStringLiteral
    <$> pText

pHexStringLiteral :: Parser (MoveTerm HexStringLiteralL)
pHexStringLiteral = do
  _sym <- pSymbol "hex_string_literal" SHexStringLiteralSymbol
  iHexStringLiteral
    <$> pText

pAnnotationExpression :: Parser (MoveTerm AnnotationExpressionL)
pAnnotationExpression = do
  _sym <- pSymbol "annotation_expression" SAnnotationExpressionSymbol
  iAnnotationExpression
    <$> pPair pHiddenExpression pHiddenType

pBreakExpression :: Parser (MoveTerm BreakExpressionL)
pBreakExpression = do
  _sym <- pSymbol "break_expression" SBreakExpressionSymbol
  iBreakExpression
    <$> pure ()
    <*> pMaybe pLabel
    <*> pMaybe pHiddenExpression

pLabel :: Parser (MoveTerm LabelL)
pLabel = do
  _sym <- pSymbol "label" SLabelSymbol
  iLabel
    <$> pure ()
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
    <$> pMany pHiddenExpression

pNameExpression :: Parser (MoveTerm NameExpressionL)
pNameExpression = do
  _sym <- pSymbol "name_expression" SNameExpressionSymbol
  iNameExpression
    <$> pMaybe pure ()
    <*> pModuleAccess

pContinueExpression :: Parser (MoveTerm ContinueExpressionL)
pContinueExpression = do
  _sym <- pSymbol "continue_expression" SContinueExpressionSymbol
  iContinueExpression
    <$> pure ()
    <*> pMaybe pLabel

pDotExpression :: Parser (MoveTerm DotExpressionL)
pDotExpression = do
  _sym <- pSymbol "dot_expression" SDotExpressionSymbol
  iDotExpression
    <$> pPair pHiddenExpressionTerm pHiddenExpressionTerm

pExpressionList :: Parser (MoveTerm ExpressionListL)
pExpressionList = do
  _sym <- pSymbol "expression_list" SExpressionListSymbol
  iExpressionList
    <$> pSome pHiddenExpression

pIfExpression :: Parser (MoveTerm IfExpressionL)
pIfExpression = do
  _sym <- pSymbol "if_expression" SIfExpressionSymbol
  iIfExpression
    <$> pPair (pPair pHiddenExpression pHiddenExpression) (pMaybe pHiddenExpression)

pIndexExpression :: Parser (MoveTerm IndexExpressionL)
pIndexExpression = do
  _sym <- pSymbol "index_expression" SIndexExpressionSymbol
  iIndexExpression
    <$> pPair pHiddenExpressionTerm (pMany pHiddenExpression)

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
    <*> pBang

pMatchExpression :: Parser (MoveTerm MatchExpressionL)
pMatchExpression = do
  _sym <- pSymbol "match_expression" SMatchExpressionSymbol
  iMatchExpression
    <$> pure ()
    <*> pure ()
    <*> pHiddenExpression
    <*> pure ()
    <*> pHiddenMatchBody

pHiddenMatchBody :: Parser (MoveTerm HiddenMatchBodyL)
pHiddenMatchBody = do
  iHiddenMatchBody
    <$> pMany pMatchArm

pMatchArm :: Parser (MoveTerm MatchArmL)
pMatchArm = do
  _sym <- pSymbol "match_arm" SMatchArmSymbol
  iMatchArm
    <$> pBindList
    <*> pMaybe pMatchCondition
    <*> pure ()
    <*> pHiddenExpression

pBindList :: Parser (MoveTerm BindListL)
pBindList = do
  _sym <- pSymbol "bind_list" SBindListSymbol
  choice [ Megaparsec.try pBindListBind
         , Megaparsec.try pBindListCommaBindList
         , Megaparsec.try pBindListOrBindList
         ]
  where
    pBindListBind :: Parser (MoveTerm BindListL)
    pBindListBind =
      iBindListBind
        <$> pHiddenBind
    pBindListCommaBindList :: Parser (MoveTerm BindListL)
    pBindListCommaBindList =
      iBindListCommaBindList
        <$> pCommaBindList
    pBindListOrBindList :: Parser (MoveTerm BindListL)
    pBindListOrBindList =
      iBindListOrBindList
        <$> pOrBindList

pHiddenBind :: Parser (MoveTerm HiddenBindL)
pHiddenBind = do
  choice [ Megaparsec.try pHiddenBindBindInternal0
         , Megaparsec.try pHiddenBindBindUnpack
         , Megaparsec.try pHiddenBindAtBind
         , Megaparsec.try pHiddenBindLiteralValue
         ]
  where
    pHiddenBindBindInternal0 :: Parser (MoveTerm HiddenBindL)
    pHiddenBindBindInternal0 =
      iHiddenBindBindInternal0
        <$> pHiddenBindInternal0
    pHiddenBindBindUnpack :: Parser (MoveTerm HiddenBindL)
    pHiddenBindBindUnpack =
      iHiddenBindBindUnpack
        <$> pBindUnpack
    pHiddenBindAtBind :: Parser (MoveTerm HiddenBindL)
    pHiddenBindAtBind =
      iHiddenBindAtBind
        <$> pAtBind
    pHiddenBindLiteralValue :: Parser (MoveTerm HiddenBindL)
    pHiddenBindLiteralValue =
      iHiddenBindLiteralValue
        <$> pHiddenLiteralValue

pHiddenBindInternal0 :: Parser (MoveTerm HiddenBindInternal0L)
pHiddenBindInternal0 = do
  choice [ Megaparsec.try pHiddenBindInternal0MutBindVar
         , Megaparsec.try pHiddenBindInternal0VariableIdentifier
         ]
  where
    pHiddenBindInternal0MutBindVar :: Parser (MoveTerm HiddenBindInternal0L)
    pHiddenBindInternal0MutBindVar =
      iHiddenBindInternal0MutBindVar
        <$> pMutBindVar
    pHiddenBindInternal0VariableIdentifier :: Parser (MoveTerm HiddenBindInternal0L)
    pHiddenBindInternal0VariableIdentifier =
      iHiddenBindInternal0VariableIdentifier
        <$> pHiddenVariableIdentifier

pMutBindVar :: Parser (MoveTerm MutBindVarL)
pMutBindVar = do
  _sym <- pSymbol "mut_bind_var" SMutBindVarSymbol
  iMutBindVar
    <$> pure ()
    <*> pHiddenVariableIdentifier

pAtBind :: Parser (MoveTerm AtBindL)
pAtBind = do
  _sym <- pSymbol "at_bind" SAtBindSymbol
  iAtBind
    <$> pHiddenVariableIdentifier
    <*> pAt
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
  choice [ Megaparsec.try pBindFieldsBindPositionalFields
         , Megaparsec.try pBindFieldsBindNamedFields
         ]
  where
    pBindFieldsBindPositionalFields :: Parser (MoveTerm BindFieldsL)
    pBindFieldsBindPositionalFields =
      iBindFieldsBindPositionalFields
        <$> pBindPositionalFields
    pBindFieldsBindNamedFields :: Parser (MoveTerm BindFieldsL)
    pBindFieldsBindNamedFields =
      iBindFieldsBindNamedFields
        <$> pBindNamedFields

pBindNamedFields :: Parser (MoveTerm BindNamedFieldsL)
pBindNamedFields = do
  _sym <- pSymbol "bind_named_fields" SBindNamedFieldsSymbol
  iBindNamedFields
    <$> pMany pBindNamedFieldsInternal0

pBindNamedFieldsInternal0 :: Parser (MoveTerm BindNamedFieldsInternal0L)
pBindNamedFieldsInternal0 = do
  choice [ Megaparsec.try pBindNamedFieldsInternal0BindField
         , Megaparsec.try pBindNamedFieldsInternal0MutBindField
         ]
  where
    pBindNamedFieldsInternal0BindField :: Parser (MoveTerm BindNamedFieldsInternal0L)
    pBindNamedFieldsInternal0BindField =
      iBindNamedFieldsInternal0BindField
        <$> pBindField
    pBindNamedFieldsInternal0MutBindField :: Parser (MoveTerm BindNamedFieldsInternal0L)
    pBindNamedFieldsInternal0MutBindField =
      iBindNamedFieldsInternal0MutBindField
        <$> pMutBindField

pBindField :: Parser (MoveTerm BindFieldL)
pBindField = do
  _sym <- pSymbol "bind_field" SBindFieldSymbol
  choice [ Megaparsec.try pBindField1
         , Megaparsec.try pBindFieldSpreadOperator
         ]
  where
    pBindField1 :: Parser (MoveTerm BindFieldL)
    pBindField1 =
      iBindField1
        <$> pBindList
        <*> pMaybe pBindList
    pBindFieldSpreadOperator :: Parser (MoveTerm BindFieldL)
    pBindFieldSpreadOperator =
      iBindFieldSpreadOperator
        <$> pHiddenSpreadOperator

pHiddenSpreadOperator :: Parser (MoveTerm HiddenSpreadOperatorL)
pHiddenSpreadOperator = do
  iHiddenSpreadOperator
    <$> pRange

pMutBindField :: Parser (MoveTerm MutBindFieldL)
pMutBindField = do
  _sym <- pSymbol "mut_bind_field" SMutBindFieldSymbol
  iMutBindField
    <$> pure ()
    <*> pBindField

pBindPositionalFields :: Parser (MoveTerm BindPositionalFieldsL)
pBindPositionalFields = do
  _sym <- pSymbol "bind_positional_fields" SBindPositionalFieldsSymbol
  iBindPositionalFields
    <$> pMany pBindNamedFieldsInternal0

pCommaBindList :: Parser (MoveTerm CommaBindListL)
pCommaBindList = do
  _sym <- pSymbol "comma_bind_list" SCommaBindListSymbol
  iCommaBindList
    <$> pMany pHiddenBind

pOrBindList :: Parser (MoveTerm OrBindListL)
pOrBindList = do
  _sym <- pSymbol "or_bind_list" SOrBindListSymbol
  pure iOrBindList
    <$> pMaybe pure ()
    <*> pSome (pPair (pPair (pMaybe pure ()) pHiddenBind) (pMaybe pure ()))
    <*> pMaybe pure ()

pMatchCondition :: Parser (MoveTerm MatchConditionL)
pMatchCondition = do
  _sym <- pSymbol "match_condition" SMatchConditionSymbol
  iMatchCondition
    <$> pHiddenExpression

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
    <$> pHiddenFieldIdentifier
    <*> pMaybe pHiddenExpression

pSpecBlock :: Parser (MoveTerm SpecBlockL)
pSpecBlock = do
  _sym <- pSymbol "spec_block" SSpecBlockSymbol
  iSpecBlock
    <$> pure ()
    <*> pSpecBlockInternal0

pSpecBlockInternal0 :: Parser (MoveTerm SpecBlockInternal0L)
pSpecBlockInternal0 = do
  choice [ Megaparsec.try pSpecBlockInternal01
         , Megaparsec.try pSpecBlockInternal0SpecFunction
         ]
  where
    pSpecBlockInternal01 :: Parser (MoveTerm SpecBlockInternal0L)
    pSpecBlockInternal01 =
      iSpecBlockInternal01
        <$> pMaybe pHiddenSpecBlockTarget
        <*> pSpecBody
    pSpecBlockInternal0SpecFunction :: Parser (MoveTerm SpecBlockInternal0L)
    pSpecBlockInternal0SpecFunction =
      iSpecBlockInternal0SpecFunction
        <$> pHiddenSpecFunction

pHiddenSpecBlockTarget :: Parser (MoveTerm HiddenSpecBlockTargetL)
pHiddenSpecBlockTarget = do
  choice [ Megaparsec.try pHiddenSpecBlockTargetIdentifier
         , Megaparsec.try pHiddenSpecBlockTargetModule
         , Megaparsec.try pHiddenSpecBlockTargetSpecBlockTargetSchema
         ]
  where
    pHiddenSpecBlockTargetIdentifier :: Parser (MoveTerm HiddenSpecBlockTargetL)
    pHiddenSpecBlockTargetIdentifier =
      iHiddenSpecBlockTargetIdentifier
        <$> pIdentifier
    pHiddenSpecBlockTargetModule :: Parser (MoveTerm HiddenSpecBlockTargetL)
    pHiddenSpecBlockTargetModule =
      iHiddenSpecBlockTargetModule
        <$> pModule
    pHiddenSpecBlockTargetSpecBlockTargetSchema :: Parser (MoveTerm HiddenSpecBlockTargetL)
    pHiddenSpecBlockTargetSpecBlockTargetSchema =
      iHiddenSpecBlockTargetSpecBlockTargetSchema
        <$> pSpecBlockTargetSchema

pSpecBlockTargetSchema :: Parser (MoveTerm SpecBlockTargetSchemaL)
pSpecBlockTargetSchema = do
  _sym <- pSymbol "spec_block_target_schema" SSpecBlockTargetSchemaSymbol
  iSpecBlockTargetSchema
    <$> pure ()
    <*> pHiddenStructIdentifier
    <*> pMaybe pTypeParameters

pHiddenStructIdentifier :: Parser (MoveTerm HiddenStructIdentifierL)
pHiddenStructIdentifier = do
  iHiddenStructIdentifier
    <$> pIdentifier

pHiddenSpecFunction :: Parser (MoveTerm HiddenSpecFunctionL)
pHiddenSpecFunction = do
  choice [ Megaparsec.try pHiddenSpecFunctionNativeSpecFunction
         , Megaparsec.try pHiddenSpecFunctionUsualSpecFunction
         , Megaparsec.try pHiddenSpecFunctionUninterpretedSpecFunction
         ]
  where
    pHiddenSpecFunctionNativeSpecFunction :: Parser (MoveTerm HiddenSpecFunctionL)
    pHiddenSpecFunctionNativeSpecFunction =
      iHiddenSpecFunctionNativeSpecFunction
        <$> pNativeSpecFunction
    pHiddenSpecFunctionUsualSpecFunction :: Parser (MoveTerm HiddenSpecFunctionL)
    pHiddenSpecFunctionUsualSpecFunction =
      iHiddenSpecFunctionUsualSpecFunction
        <$> pUsualSpecFunction
    pHiddenSpecFunctionUninterpretedSpecFunction :: Parser (MoveTerm HiddenSpecFunctionL)
    pHiddenSpecFunctionUninterpretedSpecFunction =
      iHiddenSpecFunctionUninterpretedSpecFunction
        <$> pUninterpretedSpecFunction

pNativeSpecFunction :: Parser (MoveTerm NativeSpecFunctionL)
pNativeSpecFunction = do
  _sym <- pSymbol "native_spec_function" SNativeSpecFunctionSymbol
  iNativeSpecFunction
    <$> pHiddenSpecFunctionSignature

pHiddenSpecFunctionSignature :: Parser (MoveTerm HiddenSpecFunctionSignatureL)
pHiddenSpecFunctionSignature = do
  iHiddenSpecFunctionSignature
    <$> pHiddenFunctionIdentifier
    <*> pMaybe pTypeParameters
    <*> pFunctionParameters
    <*> pRetType

pUninterpretedSpecFunction :: Parser (MoveTerm UninterpretedSpecFunctionL)
pUninterpretedSpecFunction = do
  _sym <- pSymbol "uninterpreted_spec_function" SUninterpretedSpecFunctionSymbol
  iUninterpretedSpecFunction
    <$> pHiddenSpecFunctionSignature

pUsualSpecFunction :: Parser (MoveTerm UsualSpecFunctionL)
pUsualSpecFunction = do
  _sym <- pSymbol "usual_spec_function" SUsualSpecFunctionSymbol
  iUsualSpecFunction
    <$> pure ()
    <*> pHiddenSpecFunctionSignature
    <*> pBlock

pSpecBody :: Parser (MoveTerm SpecBodyL)
pSpecBody = do
  _sym <- pSymbol "spec_body" SSpecBodySymbol
  iSpecBody
    <$> pPair (pMany pUseDeclaration) (pMany pHiddenSpecBlockMemeber)

pHiddenSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
pHiddenSpecBlockMemeber = do
  choice [ Megaparsec.try pHiddenSpecBlockMemeberSpecInvariant
         , Megaparsec.try pHiddenSpecBlockMemeberSpecFunction
         , Megaparsec.try pHiddenSpecBlockMemeberSpecCondition
         , Megaparsec.try pHiddenSpecBlockMemeberSpecInclude
         , Megaparsec.try pHiddenSpecBlockMemeberSpecApply
         , Megaparsec.try pHiddenSpecBlockMemeberSpecPragma
         , Megaparsec.try pHiddenSpecBlockMemeberSpecVariable
         , Megaparsec.try pHiddenSpecBlockMemeberSpecLet
         ]
  where
    pHiddenSpecBlockMemeberSpecInvariant :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecInvariant =
      iHiddenSpecBlockMemeberSpecInvariant
        <$> pSpecInvariant
    pHiddenSpecBlockMemeberSpecFunction :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecFunction =
      iHiddenSpecBlockMemeberSpecFunction
        <$> pHiddenSpecFunction
    pHiddenSpecBlockMemeberSpecCondition :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecCondition =
      iHiddenSpecBlockMemeberSpecCondition
        <$> pSpecCondition
    pHiddenSpecBlockMemeberSpecInclude :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecInclude =
      iHiddenSpecBlockMemeberSpecInclude
        <$> pSpecInclude
    pHiddenSpecBlockMemeberSpecApply :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecApply =
      iHiddenSpecBlockMemeberSpecApply
        <$> pSpecApply
    pHiddenSpecBlockMemeberSpecPragma :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecPragma =
      iHiddenSpecBlockMemeberSpecPragma
        <$> pSpecPragma
    pHiddenSpecBlockMemeberSpecVariable :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecVariable =
      iHiddenSpecBlockMemeberSpecVariable
        <$> pSpecVariable
    pHiddenSpecBlockMemeberSpecLet :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecLet =
      iHiddenSpecBlockMemeberSpecLet
        <$> pSpecLet

pSpecApply :: Parser (MoveTerm SpecApplyL)
pSpecApply = do
  _sym <- pSymbol "spec_apply" SSpecApplySymbol
  iSpecApply
    <$> pPair (pPair pHiddenExpression (pSome pSpecApplyPattern)) (pMaybe (pSome pSpecApplyPattern))

pSpecApplyPattern :: Parser (MoveTerm SpecApplyPatternL)
pSpecApplyPattern = do
  _sym <- pSymbol "spec_apply_pattern" SSpecApplyPatternSymbol
  iSpecApplyPattern
    <$> pMaybe pSpecApplyPatternInternal0
    <*> pSpecApplyNamePattern
    <*> pMaybe pTypeParameters

pSpecApplyNamePattern :: Parser (MoveTerm SpecApplyNamePatternL)
pSpecApplyNamePattern = do
  _sym <- pSymbol "spec_apply_name_pattern" SSpecApplyNamePatternSymbol
  iSpecApplyNamePattern
    <$> pText

pSpecApplyPatternInternal0 :: Parser (MoveTerm SpecApplyPatternInternal0L)
pSpecApplyPatternInternal0 = do
  choice [ Megaparsec.try pSpecApplyPatternInternal0Public
         , Megaparsec.try pSpecApplyPatternInternal0Internal
         ]
  where
    pSpecApplyPatternInternal0Public :: Parser (MoveTerm SpecApplyPatternInternal0L)
    pSpecApplyPatternInternal0Public =
      iSpecApplyPatternInternal0Public
        <$> pPublic
    pSpecApplyPatternInternal0Internal :: Parser (MoveTerm SpecApplyPatternInternal0L)
    pSpecApplyPatternInternal0Internal =
      iSpecApplyPatternInternal0Internal
        <$> pInternal

pSpecCondition :: Parser (MoveTerm SpecConditionL)
pSpecCondition = do
  _sym <- pSymbol "spec_condition" SSpecConditionSymbol
  choice [ Megaparsec.try pSpecConditionSpecCondition
         , Megaparsec.try pSpecConditionSpecAbortIf
         , Megaparsec.try pSpecConditionSpecAbortWithOrModifies
         ]
  where
    pSpecConditionSpecCondition :: Parser (MoveTerm SpecConditionL)
    pSpecConditionSpecCondition =
      iSpecConditionSpecCondition
        <$> pHiddenSpecCondition
    pSpecConditionSpecAbortIf :: Parser (MoveTerm SpecConditionL)
    pSpecConditionSpecAbortIf =
      iSpecConditionSpecAbortIf
        <$> pHiddenSpecAbortIf
    pSpecConditionSpecAbortWithOrModifies :: Parser (MoveTerm SpecConditionL)
    pSpecConditionSpecAbortWithOrModifies =
      iSpecConditionSpecAbortWithOrModifies
        <$> pHiddenSpecAbortWithOrModifies

pHiddenSpecAbortIf :: Parser (MoveTerm HiddenSpecAbortIfL)
pHiddenSpecAbortIf = do
  iHiddenSpecAbortIf
    <$> pAbortsIf
    <*> pMaybe pConditionProperties
    <*> pHiddenExpression
    <*> pMaybe pHiddenExpression
    <*> pure ()

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
    <*> pMaybe (pPair pAssign pHiddenLiteralValue)

pHiddenSpecAbortWithOrModifies :: Parser (MoveTerm HiddenSpecAbortWithOrModifiesL)
pHiddenSpecAbortWithOrModifies = do
  iHiddenSpecAbortWithOrModifies
    <$> pHiddenSpecAbortWithOrModifiesInternal0
    <*> pMaybe pConditionProperties
    <*> pSome pHiddenExpression
    <*> pure ()

pHiddenSpecAbortWithOrModifiesInternal0 :: Parser (MoveTerm HiddenSpecAbortWithOrModifiesInternal0L)
pHiddenSpecAbortWithOrModifiesInternal0 = do
  choice [ Megaparsec.try pHiddenSpecAbortWithOrModifiesInternal0AbortsWith
         , Megaparsec.try pHiddenSpecAbortWithOrModifiesInternal0Modifies
         ]
  where
    pHiddenSpecAbortWithOrModifiesInternal0AbortsWith :: Parser (MoveTerm HiddenSpecAbortWithOrModifiesInternal0L)
    pHiddenSpecAbortWithOrModifiesInternal0AbortsWith =
      iHiddenSpecAbortWithOrModifiesInternal0AbortsWith
        <$> pAbortsWith
    pHiddenSpecAbortWithOrModifiesInternal0Modifies :: Parser (MoveTerm HiddenSpecAbortWithOrModifiesInternal0L)
    pHiddenSpecAbortWithOrModifiesInternal0Modifies =
      iHiddenSpecAbortWithOrModifiesInternal0Modifies
        <$> pModifies

pHiddenSpecCondition :: Parser (MoveTerm HiddenSpecConditionL)
pHiddenSpecCondition = do
  iHiddenSpecCondition
    <$> pHiddenSpecConditionInternal0
    <*> pMaybe pConditionProperties
    <*> pHiddenExpression
    <*> pure ()

pHiddenSpecConditionInternal0 :: Parser (MoveTerm HiddenSpecConditionInternal0L)
pHiddenSpecConditionInternal0 = do
  choice [ Megaparsec.try pHiddenSpecConditionInternal0Kind
         , Megaparsec.try pHiddenSpecConditionInternal02
         ]
  where
    pHiddenSpecConditionInternal0Kind :: Parser (MoveTerm HiddenSpecConditionInternal0L)
    pHiddenSpecConditionInternal0Kind =
      iHiddenSpecConditionInternal0Kind
        <$> pHiddenSpecConditionKind
    pHiddenSpecConditionInternal02 :: Parser (MoveTerm HiddenSpecConditionInternal0L)
    pHiddenSpecConditionInternal02 =
      iHiddenSpecConditionInternal02
        <$> pRequires
        <*> pMaybe pModule

pHiddenSpecConditionKind :: Parser (MoveTerm HiddenSpecConditionKindL)
pHiddenSpecConditionKind = do
  choice [ Megaparsec.try pHiddenSpecConditionKindAssert
         , Megaparsec.try pHiddenSpecConditionKindAssume
         , Megaparsec.try pHiddenSpecConditionKindDecreases
         , Megaparsec.try pHiddenSpecConditionKindEnsures
         , Megaparsec.try pHiddenSpecConditionKindSucceedsIf
         ]
  where
    pHiddenSpecConditionKindAssert :: Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindAssert =
      iHiddenSpecConditionKindAssert
        <$> pAssert
    pHiddenSpecConditionKindAssume :: Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindAssume =
      iHiddenSpecConditionKindAssume
        <$> pAssume
    pHiddenSpecConditionKindDecreases :: Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindDecreases =
      iHiddenSpecConditionKindDecreases
        <$> pDecreases
    pHiddenSpecConditionKindEnsures :: Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindEnsures =
      iHiddenSpecConditionKindEnsures
        <$> pEnsures
    pHiddenSpecConditionKindSucceedsIf :: Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindSucceedsIf =
      iHiddenSpecConditionKindSucceedsIf
        <$> pSucceedsIf

pSpecInclude :: Parser (MoveTerm SpecIncludeL)
pSpecInclude = do
  _sym <- pSymbol "spec_include" SSpecIncludeSymbol
  iSpecInclude
    <$> pHiddenExpression

pSpecInvariant :: Parser (MoveTerm SpecInvariantL)
pSpecInvariant = do
  _sym <- pSymbol "spec_invariant" SSpecInvariantSymbol
  iSpecInvariant
    <$> pInvariant
    <*> pMaybe pSpecInvariantInternal0
    <*> pMaybe pConditionProperties
    <*> pHiddenExpression
    <*> pure ()

pSpecInvariantInternal0 :: Parser (MoveTerm SpecInvariantInternal0L)
pSpecInvariantInternal0 = do
  choice [ Megaparsec.try pSpecInvariantInternal0Update
         , Megaparsec.try pSpecInvariantInternal0Pack
         , Megaparsec.try pSpecInvariantInternal0Unpack
         , Megaparsec.try pSpecInvariantInternal0Module
         ]
  where
    pSpecInvariantInternal0Update :: Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Update =
      iSpecInvariantInternal0Update
        <$> pUpdate
    pSpecInvariantInternal0Pack :: Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Pack =
      iSpecInvariantInternal0Pack
        <$> pPack
    pSpecInvariantInternal0Unpack :: Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Unpack =
      iSpecInvariantInternal0Unpack
        <$> pUnpack
    pSpecInvariantInternal0Module :: Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Module =
      iSpecInvariantInternal0Module
        <$> pModule

pSpecLet :: Parser (MoveTerm SpecLetL)
pSpecLet = do
  _sym <- pSymbol "spec_let" SSpecLetSymbol
  iSpecLet
    <$> pPair (pPair (pPair (pMaybe pPost) pIdentifier) pAssign) pHiddenExpression

pSpecPragma :: Parser (MoveTerm SpecPragmaL)
pSpecPragma = do
  _sym <- pSymbol "spec_pragma" SSpecPragmaSymbol
  iSpecPragma
    <$> pMany pSpecProperty

pSpecVariable :: Parser (MoveTerm SpecVariableL)
pSpecVariable = do
  _sym <- pSymbol "spec_variable" SSpecVariableSymbol
  iSpecVariable
    <$> pMaybe pSpecVariableInternal0
    <*> pIdentifier
    <*> pMaybe pTypeParameters
    <*> pure ()
    <*> pHiddenType
    <*> pure ()

pSpecVariableInternal0 :: Parser (MoveTerm SpecVariableInternal0L)
pSpecVariableInternal0 = do
  choice [ Megaparsec.try pSpecVariableInternal0Global
         , Megaparsec.try pSpecVariableInternal0Local
         ]
  where
    pSpecVariableInternal0Global :: Parser (MoveTerm SpecVariableInternal0L)
    pSpecVariableInternal0Global =
      iSpecVariableInternal0Global
        <$> pGlobal
    pSpecVariableInternal0Local :: Parser (MoveTerm SpecVariableInternal0L)
    pSpecVariableInternal0Local =
      iSpecVariableInternal0Local
        <$> pLocal

pUseDeclaration :: Parser (MoveTerm UseDeclarationL)
pUseDeclaration = do
  _sym <- pSymbol "use_declaration" SUseDeclarationSymbol
  iUseDeclaration
    <$> pMaybe pPublic
    <*> pure ()
    <*> pUseDeclarationInternal0
    <*> pure ()

pUseDeclarationInternal0 :: Parser (MoveTerm UseDeclarationInternal0L)
pUseDeclarationInternal0 = do
  choice [ Megaparsec.try pUseDeclarationInternal0UseFun
         , Megaparsec.try pUseDeclarationInternal0UseModule
         , Megaparsec.try pUseDeclarationInternal0UseModuleMember
         , Megaparsec.try pUseDeclarationInternal0UseModuleMembers
         ]
  where
    pUseDeclarationInternal0UseFun :: Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseFun =
      iUseDeclarationInternal0UseFun
        <$> pUseFun
    pUseDeclarationInternal0UseModule :: Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseModule =
      iUseDeclarationInternal0UseModule
        <$> pUseModule
    pUseDeclarationInternal0UseModuleMember :: Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseModuleMember =
      iUseDeclarationInternal0UseModuleMember
        <$> pUseModuleMember
    pUseDeclarationInternal0UseModuleMembers :: Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseModuleMembers =
      iUseDeclarationInternal0UseModuleMembers
        <$> pUseModuleMembers

pUseFun :: Parser (MoveTerm UseFunL)
pUseFun = do
  _sym <- pSymbol "use_fun" SUseFunSymbol
  iUseFun
    <$> pure ()
    <*> pModuleAccess
    <*> pure ()
    <*> pPair pModuleAccess pHiddenFunctionIdentifier

pUseModule :: Parser (MoveTerm UseModuleL)
pUseModule = do
  _sym <- pSymbol "use_module" SUseModuleSymbol
  iUseModule
    <$> pModuleIdentity
    <*> pMaybe pHiddenModuleIdentifier

pUseModuleMember :: Parser (MoveTerm UseModuleMemberL)
pUseModuleMember = do
  _sym <- pSymbol "use_module_member" SUseModuleMemberSymbol
  iUseModuleMember
    <$> pModuleIdentity
    <*> pure ()
    <*> pUseMember

pUseMember :: Parser (MoveTerm UseMemberL)
pUseMember = do
  _sym <- pSymbol "use_member" SUseMemberSymbol
  choice [ Megaparsec.try pUseMember1
         , Megaparsec.try pUseMember2
         , Megaparsec.try pUseMember3
         ]
  where
    pUseMember1 :: Parser (MoveTerm UseMemberL)
    pUseMember1 =
      iUseMember1
        <$> pIdentifier
        <*> pure ()
        <*> pure ()
        <*> pSome pUseMember
        <*> pure ()
    pUseMember2 :: Parser (MoveTerm UseMemberL)
    pUseMember2 =
      iUseMember2
        <$> pIdentifier
        <*> pure ()
        <*> pIdentifier
        <*> pMaybe pIdentifier
    pUseMember3 :: Parser (MoveTerm UseMemberL)
    pUseMember3 =
      iUseMember3
        <$> pIdentifier
        <*> pMaybe pIdentifier

pUseModuleMembers :: Parser (MoveTerm UseModuleMembersL)
pUseModuleMembers = do
  _sym <- pSymbol "use_module_members" SUseModuleMembersSymbol
  choice [ Megaparsec.try pUseModuleMembers1
         , Megaparsec.try pUseModuleMembers2
         ]
  where
    pUseModuleMembers1 :: Parser (MoveTerm UseModuleMembersL)
    pUseModuleMembers1 =
      iUseModuleMembers1
        <$> pModuleIdentityInternal0
        <*> pure ()
        <*> pure ()
        <*> pSome pUseMember
        <*> pure ()
    pUseModuleMembers2 :: Parser (MoveTerm UseModuleMembersL)
    pUseModuleMembers2 =
      iUseModuleMembers2
        <$> pModuleIdentity
        <*> pure ()
        <*> pure ()
        <*> pSome pUseMember
        <*> pure ()

pUnitExpression :: Parser (MoveTerm UnitExpressionL)
pUnitExpression = do
  _sym <- pSymbol "unit_expression" SUnitExpressionSymbol
  pure iUnitExpression
    <$> pure ()
    <*> pure ()

pVectorExpression :: Parser (MoveTerm VectorExpressionL)
pVectorExpression = do
  _sym <- pSymbol "vector_expression" SVectorExpressionSymbol
  iVectorExpression
    <$> pVectorExpressionInternal0
    <*> pMany pHiddenExpression
    <*> pure ()

pVectorExpressionInternal0 :: Parser (MoveTerm VectorExpressionInternal0L)
pVectorExpressionInternal0 = do
  choice [ Megaparsec.try pVectorExpressionInternal0Vectorleftspacesquarespacebracket
         , Megaparsec.try pVectorExpressionInternal02
         ]
  where
    pVectorExpressionInternal0Vectorleftspacesquarespacebracket :: Parser (MoveTerm VectorExpressionInternal0L)
    pVectorExpressionInternal0Vectorleftspacesquarespacebracket =
      pure iVectorExpressionInternal0Vectorleftspacesquarespacebracket
        <$> pure ()
    pVectorExpressionInternal02 :: Parser (MoveTerm VectorExpressionInternal0L)
    pVectorExpressionInternal02 =
      iVectorExpressionInternal02
        <$> pPair (pSome pHiddenType) pGt

pBorrowExpression :: Parser (MoveTerm BorrowExpressionL)
pBorrowExpression = do
  _sym <- pSymbol "borrow_expression" SBorrowExpressionSymbol
  iBorrowExpression
    <$> pPair pHiddenReference pHiddenExpression

pDereferenceExpression :: Parser (MoveTerm DereferenceExpressionL)
pDereferenceExpression = do
  _sym <- pSymbol "dereference_expression" SDereferenceExpressionSymbol
  iDereferenceExpression
    <$> pPair pMul pHiddenExpression

pMoveOrCopyExpression :: Parser (MoveTerm MoveOrCopyExpressionL)
pMoveOrCopyExpression = do
  _sym <- pSymbol "move_or_copy_expression" SMoveOrCopyExpressionSymbol
  iMoveOrCopyExpression
    <$> pPair pMoveOrCopyExpressionInternal0 pHiddenExpression

pMoveOrCopyExpressionInternal0 :: Parser (MoveTerm MoveOrCopyExpressionInternal0L)
pMoveOrCopyExpressionInternal0 = do
  choice [ Megaparsec.try pMoveOrCopyExpressionInternal0Move
         , Megaparsec.try pMoveOrCopyExpressionInternal0Copy
         ]
  where
    pMoveOrCopyExpressionInternal0Move :: Parser (MoveTerm MoveOrCopyExpressionInternal0L)
    pMoveOrCopyExpressionInternal0Move =
      iMoveOrCopyExpressionInternal0Move
        <$> pMove
    pMoveOrCopyExpressionInternal0Copy :: Parser (MoveTerm MoveOrCopyExpressionInternal0L)
    pMoveOrCopyExpressionInternal0Copy =
      iMoveOrCopyExpressionInternal0Copy
        <$> pCopy

pUnaryExpression :: Parser (MoveTerm UnaryExpressionL)
pUnaryExpression = do
  _sym <- pSymbol "unary_expression" SUnaryExpressionSymbol
  iUnaryExpression
    <$> pUnaryOp
    <*> pHiddenExpression

pUnaryOp :: Parser (MoveTerm UnaryOpL)
pUnaryOp = do
  _sym <- pSymbol "unary_op" SUnaryOpSymbol
  iUnaryOp
    <$> pBang

pAbortExpression :: Parser (MoveTerm AbortExpressionL)
pAbortExpression = do
  _sym <- pSymbol "abort_expression" SAbortExpressionSymbol
  iAbortExpression
    <$> pure ()
    <*> pMaybe pHiddenExpression

pAssignExpression :: Parser (MoveTerm AssignExpressionL)
pAssignExpression = do
  _sym <- pSymbol "assign_expression" SAssignExpressionSymbol
  iAssignExpression
    <$> pPair (pPair pHiddenUnaryExpression pAssign) pHiddenExpression

pBinaryExpression :: Parser (MoveTerm BinaryExpressionL)
pBinaryExpression = do
  _sym <- pSymbol "binary_expression" SBinaryExpressionSymbol
  choice [ Megaparsec.try pBinaryExpression1
         , Megaparsec.try pBinaryExpression2
         , Megaparsec.try pBinaryExpression3
         , Megaparsec.try pBinaryExpression4
         , Megaparsec.try pBinaryExpression5
         , Megaparsec.try pBinaryExpression6
         , Megaparsec.try pBinaryExpression7
         , Megaparsec.try pBinaryExpression8
         , Megaparsec.try pBinaryExpression9
         , Megaparsec.try pBinaryExpression10
         , Megaparsec.try pBinaryExpression11
         , Megaparsec.try pBinaryExpression12
         , Megaparsec.try pBinaryExpression13
         , Megaparsec.try pBinaryExpression14
         , Megaparsec.try pBinaryExpression15
         , Megaparsec.try pBinaryExpression16
         , Megaparsec.try pBinaryExpression17
         , Megaparsec.try pBinaryExpression18
         , Megaparsec.try pBinaryExpression19
         , Megaparsec.try pBinaryExpression20
         ]
  where
    pBinaryExpression1 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression1 =
      iBinaryExpression1
        <$> pHiddenExpression
        <*> pImplies
        <*> pHiddenExpression
    pBinaryExpression2 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression2 =
      iBinaryExpression2
        <$> pHiddenExpression
        <*> pOr
        <*> pHiddenExpression
    pBinaryExpression3 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression3 =
      iBinaryExpression3
        <$> pHiddenExpression
        <*> pAnd
        <*> pHiddenExpression
    pBinaryExpression4 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression4 =
      iBinaryExpression4
        <$> pHiddenExpression
        <*> pEq
        <*> pHiddenExpression
    pBinaryExpression5 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression5 =
      iBinaryExpression5
        <$> pHiddenExpression
        <*> pNeq
        <*> pHiddenExpression
    pBinaryExpression6 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression6 =
      iBinaryExpression6
        <$> pHiddenExpression
        <*> pLt
        <*> pHiddenExpression
    pBinaryExpression7 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression7 =
      iBinaryExpression7
        <$> pHiddenExpression
        <*> pGt
        <*> pHiddenExpression
    pBinaryExpression8 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression8 =
      iBinaryExpression8
        <$> pHiddenExpression
        <*> pLe
        <*> pHiddenExpression
    pBinaryExpression9 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression9 =
      iBinaryExpression9
        <$> pHiddenExpression
        <*> pGe
        <*> pHiddenExpression
    pBinaryExpression10 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression10 =
      iBinaryExpression10
        <$> pHiddenExpression
        <*> pRange
        <*> pHiddenExpression
    pBinaryExpression11 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression11 =
      iBinaryExpression11
        <$> pHiddenExpression
        <*> pBitor
        <*> pHiddenExpression
    pBinaryExpression12 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression12 =
      iBinaryExpression12
        <$> pHiddenExpression
        <*> pXor
        <*> pHiddenExpression
    pBinaryExpression13 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression13 =
      iBinaryExpression13
        <$> pHiddenExpression
        <*> pBitand
        <*> pHiddenExpression
    pBinaryExpression14 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression14 =
      iBinaryExpression14
        <$> pHiddenExpression
        <*> pShl
        <*> pHiddenExpression
    pBinaryExpression15 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression15 =
      iBinaryExpression15
        <$> pHiddenExpression
        <*> pShr
        <*> pHiddenExpression
    pBinaryExpression16 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression16 =
      iBinaryExpression16
        <$> pHiddenExpression
        <*> pAdd
        <*> pHiddenExpression
    pBinaryExpression17 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression17 =
      iBinaryExpression17
        <$> pHiddenExpression
        <*> pSub
        <*> pHiddenExpression
    pBinaryExpression18 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression18 =
      iBinaryExpression18
        <$> pHiddenExpression
        <*> pMul
        <*> pHiddenExpression
    pBinaryExpression19 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression19 =
      iBinaryExpression19
        <$> pHiddenExpression
        <*> pDiv
        <*> pHiddenExpression
    pBinaryExpression20 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression20 =
      iBinaryExpression20
        <$> pHiddenExpression
        <*> pMod
        <*> pHiddenExpression

pCastExpression :: Parser (MoveTerm CastExpressionL)
pCastExpression = do
  _sym <- pSymbol "cast_expression" SCastExpressionSymbol
  iCastExpression
    <$> pPair pHiddenExpression pHiddenType

pIdentifiedExpression :: Parser (MoveTerm IdentifiedExpressionL)
pIdentifiedExpression = do
  _sym <- pSymbol "identified_expression" SIdentifiedExpressionSymbol
  iIdentifiedExpression
    <$> pBlockIdentifier
    <*> pHiddenExpression

pBlockIdentifier :: Parser (MoveTerm BlockIdentifierL)
pBlockIdentifier = do
  _sym <- pSymbol "block_identifier" SBlockIdentifierSymbol
  iBlockIdentifier
    <$> pLabel
    <*> pure ()

pLambdaExpression :: Parser (MoveTerm LambdaExpressionL)
pLambdaExpression = do
  _sym <- pSymbol "lambda_expression" SLambdaExpressionSymbol
  iLambdaExpression
    <$> pLambdaBindings
    <*> pMaybe pHiddenType
    <*> pHiddenExpression

pLambdaBindings :: Parser (MoveTerm LambdaBindingsL)
pLambdaBindings = do
  _sym <- pSymbol "lambda_bindings" SLambdaBindingsSymbol
  iLambdaBindings
    <$> pMany pLambdaBinding

pLambdaBinding :: Parser (MoveTerm LambdaBindingL)
pLambdaBinding = do
  _sym <- pSymbol "lambda_binding" SLambdaBindingSymbol
  choice [ Megaparsec.try pLambdaBindingCommaBindList
         , Megaparsec.try pLambdaBindingBind
         , Megaparsec.try pLambdaBinding3
         ]
  where
    pLambdaBindingCommaBindList :: Parser (MoveTerm LambdaBindingL)
    pLambdaBindingCommaBindList =
      iLambdaBindingCommaBindList
        <$> pCommaBindList
    pLambdaBindingBind :: Parser (MoveTerm LambdaBindingL)
    pLambdaBindingBind =
      iLambdaBindingBind
        <$> pHiddenBind
    pLambdaBinding3 :: Parser (MoveTerm LambdaBindingL)
    pLambdaBinding3 =
      iLambdaBinding3
        <$> pHiddenBind
        <*> pMaybe pHiddenType

pLoopExpression :: Parser (MoveTerm LoopExpressionL)
pLoopExpression = do
  _sym <- pSymbol "loop_expression" SLoopExpressionSymbol
  iLoopExpression
    <$> pure ()
    <*> pHiddenExpression

pQuantifierExpression :: Parser (MoveTerm QuantifierExpressionL)
pQuantifierExpression = do
  _sym <- pSymbol "quantifier_expression" SQuantifierExpressionSymbol
  iQuantifierExpression
    <$> pPair (pPair (pPair pHiddenReservedIdentifier pQuantifierBindings) (pMaybe pHiddenExpression)) pHiddenExpression

pQuantifierBindings :: Parser (MoveTerm QuantifierBindingsL)
pQuantifierBindings = do
  _sym <- pSymbol "quantifier_bindings" SQuantifierBindingsSymbol
  iQuantifierBindings
    <$> pSome pQuantifierBinding

pQuantifierBinding :: Parser (MoveTerm QuantifierBindingL)
pQuantifierBinding = do
  _sym <- pSymbol "quantifier_binding" SQuantifierBindingSymbol
  choice [ Megaparsec.try pQuantifierBinding1
         , Megaparsec.try pQuantifierBinding2
         ]
  where
    pQuantifierBinding1 :: Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding1 =
      iQuantifierBinding1
        <$> pIdentifier
        <*> pure ()
        <*> pHiddenType
    pQuantifierBinding2 :: Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding2 =
      iQuantifierBinding2
        <$> pIdentifier
        <*> pure ()
        <*> pHiddenExpression

pReturnExpression :: Parser (MoveTerm ReturnExpressionL)
pReturnExpression = do
  _sym <- pSymbol "return_expression" SReturnExpressionSymbol
  choice [ Megaparsec.try pReturnExpression1
         , Megaparsec.try pReturnExpression2
         ]
  where
    pReturnExpression1 :: Parser (MoveTerm ReturnExpressionL)
    pReturnExpression1 =
      iReturnExpression1
        <$> pure ()
        <*> pMaybe pLabel
        <*> pHiddenExpression
    pReturnExpression2 :: Parser (MoveTerm ReturnExpressionL)
    pReturnExpression2 =
      iReturnExpression2
        <$> pure ()
        <*> pMaybe pLabel

pWhileExpression :: Parser (MoveTerm WhileExpressionL)
pWhileExpression = do
  _sym <- pSymbol "while_expression" SWhileExpressionSymbol
  iWhileExpression
    <$> pure ()
    <*> pure ()
    <*> pHiddenExpression
    <*> pure ()
    <*> pHiddenExpression

pBlockItem :: Parser (MoveTerm BlockItemL)
pBlockItem = do
  _sym <- pSymbol "block_item" SBlockItemSymbol
  iBlockItem
    <$> pBlockItemInternal0
    <*> pure ()

pBlockItemInternal0 :: Parser (MoveTerm BlockItemInternal0L)
pBlockItemInternal0 = do
  choice [ Megaparsec.try pBlockItemInternal0Expression
         , Megaparsec.try pBlockItemInternal0LetStatement
         ]
  where
    pBlockItemInternal0Expression :: Parser (MoveTerm BlockItemInternal0L)
    pBlockItemInternal0Expression =
      iBlockItemInternal0Expression
        <$> pHiddenExpression
    pBlockItemInternal0LetStatement :: Parser (MoveTerm BlockItemInternal0L)
    pBlockItemInternal0LetStatement =
      iBlockItemInternal0LetStatement
        <$> pLetStatement

pLetStatement :: Parser (MoveTerm LetStatementL)
pLetStatement = do
  _sym <- pSymbol "let_statement" SLetStatementSymbol
  iLetStatement
    <$> pure ()
    <*> pBindList
    <*> pMaybe pHiddenType
    <*> pMaybe (pPair pAssign pHiddenExpression)

pMacroFunctionDefinition :: Parser (MoveTerm MacroFunctionDefinitionL)
pMacroFunctionDefinition = do
  _sym <- pSymbol "macro_function_definition" SMacroFunctionDefinitionSymbol
  iMacroFunctionDefinition
    <$> pMaybe pModifier
    <*> pure ()
    <*> pHiddenMacroSignature
    <*> pBlock

pHiddenMacroSignature :: Parser (MoveTerm HiddenMacroSignatureL)
pHiddenMacroSignature = do
  iHiddenMacroSignature
    <$> pMaybe pModifier
    <*> pure ()
    <*> pHiddenFunctionIdentifier
    <*> pMaybe pTypeParameters
    <*> pFunctionParameters
    <*> pMaybe pRetType

pNativeFunctionDefinition :: Parser (MoveTerm NativeFunctionDefinitionL)
pNativeFunctionDefinition = do
  _sym <- pSymbol "native_function_definition" SNativeFunctionDefinitionSymbol
  iNativeFunctionDefinition
    <$> pHiddenFunctionSignature
    <*> pure ()

pHiddenStructItem :: Parser (MoveTerm HiddenStructItemL)
pHiddenStructItem = do
  choice [ Megaparsec.try pHiddenStructItemNativeStructDefinition
         , Megaparsec.try pHiddenStructItemStructDefinition
         ]
  where
    pHiddenStructItemNativeStructDefinition :: Parser (MoveTerm HiddenStructItemL)
    pHiddenStructItemNativeStructDefinition =
      iHiddenStructItemNativeStructDefinition
        <$> pNativeStructDefinition
    pHiddenStructItemStructDefinition :: Parser (MoveTerm HiddenStructItemL)
    pHiddenStructItemStructDefinition =
      iHiddenStructItemStructDefinition
        <$> pStructDefinition

pNativeStructDefinition :: Parser (MoveTerm NativeStructDefinitionL)
pNativeStructDefinition = do
  _sym <- pSymbol "native_struct_definition" SNativeStructDefinitionSymbol
  iNativeStructDefinition
    <$> pMaybe pPublic
    <*> pNative
    <*> pHiddenStructSignature
    <*> pure ()

pHiddenStructSignature :: Parser (MoveTerm HiddenStructSignatureL)
pHiddenStructSignature = do
  iHiddenStructSignature
    <$> pure ()
    <*> pHiddenStructIdentifier
    <*> pMaybe pTypeParameters
    <*> pMaybe pAbilityDecls

pStructDefinition :: Parser (MoveTerm StructDefinitionL)
pStructDefinition = do
  _sym <- pSymbol "struct_definition" SStructDefinitionSymbol
  iStructDefinition
    <$> pMaybe pPublic
    <*> pHiddenStructSignature
    <*> pDatatypeFields
    <*> pMaybe pPostfixAbilityDecls

pConstant :: Parser (MoveTerm ConstantL)
pConstant = do
  _sym <- pSymbol "constant" SConstantSymbol
  iConstant
    <$> pPair (pPair (pPair pIdentifier pHiddenType) pAssign) pHiddenExpression

pFriendDeclaration :: Parser (MoveTerm FriendDeclarationL)
pFriendDeclaration = do
  _sym <- pSymbol "friend_declaration" SFriendDeclarationSymbol
  iFriendDeclaration
    <$> pFriendAccess

pFriendAccess :: Parser (MoveTerm FriendAccessL)
pFriendAccess = do
  _sym <- pSymbol "friend_access" SFriendAccessSymbol
  choice [ Megaparsec.try pFriendAccessLocalModule
         , Megaparsec.try pFriendAccessFullyQualifiedModule
         ]
  where
    pFriendAccessLocalModule :: Parser (MoveTerm FriendAccessL)
    pFriendAccessLocalModule =
      iFriendAccessLocalModule
        <$> pIdentifier
    pFriendAccessFullyQualifiedModule :: Parser (MoveTerm FriendAccessL)
    pFriendAccessFullyQualifiedModule =
      iFriendAccessFullyQualifiedModule
        <$> pModuleIdentity
