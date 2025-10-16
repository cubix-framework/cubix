module Cubix.Language.SuiMove.ParsePretty where

import Control.Applicative.Combinators
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IM
import Data.Functor (($>))
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

getSymbol :: SymbolTable -> TS.Node -> IO (Maybe SomeSymbolSing)
getSymbol map node = do
  symbol <- TS.nodeSymbol node
  pure $ IM.lookup (fromIntegral symbol) (unSymbolTable map)

parse :: FilePath -> IO (Maybe (MoveTerm (RootSort MoveSig)))
parse path = do
  input <- BS.readFile path

  withLanguage tree_sitter_sui_move $ \lang -> do
    symbolTable <- liftIO (mkSymbolTable lang)

    withParser lang $ \parser -> do
      maybeTree <- TS.parserParseByteString parser Nothing input
      tree <- maybe (error "failed to parse the program") pure maybeTree
      rootNode <- TS.treeRootNode tree

      toks <- Streaming.toList_
        $ Streaming.mapMaybeM
            (\tok ->
              fmap (\sym -> tok { tokenValue = sym }) <$> getSymbol symbolTable (tokenValue tok))
              -- pure $ case msym of
              --   Just sym -> Just $ ann { tokenValue = sym }
              --   Nothing -> Nothing)
                                 
        $ annotated path rootNode

      let lexed = Megaparsec.TreeSitter.Lexed input toks
      case Megaparsec.parse pRoot path lexed of
        Right ast -> pure $ Just ast
        Left err -> do
          putStrLn $ Megaparsec.errorBundlePretty err
          pure Nothing

-- --------------------------------------------------------------------------------
-- -- Parser 
-- --------------------------------------------------------------------------------
type Parser = Megaparsec.TreeSitter.Parser SomeSymbolSing

pSymbol
  :: forall (symbolType :: SymbolType) (symbol :: Symbol symbolType)
   . SymbolSing symbolType symbol
  -> Parser (Cubix.TreeSitter.Token (SymbolSing symbolType symbol))
pSymbol sym = Megaparsec.TreeSitter.pToken $ \case
  SomeSymbolSing _isReal symSing -> case decSymbolSing sym symSing of
    Just (Refl, HRefl) -> Just symSing
    Nothing -> Nothing

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
pBang = pSymbol SBangTokSymbol $> iBang

pNeq :: Parser (MoveTerm NeqTokL)
pNeq = pSymbol SNeqTokSymbol $> iNeq

pDollar :: Parser (MoveTerm DollarTokL)
pDollar = pSymbol SDollarTokSymbol $> iDollar

pMod :: Parser (MoveTerm ModTokL)
pMod = pSymbol SModTokSymbol $> iMod

pBitand :: Parser (MoveTerm BitandTokL)
pBitand = pSymbol SBitandTokSymbol $> iBitand

pAnd :: Parser (MoveTerm AndTokL)
pAnd = pSymbol SAndTokSymbol $> iAnd

pMul :: Parser (MoveTerm MulTokL)
pMul = pSymbol SMulTokSymbol $> iMul

pAdd :: Parser (MoveTerm AddTokL)
pAdd = pSymbol SAddTokSymbol $> iAdd

pSub :: Parser (MoveTerm SubTokL)
pSub = pSymbol SSubTokSymbol $> iSub

pRange :: Parser (MoveTerm RangeTokL)
pRange = pSymbol SRangeTokSymbol $> iRange

pDiv :: Parser (MoveTerm DivTokL)
pDiv = pSymbol SDivTokSymbol $> iDiv

pLt :: Parser (MoveTerm LtTokL)
pLt = pSymbol SLtTokSymbol $> iLt

pShl :: Parser (MoveTerm ShlTokL)
pShl = pSymbol SShlTokSymbol $> iShl

pLe :: Parser (MoveTerm LeTokL)
pLe = pSymbol SLeTokSymbol $> iLe

pAssign :: Parser (MoveTerm AssignTokL)
pAssign = pSymbol SAssignTokSymbol $> iAssign

pEq :: Parser (MoveTerm EqTokL)
pEq = pSymbol SEqTokSymbol $> iEq

pImplies :: Parser (MoveTerm ImpliesTokL)
pImplies = pSymbol SImpliesTokSymbol $> iImplies

pGt :: Parser (MoveTerm GtTokL)
pGt = pSymbol SGtTokSymbol $> iGt

pGe :: Parser (MoveTerm GeTokL)
pGe = pSymbol SGeTokSymbol $> iGe

pShr :: Parser (MoveTerm ShrTokL)
pShr = pSymbol SShrTokSymbol $> iShr

pXor :: Parser (MoveTerm XorTokL)
pXor = pSymbol SXorTokSymbol $> iXor

pAbortsWith :: Parser (MoveTerm AbortsWithTokL)
pAbortsWith = pSymbol SAbortsWithTokSymbol $> iAbortsWith

pAddress :: Parser (MoveTerm AddressTokL)
pAddress = pSymbol SAddressTokSymbol $> iAddress

pAssert :: Parser (MoveTerm AssertTokL)
pAssert = pSymbol SAssertTokSymbol $> iAssert

pAssume :: Parser (MoveTerm AssumeTokL)
pAssume = pSymbol SAssumeTokSymbol $> iAssume

pBool :: Parser (MoveTerm BoolTokL)
pBool = pSymbol SBoolTokSymbol $> iBool

pBytearray :: Parser (MoveTerm BytearrayTokL)
pBytearray = pSymbol SBytearrayTokSymbol $> iBytearray

pCopy :: Parser (MoveTerm CopyTokL)
pCopy = pSymbol SCopyTokSymbol $> iCopy

pDecreases :: Parser (MoveTerm DecreasesTokL)
pDecreases = pSymbol SDecreasesTokSymbol $> iDecreases

pDrop :: Parser (MoveTerm DropTokL)
pDrop = pSymbol SDropTokSymbol $> iDrop

pEnsures :: Parser (MoveTerm EnsuresTokL)
pEnsures = pSymbol SEnsuresTokSymbol $> iEnsures

pFalse :: Parser (MoveTerm FalseTokL)
pFalse = pSymbol SFalseTokSymbol $> iFalse

pFriend :: Parser (MoveTerm FriendTokL)
pFriend = pSymbol SFriendTokSymbol $> iFriend

pGlobal :: Parser (MoveTerm GlobalTokL)
pGlobal = pSymbol SGlobalTokSymbol $> iGlobal

pInternal :: Parser (MoveTerm InternalTokL)
pInternal = pSymbol SInternalTokSymbol $> iInternal

pKey :: Parser (MoveTerm KeyTokL)
pKey = pSymbol SKeyTokSymbol $> iKey

pLocal :: Parser (MoveTerm LocalTokL)
pLocal = pSymbol SLocalTokSymbol $> iLocal

pModifies :: Parser (MoveTerm ModifiesTokL)
pModifies = pSymbol SModifiesTokSymbol $> iModifies

pModule :: Parser (MoveTerm ModuleTokL)
pModule = pSymbol SModuleTokSymbol $> iModule

pMove :: Parser (MoveTerm MoveTokL)
pMove = pSymbol SMoveTokSymbol $> iMove

pPack :: Parser (MoveTerm PackTokL)
pPack = pSymbol SPackTokSymbol $> iPack

pPackage :: Parser (MoveTerm PackageTokL)
pPackage = pSymbol SPackageTokSymbol $> iPackage

pPhantom :: Parser (MoveTerm PhantomTokL)
pPhantom = pSymbol SPhantomTokSymbol $> iPhantom

pPost :: Parser (MoveTerm PostTokL)
pPost = pSymbol SPostTokSymbol $> iPost

pPublic :: Parser (MoveTerm PublicTokL)
pPublic = pSymbol SPublicTokSymbol $> iPublic

pSigner :: Parser (MoveTerm SignerTokL)
pSigner = pSymbol SSignerTokSymbol $> iSigner

pStore :: Parser (MoveTerm StoreTokL)
pStore = pSymbol SStoreTokSymbol $> iStore

pSucceedsIf :: Parser (MoveTerm SucceedsIfTokL)
pSucceedsIf = pSymbol SSucceedsIfTokSymbol $> iSucceedsIf

pTrue :: Parser (MoveTerm TrueTokL)
pTrue = pSymbol STrueTokSymbol $> iTrue

pU128 :: Parser (MoveTerm U128TokL)
pU128 = pSymbol SU128TokSymbol $> iU128

pU16 :: Parser (MoveTerm U16TokL)
pU16 = pSymbol SU16TokSymbol $> iU16

pU256 :: Parser (MoveTerm U256TokL)
pU256 = pSymbol SU256TokSymbol $> iU256

pU32 :: Parser (MoveTerm U32TokL)
pU32 = pSymbol SU32TokSymbol $> iU32

pU64 :: Parser (MoveTerm U64TokL)
pU64 = pSymbol SU64TokSymbol $> iU64

pU8 :: Parser (MoveTerm U8TokL)
pU8 = pSymbol SU8TokSymbol $> iU8

pUnpack :: Parser (MoveTerm UnpackTokL)
pUnpack = pSymbol SUnpackTokSymbol $> iUnpack

pUpdate :: Parser (MoveTerm UpdateTokL)
pUpdate = pSymbol SUpdateTokSymbol $> iUpdate

pBitor :: Parser (MoveTerm BitorTokL)
pBitor = pSymbol SBitorTokSymbol $> iBitor

pOr :: Parser (MoveTerm OrTokL)
pOr = pSymbol SOrTokSymbol $> iOr

pSourceFile :: Parser (MoveTerm SourceFileL)
pSourceFile = do
  _sym <- pSymbol SSourceFileSymbol
  iSourceFile
    <$> pMany pModuleDefinition

pModuleDefinition :: Parser (MoveTerm ModuleDefinitionL)
pModuleDefinition = do
  _sym <- pSymbol SModuleDefinitionSymbol
  iModuleDefinition
    <$> pModule
    <*> pModuleIdentity
    <*> pModuleBody

pModuleBody :: Parser (MoveTerm ModuleBodyL)
pModuleBody = do
  _sym <- pSymbol SModuleBodySymbol
  iModuleBody
    <$> pMany (pEither pFriendDeclaration (pEither pConstant (pEither pHiddenFunctionItem (pEither pHiddenStructItem (pEither pHiddenEnumItem (pEither pSpecBlock pUseDeclaration))))))

pHiddenEnumItem :: Parser (MoveTerm HiddenEnumItemL)
pHiddenEnumItem = do
  iEnumDefinitionEnumItem
    <$> pEnumDefinition

pEnumDefinition :: Parser (MoveTerm EnumDefinitionL)
pEnumDefinition = do
  _sym <- pSymbol SEnumDefinitionSymbol
  iEnumDefinition
    <$> pMaybe pPublic
    <*> pHiddenEnumSignature
    <*> pEnumVariants
    <*> pMaybe pPostfixAbilityDecls

pHiddenEnumSignature :: Parser (MoveTerm HiddenEnumSignatureL)
pHiddenEnumSignature = do
  iHiddenEnumSignature
    <$> pHiddenEnumIdentifier
    <*> pMaybe pTypeParameters
    <*> pMaybe pAbilityDecls

pHiddenEnumIdentifier :: Parser (MoveTerm HiddenEnumIdentifierL)
pHiddenEnumIdentifier = do
  iHiddenEnumIdentifier
    <$> pEnumIdentifier

pEnumIdentifier :: Parser (MoveTerm EnumIdentifierL)
pEnumIdentifier = do
  _sym <- pSymbol SEnumIdentifierSymbol
  pure iEnumIdentifier

pAbilityDecls :: Parser (MoveTerm AbilityDeclsL)
pAbilityDecls = do
  _sym <- pSymbol SAbilityDeclsSymbol
  iAbilityDecls
    <$> pPair (pMaybe pAbility) (pMany pAbility)

pAbility :: Parser (MoveTerm AbilityL)
pAbility = do
  _sym <- pSymbol SAbilitySymbol
  choice [ pCopyAbility
         , pDropAbility
         , pStoreAbility
         , pKeyAbility
         ]
  where
    pCopyAbility :: Parser (MoveTerm AbilityL)
    pCopyAbility =
      iCopyAbility
        <$> pCopy
    pDropAbility :: Parser (MoveTerm AbilityL)
    pDropAbility =
      iDropAbility
        <$> pDrop
    pStoreAbility :: Parser (MoveTerm AbilityL)
    pStoreAbility =
      iStoreAbility
        <$> pStore
    pKeyAbility :: Parser (MoveTerm AbilityL)
    pKeyAbility =
      iKeyAbility
        <$> pKey

pTypeParameters :: Parser (MoveTerm TypeParametersL)
pTypeParameters = do
  _sym <- pSymbol STypeParametersSymbol
  iTypeParameters
    <$> pLt
    <*> pPair (pMany pTypeParameter) pTypeParameter
    <*> pGt

pTypeParameter :: Parser (MoveTerm TypeParameterL)
pTypeParameter = do
  _sym <- pSymbol STypeParameterSymbol
  iTypeParameter
    <$> pMaybe pDollar
    <*> pMaybe pPhantom
    <*> pHiddenTypeParameterIdentifier
    <*> pMaybe (pPair (pMany (pPair pAbility pAdd)) (pPair (pMaybe pAdd) pAbility))

pHiddenTypeParameterIdentifier :: Parser (MoveTerm HiddenTypeParameterIdentifierL)
pHiddenTypeParameterIdentifier = do
  iHiddenTypeParameterIdentifier
    <$> pTypeParameterIdentifier

pTypeParameterIdentifier :: Parser (MoveTerm TypeParameterIdentifierL)
pTypeParameterIdentifier = do
  _sym <- pSymbol STypeParameterIdentifierSymbol
  pure iTypeParameterIdentifier

pEnumVariants :: Parser (MoveTerm EnumVariantsL)
pEnumVariants = do
  _sym <- pSymbol SEnumVariantsSymbol
  iEnumVariants
    <$> pPair (pMaybe pVariant) (pMany pVariant)

pVariant :: Parser (MoveTerm VariantL)
pVariant = do
  _sym <- pSymbol SVariantSymbol
  iVariant
    <$> pHiddenVariantIdentifier
    <*> pMaybe pDatatypeFields

pHiddenVariantIdentifier :: Parser (MoveTerm HiddenVariantIdentifierL)
pHiddenVariantIdentifier = do
  iHiddenVariantIdentifier
    <$> pVariantIdentifier

pVariantIdentifier :: Parser (MoveTerm VariantIdentifierL)
pVariantIdentifier = do
  _sym <- pSymbol SVariantIdentifierSymbol
  pure iVariantIdentifier

pDatatypeFields :: Parser (MoveTerm DatatypeFieldsL)
pDatatypeFields = do
  _sym <- pSymbol SDatatypeFieldsSymbol
  choice [ pPositionalFieldsDatatypeFields
         , pNamedFieldsDatatypeFields
         ]
  where
    pPositionalFieldsDatatypeFields :: Parser (MoveTerm DatatypeFieldsL)
    pPositionalFieldsDatatypeFields =
      iPositionalFieldsDatatypeFields
        <$> pPositionalFields
    pNamedFieldsDatatypeFields :: Parser (MoveTerm DatatypeFieldsL)
    pNamedFieldsDatatypeFields =
      iNamedFieldsDatatypeFields
        <$> pNamedFields

pNamedFields :: Parser (MoveTerm NamedFieldsL)
pNamedFields = do
  _sym <- pSymbol SNamedFieldsSymbol
  iNamedFields
    <$> pPair (pMaybe pFieldAnnotation) (pMany pFieldAnnotation)

pFieldAnnotation :: Parser (MoveTerm FieldAnnotationL)
pFieldAnnotation = do
  _sym <- pSymbol SFieldAnnotationSymbol
  iFieldAnnotation
    <$> pHiddenFieldIdentifier
    <*> pHiddenType

pHiddenFieldIdentifier :: Parser (MoveTerm HiddenFieldIdentifierL)
pHiddenFieldIdentifier = do
  iHiddenFieldIdentifier
    <$> pFieldIdentifier

pFieldIdentifier :: Parser (MoveTerm FieldIdentifierL)
pFieldIdentifier = do
  _sym <- pSymbol SFieldIdentifierSymbol
  pure iFieldIdentifier

pHiddenType :: Parser (MoveTerm HiddenTypeL)
pHiddenType = do
  choice [ pApplyTypeType
         , pRefTypeType
         , pTupleTypeType
         , pFunctionTypeType
         , pPrimitiveTypeType
         ]
  where
    pApplyTypeType :: Parser (MoveTerm HiddenTypeL)
    pApplyTypeType =
      iApplyTypeType
        <$> pApplyType
    pRefTypeType :: Parser (MoveTerm HiddenTypeL)
    pRefTypeType =
      iRefTypeType
        <$> pRefType
    pTupleTypeType :: Parser (MoveTerm HiddenTypeL)
    pTupleTypeType =
      iTupleTypeType
        <$> pTupleType
    pFunctionTypeType :: Parser (MoveTerm HiddenTypeL)
    pFunctionTypeType =
      iFunctionTypeType
        <$> pFunctionType
    pPrimitiveTypeType :: Parser (MoveTerm HiddenTypeL)
    pPrimitiveTypeType =
      iPrimitiveTypeType
        <$> pPrimitiveType

pApplyType :: Parser (MoveTerm ApplyTypeL)
pApplyType = do
  _sym <- pSymbol SApplyTypeSymbol
  iApplyType
    <$> pModuleAccess
    <*> pMaybe pTypeArguments

pModuleAccess :: Parser (MoveTerm ModuleAccessL)
pModuleAccess = do
  _sym <- pSymbol SModuleAccessSymbol
  choice [ pModuleAccess1
         , pModuleAccess2
         , pMemberModuleAccess
         , pModuleAccess4
         , pModuleAccess5
         , pModuleAccess6
         , pModuleAccess7
         , pModuleAccess8
         , pModuleAccess9
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
        <$> pIdentifier
    pMemberModuleAccess :: Parser (MoveTerm ModuleAccessL)
    pMemberModuleAccess =
      iMemberModuleAccess
        <$> pIdentifier
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
        <*> pIdentifier
    pModuleAccess6 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess6 =
      iModuleAccess6
        <$> pModuleIdentity
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
        <*> pIdentifier
    pModuleAccess9 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess9 =
      iModuleAccess9
        <$> pModuleIdentity
        <*> pIdentifier
        <*> pMaybe pTypeArguments
        <*> pIdentifier

pHiddenModuleIdentifier :: Parser (MoveTerm HiddenModuleIdentifierL)
pHiddenModuleIdentifier = do
  iHiddenModuleIdentifier
    <$> pModuleIdentifier

pModuleIdentifier :: Parser (MoveTerm ModuleIdentifierL)
pModuleIdentifier = do
  _sym <- pSymbol SModuleIdentifierSymbol
  pure iModuleIdentifier

pIdentifier :: Parser (MoveTerm IdentifierL)
pIdentifier = do
  _sym <- pSymbol SIdentifierSymbol
  pure iIdentifier

pModuleIdentity :: Parser (MoveTerm ModuleIdentityL)
pModuleIdentity = do
  _sym <- pSymbol SModuleIdentitySymbol
  iModuleIdentity
    <$> pEither pHiddenModuleIdentifier pNumLiteral
    <*> pHiddenModuleIdentifier

pNumLiteral :: Parser (MoveTerm NumLiteralL)
pNumLiteral = do
  _sym <- pSymbol SNumLiteralSymbol
  iNumLiteral
    <$> pMaybe (pEither pU16 (pEither pU32 (pEither pU64 (pEither pU128 (pEither pU256 pU8)))))

pTypeArguments :: Parser (MoveTerm TypeArgumentsL)
pTypeArguments = do
  _sym <- pSymbol STypeArgumentsSymbol
  iTypeArguments
    <$> pLt
    <*> pPair (pMany pHiddenType) pHiddenType
    <*> pGt

pFunctionType :: Parser (MoveTerm FunctionTypeL)
pFunctionType = do
  _sym <- pSymbol SFunctionTypeSymbol
  iFunctionType
    <$> pFunctionTypeParameters
    <*> pMaybe pHiddenType

pFunctionTypeParameters :: Parser (MoveTerm FunctionTypeParametersL)
pFunctionTypeParameters = do
  _sym <- pSymbol SFunctionTypeParametersSymbol
  iFunctionTypeParameters
    <$> pBitor
    <*> pPair (pMaybe pHiddenType) (pMany pHiddenType)
    <*> pBitor

pPrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
pPrimitiveType = do
  _sym <- pSymbol SPrimitiveTypeSymbol
  choice [ pU8PrimitiveType
         , pU16PrimitiveType
         , pU32PrimitiveType
         , pU64PrimitiveType
         , pU128PrimitiveType
         , pU256PrimitiveType
         , pBoolPrimitiveType
         , pAddressPrimitiveType
         , pSignerPrimitiveType
         , pBytearrayPrimitiveType
         ]
  where
    pU8PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU8PrimitiveType =
      iU8PrimitiveType
        <$> pU8
    pU16PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU16PrimitiveType =
      iU16PrimitiveType
        <$> pU16
    pU32PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU32PrimitiveType =
      iU32PrimitiveType
        <$> pU32
    pU64PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU64PrimitiveType =
      iU64PrimitiveType
        <$> pU64
    pU128PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU128PrimitiveType =
      iU128PrimitiveType
        <$> pU128
    pU256PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU256PrimitiveType =
      iU256PrimitiveType
        <$> pU256
    pBoolPrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pBoolPrimitiveType =
      iBoolPrimitiveType
        <$> pBool
    pAddressPrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pAddressPrimitiveType =
      iAddressPrimitiveType
        <$> pAddress
    pSignerPrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pSignerPrimitiveType =
      iSignerPrimitiveType
        <$> pSigner
    pBytearrayPrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pBytearrayPrimitiveType =
      iBytearrayPrimitiveType
        <$> pBytearray

pRefType :: Parser (MoveTerm RefTypeL)
pRefType = do
  _sym <- pSymbol SRefTypeSymbol
  iRefType
    <$> pHiddenReference
    <*> pHiddenType

pHiddenReference :: Parser (MoveTerm HiddenReferenceL)
pHiddenReference = do
  choice [ pImmRefReference
         , pMutRefReference
         ]
  where
    pImmRefReference :: Parser (MoveTerm HiddenReferenceL)
    pImmRefReference =
      iImmRefReference
        <$> pImmRef
    pMutRefReference :: Parser (MoveTerm HiddenReferenceL)
    pMutRefReference =
      iMutRefReference
        <$> pMutRef

pImmRef :: Parser (MoveTerm ImmRefL)
pImmRef = do
  _sym <- pSymbol SImmRefSymbol
  iImmRef
    <$> pBitand

pMutRef :: Parser (MoveTerm MutRefL)
pMutRef = do
  _sym <- pSymbol SMutRefSymbol
  iMutRef
    <$> pBitand

pTupleType :: Parser (MoveTerm TupleTypeL)
pTupleType = do
  _sym <- pSymbol STupleTypeSymbol
  iTupleType
    <$> pPair (pMaybe pHiddenType) (pMany pHiddenType)

pPositionalFields :: Parser (MoveTerm PositionalFieldsL)
pPositionalFields = do
  _sym <- pSymbol SPositionalFieldsSymbol
  iPositionalFields
    <$> pPair (pMaybe pHiddenType) (pMany pHiddenType)

pPostfixAbilityDecls :: Parser (MoveTerm PostfixAbilityDeclsL)
pPostfixAbilityDecls = do
  _sym <- pSymbol SPostfixAbilityDeclsSymbol
  iPostfixAbilityDecls
    <$> pPair (pMaybe pAbility) (pMany pAbility)

pHiddenFunctionItem :: Parser (MoveTerm HiddenFunctionItemL)
pHiddenFunctionItem = do
  choice [ pNativeFunctionDefinitionFunctionItem
         , pMacroFunctionDefinitionFunctionItem
         , pFunctionDefinitionFunctionItem
         ]
  where
    pNativeFunctionDefinitionFunctionItem :: Parser (MoveTerm HiddenFunctionItemL)
    pNativeFunctionDefinitionFunctionItem =
      iNativeFunctionDefinitionFunctionItem
        <$> pNativeFunctionDefinition
    pMacroFunctionDefinitionFunctionItem :: Parser (MoveTerm HiddenFunctionItemL)
    pMacroFunctionDefinitionFunctionItem =
      iMacroFunctionDefinitionFunctionItem
        <$> pMacroFunctionDefinition
    pFunctionDefinitionFunctionItem :: Parser (MoveTerm HiddenFunctionItemL)
    pFunctionDefinitionFunctionItem =
      iFunctionDefinitionFunctionItem
        <$> pFunctionDefinition

pFunctionDefinition :: Parser (MoveTerm FunctionDefinitionL)
pFunctionDefinition = do
  _sym <- pSymbol SFunctionDefinitionSymbol
  iFunctionDefinition
    <$> pHiddenFunctionSignature
    <*> pBlock

pHiddenFunctionSignature :: Parser (MoveTerm HiddenFunctionSignatureL)
pHiddenFunctionSignature = do
  iHiddenFunctionSignature
    <$> pMaybe pModifier
    <*> pMaybe pModifier
    <*> pMaybe pModifier
    <*> pHiddenFunctionIdentifier
    <*> pMaybe pTypeParameters
    <*> pFunctionParameters
    <*> pMaybe pRetType

pHiddenFunctionIdentifier :: Parser (MoveTerm HiddenFunctionIdentifierL)
pHiddenFunctionIdentifier = do
  iHiddenFunctionIdentifier
    <$> pFunctionIdentifier

pFunctionIdentifier :: Parser (MoveTerm FunctionIdentifierL)
pFunctionIdentifier = do
  _sym <- pSymbol SFunctionIdentifierSymbol
  pure iFunctionIdentifier

pFunctionParameters :: Parser (MoveTerm FunctionParametersL)
pFunctionParameters = do
  _sym <- pSymbol SFunctionParametersSymbol
  iFunctionParameters
    <$> pPair (pMaybe (pEither pFunctionParameter pMutFunctionParameter)) (pMany (pEither pFunctionParameter pMutFunctionParameter))

pFunctionParameter :: Parser (MoveTerm FunctionParameterL)
pFunctionParameter = do
  _sym <- pSymbol SFunctionParameterSymbol
  iFunctionParameter
    <$> pEither (pPair pHiddenVariableIdentifier pDollar) pHiddenVariableIdentifier
    <*> pHiddenType

pHiddenVariableIdentifier :: Parser (MoveTerm HiddenVariableIdentifierL)
pHiddenVariableIdentifier = do
  iHiddenVariableIdentifier
    <$> pVariableIdentifier

pVariableIdentifier :: Parser (MoveTerm VariableIdentifierL)
pVariableIdentifier = do
  _sym <- pSymbol SVariableIdentifierSymbol
  pure iVariableIdentifier

pMutFunctionParameter :: Parser (MoveTerm MutFunctionParameterL)
pMutFunctionParameter = do
  _sym <- pSymbol SMutFunctionParameterSymbol
  iMutFunctionParameter
    <$> pFunctionParameter

pModifier :: Parser (MoveTerm ModifierL)
pModifier = do
  _sym <- pSymbol SModifierSymbol
  iModifier1
    <$> pPublic
    <*> pMaybe (pEither pFriend pPackage)

pRetType :: Parser (MoveTerm RetTypeL)
pRetType = do
  _sym <- pSymbol SRetTypeSymbol
  iRetType
    <$> pHiddenType

pBlock :: Parser (MoveTerm BlockL)
pBlock = do
  _sym <- pSymbol SBlockSymbol
  iBlock
    <$> pMany pUseDeclaration
    <*> pMany pBlockItem
    <*> pMaybe pHiddenExpression

pHiddenExpression :: Parser (MoveTerm HiddenExpressionL)
pHiddenExpression = do
  choice [ pCallExpressionExpression
         , pMacroCallExpressionExpression
         , pLambdaExpressionExpression
         , pIfExpressionExpression
         , pWhileExpressionExpression
         , pReturnExpressionExpression
         , pAbortExpressionExpression
         , pAssignExpressionExpression
         , pHiddenUnaryExpressionExpression
         , pBinaryExpressionExpression
         , pCastExpressionExpression
         , pQuantifierExpressionExpression
         , pMatchExpressionExpression
         , pVectorExpressionExpression
         , pLoopExpressionExpression
         , pIdentifiedExpressionExpression
         ]
  where
    pCallExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pCallExpressionExpression =
      iCallExpressionExpression
        <$> pCallExpression
    pMacroCallExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pMacroCallExpressionExpression =
      iMacroCallExpressionExpression
        <$> pMacroCallExpression
    pLambdaExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pLambdaExpressionExpression =
      iLambdaExpressionExpression
        <$> pLambdaExpression
    pIfExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pIfExpressionExpression =
      iIfExpressionExpression
        <$> pIfExpression
    pWhileExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pWhileExpressionExpression =
      iWhileExpressionExpression
        <$> pWhileExpression
    pReturnExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pReturnExpressionExpression =
      iReturnExpressionExpression
        <$> pReturnExpression
    pAbortExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pAbortExpressionExpression =
      iAbortExpressionExpression
        <$> pAbortExpression
    pAssignExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pAssignExpressionExpression =
      iAssignExpressionExpression
        <$> pAssignExpression
    pHiddenUnaryExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenUnaryExpressionExpression =
      iHiddenUnaryExpressionExpression
        <$> pHiddenUnaryExpression
    pBinaryExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pBinaryExpressionExpression =
      iBinaryExpressionExpression
        <$> pBinaryExpression
    pCastExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pCastExpressionExpression =
      iCastExpressionExpression
        <$> pCastExpression
    pQuantifierExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pQuantifierExpressionExpression =
      iQuantifierExpressionExpression
        <$> pQuantifierExpression
    pMatchExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pMatchExpressionExpression =
      iMatchExpressionExpression
        <$> pMatchExpression
    pVectorExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pVectorExpressionExpression =
      iVectorExpressionExpression
        <$> pVectorExpression
    pLoopExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pLoopExpressionExpression =
      iLoopExpressionExpression
        <$> pLoopExpression
    pIdentifiedExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pIdentifiedExpressionExpression =
      iIdentifiedExpressionExpression
        <$> pIdentifiedExpression

pHiddenUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
pHiddenUnaryExpression = do
  choice [ pUnaryExpressionUnaryExpression
         , pBorrowExpressionUnaryExpression
         , pDereferenceExpressionUnaryExpression
         , pMoveOrCopyExpressionUnaryExpression
         , pHiddenExpressionTermUnaryExpression
         ]
  where
    pUnaryExpressionUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
    pUnaryExpressionUnaryExpression =
      iUnaryExpressionUnaryExpression
        <$> pUnaryExpression
    pBorrowExpressionUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
    pBorrowExpressionUnaryExpression =
      iBorrowExpressionUnaryExpression
        <$> pBorrowExpression
    pDereferenceExpressionUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
    pDereferenceExpressionUnaryExpression =
      iDereferenceExpressionUnaryExpression
        <$> pDereferenceExpression
    pMoveOrCopyExpressionUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
    pMoveOrCopyExpressionUnaryExpression =
      iMoveOrCopyExpressionUnaryExpression
        <$> pMoveOrCopyExpression
    pHiddenExpressionTermUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
    pHiddenExpressionTermUnaryExpression =
      iHiddenExpressionTermUnaryExpression
        <$> pHiddenExpressionTerm

pHiddenExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
pHiddenExpressionTerm = do
  choice [ pCallExpressionExpressionTerm
         , pBreakExpressionExpressionTerm
         , pContinueExpressionExpressionTerm
         , pNameExpressionExpressionTerm
         , pMacroCallExpressionExpressionTerm
         , pPackExpressionExpressionTerm
         , pHiddenLiteralValueExpressionTerm
         , pUnitExpressionExpressionTerm
         , pExpressionListExpressionTerm
         , pAnnotationExpressionExpressionTerm
         , pBlockExpressionTerm
         , pSpecBlockExpressionTerm
         , pIfExpressionExpressionTerm
         , pDotExpressionExpressionTerm
         , pIndexExpressionExpressionTerm
         , pVectorExpressionExpressionTerm
         , pMatchExpressionExpressionTerm
         ]
  where
    pCallExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pCallExpressionExpressionTerm =
      iCallExpressionExpressionTerm
        <$> pCallExpression
    pBreakExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pBreakExpressionExpressionTerm =
      iBreakExpressionExpressionTerm
        <$> pBreakExpression
    pContinueExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pContinueExpressionExpressionTerm =
      iContinueExpressionExpressionTerm
        <$> pContinueExpression
    pNameExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pNameExpressionExpressionTerm =
      iNameExpressionExpressionTerm
        <$> pNameExpression
    pMacroCallExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pMacroCallExpressionExpressionTerm =
      iMacroCallExpressionExpressionTerm
        <$> pMacroCallExpression
    pPackExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pPackExpressionExpressionTerm =
      iPackExpressionExpressionTerm
        <$> pPackExpression
    pHiddenLiteralValueExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenLiteralValueExpressionTerm =
      iHiddenLiteralValueExpressionTerm
        <$> pHiddenLiteralValue
    pUnitExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pUnitExpressionExpressionTerm =
      iUnitExpressionExpressionTerm
        <$> pUnitExpression
    pExpressionListExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pExpressionListExpressionTerm =
      iExpressionListExpressionTerm
        <$> pExpressionList
    pAnnotationExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pAnnotationExpressionExpressionTerm =
      iAnnotationExpressionExpressionTerm
        <$> pAnnotationExpression
    pBlockExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pBlockExpressionTerm =
      iBlockExpressionTerm
        <$> pBlock
    pSpecBlockExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pSpecBlockExpressionTerm =
      iSpecBlockExpressionTerm
        <$> pSpecBlock
    pIfExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pIfExpressionExpressionTerm =
      iIfExpressionExpressionTerm
        <$> pIfExpression
    pDotExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pDotExpressionExpressionTerm =
      iDotExpressionExpressionTerm
        <$> pDotExpression
    pIndexExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pIndexExpressionExpressionTerm =
      iIndexExpressionExpressionTerm
        <$> pIndexExpression
    pVectorExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pVectorExpressionExpressionTerm =
      iVectorExpressionExpressionTerm
        <$> pVectorExpression
    pMatchExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pMatchExpressionExpressionTerm =
      iMatchExpressionExpressionTerm
        <$> pMatchExpression

pHiddenLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
pHiddenLiteralValue = do
  choice [ pAddressLiteralLiteralValue
         , pBoolLiteralLiteralValue
         , pNumLiteralLiteralValue
         , pHexStringLiteralLiteralValue
         , pByteStringLiteralLiteralValue
         ]
  where
    pAddressLiteralLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
    pAddressLiteralLiteralValue =
      iAddressLiteralLiteralValue
        <$> pAddressLiteral
    pBoolLiteralLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
    pBoolLiteralLiteralValue =
      iBoolLiteralLiteralValue
        <$> pBoolLiteral
    pNumLiteralLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
    pNumLiteralLiteralValue =
      iNumLiteralLiteralValue
        <$> pNumLiteral
    pHexStringLiteralLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
    pHexStringLiteralLiteralValue =
      iHexStringLiteralLiteralValue
        <$> pHexStringLiteral
    pByteStringLiteralLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
    pByteStringLiteralLiteralValue =
      iByteStringLiteralLiteralValue
        <$> pByteStringLiteral

pAddressLiteral :: Parser (MoveTerm AddressLiteralL)
pAddressLiteral = do
  _sym <- pSymbol SAddressLiteralSymbol
  pure iAddressLiteral

pBoolLiteral :: Parser (MoveTerm BoolLiteralL)
pBoolLiteral = do
  _sym <- pSymbol SBoolLiteralSymbol
  choice [ pTrueBoolLiteral
         , pFalseBoolLiteral
         ]
  where
    pTrueBoolLiteral :: Parser (MoveTerm BoolLiteralL)
    pTrueBoolLiteral =
      iTrueBoolLiteral
        <$> pTrue
    pFalseBoolLiteral :: Parser (MoveTerm BoolLiteralL)
    pFalseBoolLiteral =
      iFalseBoolLiteral
        <$> pFalse

pByteStringLiteral :: Parser (MoveTerm ByteStringLiteralL)
pByteStringLiteral = do
  _sym <- pSymbol SByteStringLiteralSymbol
  pure iByteStringLiteral

pHexStringLiteral :: Parser (MoveTerm HexStringLiteralL)
pHexStringLiteral = do
  _sym <- pSymbol SHexStringLiteralSymbol
  pure iHexStringLiteral

pAnnotationExpression :: Parser (MoveTerm AnnotationExpressionL)
pAnnotationExpression = do
  _sym <- pSymbol SAnnotationExpressionSymbol
  iAnnotationExpression
    <$> pHiddenExpression
    <*> pHiddenType

pBreakExpression :: Parser (MoveTerm BreakExpressionL)
pBreakExpression = do
  _sym <- pSymbol SBreakExpressionSymbol
  iBreakExpression
    <$> pMaybe pLabel
    <*> pMaybe pHiddenExpression

pLabel :: Parser (MoveTerm LabelL)
pLabel = do
  _sym <- pSymbol SLabelSymbol
  iLabel
    <$> pIdentifier

pCallExpression :: Parser (MoveTerm CallExpressionL)
pCallExpression = do
  _sym <- pSymbol SCallExpressionSymbol
  iCallExpression
    <$> pNameExpression
    <*> pArgList

pArgList :: Parser (MoveTerm ArgListL)
pArgList = do
  _sym <- pSymbol SArgListSymbol
  iArgList
    <$> pPair (pMaybe pHiddenExpression) (pMany pHiddenExpression)

pNameExpression :: Parser (MoveTerm NameExpressionL)
pNameExpression = do
  _sym <- pSymbol SNameExpressionSymbol
  iNameExpression
    <$> pModuleAccess

pContinueExpression :: Parser (MoveTerm ContinueExpressionL)
pContinueExpression = do
  _sym <- pSymbol SContinueExpressionSymbol
  iContinueExpression
    <$> pMaybe pLabel

pDotExpression :: Parser (MoveTerm DotExpressionL)
pDotExpression = do
  _sym <- pSymbol SDotExpressionSymbol
  iDotExpression
    <$> pHiddenExpressionTerm
    <*> pHiddenExpressionTerm

pExpressionList :: Parser (MoveTerm ExpressionListL)
pExpressionList = do
  _sym <- pSymbol SExpressionListSymbol
  iExpressionList
    <$> pPair (pMany pHiddenExpression) pHiddenExpression

pIfExpression :: Parser (MoveTerm IfExpressionL)
pIfExpression = do
  _sym <- pSymbol SIfExpressionSymbol
  iIfExpression
    <$> pHiddenExpression
    <*> pHiddenExpression
    <*> pMaybe pHiddenExpression

pIndexExpression :: Parser (MoveTerm IndexExpressionL)
pIndexExpression = do
  _sym <- pSymbol SIndexExpressionSymbol
  iIndexExpression
    <$> pHiddenExpressionTerm
    <*> pPair (pMaybe pHiddenExpression) (pMany pHiddenExpression)

pMacroCallExpression :: Parser (MoveTerm MacroCallExpressionL)
pMacroCallExpression = do
  _sym <- pSymbol SMacroCallExpressionSymbol
  iMacroCallExpression
    <$> pMacroModuleAccess
    <*> pMaybe pTypeArguments
    <*> pArgList

pMacroModuleAccess :: Parser (MoveTerm MacroModuleAccessL)
pMacroModuleAccess = do
  _sym <- pSymbol SMacroModuleAccessSymbol
  iMacroModuleAccess
    <$> pModuleAccess
    <*> pBang

pMatchExpression :: Parser (MoveTerm MatchExpressionL)
pMatchExpression = do
  _sym <- pSymbol SMatchExpressionSymbol
  iMatchExpression
    <$> pHiddenExpression
    <*> pHiddenMatchBody

pHiddenMatchBody :: Parser (MoveTerm HiddenMatchBodyL)
pHiddenMatchBody = do
  iHiddenMatchBody
    <$> pPair (pMaybe pMatchArm) (pMany pMatchArm)

pMatchArm :: Parser (MoveTerm MatchArmL)
pMatchArm = do
  _sym <- pSymbol SMatchArmSymbol
  iMatchArm
    <$> pBindList
    <*> pMaybe pMatchCondition
    <*> pHiddenExpression

pBindList :: Parser (MoveTerm BindListL)
pBindList = do
  _sym <- pSymbol SBindListSymbol
  choice [ pHiddenBindBindList
         , pCommaBindListBindList
         , pOrBindListBindList
         ]
  where
    pHiddenBindBindList :: Parser (MoveTerm BindListL)
    pHiddenBindBindList =
      iHiddenBindBindList
        <$> pHiddenBind
    pCommaBindListBindList :: Parser (MoveTerm BindListL)
    pCommaBindListBindList =
      iCommaBindListBindList
        <$> pCommaBindList
    pOrBindListBindList :: Parser (MoveTerm BindListL)
    pOrBindListBindList =
      iOrBindListBindList
        <$> pOrBindList

pHiddenBind :: Parser (MoveTerm HiddenBindL)
pHiddenBind = do
  choice [ pMutBindVarBind
         , pBindVarBind
         , pBindUnpackBind
         , pAtBindBind
         , pHiddenLiteralValueBind
         ]
  where
    pMutBindVarBind :: Parser (MoveTerm HiddenBindL)
    pMutBindVarBind =
      iMutBindVarBind
        <$> pMutBindVar
    pBindVarBind :: Parser (MoveTerm HiddenBindL)
    pBindVarBind =
      iBindVarBind
        <$> pHiddenVariableIdentifier
    pBindUnpackBind :: Parser (MoveTerm HiddenBindL)
    pBindUnpackBind =
      iBindUnpackBind
        <$> pBindUnpack
    pAtBindBind :: Parser (MoveTerm HiddenBindL)
    pAtBindBind =
      iAtBindBind
        <$> pAtBind
    pHiddenLiteralValueBind :: Parser (MoveTerm HiddenBindL)
    pHiddenLiteralValueBind =
      iHiddenLiteralValueBind
        <$> pHiddenLiteralValue

pAtBind :: Parser (MoveTerm AtBindL)
pAtBind = do
  _sym <- pSymbol SAtBindSymbol
  iAtBind
    <$> pHiddenVariableIdentifier
    <*> pBindList

pBindUnpack :: Parser (MoveTerm BindUnpackL)
pBindUnpack = do
  _sym <- pSymbol SBindUnpackSymbol
  iBindUnpack
    <$> pNameExpression
    <*> pMaybe pBindFields

pBindFields :: Parser (MoveTerm BindFieldsL)
pBindFields = do
  _sym <- pSymbol SBindFieldsSymbol
  choice [ pBindPositionalFieldsBindFields
         , pBindNamedFieldsBindFields
         ]
  where
    pBindPositionalFieldsBindFields :: Parser (MoveTerm BindFieldsL)
    pBindPositionalFieldsBindFields =
      iBindPositionalFieldsBindFields
        <$> pBindPositionalFields
    pBindNamedFieldsBindFields :: Parser (MoveTerm BindFieldsL)
    pBindNamedFieldsBindFields =
      iBindNamedFieldsBindFields
        <$> pBindNamedFields

pBindNamedFields :: Parser (MoveTerm BindNamedFieldsL)
pBindNamedFields = do
  _sym <- pSymbol SBindNamedFieldsSymbol
  iBindNamedFields
    <$> pPair (pMaybe (pEither pMutBindField pBindField)) (pMany (pEither pMutBindField pBindField))

pBindField :: Parser (MoveTerm BindFieldL)
pBindField = do
  _sym <- pSymbol SBindFieldSymbol
  choice [ pBindField1
         , pHiddenSpreadOperatorBindField
         ]
  where
    pBindField1 :: Parser (MoveTerm BindFieldL)
    pBindField1 =
      iBindField1
        <$> pBindList
        <*> pMaybe pBindList
    pHiddenSpreadOperatorBindField :: Parser (MoveTerm BindFieldL)
    pHiddenSpreadOperatorBindField =
      iHiddenSpreadOperatorBindField
        <$> pHiddenSpreadOperator

pHiddenSpreadOperator :: Parser (MoveTerm HiddenSpreadOperatorL)
pHiddenSpreadOperator = do
  iHiddenSpreadOperator
    <$> pRange

pMutBindField :: Parser (MoveTerm MutBindFieldL)
pMutBindField = do
  _sym <- pSymbol SMutBindFieldSymbol
  iMutBindField
    <$> pBindField

pBindPositionalFields :: Parser (MoveTerm BindPositionalFieldsL)
pBindPositionalFields = do
  _sym <- pSymbol SBindPositionalFieldsSymbol
  iBindPositionalFields
    <$> pPair (pMaybe (pEither pMutBindField pBindField)) (pMany (pEither pMutBindField pBindField))

pMutBindVar :: Parser (MoveTerm MutBindVarL)
pMutBindVar = do
  _sym <- pSymbol SMutBindVarSymbol
  iMutBindVar
    <$> pBindVar

pBindVar :: Parser (MoveTerm BindVarL)
pBindVar = do
  _sym <- pSymbol SBindVarSymbol
  iBindVar
    <$> pVariableIdentifier

pCommaBindList :: Parser (MoveTerm CommaBindListL)
pCommaBindList = do
  _sym <- pSymbol SCommaBindListSymbol
  iCommaBindList
    <$> pPair (pMaybe pHiddenBind) (pMany pHiddenBind)

pOrBindList :: Parser (MoveTerm OrBindListL)
pOrBindList = do
  _sym <- pSymbol SOrBindListSymbol
  iOrBindList
    <$> pPair (pMany (pPair pHiddenBind pBitor)) (pPair (pMaybe pBitor) pHiddenBind)

pMatchCondition :: Parser (MoveTerm MatchConditionL)
pMatchCondition = do
  _sym <- pSymbol SMatchConditionSymbol
  iMatchCondition
    <$> pHiddenExpression

pPackExpression :: Parser (MoveTerm PackExpressionL)
pPackExpression = do
  _sym <- pSymbol SPackExpressionSymbol
  iPackExpression
    <$> pNameExpression
    <*> pFieldInitializeList

pFieldInitializeList :: Parser (MoveTerm FieldInitializeListL)
pFieldInitializeList = do
  _sym <- pSymbol SFieldInitializeListSymbol
  iFieldInitializeList
    <$> pPair (pMaybe pExpField) (pMany pExpField)

pExpField :: Parser (MoveTerm ExpFieldL)
pExpField = do
  _sym <- pSymbol SExpFieldSymbol
  iExpField
    <$> pHiddenFieldIdentifier
    <*> pMaybe pHiddenExpression

pSpecBlock :: Parser (MoveTerm SpecBlockL)
pSpecBlock = do
  _sym <- pSymbol SSpecBlockSymbol
  iSpecBlock
    <$> pEither pHiddenSpecFunction (pPair pSpecBody (pMaybe pHiddenSpecBlockTarget))

pHiddenSpecBlockTarget :: Parser (MoveTerm HiddenSpecBlockTargetL)
pHiddenSpecBlockTarget = do
  choice [ pIdentifierSpecBlockTarget
         , pSpecBlockTargetModuleSpecBlockTarget
         , pSpecBlockTargetSchemaSpecBlockTarget
         ]
  where
    pIdentifierSpecBlockTarget :: Parser (MoveTerm HiddenSpecBlockTargetL)
    pIdentifierSpecBlockTarget =
      iIdentifierSpecBlockTarget
        <$> pIdentifier
    pSpecBlockTargetModuleSpecBlockTarget :: Parser (MoveTerm HiddenSpecBlockTargetL)
    pSpecBlockTargetModuleSpecBlockTarget =
      iSpecBlockTargetModuleSpecBlockTarget
        <$> pModule
    pSpecBlockTargetSchemaSpecBlockTarget :: Parser (MoveTerm HiddenSpecBlockTargetL)
    pSpecBlockTargetSchemaSpecBlockTarget =
      iSpecBlockTargetSchemaSpecBlockTarget
        <$> pSpecBlockTargetSchema

pSpecBlockTargetSchema :: Parser (MoveTerm SpecBlockTargetSchemaL)
pSpecBlockTargetSchema = do
  _sym <- pSymbol SSpecBlockTargetSchemaSymbol
  iSpecBlockTargetSchema
    <$> pHiddenStructIdentifier
    <*> pMaybe pTypeParameters

pHiddenStructIdentifier :: Parser (MoveTerm HiddenStructIdentifierL)
pHiddenStructIdentifier = do
  iHiddenStructIdentifier
    <$> pStructIdentifier

pStructIdentifier :: Parser (MoveTerm StructIdentifierL)
pStructIdentifier = do
  _sym <- pSymbol SStructIdentifierSymbol
  pure iStructIdentifier

pHiddenSpecFunction :: Parser (MoveTerm HiddenSpecFunctionL)
pHiddenSpecFunction = do
  choice [ pNativeSpecFunctionSpecFunction
         , pUsualSpecFunctionSpecFunction
         , pUninterpretedSpecFunctionSpecFunction
         ]
  where
    pNativeSpecFunctionSpecFunction :: Parser (MoveTerm HiddenSpecFunctionL)
    pNativeSpecFunctionSpecFunction =
      iNativeSpecFunctionSpecFunction
        <$> pNativeSpecFunction
    pUsualSpecFunctionSpecFunction :: Parser (MoveTerm HiddenSpecFunctionL)
    pUsualSpecFunctionSpecFunction =
      iUsualSpecFunctionSpecFunction
        <$> pUsualSpecFunction
    pUninterpretedSpecFunctionSpecFunction :: Parser (MoveTerm HiddenSpecFunctionL)
    pUninterpretedSpecFunctionSpecFunction =
      iUninterpretedSpecFunctionSpecFunction
        <$> pUninterpretedSpecFunction

pNativeSpecFunction :: Parser (MoveTerm NativeSpecFunctionL)
pNativeSpecFunction = do
  _sym <- pSymbol SNativeSpecFunctionSymbol
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
  _sym <- pSymbol SUninterpretedSpecFunctionSymbol
  iUninterpretedSpecFunction
    <$> pHiddenSpecFunctionSignature

pUsualSpecFunction :: Parser (MoveTerm UsualSpecFunctionL)
pUsualSpecFunction = do
  _sym <- pSymbol SUsualSpecFunctionSymbol
  iUsualSpecFunction
    <$> pHiddenSpecFunctionSignature
    <*> pBlock

pSpecBody :: Parser (MoveTerm SpecBodyL)
pSpecBody = do
  _sym <- pSymbol SSpecBodySymbol
  iSpecBody
    <$> pMany pUseDeclaration
    <*> pMany pHiddenSpecBlockMemeber

pHiddenSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
pHiddenSpecBlockMemeber = do
  choice [ pSpecInvariantSpecBlockMemeber
         , pHiddenSpecFunctionSpecBlockMemeber
         , pSpecConditionSpecBlockMemeber
         , pSpecIncludeSpecBlockMemeber
         , pSpecApplySpecBlockMemeber
         , pSpecPragmaSpecBlockMemeber
         , pSpecVariableSpecBlockMemeber
         , pSpecLetSpecBlockMemeber
         ]
  where
    pSpecInvariantSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecInvariantSpecBlockMemeber =
      iSpecInvariantSpecBlockMemeber
        <$> pSpecInvariant
    pHiddenSpecFunctionSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecFunctionSpecBlockMemeber =
      iHiddenSpecFunctionSpecBlockMemeber
        <$> pHiddenSpecFunction
    pSpecConditionSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecConditionSpecBlockMemeber =
      iSpecConditionSpecBlockMemeber
        <$> pSpecCondition
    pSpecIncludeSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecIncludeSpecBlockMemeber =
      iSpecIncludeSpecBlockMemeber
        <$> pSpecInclude
    pSpecApplySpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecApplySpecBlockMemeber =
      iSpecApplySpecBlockMemeber
        <$> pSpecApply
    pSpecPragmaSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecPragmaSpecBlockMemeber =
      iSpecPragmaSpecBlockMemeber
        <$> pSpecPragma
    pSpecVariableSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecVariableSpecBlockMemeber =
      iSpecVariableSpecBlockMemeber
        <$> pSpecVariable
    pSpecLetSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecLetSpecBlockMemeber =
      iSpecLetSpecBlockMemeber
        <$> pSpecLet

pSpecApply :: Parser (MoveTerm SpecApplyL)
pSpecApply = do
  _sym <- pSymbol SSpecApplySymbol
  iSpecApply
    <$> pHiddenExpression
    <*> pPair (pMany pSpecApplyPattern) pSpecApplyPattern
    <*> pMaybe (pPair (pMany pSpecApplyPattern) pSpecApplyPattern)

pSpecApplyPattern :: Parser (MoveTerm SpecApplyPatternL)
pSpecApplyPattern = do
  _sym <- pSymbol SSpecApplyPatternSymbol
  iSpecApplyPattern
    <$> pMaybe (pEither pInternal pPublic)
    <*> pSpecApplyNamePattern
    <*> pMaybe pTypeParameters

pSpecApplyNamePattern :: Parser (MoveTerm SpecApplyNamePatternL)
pSpecApplyNamePattern = do
  _sym <- pSymbol SSpecApplyNamePatternSymbol
  pure iSpecApplyNamePattern

pSpecCondition :: Parser (MoveTerm SpecConditionL)
pSpecCondition = do
  _sym <- pSymbol SSpecConditionSymbol
  choice [ pHiddenSpecConditionSpecCondition
         , pHiddenSpecAbortIfSpecCondition
         , pHiddenSpecAbortWithOrModifiesSpecCondition
         ]
  where
    pHiddenSpecConditionSpecCondition :: Parser (MoveTerm SpecConditionL)
    pHiddenSpecConditionSpecCondition =
      iHiddenSpecConditionSpecCondition
        <$> pHiddenSpecCondition
    pHiddenSpecAbortIfSpecCondition :: Parser (MoveTerm SpecConditionL)
    pHiddenSpecAbortIfSpecCondition =
      iHiddenSpecAbortIfSpecCondition
        <$> pHiddenSpecAbortIf
    pHiddenSpecAbortWithOrModifiesSpecCondition :: Parser (MoveTerm SpecConditionL)
    pHiddenSpecAbortWithOrModifiesSpecCondition =
      iHiddenSpecAbortWithOrModifiesSpecCondition
        <$> pHiddenSpecAbortWithOrModifies

pHiddenSpecAbortIf :: Parser (MoveTerm HiddenSpecAbortIfL)
pHiddenSpecAbortIf = do
  iHiddenSpecAbortIf
    <$> pConditionKind
    <*> pMaybe pConditionProperties
    <*> pHiddenExpression
    <*> pMaybe pHiddenExpression

pConditionKind :: Parser (MoveTerm ConditionKindL)
pConditionKind = do
  _sym <- pSymbol SConditionKindSymbol
  pure iConditionKind

pConditionProperties :: Parser (MoveTerm ConditionPropertiesL)
pConditionProperties = do
  _sym <- pSymbol SConditionPropertiesSymbol
  iConditionProperties
    <$> pPair (pMaybe pSpecProperty) (pMany pSpecProperty)

pSpecProperty :: Parser (MoveTerm SpecPropertyL)
pSpecProperty = do
  _sym <- pSymbol SSpecPropertySymbol
  iSpecProperty
    <$> pIdentifier
    <*> pMaybe (pPair pHiddenLiteralValue pAssign)

pHiddenSpecAbortWithOrModifies :: Parser (MoveTerm HiddenSpecAbortWithOrModifiesL)
pHiddenSpecAbortWithOrModifies = do
  iHiddenSpecAbortWithOrModifies
    <$> pConditionKind
    <*> pMaybe pConditionProperties
    <*> pPair (pMany pHiddenExpression) pHiddenExpression

pHiddenSpecCondition :: Parser (MoveTerm HiddenSpecConditionL)
pHiddenSpecCondition = do
  iHiddenSpecCondition
    <$> pEither (pPair (pMaybe pModule) pConditionKind) pConditionKind
    <*> pMaybe pConditionProperties
    <*> pHiddenExpression

pSpecInclude :: Parser (MoveTerm SpecIncludeL)
pSpecInclude = do
  _sym <- pSymbol SSpecIncludeSymbol
  iSpecInclude
    <$> pHiddenExpression

pSpecInvariant :: Parser (MoveTerm SpecInvariantL)
pSpecInvariant = do
  _sym <- pSymbol SSpecInvariantSymbol
  iSpecInvariant
    <$> pConditionKind
    <*> pMaybe pInvariantModifier
    <*> pMaybe pConditionProperties
    <*> pHiddenExpression

pInvariantModifier :: Parser (MoveTerm InvariantModifierL)
pInvariantModifier = do
  _sym <- pSymbol SInvariantModifierSymbol
  choice [ pUpdateInvariantModifier
         , pPackInvariantModifier
         , pUnpackInvariantModifier
         , pModuleInvariantModifier
         ]
  where
    pUpdateInvariantModifier :: Parser (MoveTerm InvariantModifierL)
    pUpdateInvariantModifier =
      iUpdateInvariantModifier
        <$> pUpdate
    pPackInvariantModifier :: Parser (MoveTerm InvariantModifierL)
    pPackInvariantModifier =
      iPackInvariantModifier
        <$> pPack
    pUnpackInvariantModifier :: Parser (MoveTerm InvariantModifierL)
    pUnpackInvariantModifier =
      iUnpackInvariantModifier
        <$> pUnpack
    pModuleInvariantModifier :: Parser (MoveTerm InvariantModifierL)
    pModuleInvariantModifier =
      iModuleInvariantModifier
        <$> pModule

pSpecLet :: Parser (MoveTerm SpecLetL)
pSpecLet = do
  _sym <- pSymbol SSpecLetSymbol
  iSpecLet
    <$> pMaybe pPost
    <*> pIdentifier
    <*> pAssign
    <*> pHiddenExpression

pSpecPragma :: Parser (MoveTerm SpecPragmaL)
pSpecPragma = do
  _sym <- pSymbol SSpecPragmaSymbol
  iSpecPragma
    <$> pPair (pMaybe pSpecProperty) (pMany pSpecProperty)

pSpecVariable :: Parser (MoveTerm SpecVariableL)
pSpecVariable = do
  _sym <- pSymbol SSpecVariableSymbol
  iSpecVariable
    <$> pMaybe (pEither pLocal pGlobal)
    <*> pIdentifier
    <*> pMaybe pTypeParameters
    <*> pHiddenType

pUseDeclaration :: Parser (MoveTerm UseDeclarationL)
pUseDeclaration = do
  _sym <- pSymbol SUseDeclarationSymbol
  iUseDeclaration
    <$> pMaybe pPublic
    <*> pEither pUseModule (pEither pUseModuleMember (pEither pUseModuleMembers pUseFun))

pUseFun :: Parser (MoveTerm UseFunL)
pUseFun = do
  _sym <- pSymbol SUseFunSymbol
  iUseFun
    <$> pModuleAccess
    <*> pPair pHiddenFunctionIdentifier pModuleAccess

pUseModule :: Parser (MoveTerm UseModuleL)
pUseModule = do
  _sym <- pSymbol SUseModuleSymbol
  iUseModule
    <$> pModuleIdentity
    <*> pMaybe pHiddenModuleIdentifier

pUseModuleMember :: Parser (MoveTerm UseModuleMemberL)
pUseModuleMember = do
  _sym <- pSymbol SUseModuleMemberSymbol
  iUseModuleMember
    <$> pModuleIdentity
    <*> pUseMember

pUseMember :: Parser (MoveTerm UseMemberL)
pUseMember = do
  _sym <- pSymbol SUseMemberSymbol
  choice [ pUseMember1
         , pUseMember2
         , pUseMember3
         ]
  where
    pUseMember1 :: Parser (MoveTerm UseMemberL)
    pUseMember1 =
      iUseMember1
        <$> pIdentifier
        <*> pPair (pMany pUseMember) pUseMember
    pUseMember2 :: Parser (MoveTerm UseMemberL)
    pUseMember2 =
      iUseMember2
        <$> pIdentifier
        <*> pIdentifier
        <*> pMaybe pIdentifier
    pUseMember3 :: Parser (MoveTerm UseMemberL)
    pUseMember3 =
      iUseMember3
        <$> pIdentifier
        <*> pMaybe pIdentifier

pUseModuleMembers :: Parser (MoveTerm UseModuleMembersL)
pUseModuleMembers = do
  _sym <- pSymbol SUseModuleMembersSymbol
  choice [ pUseModuleMembers1
         , pUseModuleMembers2
         ]
  where
    pUseModuleMembers1 :: Parser (MoveTerm UseModuleMembersL)
    pUseModuleMembers1 =
      iUseModuleMembers1
        <$> pEither pHiddenModuleIdentifier pNumLiteral
        <*> pPair (pMany pUseMember) pUseMember
    pUseModuleMembers2 :: Parser (MoveTerm UseModuleMembersL)
    pUseModuleMembers2 =
      iUseModuleMembers2
        <$> pModuleIdentity
        <*> pPair (pMany pUseMember) pUseMember

pUnitExpression :: Parser (MoveTerm UnitExpressionL)
pUnitExpression = do
  _sym <- pSymbol SUnitExpressionSymbol
  pure iUnitExpression

pVectorExpression :: Parser (MoveTerm VectorExpressionL)
pVectorExpression = do
  _sym <- pSymbol SVectorExpressionSymbol
  iVectorExpression
    <$> pMaybe (pPair (pPair (pMany pHiddenType) pHiddenType) pGt)
    <*> pPair (pMaybe pHiddenExpression) (pMany pHiddenExpression)

pBorrowExpression :: Parser (MoveTerm BorrowExpressionL)
pBorrowExpression = do
  _sym <- pSymbol SBorrowExpressionSymbol
  iBorrowExpression
    <$> pHiddenReference
    <*> pHiddenExpression

pDereferenceExpression :: Parser (MoveTerm DereferenceExpressionL)
pDereferenceExpression = do
  _sym <- pSymbol SDereferenceExpressionSymbol
  iDereferenceExpression
    <$> pMul
    <*> pHiddenExpression

pMoveOrCopyExpression :: Parser (MoveTerm MoveOrCopyExpressionL)
pMoveOrCopyExpression = do
  _sym <- pSymbol SMoveOrCopyExpressionSymbol
  iMoveOrCopyExpression
    <$> pEither pCopy pMove
    <*> pHiddenExpression

pUnaryExpression :: Parser (MoveTerm UnaryExpressionL)
pUnaryExpression = do
  _sym <- pSymbol SUnaryExpressionSymbol
  iUnaryExpression
    <$> pUnaryOp
    <*> pHiddenExpression

pUnaryOp :: Parser (MoveTerm UnaryOpL)
pUnaryOp = do
  _sym <- pSymbol SUnaryOpSymbol
  iBangUnaryOp
    <$> pBang

pAbortExpression :: Parser (MoveTerm AbortExpressionL)
pAbortExpression = do
  _sym <- pSymbol SAbortExpressionSymbol
  iAbortExpression
    <$> pMaybe pHiddenExpression

pAssignExpression :: Parser (MoveTerm AssignExpressionL)
pAssignExpression = do
  _sym <- pSymbol SAssignExpressionSymbol
  iAssignExpression
    <$> pHiddenUnaryExpression
    <*> pAssign
    <*> pHiddenExpression

pBinaryExpression :: Parser (MoveTerm BinaryExpressionL)
pBinaryExpression = do
  _sym <- pSymbol SBinaryExpressionSymbol
  choice [ pBinaryExpression1
         , pBinaryExpression2
         , pBinaryExpression3
         , pBinaryExpression4
         , pBinaryExpression5
         , pBinaryExpression6
         , pBinaryExpression7
         , pBinaryExpression8
         , pBinaryExpression9
         , pBinaryExpression10
         , pBinaryExpression11
         , pBinaryExpression12
         , pBinaryExpression13
         , pBinaryExpression14
         , pBinaryExpression15
         , pBinaryExpression16
         , pBinaryExpression17
         , pBinaryExpression18
         , pBinaryExpression19
         , pBinaryExpression20
         ]
  where
    pBinaryExpression1 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression1 =
      iBinaryExpression1
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression2 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression2 =
      iBinaryExpression2
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression3 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression3 =
      iBinaryExpression3
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression4 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression4 =
      iBinaryExpression4
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression5 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression5 =
      iBinaryExpression5
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression6 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression6 =
      iBinaryExpression6
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression7 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression7 =
      iBinaryExpression7
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression8 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression8 =
      iBinaryExpression8
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression9 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression9 =
      iBinaryExpression9
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression10 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression10 =
      iBinaryExpression10
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression11 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression11 =
      iBinaryExpression11
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression12 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression12 =
      iBinaryExpression12
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression13 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression13 =
      iBinaryExpression13
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression14 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression14 =
      iBinaryExpression14
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression15 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression15 =
      iBinaryExpression15
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression16 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression16 =
      iBinaryExpression16
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression17 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression17 =
      iBinaryExpression17
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression18 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression18 =
      iBinaryExpression18
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression19 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression19 =
      iBinaryExpression19
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression
    pBinaryExpression20 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression20 =
      iBinaryExpression20
        <$> pHiddenExpression
        <*> pBinaryOperator
        <*> pHiddenExpression

pBinaryOperator :: Parser (MoveTerm BinaryOperatorL)
pBinaryOperator = do
  _sym <- pSymbol SBinaryOperatorSymbol
  iBinaryOperator
    <$> pImplies

pCastExpression :: Parser (MoveTerm CastExpressionL)
pCastExpression = do
  _sym <- pSymbol SCastExpressionSymbol
  iCastExpression
    <$> pHiddenExpression
    <*> pHiddenType

pIdentifiedExpression :: Parser (MoveTerm IdentifiedExpressionL)
pIdentifiedExpression = do
  _sym <- pSymbol SIdentifiedExpressionSymbol
  iIdentifiedExpression
    <$> pBlockIdentifier
    <*> pHiddenExpression

pBlockIdentifier :: Parser (MoveTerm BlockIdentifierL)
pBlockIdentifier = do
  _sym <- pSymbol SBlockIdentifierSymbol
  iBlockIdentifier
    <$> pLabel

pLambdaExpression :: Parser (MoveTerm LambdaExpressionL)
pLambdaExpression = do
  _sym <- pSymbol SLambdaExpressionSymbol
  iLambdaExpression
    <$> pLambdaBindings
    <*> pMaybe pHiddenType
    <*> pHiddenExpression

pLambdaBindings :: Parser (MoveTerm LambdaBindingsL)
pLambdaBindings = do
  _sym <- pSymbol SLambdaBindingsSymbol
  iLambdaBindings
    <$> pBitor
    <*> pPair (pMaybe pLambdaBinding) (pMany pLambdaBinding)
    <*> pBitor

pLambdaBinding :: Parser (MoveTerm LambdaBindingL)
pLambdaBinding = do
  _sym <- pSymbol SLambdaBindingSymbol
  choice [ pCommaBindListLambdaBinding
         , pBindLambdaBinding
         , pLambdaBinding3
         ]
  where
    pCommaBindListLambdaBinding :: Parser (MoveTerm LambdaBindingL)
    pCommaBindListLambdaBinding =
      iCommaBindListLambdaBinding
        <$> pCommaBindList
    pBindLambdaBinding :: Parser (MoveTerm LambdaBindingL)
    pBindLambdaBinding =
      iBindLambdaBinding
        <$> pHiddenBind
    pLambdaBinding3 :: Parser (MoveTerm LambdaBindingL)
    pLambdaBinding3 =
      iLambdaBinding3
        <$> pHiddenBind
        <*> pMaybe pHiddenType

pLoopExpression :: Parser (MoveTerm LoopExpressionL)
pLoopExpression = do
  _sym <- pSymbol SLoopExpressionSymbol
  iLoopExpression
    <$> pHiddenExpression

pQuantifierExpression :: Parser (MoveTerm QuantifierExpressionL)
pQuantifierExpression = do
  _sym <- pSymbol SQuantifierExpressionSymbol
  iQuantifierExpression
    <$> pEither pHiddenExists pHiddenForall
    <*> pQuantifierBindings
    <*> pMaybe pHiddenExpression
    <*> pHiddenExpression

pHiddenExists :: Parser (MoveTerm HiddenExistsL)
pHiddenExists = do
  pure iHiddenExists

pHiddenForall :: Parser (MoveTerm HiddenForallL)
pHiddenForall = do
  pure iHiddenForall

pQuantifierBindings :: Parser (MoveTerm QuantifierBindingsL)
pQuantifierBindings = do
  _sym <- pSymbol SQuantifierBindingsSymbol
  iQuantifierBindings
    <$> pQuantifierBinding
    <*> pMany pQuantifierBinding

pQuantifierBinding :: Parser (MoveTerm QuantifierBindingL)
pQuantifierBinding = do
  _sym <- pSymbol SQuantifierBindingSymbol
  choice [ pQuantifierBinding1
         , pQuantifierBinding2
         ]
  where
    pQuantifierBinding1 :: Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding1 =
      iQuantifierBinding1
        <$> pIdentifier
        <*> pHiddenType
    pQuantifierBinding2 :: Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding2 =
      iQuantifierBinding2
        <$> pIdentifier
        <*> pHiddenExpression

pReturnExpression :: Parser (MoveTerm ReturnExpressionL)
pReturnExpression = do
  _sym <- pSymbol SReturnExpressionSymbol
  choice [ pReturnExpression1
         , pReturnExpression2
         ]
  where
    pReturnExpression1 :: Parser (MoveTerm ReturnExpressionL)
    pReturnExpression1 =
      iReturnExpression1
        <$> pMaybe pLabel
        <*> pHiddenExpression
    pReturnExpression2 :: Parser (MoveTerm ReturnExpressionL)
    pReturnExpression2 =
      iReturnExpression2
        <$> pMaybe pLabel

pWhileExpression :: Parser (MoveTerm WhileExpressionL)
pWhileExpression = do
  _sym <- pSymbol SWhileExpressionSymbol
  iWhileExpression
    <$> pHiddenExpression
    <*> pHiddenExpression

pBlockItem :: Parser (MoveTerm BlockItemL)
pBlockItem = do
  _sym <- pSymbol SBlockItemSymbol
  iBlockItem
    <$> pEither pLetStatement pHiddenExpression

pLetStatement :: Parser (MoveTerm LetStatementL)
pLetStatement = do
  _sym <- pSymbol SLetStatementSymbol
  iLetStatement
    <$> pBindList
    <*> pMaybe pHiddenType
    <*> pMaybe (pPair pHiddenExpression pAssign)

pMacroFunctionDefinition :: Parser (MoveTerm MacroFunctionDefinitionL)
pMacroFunctionDefinition = do
  _sym <- pSymbol SMacroFunctionDefinitionSymbol
  iMacroFunctionDefinition
    <$> pMaybe pModifier
    <*> pHiddenMacroSignature
    <*> pBlock

pHiddenMacroSignature :: Parser (MoveTerm HiddenMacroSignatureL)
pHiddenMacroSignature = do
  iHiddenMacroSignature
    <$> pMaybe pModifier
    <*> pHiddenFunctionIdentifier
    <*> pMaybe pTypeParameters
    <*> pFunctionParameters
    <*> pMaybe pRetType

pNativeFunctionDefinition :: Parser (MoveTerm NativeFunctionDefinitionL)
pNativeFunctionDefinition = do
  _sym <- pSymbol SNativeFunctionDefinitionSymbol
  iNativeFunctionDefinition
    <$> pHiddenFunctionSignature

pHiddenStructItem :: Parser (MoveTerm HiddenStructItemL)
pHiddenStructItem = do
  choice [ pNativeStructDefinitionStructItem
         , pStructDefinitionStructItem
         ]
  where
    pNativeStructDefinitionStructItem :: Parser (MoveTerm HiddenStructItemL)
    pNativeStructDefinitionStructItem =
      iNativeStructDefinitionStructItem
        <$> pNativeStructDefinition
    pStructDefinitionStructItem :: Parser (MoveTerm HiddenStructItemL)
    pStructDefinitionStructItem =
      iStructDefinitionStructItem
        <$> pStructDefinition

pNativeStructDefinition :: Parser (MoveTerm NativeStructDefinitionL)
pNativeStructDefinition = do
  _sym <- pSymbol SNativeStructDefinitionSymbol
  iNativeStructDefinition
    <$> pMaybe pPublic
    <*> pHiddenStructSignature

pHiddenStructSignature :: Parser (MoveTerm HiddenStructSignatureL)
pHiddenStructSignature = do
  iHiddenStructSignature
    <$> pHiddenStructIdentifier
    <*> pMaybe pTypeParameters
    <*> pMaybe pAbilityDecls

pStructDefinition :: Parser (MoveTerm StructDefinitionL)
pStructDefinition = do
  _sym <- pSymbol SStructDefinitionSymbol
  iStructDefinition
    <$> pMaybe pPublic
    <*> pHiddenStructSignature
    <*> pDatatypeFields
    <*> pMaybe pPostfixAbilityDecls

pConstant :: Parser (MoveTerm ConstantL)
pConstant = do
  _sym <- pSymbol SConstantSymbol
  iConstant
    <$> pConstantIdentifier
    <*> pHiddenType
    <*> pAssign
    <*> pHiddenExpression

pConstantIdentifier :: Parser (MoveTerm ConstantIdentifierL)
pConstantIdentifier = do
  _sym <- pSymbol SConstantIdentifierSymbol
  pure iConstantIdentifier

pFriendDeclaration :: Parser (MoveTerm FriendDeclarationL)
pFriendDeclaration = do
  _sym <- pSymbol SFriendDeclarationSymbol
  iFriendDeclaration
    <$> pFriend
    <*> pFriendAccess

pFriendAccess :: Parser (MoveTerm FriendAccessL)
pFriendAccess = do
  _sym <- pSymbol SFriendAccessSymbol
  choice [ pLocalModuleFriendAccess
         , pFullyQualifiedModuleFriendAccess
         ]
  where
    pLocalModuleFriendAccess :: Parser (MoveTerm FriendAccessL)
    pLocalModuleFriendAccess =
      iLocalModuleFriendAccess
        <$> pIdentifier
    pFullyQualifiedModuleFriendAccess :: Parser (MoveTerm FriendAccessL)
    pFullyQualifiedModuleFriendAccess =
      iFullyQualifiedModuleFriendAccess
        <$> pModuleIdentity
