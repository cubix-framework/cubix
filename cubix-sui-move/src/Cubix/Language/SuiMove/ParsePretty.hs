module Cubix.Language.SuiMove.ParsePretty where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (MonadPlus (..), forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, modify')
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IORef qualified as IORef
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Type.Equality (type (:~:) (..), type (:~~:) (..))
import Streaming qualified
import Streaming.Prelude qualified as Streaming

import TreeSitter (NodeId (..), Range (..))
import TreeSitter qualified as TS
import TreeSitter.SuiMove (tree_sitter_sui_move)

import Cubix.ParsePretty
import Cubix.Language.SuiMove.Modularized
import Cubix.TreeSitter

getSymbol :: SymbolTable -> TS.Node -> IO (Maybe SomeSymbolSing)
getSymbol map node = do
  symbol <- TS.nodeSymbol node
  pure $ IM.lookup (fromIntegral symbol) (unSymbolTable map)

parse :: FilePath -> IO (Maybe (MoveTerm (RootSort MoveSig)))
parse path = runResourceT $
  withLanguage tree_sitter_sui_move $ \lang -> do
  --count <- fromIntegral <$> liftIO (TS.languageSymbolCount lang)
  -- foldrM _ Map.empty [0..count]
  -- forM_ [0..count] $ \id -> do
  --   symName <- liftIO $ TS.languageSymbolName lang id
  --   liftIO $ print symName

  symbolTable <- liftIO (mkSymbolTable' lang)
  Streaming.print
    $ Streaming.filter (isJust . tokenValue)
    $ Streaming.mapM
      (\tok -> do
          sym <- liftIO $ getSymbol symbolTable (tokenValue tok)
          pure $ tok { tokenValue = sym }
      )
    -- $ Streaming.mapM (liftIO . TS.nodeGrammarTypeAsString . tokenValue)
    $ lexer lang path

  pure undefined

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

pRoot :: Parser (MoveTerm (RootSort MoveSig))
pRoot = pSourceFileL

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

pSourceFile :: Parser (MoveTerm L)
pSourceFile = do
  _sym <- pSymbol SSourceFileSymbol
  pure undefined

pModuleDefinition :: Parser (MoveTerm L)
pModuleDefinition = do
  _sym <- pSymbol SModuleDefinitionSymbol
  pure undefined

pModuleBody :: Parser (MoveTerm L)
pModuleBody = do
  _sym <- pSymbol SModuleBodySymbol
  pure undefined

pHiddenEnumItem :: Parser (MoveTerm L)
pHiddenEnumItem = do
  _sym <- pSymbol SHiddenEnumItemSymbol
  pure undefined

pEnumDefinition :: Parser (MoveTerm L)
pEnumDefinition = do
  _sym <- pSymbol SEnumDefinitionSymbol
  pure undefined

pHiddenEnumSignature :: Parser (MoveTerm L)
pHiddenEnumSignature = do
  _sym <- pSymbol SHiddenEnumSignatureSymbol
  pure undefined

pHiddenEnumIdentifier :: Parser (MoveTerm L)
pHiddenEnumIdentifier = do
  _sym <- pSymbol SHiddenEnumIdentifierSymbol
  pure undefined

pIdentifier :: Parser (MoveTerm L)
pIdentifier = do
  _sym <- pSymbol SIdentifierSymbol
  pure undefined

pAbilityDecls :: Parser (MoveTerm L)
pAbilityDecls = do
  _sym <- pSymbol SAbilityDeclsSymbol
  pure undefined

pAbility :: Parser (MoveTerm L)
pAbility = do
  _sym <- pSymbol SAbilitySymbol
  pure undefined
  where
    pCopyAbility :: Parser (MoveTerm AbilityL)
    pCopyAbility = do
      pure undefined
    pDropAbility :: Parser (MoveTerm AbilityL)
    pDropAbility = do
      pure undefined
    pStoreAbility :: Parser (MoveTerm AbilityL)
    pStoreAbility = do
      pure undefined
    pKeyAbility :: Parser (MoveTerm AbilityL)
    pKeyAbility = do
      pure undefined

pTypeParameters :: Parser (MoveTerm L)
pTypeParameters = do
  _sym <- pSymbol STypeParametersSymbol
  pure undefined

pTypeParameter :: Parser (MoveTerm L)
pTypeParameter = do
  _sym <- pSymbol STypeParameterSymbol
  pure undefined

pHiddenTypeParameterIdentifier :: Parser (MoveTerm L)
pHiddenTypeParameterIdentifier = do
  _sym <- pSymbol SHiddenTypeParameterIdentifierSymbol
  pure undefined

pEnumVariants :: Parser (MoveTerm L)
pEnumVariants = do
  _sym <- pSymbol SEnumVariantsSymbol
  pure undefined

pVariant :: Parser (MoveTerm L)
pVariant = do
  _sym <- pSymbol SVariantSymbol
  pure undefined

pHiddenVariantIdentifier :: Parser (MoveTerm L)
pHiddenVariantIdentifier = do
  _sym <- pSymbol SHiddenVariantIdentifierSymbol
  pure undefined

pDatatypeFields :: Parser (MoveTerm L)
pDatatypeFields = do
  _sym <- pSymbol SDatatypeFieldsSymbol
  pure undefined
  where
    pPositionalFieldsDatatypeFields :: Parser (MoveTerm DatatypeFieldsL)
    pPositionalFieldsDatatypeFields = do
      pure undefined
    pNamedFieldsDatatypeFields :: Parser (MoveTerm DatatypeFieldsL)
    pNamedFieldsDatatypeFields = do
      pure undefined

pNamedFields :: Parser (MoveTerm L)
pNamedFields = do
  _sym <- pSymbol SNamedFieldsSymbol
  pure undefined

pFieldAnnotation :: Parser (MoveTerm L)
pFieldAnnotation = do
  _sym <- pSymbol SFieldAnnotationSymbol
  pure undefined

pHiddenFieldIdentifier :: Parser (MoveTerm L)
pHiddenFieldIdentifier = do
  _sym <- pSymbol SHiddenFieldIdentifierSymbol
  pure undefined

pHiddenType :: Parser (MoveTerm L)
pHiddenType = do
  _sym <- pSymbol SHiddenTypeSymbol
  pure undefined
  where
    pApplyTypeType :: Parser (MoveTerm HiddenTypeL)
    pApplyTypeType = do
      pure undefined
    pRefTypeType :: Parser (MoveTerm HiddenTypeL)
    pRefTypeType = do
      pure undefined
    pTupleTypeType :: Parser (MoveTerm HiddenTypeL)
    pTupleTypeType = do
      pure undefined
    pFunctionTypeType :: Parser (MoveTerm HiddenTypeL)
    pFunctionTypeType = do
      pure undefined
    pPrimitiveTypeType :: Parser (MoveTerm HiddenTypeL)
    pPrimitiveTypeType = do
      pure undefined

pApplyType :: Parser (MoveTerm L)
pApplyType = do
  _sym <- pSymbol SApplyTypeSymbol
  pure undefined

pModuleAccess :: Parser (MoveTerm L)
pModuleAccess = do
  _sym <- pSymbol SModuleAccessSymbol
  pure undefined
  where
    pModuleAccess1 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess1 = do
      pure undefined
    pModuleAccess2 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess2 = do
      pure undefined
    pMemberModuleAccess :: Parser (MoveTerm ModuleAccessL)
    pMemberModuleAccess = do
      pure undefined
    pModuleAccess4 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess4 = do
      pure undefined
    pModuleAccess5 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess5 = do
      pure undefined
    pModuleAccess6 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess6 = do
      pure undefined
    pModuleAccess7 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess7 = do
      pure undefined
    pModuleAccess8 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess8 = do
      pure undefined
    pModuleAccess9 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess9 = do
      pure undefined

pHiddenModuleIdentifier :: Parser (MoveTerm L)
pHiddenModuleIdentifier = do
  _sym <- pSymbol SHiddenModuleIdentifierSymbol
  pure undefined

pHiddenReservedIdentifier :: Parser (MoveTerm L)
pHiddenReservedIdentifier = do
  _sym <- pSymbol SHiddenReservedIdentifierSymbol
  pure undefined
  where
    pHiddenForallReservedIdentifier :: Parser (MoveTerm HiddenReservedIdentifierL)
    pHiddenForallReservedIdentifier = do
      pure undefined
    pHiddenExistsReservedIdentifier :: Parser (MoveTerm HiddenReservedIdentifierL)
    pHiddenExistsReservedIdentifier = do
      pure undefined

pHiddenExists :: Parser (MoveTerm L)
pHiddenExists = do
  _sym <- pSymbol SHiddenExistsSymbol
  pure undefined

pHiddenForall :: Parser (MoveTerm L)
pHiddenForall = do
  _sym <- pSymbol SHiddenForallSymbol
  pure undefined

pModuleIdentity :: Parser (MoveTerm L)
pModuleIdentity = do
  _sym <- pSymbol SModuleIdentitySymbol
  pure undefined

pNumLiteral :: Parser (MoveTerm L)
pNumLiteral = do
  _sym <- pSymbol SNumLiteralSymbol
  pure undefined

pTypeArguments :: Parser (MoveTerm L)
pTypeArguments = do
  _sym <- pSymbol STypeArgumentsSymbol
  pure undefined

pFunctionType :: Parser (MoveTerm L)
pFunctionType = do
  _sym <- pSymbol SFunctionTypeSymbol
  pure undefined

pFunctionTypeParameters :: Parser (MoveTerm L)
pFunctionTypeParameters = do
  _sym <- pSymbol SFunctionTypeParametersSymbol
  pure undefined

pPrimitiveType :: Parser (MoveTerm L)
pPrimitiveType = do
  _sym <- pSymbol SPrimitiveTypeSymbol
  pure undefined
  where
    pU8PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU8PrimitiveType = do
      pure undefined
    pU16PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU16PrimitiveType = do
      pure undefined
    pU32PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU32PrimitiveType = do
      pure undefined
    pU64PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU64PrimitiveType = do
      pure undefined
    pU128PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU128PrimitiveType = do
      pure undefined
    pU256PrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pU256PrimitiveType = do
      pure undefined
    pBoolPrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pBoolPrimitiveType = do
      pure undefined
    pAddressPrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pAddressPrimitiveType = do
      pure undefined
    pSignerPrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pSignerPrimitiveType = do
      pure undefined
    pBytearrayPrimitiveType :: Parser (MoveTerm PrimitiveTypeL)
    pBytearrayPrimitiveType = do
      pure undefined

pRefType :: Parser (MoveTerm L)
pRefType = do
  _sym <- pSymbol SRefTypeSymbol
  pure undefined

pHiddenReference :: Parser (MoveTerm L)
pHiddenReference = do
  _sym <- pSymbol SHiddenReferenceSymbol
  pure undefined
  where
    pImmRefReference :: Parser (MoveTerm HiddenReferenceL)
    pImmRefReference = do
      pure undefined
    pMutRefReference :: Parser (MoveTerm HiddenReferenceL)
    pMutRefReference = do
      pure undefined

pImmRef :: Parser (MoveTerm L)
pImmRef = do
  _sym <- pSymbol SImmRefSymbol
  pure undefined

pMutRef :: Parser (MoveTerm L)
pMutRef = do
  _sym <- pSymbol SMutRefSymbol
  pure undefined

pTupleType :: Parser (MoveTerm L)
pTupleType = do
  _sym <- pSymbol STupleTypeSymbol
  pure undefined

pPositionalFields :: Parser (MoveTerm L)
pPositionalFields = do
  _sym <- pSymbol SPositionalFieldsSymbol
  pure undefined

pPostfixAbilityDecls :: Parser (MoveTerm L)
pPostfixAbilityDecls = do
  _sym <- pSymbol SPostfixAbilityDeclsSymbol
  pure undefined

pHiddenFunctionItem :: Parser (MoveTerm L)
pHiddenFunctionItem = do
  _sym <- pSymbol SHiddenFunctionItemSymbol
  pure undefined
  where
    pNativeFunctionDefinitionFunctionItem :: Parser (MoveTerm HiddenFunctionItemL)
    pNativeFunctionDefinitionFunctionItem = do
      pure undefined
    pMacroFunctionDefinitionFunctionItem :: Parser (MoveTerm HiddenFunctionItemL)
    pMacroFunctionDefinitionFunctionItem = do
      pure undefined
    pFunctionDefinitionFunctionItem :: Parser (MoveTerm HiddenFunctionItemL)
    pFunctionDefinitionFunctionItem = do
      pure undefined

pFunctionDefinition :: Parser (MoveTerm L)
pFunctionDefinition = do
  _sym <- pSymbol SFunctionDefinitionSymbol
  pure undefined

pHiddenFunctionSignature :: Parser (MoveTerm L)
pHiddenFunctionSignature = do
  _sym <- pSymbol SHiddenFunctionSignatureSymbol
  pure undefined

pHiddenFunctionIdentifier :: Parser (MoveTerm L)
pHiddenFunctionIdentifier = do
  _sym <- pSymbol SHiddenFunctionIdentifierSymbol
  pure undefined

pFunctionParameters :: Parser (MoveTerm L)
pFunctionParameters = do
  _sym <- pSymbol SFunctionParametersSymbol
  pure undefined

pFunctionParameter :: Parser (MoveTerm L)
pFunctionParameter = do
  _sym <- pSymbol SFunctionParameterSymbol
  pure undefined

pHiddenVariableIdentifier :: Parser (MoveTerm L)
pHiddenVariableIdentifier = do
  _sym <- pSymbol SHiddenVariableIdentifierSymbol
  pure undefined

pMutFunctionParameter :: Parser (MoveTerm L)
pMutFunctionParameter = do
  _sym <- pSymbol SMutFunctionParameterSymbol
  pure undefined

pModifier :: Parser (MoveTerm L)
pModifier = do
  _sym <- pSymbol SModifierSymbol
  pure undefined
  where
    pModifier1 :: Parser (MoveTerm ModifierL)
    pModifier1 = do
      pure undefined
    pEntryModifier :: Parser (MoveTerm ModifierL)
    pEntryModifier = do
      pure undefined
    pNativeModifier :: Parser (MoveTerm ModifierL)
    pNativeModifier = do
      pure undefined

pRetType :: Parser (MoveTerm L)
pRetType = do
  _sym <- pSymbol SRetTypeSymbol
  pure undefined

pBlock :: Parser (MoveTerm L)
pBlock = do
  _sym <- pSymbol SBlockSymbol
  pure undefined

pHiddenExpression :: Parser (MoveTerm L)
pHiddenExpression = do
  _sym <- pSymbol SHiddenExpressionSymbol
  pure undefined
  where
    pCallExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pCallExpressionExpression = do
      pure undefined
    pMacroCallExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pMacroCallExpressionExpression = do
      pure undefined
    pLambdaExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pLambdaExpressionExpression = do
      pure undefined
    pIfExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pIfExpressionExpression = do
      pure undefined
    pWhileExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pWhileExpressionExpression = do
      pure undefined
    pReturnExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pReturnExpressionExpression = do
      pure undefined
    pAbortExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pAbortExpressionExpression = do
      pure undefined
    pAssignExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pAssignExpressionExpression = do
      pure undefined
    pHiddenUnaryExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pHiddenUnaryExpressionExpression = do
      pure undefined
    pBinaryExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pBinaryExpressionExpression = do
      pure undefined
    pCastExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pCastExpressionExpression = do
      pure undefined
    pQuantifierExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pQuantifierExpressionExpression = do
      pure undefined
    pMatchExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pMatchExpressionExpression = do
      pure undefined
    pVectorExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pVectorExpressionExpression = do
      pure undefined
    pLoopExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pLoopExpressionExpression = do
      pure undefined
    pIdentifiedExpressionExpression :: Parser (MoveTerm HiddenExpressionL)
    pIdentifiedExpressionExpression = do
      pure undefined

pHiddenUnaryExpression :: Parser (MoveTerm L)
pHiddenUnaryExpression = do
  _sym <- pSymbol SHiddenUnaryExpressionSymbol
  pure undefined
  where
    pUnaryExpressionUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
    pUnaryExpressionUnaryExpression = do
      pure undefined
    pBorrowExpressionUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
    pBorrowExpressionUnaryExpression = do
      pure undefined
    pDereferenceExpressionUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
    pDereferenceExpressionUnaryExpression = do
      pure undefined
    pMoveOrCopyExpressionUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
    pMoveOrCopyExpressionUnaryExpression = do
      pure undefined
    pHiddenExpressionTermUnaryExpression :: Parser (MoveTerm HiddenUnaryExpressionL)
    pHiddenExpressionTermUnaryExpression = do
      pure undefined

pHiddenExpressionTerm :: Parser (MoveTerm L)
pHiddenExpressionTerm = do
  _sym <- pSymbol SHiddenExpressionTermSymbol
  pure undefined
  where
    pCallExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pCallExpressionExpressionTerm = do
      pure undefined
    pBreakExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pBreakExpressionExpressionTerm = do
      pure undefined
    pContinueExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pContinueExpressionExpressionTerm = do
      pure undefined
    pNameExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pNameExpressionExpressionTerm = do
      pure undefined
    pMacroCallExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pMacroCallExpressionExpressionTerm = do
      pure undefined
    pPackExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pPackExpressionExpressionTerm = do
      pure undefined
    pHiddenLiteralValueExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pHiddenLiteralValueExpressionTerm = do
      pure undefined
    pUnitExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pUnitExpressionExpressionTerm = do
      pure undefined
    pExpressionListExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pExpressionListExpressionTerm = do
      pure undefined
    pAnnotationExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pAnnotationExpressionExpressionTerm = do
      pure undefined
    pBlockExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pBlockExpressionTerm = do
      pure undefined
    pSpecBlockExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pSpecBlockExpressionTerm = do
      pure undefined
    pIfExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pIfExpressionExpressionTerm = do
      pure undefined
    pDotExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pDotExpressionExpressionTerm = do
      pure undefined
    pIndexExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pIndexExpressionExpressionTerm = do
      pure undefined
    pVectorExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pVectorExpressionExpressionTerm = do
      pure undefined
    pMatchExpressionExpressionTerm :: Parser (MoveTerm HiddenExpressionTermL)
    pMatchExpressionExpressionTerm = do
      pure undefined

pHiddenLiteralValue :: Parser (MoveTerm L)
pHiddenLiteralValue = do
  _sym <- pSymbol SHiddenLiteralValueSymbol
  pure undefined
  where
    pAddressLiteralLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
    pAddressLiteralLiteralValue = do
      pure undefined
    pBoolLiteralLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
    pBoolLiteralLiteralValue = do
      pure undefined
    pNumLiteralLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
    pNumLiteralLiteralValue = do
      pure undefined
    pHexStringLiteralLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
    pHexStringLiteralLiteralValue = do
      pure undefined
    pByteStringLiteralLiteralValue :: Parser (MoveTerm HiddenLiteralValueL)
    pByteStringLiteralLiteralValue = do
      pure undefined

pAddressLiteral :: Parser (MoveTerm L)
pAddressLiteral = do
  _sym <- pSymbol SAddressLiteralSymbol
  pure undefined

pBoolLiteral :: Parser (MoveTerm L)
pBoolLiteral = do
  _sym <- pSymbol SBoolLiteralSymbol
  pure undefined
  where
    pTrueBoolLiteral :: Parser (MoveTerm BoolLiteralL)
    pTrueBoolLiteral = do
      pure undefined
    pFalseBoolLiteral :: Parser (MoveTerm BoolLiteralL)
    pFalseBoolLiteral = do
      pure undefined

pByteStringLiteral :: Parser (MoveTerm L)
pByteStringLiteral = do
  _sym <- pSymbol SByteStringLiteralSymbol
  pure undefined

pHexStringLiteral :: Parser (MoveTerm L)
pHexStringLiteral = do
  _sym <- pSymbol SHexStringLiteralSymbol
  pure undefined

pAnnotationExpression :: Parser (MoveTerm L)
pAnnotationExpression = do
  _sym <- pSymbol SAnnotationExpressionSymbol
  pure undefined

pBreakExpression :: Parser (MoveTerm L)
pBreakExpression = do
  _sym <- pSymbol SBreakExpressionSymbol
  pure undefined

pLabel :: Parser (MoveTerm L)
pLabel = do
  _sym <- pSymbol SLabelSymbol
  pure undefined

pCallExpression :: Parser (MoveTerm L)
pCallExpression = do
  _sym <- pSymbol SCallExpressionSymbol
  pure undefined

pArgList :: Parser (MoveTerm L)
pArgList = do
  _sym <- pSymbol SArgListSymbol
  pure undefined

pNameExpression :: Parser (MoveTerm L)
pNameExpression = do
  _sym <- pSymbol SNameExpressionSymbol
  pure undefined

pContinueExpression :: Parser (MoveTerm L)
pContinueExpression = do
  _sym <- pSymbol SContinueExpressionSymbol
  pure undefined

pDotExpression :: Parser (MoveTerm L)
pDotExpression = do
  _sym <- pSymbol SDotExpressionSymbol
  pure undefined

pExpressionList :: Parser (MoveTerm L)
pExpressionList = do
  _sym <- pSymbol SExpressionListSymbol
  pure undefined

pIfExpression :: Parser (MoveTerm L)
pIfExpression = do
  _sym <- pSymbol SIfExpressionSymbol
  pure undefined

pIndexExpression :: Parser (MoveTerm L)
pIndexExpression = do
  _sym <- pSymbol SIndexExpressionSymbol
  pure undefined

pMacroCallExpression :: Parser (MoveTerm L)
pMacroCallExpression = do
  _sym <- pSymbol SMacroCallExpressionSymbol
  pure undefined

pMacroModuleAccess :: Parser (MoveTerm L)
pMacroModuleAccess = do
  _sym <- pSymbol SMacroModuleAccessSymbol
  pure undefined

pMatchExpression :: Parser (MoveTerm L)
pMatchExpression = do
  _sym <- pSymbol SMatchExpressionSymbol
  pure undefined

pHiddenMatchBody :: Parser (MoveTerm L)
pHiddenMatchBody = do
  _sym <- pSymbol SHiddenMatchBodySymbol
  pure undefined

pMatchArm :: Parser (MoveTerm L)
pMatchArm = do
  _sym <- pSymbol SMatchArmSymbol
  pure undefined

pBindList :: Parser (MoveTerm L)
pBindList = do
  _sym <- pSymbol SBindListSymbol
  pure undefined
  where
    pHiddenBindBindList :: Parser (MoveTerm BindListL)
    pHiddenBindBindList = do
      pure undefined
    pCommaBindListBindList :: Parser (MoveTerm BindListL)
    pCommaBindListBindList = do
      pure undefined
    pOrBindListBindList :: Parser (MoveTerm BindListL)
    pOrBindListBindList = do
      pure undefined

pHiddenBind :: Parser (MoveTerm L)
pHiddenBind = do
  _sym <- pSymbol SHiddenBindSymbol
  pure undefined
  where
    pMutBindVarBind :: Parser (MoveTerm HiddenBindL)
    pMutBindVarBind = do
      pure undefined
    pBindVarBind :: Parser (MoveTerm HiddenBindL)
    pBindVarBind = do
      pure undefined
    pBindUnpackBind :: Parser (MoveTerm HiddenBindL)
    pBindUnpackBind = do
      pure undefined
    pAtBindBind :: Parser (MoveTerm HiddenBindL)
    pAtBindBind = do
      pure undefined
    pHiddenLiteralValueBind :: Parser (MoveTerm HiddenBindL)
    pHiddenLiteralValueBind = do
      pure undefined

pAtBind :: Parser (MoveTerm L)
pAtBind = do
  _sym <- pSymbol SAtBindSymbol
  pure undefined

pBindUnpack :: Parser (MoveTerm L)
pBindUnpack = do
  _sym <- pSymbol SBindUnpackSymbol
  pure undefined

pBindFields :: Parser (MoveTerm L)
pBindFields = do
  _sym <- pSymbol SBindFieldsSymbol
  pure undefined
  where
    pBindPositionalFieldsBindFields :: Parser (MoveTerm BindFieldsL)
    pBindPositionalFieldsBindFields = do
      pure undefined
    pBindNamedFieldsBindFields :: Parser (MoveTerm BindFieldsL)
    pBindNamedFieldsBindFields = do
      pure undefined

pBindNamedFields :: Parser (MoveTerm L)
pBindNamedFields = do
  _sym <- pSymbol SBindNamedFieldsSymbol
  pure undefined

pBindField :: Parser (MoveTerm L)
pBindField = do
  _sym <- pSymbol SBindFieldSymbol
  pure undefined
  where
    pBindField1 :: Parser (MoveTerm BindFieldL)
    pBindField1 = do
      pure undefined
    pHiddenSpreadOperatorBindField :: Parser (MoveTerm BindFieldL)
    pHiddenSpreadOperatorBindField = do
      pure undefined

pHiddenSpreadOperator :: Parser (MoveTerm L)
pHiddenSpreadOperator = do
  _sym <- pSymbol SHiddenSpreadOperatorSymbol
  pure undefined

pMutBindField :: Parser (MoveTerm L)
pMutBindField = do
  _sym <- pSymbol SMutBindFieldSymbol
  pure undefined

pBindPositionalFields :: Parser (MoveTerm L)
pBindPositionalFields = do
  _sym <- pSymbol SBindPositionalFieldsSymbol
  pure undefined

pMutBindVar :: Parser (MoveTerm L)
pMutBindVar = do
  _sym <- pSymbol SMutBindVarSymbol
  pure undefined

pCommaBindList :: Parser (MoveTerm L)
pCommaBindList = do
  _sym <- pSymbol SCommaBindListSymbol
  pure undefined

pOrBindList :: Parser (MoveTerm L)
pOrBindList = do
  _sym <- pSymbol SOrBindListSymbol
  pure undefined

pMatchCondition :: Parser (MoveTerm L)
pMatchCondition = do
  _sym <- pSymbol SMatchConditionSymbol
  pure undefined

pPackExpression :: Parser (MoveTerm L)
pPackExpression = do
  _sym <- pSymbol SPackExpressionSymbol
  pure undefined

pFieldInitializeList :: Parser (MoveTerm L)
pFieldInitializeList = do
  _sym <- pSymbol SFieldInitializeListSymbol
  pure undefined

pExpField :: Parser (MoveTerm L)
pExpField = do
  _sym <- pSymbol SExpFieldSymbol
  pure undefined

pSpecBlock :: Parser (MoveTerm L)
pSpecBlock = do
  _sym <- pSymbol SSpecBlockSymbol
  pure undefined

pHiddenSpecBlockTarget :: Parser (MoveTerm L)
pHiddenSpecBlockTarget = do
  _sym <- pSymbol SHiddenSpecBlockTargetSymbol
  pure undefined
  where
    pIdentifierSpecBlockTarget :: Parser (MoveTerm HiddenSpecBlockTargetL)
    pIdentifierSpecBlockTarget = do
      pure undefined
    pSpecBlockTargetModuleSpecBlockTarget :: Parser (MoveTerm HiddenSpecBlockTargetL)
    pSpecBlockTargetModuleSpecBlockTarget = do
      pure undefined
    pSpecBlockTargetSchemaSpecBlockTarget :: Parser (MoveTerm HiddenSpecBlockTargetL)
    pSpecBlockTargetSchemaSpecBlockTarget = do
      pure undefined

pSpecBlockTargetSchema :: Parser (MoveTerm L)
pSpecBlockTargetSchema = do
  _sym <- pSymbol SSpecBlockTargetSchemaSymbol
  pure undefined

pHiddenStructIdentifier :: Parser (MoveTerm L)
pHiddenStructIdentifier = do
  _sym <- pSymbol SHiddenStructIdentifierSymbol
  pure undefined

pHiddenSpecFunction :: Parser (MoveTerm L)
pHiddenSpecFunction = do
  _sym <- pSymbol SHiddenSpecFunctionSymbol
  pure undefined
  where
    pNativeSpecFunctionSpecFunction :: Parser (MoveTerm HiddenSpecFunctionL)
    pNativeSpecFunctionSpecFunction = do
      pure undefined
    pUsualSpecFunctionSpecFunction :: Parser (MoveTerm HiddenSpecFunctionL)
    pUsualSpecFunctionSpecFunction = do
      pure undefined
    pUninterpretedSpecFunctionSpecFunction :: Parser (MoveTerm HiddenSpecFunctionL)
    pUninterpretedSpecFunctionSpecFunction = do
      pure undefined

pNativeSpecFunction :: Parser (MoveTerm L)
pNativeSpecFunction = do
  _sym <- pSymbol SNativeSpecFunctionSymbol
  pure undefined

pHiddenSpecFunctionSignature :: Parser (MoveTerm L)
pHiddenSpecFunctionSignature = do
  _sym <- pSymbol SHiddenSpecFunctionSignatureSymbol
  pure undefined

pUninterpretedSpecFunction :: Parser (MoveTerm L)
pUninterpretedSpecFunction = do
  _sym <- pSymbol SUninterpretedSpecFunctionSymbol
  pure undefined

pUsualSpecFunction :: Parser (MoveTerm L)
pUsualSpecFunction = do
  _sym <- pSymbol SUsualSpecFunctionSymbol
  pure undefined

pSpecBody :: Parser (MoveTerm L)
pSpecBody = do
  _sym <- pSymbol SSpecBodySymbol
  pure undefined

pHiddenSpecBlockMemeber :: Parser (MoveTerm L)
pHiddenSpecBlockMemeber = do
  _sym <- pSymbol SHiddenSpecBlockMemeberSymbol
  pure undefined
  where
    pSpecInvariantSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecInvariantSpecBlockMemeber = do
      pure undefined
    pHiddenSpecFunctionSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecFunctionSpecBlockMemeber = do
      pure undefined
    pSpecConditionSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecConditionSpecBlockMemeber = do
      pure undefined
    pSpecIncludeSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecIncludeSpecBlockMemeber = do
      pure undefined
    pSpecApplySpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecApplySpecBlockMemeber = do
      pure undefined
    pSpecPragmaSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecPragmaSpecBlockMemeber = do
      pure undefined
    pSpecVariableSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecVariableSpecBlockMemeber = do
      pure undefined
    pSpecLetSpecBlockMemeber :: Parser (MoveTerm HiddenSpecBlockMemeberL)
    pSpecLetSpecBlockMemeber = do
      pure undefined

pSpecApply :: Parser (MoveTerm L)
pSpecApply = do
  _sym <- pSymbol SSpecApplySymbol
  pure undefined

pSpecApplyPattern :: Parser (MoveTerm L)
pSpecApplyPattern = do
  _sym <- pSymbol SSpecApplyPatternSymbol
  pure undefined

pSpecApplyNamePattern :: Parser (MoveTerm L)
pSpecApplyNamePattern = do
  _sym <- pSymbol SSpecApplyNamePatternSymbol
  pure undefined

pSpecCondition :: Parser (MoveTerm L)
pSpecCondition = do
  _sym <- pSymbol SSpecConditionSymbol
  pure undefined
  where
    pHiddenSpecConditionSpecCondition :: Parser (MoveTerm SpecConditionL)
    pHiddenSpecConditionSpecCondition = do
      pure undefined
    pHiddenSpecAbortIfSpecCondition :: Parser (MoveTerm SpecConditionL)
    pHiddenSpecAbortIfSpecCondition = do
      pure undefined
    pHiddenSpecAbortWithOrModifiesSpecCondition :: Parser (MoveTerm SpecConditionL)
    pHiddenSpecAbortWithOrModifiesSpecCondition = do
      pure undefined

pHiddenSpecAbortIf :: Parser (MoveTerm L)
pHiddenSpecAbortIf = do
  _sym <- pSymbol SHiddenSpecAbortIfSymbol
  pure undefined

pConditionProperties :: Parser (MoveTerm L)
pConditionProperties = do
  _sym <- pSymbol SConditionPropertiesSymbol
  pure undefined

pSpecProperty :: Parser (MoveTerm L)
pSpecProperty = do
  _sym <- pSymbol SSpecPropertySymbol
  pure undefined

pHiddenSpecAbortWithOrModifies :: Parser (MoveTerm L)
pHiddenSpecAbortWithOrModifies = do
  _sym <- pSymbol SHiddenSpecAbortWithOrModifiesSymbol
  pure undefined

pHiddenSpecCondition :: Parser (MoveTerm L)
pHiddenSpecCondition = do
  _sym <- pSymbol SHiddenSpecConditionSymbol
  pure undefined

pHiddenSpecConditionKind :: Parser (MoveTerm L)
pHiddenSpecConditionKind = do
  _sym <- pSymbol SHiddenSpecConditionKindSymbol
  pure undefined
  where
    pAssertSpecConditionKind :: Parser (MoveTerm HiddenSpecConditionKindL)
    pAssertSpecConditionKind = do
      pure undefined
    pAssumeSpecConditionKind :: Parser (MoveTerm HiddenSpecConditionKindL)
    pAssumeSpecConditionKind = do
      pure undefined
    pDecreasesSpecConditionKind :: Parser (MoveTerm HiddenSpecConditionKindL)
    pDecreasesSpecConditionKind = do
      pure undefined
    pEnsuresSpecConditionKind :: Parser (MoveTerm HiddenSpecConditionKindL)
    pEnsuresSpecConditionKind = do
      pure undefined
    pSucceedsIfSpecConditionKind :: Parser (MoveTerm HiddenSpecConditionKindL)
    pSucceedsIfSpecConditionKind = do
      pure undefined

pSpecInclude :: Parser (MoveTerm L)
pSpecInclude = do
  _sym <- pSymbol SSpecIncludeSymbol
  pure undefined

pSpecInvariant :: Parser (MoveTerm L)
pSpecInvariant = do
  _sym <- pSymbol SSpecInvariantSymbol
  pure undefined

pSpecLet :: Parser (MoveTerm L)
pSpecLet = do
  _sym <- pSymbol SSpecLetSymbol
  pure undefined

pSpecPragma :: Parser (MoveTerm L)
pSpecPragma = do
  _sym <- pSymbol SSpecPragmaSymbol
  pure undefined

pSpecVariable :: Parser (MoveTerm L)
pSpecVariable = do
  _sym <- pSymbol SSpecVariableSymbol
  pure undefined

pUseDeclaration :: Parser (MoveTerm L)
pUseDeclaration = do
  _sym <- pSymbol SUseDeclarationSymbol
  pure undefined

pUseFun :: Parser (MoveTerm L)
pUseFun = do
  _sym <- pSymbol SUseFunSymbol
  pure undefined

pUseModule :: Parser (MoveTerm L)
pUseModule = do
  _sym <- pSymbol SUseModuleSymbol
  pure undefined

pUseModuleMember :: Parser (MoveTerm L)
pUseModuleMember = do
  _sym <- pSymbol SUseModuleMemberSymbol
  pure undefined

pUseMember :: Parser (MoveTerm L)
pUseMember = do
  _sym <- pSymbol SUseMemberSymbol
  pure undefined
  where
    pUseMember1 :: Parser (MoveTerm UseMemberL)
    pUseMember1 = do
      pure undefined
    pUseMember2 :: Parser (MoveTerm UseMemberL)
    pUseMember2 = do
      pure undefined
    pUseMember3 :: Parser (MoveTerm UseMemberL)
    pUseMember3 = do
      pure undefined

pUseModuleMembers :: Parser (MoveTerm L)
pUseModuleMembers = do
  _sym <- pSymbol SUseModuleMembersSymbol
  pure undefined
  where
    pUseModuleMembers1 :: Parser (MoveTerm UseModuleMembersL)
    pUseModuleMembers1 = do
      pure undefined
    pUseModuleMembers2 :: Parser (MoveTerm UseModuleMembersL)
    pUseModuleMembers2 = do
      pure undefined

pUnitExpression :: Parser (MoveTerm L)
pUnitExpression = do
  _sym <- pSymbol SUnitExpressionSymbol
  pure undefined

pVectorExpression :: Parser (MoveTerm L)
pVectorExpression = do
  _sym <- pSymbol SVectorExpressionSymbol
  pure undefined

pBorrowExpression :: Parser (MoveTerm L)
pBorrowExpression = do
  _sym <- pSymbol SBorrowExpressionSymbol
  pure undefined

pDereferenceExpression :: Parser (MoveTerm L)
pDereferenceExpression = do
  _sym <- pSymbol SDereferenceExpressionSymbol
  pure undefined

pMoveOrCopyExpression :: Parser (MoveTerm L)
pMoveOrCopyExpression = do
  _sym <- pSymbol SMoveOrCopyExpressionSymbol
  pure undefined

pUnaryExpression :: Parser (MoveTerm L)
pUnaryExpression = do
  _sym <- pSymbol SUnaryExpressionSymbol
  pure undefined

pUnaryOp :: Parser (MoveTerm L)
pUnaryOp = do
  _sym <- pSymbol SUnaryOpSymbol
  pure undefined

pAbortExpression :: Parser (MoveTerm L)
pAbortExpression = do
  _sym <- pSymbol SAbortExpressionSymbol
  pure undefined

pAssignExpression :: Parser (MoveTerm L)
pAssignExpression = do
  _sym <- pSymbol SAssignExpressionSymbol
  pure undefined

pBinaryExpression :: Parser (MoveTerm L)
pBinaryExpression = do
  _sym <- pSymbol SBinaryExpressionSymbol
  pure undefined
  where
    pBinaryExpression1 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression1 = do
      pure undefined
    pBinaryExpression2 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression2 = do
      pure undefined
    pBinaryExpression3 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression3 = do
      pure undefined
    pBinaryExpression4 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression4 = do
      pure undefined
    pBinaryExpression5 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression5 = do
      pure undefined
    pBinaryExpression6 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression6 = do
      pure undefined
    pBinaryExpression7 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression7 = do
      pure undefined
    pBinaryExpression8 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression8 = do
      pure undefined
    pBinaryExpression9 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression9 = do
      pure undefined
    pBinaryExpression10 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression10 = do
      pure undefined
    pBinaryExpression11 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression11 = do
      pure undefined
    pBinaryExpression12 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression12 = do
      pure undefined
    pBinaryExpression13 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression13 = do
      pure undefined
    pBinaryExpression14 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression14 = do
      pure undefined
    pBinaryExpression15 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression15 = do
      pure undefined
    pBinaryExpression16 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression16 = do
      pure undefined
    pBinaryExpression17 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression17 = do
      pure undefined
    pBinaryExpression18 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression18 = do
      pure undefined
    pBinaryExpression19 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression19 = do
      pure undefined
    pBinaryExpression20 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression20 = do
      pure undefined

pCastExpression :: Parser (MoveTerm L)
pCastExpression = do
  _sym <- pSymbol SCastExpressionSymbol
  pure undefined

pIdentifiedExpression :: Parser (MoveTerm L)
pIdentifiedExpression = do
  _sym <- pSymbol SIdentifiedExpressionSymbol
  pure undefined

pBlockIdentifier :: Parser (MoveTerm L)
pBlockIdentifier = do
  _sym <- pSymbol SBlockIdentifierSymbol
  pure undefined

pLambdaExpression :: Parser (MoveTerm L)
pLambdaExpression = do
  _sym <- pSymbol SLambdaExpressionSymbol
  pure undefined

pLambdaBindings :: Parser (MoveTerm L)
pLambdaBindings = do
  _sym <- pSymbol SLambdaBindingsSymbol
  pure undefined

pLambdaBinding :: Parser (MoveTerm L)
pLambdaBinding = do
  _sym <- pSymbol SLambdaBindingSymbol
  pure undefined
  where
    pCommaBindListLambdaBinding :: Parser (MoveTerm LambdaBindingL)
    pCommaBindListLambdaBinding = do
      pure undefined
    pBindLambdaBinding :: Parser (MoveTerm LambdaBindingL)
    pBindLambdaBinding = do
      pure undefined
    pLambdaBinding3 :: Parser (MoveTerm LambdaBindingL)
    pLambdaBinding3 = do
      pure undefined

pLoopExpression :: Parser (MoveTerm L)
pLoopExpression = do
  _sym <- pSymbol SLoopExpressionSymbol
  pure undefined

pQuantifierExpression :: Parser (MoveTerm L)
pQuantifierExpression = do
  _sym <- pSymbol SQuantifierExpressionSymbol
  pure undefined

pQuantifierBindings :: Parser (MoveTerm L)
pQuantifierBindings = do
  _sym <- pSymbol SQuantifierBindingsSymbol
  pure undefined

pQuantifierBinding :: Parser (MoveTerm L)
pQuantifierBinding = do
  _sym <- pSymbol SQuantifierBindingSymbol
  pure undefined
  where
    pQuantifierBinding1 :: Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding1 = do
      pure undefined
    pQuantifierBinding2 :: Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding2 = do
      pure undefined

pReturnExpression :: Parser (MoveTerm L)
pReturnExpression = do
  _sym <- pSymbol SReturnExpressionSymbol
  pure undefined
  where
    pReturnExpression1 :: Parser (MoveTerm ReturnExpressionL)
    pReturnExpression1 = do
      pure undefined
    pReturnExpression2 :: Parser (MoveTerm ReturnExpressionL)
    pReturnExpression2 = do
      pure undefined

pWhileExpression :: Parser (MoveTerm L)
pWhileExpression = do
  _sym <- pSymbol SWhileExpressionSymbol
  pure undefined

pBlockItem :: Parser (MoveTerm L)
pBlockItem = do
  _sym <- pSymbol SBlockItemSymbol
  pure undefined

pLetStatement :: Parser (MoveTerm L)
pLetStatement = do
  _sym <- pSymbol SLetStatementSymbol
  pure undefined

pMacroFunctionDefinition :: Parser (MoveTerm L)
pMacroFunctionDefinition = do
  _sym <- pSymbol SMacroFunctionDefinitionSymbol
  pure undefined

pHiddenMacroSignature :: Parser (MoveTerm L)
pHiddenMacroSignature = do
  _sym <- pSymbol SHiddenMacroSignatureSymbol
  pure undefined

pNativeFunctionDefinition :: Parser (MoveTerm L)
pNativeFunctionDefinition = do
  _sym <- pSymbol SNativeFunctionDefinitionSymbol
  pure undefined

pHiddenStructItem :: Parser (MoveTerm L)
pHiddenStructItem = do
  _sym <- pSymbol SHiddenStructItemSymbol
  pure undefined
  where
    pNativeStructDefinitionStructItem :: Parser (MoveTerm HiddenStructItemL)
    pNativeStructDefinitionStructItem = do
      pure undefined
    pStructDefinitionStructItem :: Parser (MoveTerm HiddenStructItemL)
    pStructDefinitionStructItem = do
      pure undefined

pNativeStructDefinition :: Parser (MoveTerm L)
pNativeStructDefinition = do
  _sym <- pSymbol SNativeStructDefinitionSymbol
  pure undefined

pHiddenStructSignature :: Parser (MoveTerm L)
pHiddenStructSignature = do
  _sym <- pSymbol SHiddenStructSignatureSymbol
  pure undefined

pStructDefinition :: Parser (MoveTerm L)
pStructDefinition = do
  _sym <- pSymbol SStructDefinitionSymbol
  pure undefined

pConstant :: Parser (MoveTerm L)
pConstant = do
  _sym <- pSymbol SConstantSymbol
  pure undefined

pFriendDeclaration :: Parser (MoveTerm L)
pFriendDeclaration = do
  _sym <- pSymbol SFriendDeclarationSymbol
  pure undefined

pFriendAccess :: Parser (MoveTerm L)
pFriendAccess = do
  _sym <- pSymbol SFriendAccessSymbol
  pure undefined
  where
    pLocalModuleFriendAccess :: Parser (MoveTerm FriendAccessL)
    pLocalModuleFriendAccess = do
      pure undefined
    pFullyQualifiedModuleFriendAccess :: Parser (MoveTerm FriendAccessL)
    pFullyQualifiedModuleFriendAccess = do
      pure undefined
