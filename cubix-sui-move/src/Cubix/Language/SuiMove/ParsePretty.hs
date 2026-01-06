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

pSepBy :: Typeable a
       => Parser (MoveTerm a)
       -> Parser (MoveTerm sep)
       -> Parser (MoveTerm [a])
pSepBy = Megaparsec.Cubix.pSepBy

pSepBy1 :: Typeable a
       => Parser (MoveTerm a)
       -> Parser (MoveTerm sep)
       -> Parser (MoveTerm [a])
pSepBy1 = Megaparsec.Cubix.pSepBy1

pBetween :: Typeable a
       => Parser (MoveTerm open)
       -> Parser (MoveTerm close)
       -> Parser (MoveTerm a)
       -> Parser (MoveTerm a)
pBetween = Megaparsec.Cubix.pBetween

pRoot :: Parser (MoveTerm (RootSort MoveSig))
pRoot = pSourceFile <* Megaparsec.eof

pExclamationMarkTok :: Parser (MoveTerm ExclamationMarkTokL)
pExclamationMarkTok = pSymbol "!" SExclamationMarkTokSymbol $> iExclamationMarkTok

pExclamationMarkEqualsSignTok :: Parser (MoveTerm ExclamationMarkEqualsSignTokL)
pExclamationMarkEqualsSignTok = pSymbol "!=" SExclamationMarkEqualsSignTokSymbol $> iExclamationMarkEqualsSignTok

pNumberSignLeftSquareBracketTok :: Parser (MoveTerm NumberSignLeftSquareBracketTokL)
pNumberSignLeftSquareBracketTok = pSymbol "#[" SNumberSignLeftSquareBracketTokSymbol $> iNumberSignLeftSquareBracketTok

pDollarSignTok :: Parser (MoveTerm DollarSignTokL)
pDollarSignTok = pSymbol "$" SDollarSignTokSymbol $> iDollarSignTok

pPercentSignTok :: Parser (MoveTerm PercentSignTokL)
pPercentSignTok = pSymbol "%" SPercentSignTokSymbol $> iPercentSignTok

pAmpersandTok :: Parser (MoveTerm AmpersandTokL)
pAmpersandTok = pSymbol "&" SAmpersandTokSymbol $> iAmpersandTok

pAmpersandAmpersandTok :: Parser (MoveTerm AmpersandAmpersandTokL)
pAmpersandAmpersandTok = pSymbol "&&" SAmpersandAmpersandTokSymbol $> iAmpersandAmpersandTok

pApostropheTok :: Parser (MoveTerm ApostropheTokL)
pApostropheTok = pSymbol "'" SApostropheTokSymbol $> iApostropheTok

pLeftParenthesisTok :: Parser (MoveTerm LeftParenthesisTokL)
pLeftParenthesisTok = pSymbol "(" SLeftParenthesisTokSymbol $> iLeftParenthesisTok

pRightParenthesisTok :: Parser (MoveTerm RightParenthesisTokL)
pRightParenthesisTok = pSymbol ")" SRightParenthesisTokSymbol $> iRightParenthesisTok

pAsteriskTok :: Parser (MoveTerm AsteriskTokL)
pAsteriskTok = pSymbol "*" SAsteriskTokSymbol $> iAsteriskTok

pPlusSignTok :: Parser (MoveTerm PlusSignTokL)
pPlusSignTok = pSymbol "+" SPlusSignTokSymbol $> iPlusSignTok

pCommaTok :: Parser (MoveTerm CommaTokL)
pCommaTok = pSymbol "," SCommaTokSymbol $> iCommaTok

pHyphenMinusTok :: Parser (MoveTerm HyphenMinusTokL)
pHyphenMinusTok = pSymbol "-" SHyphenMinusTokSymbol $> iHyphenMinusTok

pHyphenMinusGreaterThanSignTok :: Parser (MoveTerm HyphenMinusGreaterThanSignTokL)
pHyphenMinusGreaterThanSignTok = pSymbol "->" SHyphenMinusGreaterThanSignTokSymbol $> iHyphenMinusGreaterThanSignTok

pFullStopTok :: Parser (MoveTerm FullStopTokL)
pFullStopTok = pSymbol "." SFullStopTokSymbol $> iFullStopTok

pFullStopFullStopTok :: Parser (MoveTerm FullStopFullStopTokL)
pFullStopFullStopTok = pSymbol ".." SFullStopFullStopTokSymbol $> iFullStopFullStopTok

pSolidusTok :: Parser (MoveTerm SolidusTokL)
pSolidusTok = pSymbol "/" SSolidusTokSymbol $> iSolidusTok

pSolidusAsteriskTok :: Parser (MoveTerm SolidusAsteriskTokL)
pSolidusAsteriskTok = pSymbol "/*" SSolidusAsteriskTokSymbol $> iSolidusAsteriskTok

pSolidusSolidusTok :: Parser (MoveTerm SolidusSolidusTokL)
pSolidusSolidusTok = pSymbol "//" SSolidusSolidusTokSymbol $> iSolidusSolidusTok

pColonTok :: Parser (MoveTerm ColonTokL)
pColonTok = pSymbol ":" SColonTokSymbol $> iColonTok

pColonColonTok :: Parser (MoveTerm ColonColonTokL)
pColonColonTok = pSymbol "::" SColonColonTokSymbol $> iColonColonTok

pSemicolonTok :: Parser (MoveTerm SemicolonTokL)
pSemicolonTok = pSymbol ";" SSemicolonTokSymbol $> iSemicolonTok

pLessThanSignTok :: Parser (MoveTerm LessThanSignTokL)
pLessThanSignTok = pSymbol "<" SLessThanSignTokSymbol $> iLessThanSignTok

pLessThanSignLessThanSignTok :: Parser (MoveTerm LessThanSignLessThanSignTokL)
pLessThanSignLessThanSignTok = pSymbol "<<" SLessThanSignLessThanSignTokSymbol $> iLessThanSignLessThanSignTok

pLessThanSignEqualsSignTok :: Parser (MoveTerm LessThanSignEqualsSignTokL)
pLessThanSignEqualsSignTok = pSymbol "<=" SLessThanSignEqualsSignTokSymbol $> iLessThanSignEqualsSignTok

pEqualsSignTok :: Parser (MoveTerm EqualsSignTokL)
pEqualsSignTok = pSymbol "=" SEqualsSignTokSymbol $> iEqualsSignTok

pEqualsSignEqualsSignTok :: Parser (MoveTerm EqualsSignEqualsSignTokL)
pEqualsSignEqualsSignTok = pSymbol "==" SEqualsSignEqualsSignTokSymbol $> iEqualsSignEqualsSignTok

pEqualsSignEqualsSignGreaterThanSignTok :: Parser (MoveTerm EqualsSignEqualsSignGreaterThanSignTokL)
pEqualsSignEqualsSignGreaterThanSignTok = pSymbol "==>" SEqualsSignEqualsSignGreaterThanSignTokSymbol $> iEqualsSignEqualsSignGreaterThanSignTok

pEqualsSignGreaterThanSignTok :: Parser (MoveTerm EqualsSignGreaterThanSignTokL)
pEqualsSignGreaterThanSignTok = pSymbol "=>" SEqualsSignGreaterThanSignTokSymbol $> iEqualsSignGreaterThanSignTok

pGreaterThanSignTok :: Parser (MoveTerm GreaterThanSignTokL)
pGreaterThanSignTok = pSymbol ">" SGreaterThanSignTokSymbol $> iGreaterThanSignTok

pGreaterThanSignEqualsSignTok :: Parser (MoveTerm GreaterThanSignEqualsSignTokL)
pGreaterThanSignEqualsSignTok = pSymbol ">=" SGreaterThanSignEqualsSignTokSymbol $> iGreaterThanSignEqualsSignTok

pGreaterThanSignGreaterThanSignTok :: Parser (MoveTerm GreaterThanSignGreaterThanSignTokL)
pGreaterThanSignGreaterThanSignTok = pSymbol ">>" SGreaterThanSignGreaterThanSignTokSymbol $> iGreaterThanSignGreaterThanSignTok

pCommercialAtTok :: Parser (MoveTerm CommercialAtTokL)
pCommercialAtTok = pSymbol "@" SCommercialAtTokSymbol $> iCommercialAtTok

pLeftSquareBracketTok :: Parser (MoveTerm LeftSquareBracketTokL)
pLeftSquareBracketTok = pSymbol "[" SLeftSquareBracketTokSymbol $> iLeftSquareBracketTok

pRightSquareBracketTok :: Parser (MoveTerm RightSquareBracketTokL)
pRightSquareBracketTok = pSymbol "]" SRightSquareBracketTokSymbol $> iRightSquareBracketTok

pCircumflexAccentTok :: Parser (MoveTerm CircumflexAccentTokL)
pCircumflexAccentTok = pSymbol "^" SCircumflexAccentTokSymbol $> iCircumflexAccentTok

pAbortTok :: Parser (MoveTerm AbortTokL)
pAbortTok = pSymbol "abort" SAbortTokSymbol $> iAbortTok

pAbortsIfTok :: Parser (MoveTerm AbortsIfTokL)
pAbortsIfTok = pSymbol "aborts_if" SAbortsIfTokSymbol $> iAbortsIfTok

pAbortsWithTok :: Parser (MoveTerm AbortsWithTokL)
pAbortsWithTok = pSymbol "aborts_with" SAbortsWithTokSymbol $> iAbortsWithTok

pAddressTok :: Parser (MoveTerm AddressTokL)
pAddressTok = pSymbol "address" SAddressTokSymbol $> iAddressTok

pApplyTok :: Parser (MoveTerm ApplyTokL)
pApplyTok = pSymbol "apply" SApplyTokSymbol $> iApplyTok

pAsTok :: Parser (MoveTerm AsTokL)
pAsTok = pSymbol "as" SAsTokSymbol $> iAsTok

pAssertTok :: Parser (MoveTerm AssertTokL)
pAssertTok = pSymbol "assert" SAssertTokSymbol $> iAssertTok

pAssumeTok :: Parser (MoveTerm AssumeTokL)
pAssumeTok = pSymbol "assume" SAssumeTokSymbol $> iAssumeTok

pBoolTok :: Parser (MoveTerm BoolTokL)
pBoolTok = pSymbol "bool" SBoolTokSymbol $> iBoolTok

pBreakTok :: Parser (MoveTerm BreakTokL)
pBreakTok = pSymbol "break" SBreakTokSymbol $> iBreakTok

pBytearrayTok :: Parser (MoveTerm BytearrayTokL)
pBytearrayTok = pSymbol "bytearray" SBytearrayTokSymbol $> iBytearrayTok

pConstTok :: Parser (MoveTerm ConstTokL)
pConstTok = pSymbol "const" SConstTokSymbol $> iConstTok

pContinueTok :: Parser (MoveTerm ContinueTokL)
pContinueTok = pSymbol "continue" SContinueTokSymbol $> iContinueTok

pCopyTok :: Parser (MoveTerm CopyTokL)
pCopyTok = pSymbol "copy" SCopyTokSymbol $> iCopyTok

pDecreasesTok :: Parser (MoveTerm DecreasesTokL)
pDecreasesTok = pSymbol "decreases" SDecreasesTokSymbol $> iDecreasesTok

pDropTok :: Parser (MoveTerm DropTokL)
pDropTok = pSymbol "drop" SDropTokSymbol $> iDropTok

pElseTok :: Parser (MoveTerm ElseTokL)
pElseTok = pSymbol "else" SElseTokSymbol $> iElseTok

pEnsuresTok :: Parser (MoveTerm EnsuresTokL)
pEnsuresTok = pSymbol "ensures" SEnsuresTokSymbol $> iEnsuresTok

pEntryTok :: Parser (MoveTerm EntryTokL)
pEntryTok = pSymbol "entry" SEntryTokSymbol $> iEntryTok

pEnumTok :: Parser (MoveTerm EnumTokL)
pEnumTok = pSymbol "enum" SEnumTokSymbol $> iEnumTok

pExceptTok :: Parser (MoveTerm ExceptTokL)
pExceptTok = pSymbol "except" SExceptTokSymbol $> iExceptTok

pExistsTok :: Parser (MoveTerm ExistsTokL)
pExistsTok = pSymbol "exists" SExistsTokSymbol $> iExistsTok

pFalseTok :: Parser (MoveTerm FalseTokL)
pFalseTok = pSymbol "false" SFalseTokSymbol $> iFalseTok

pForallTok :: Parser (MoveTerm ForallTokL)
pForallTok = pSymbol "forall" SForallTokSymbol $> iForallTok

pFriendTok :: Parser (MoveTerm FriendTokL)
pFriendTok = pSymbol "friend" SFriendTokSymbol $> iFriendTok

pFunTok :: Parser (MoveTerm FunTokL)
pFunTok = pSymbol "fun" SFunTokSymbol $> iFunTok

pGlobalTok :: Parser (MoveTerm GlobalTokL)
pGlobalTok = pSymbol "global" SGlobalTokSymbol $> iGlobalTok

pHasTok :: Parser (MoveTerm HasTokL)
pHasTok = pSymbol "has" SHasTokSymbol $> iHasTok

pIfTok :: Parser (MoveTerm IfTokL)
pIfTok = pSymbol "if" SIfTokSymbol $> iIfTok

pInTok :: Parser (MoveTerm InTokL)
pInTok = pSymbol "in" SInTokSymbol $> iInTok

pIncludeTok :: Parser (MoveTerm IncludeTokL)
pIncludeTok = pSymbol "include" SIncludeTokSymbol $> iIncludeTok

pInternalTok :: Parser (MoveTerm InternalTokL)
pInternalTok = pSymbol "internal" SInternalTokSymbol $> iInternalTok

pInvariantTok :: Parser (MoveTerm InvariantTokL)
pInvariantTok = pSymbol "invariant" SInvariantTokSymbol $> iInvariantTok

pKeyTok :: Parser (MoveTerm KeyTokL)
pKeyTok = pSymbol "key" SKeyTokSymbol $> iKeyTok

pLetTok :: Parser (MoveTerm LetTokL)
pLetTok = pSymbol "let" SLetTokSymbol $> iLetTok

pLocalTok :: Parser (MoveTerm LocalTokL)
pLocalTok = pSymbol "local" SLocalTokSymbol $> iLocalTok

pLoopTok :: Parser (MoveTerm LoopTokL)
pLoopTok = pSymbol "loop" SLoopTokSymbol $> iLoopTok

pMacroTok :: Parser (MoveTerm MacroTokL)
pMacroTok = pSymbol "macro" SMacroTokSymbol $> iMacroTok

pMatchTok :: Parser (MoveTerm MatchTokL)
pMatchTok = pSymbol "match" SMatchTokSymbol $> iMatchTok

pModifiesTok :: Parser (MoveTerm ModifiesTokL)
pModifiesTok = pSymbol "modifies" SModifiesTokSymbol $> iModifiesTok

pModuleTok :: Parser (MoveTerm ModuleTokL)
pModuleTok = pSymbol "module" SModuleTokSymbol $> iModuleTok

pMoveTok :: Parser (MoveTerm MoveTokL)
pMoveTok = pSymbol "move" SMoveTokSymbol $> iMoveTok

pMutTok :: Parser (MoveTerm MutTokL)
pMutTok = pSymbol "mut" SMutTokSymbol $> iMutTok

pNativeTok :: Parser (MoveTerm NativeTokL)
pNativeTok = pSymbol "native" SNativeTokSymbol $> iNativeTok

pPackTok :: Parser (MoveTerm PackTokL)
pPackTok = pSymbol "pack" SPackTokSymbol $> iPackTok

pPackageTok :: Parser (MoveTerm PackageTokL)
pPackageTok = pSymbol "package" SPackageTokSymbol $> iPackageTok

pPhantomTok :: Parser (MoveTerm PhantomTokL)
pPhantomTok = pSymbol "phantom" SPhantomTokSymbol $> iPhantomTok

pPostTok :: Parser (MoveTerm PostTokL)
pPostTok = pSymbol "post" SPostTokSymbol $> iPostTok

pPragmaTok :: Parser (MoveTerm PragmaTokL)
pPragmaTok = pSymbol "pragma" SPragmaTokSymbol $> iPragmaTok

pPublicTok :: Parser (MoveTerm PublicTokL)
pPublicTok = pSymbol "public" SPublicTokSymbol $> iPublicTok

pRequiresTok :: Parser (MoveTerm RequiresTokL)
pRequiresTok = pSymbol "requires" SRequiresTokSymbol $> iRequiresTok

pReturnTok :: Parser (MoveTerm ReturnTokL)
pReturnTok = pSymbol "return" SReturnTokSymbol $> iReturnTok

pSchemaTok :: Parser (MoveTerm SchemaTokL)
pSchemaTok = pSymbol "schema" SSchemaTokSymbol $> iSchemaTok

pSignerTok :: Parser (MoveTerm SignerTokL)
pSignerTok = pSymbol "signer" SSignerTokSymbol $> iSignerTok

pSpecTok :: Parser (MoveTerm SpecTokL)
pSpecTok = pSymbol "spec" SSpecTokSymbol $> iSpecTok

pStoreTok :: Parser (MoveTerm StoreTokL)
pStoreTok = pSymbol "store" SStoreTokSymbol $> iStoreTok

pStructTok :: Parser (MoveTerm StructTokL)
pStructTok = pSymbol "struct" SStructTokSymbol $> iStructTok

pSucceedsIfTok :: Parser (MoveTerm SucceedsIfTokL)
pSucceedsIfTok = pSymbol "succeeds_if" SSucceedsIfTokSymbol $> iSucceedsIfTok

pToTok :: Parser (MoveTerm ToTokL)
pToTok = pSymbol "to" SToTokSymbol $> iToTok

pTrueTok :: Parser (MoveTerm TrueTokL)
pTrueTok = pSymbol "true" STrueTokSymbol $> iTrueTok

pU128Tok :: Parser (MoveTerm U128TokL)
pU128Tok = pSymbol "u128" SU128TokSymbol $> iU128Tok

pU16Tok :: Parser (MoveTerm U16TokL)
pU16Tok = pSymbol "u16" SU16TokSymbol $> iU16Tok

pU256Tok :: Parser (MoveTerm U256TokL)
pU256Tok = pSymbol "u256" SU256TokSymbol $> iU256Tok

pU32Tok :: Parser (MoveTerm U32TokL)
pU32Tok = pSymbol "u32" SU32TokSymbol $> iU32Tok

pU64Tok :: Parser (MoveTerm U64TokL)
pU64Tok = pSymbol "u64" SU64TokSymbol $> iU64Tok

pU8Tok :: Parser (MoveTerm U8TokL)
pU8Tok = pSymbol "u8" SU8TokSymbol $> iU8Tok

pUnpackTok :: Parser (MoveTerm UnpackTokL)
pUnpackTok = pSymbol "unpack" SUnpackTokSymbol $> iUnpackTok

pUpdateTok :: Parser (MoveTerm UpdateTokL)
pUpdateTok = pSymbol "update" SUpdateTokSymbol $> iUpdateTok

pUseTok :: Parser (MoveTerm UseTokL)
pUseTok = pSymbol "use" SUseTokSymbol $> iUseTok

pVectorLessThanSignTok :: Parser (MoveTerm VectorLessThanSignTokL)
pVectorLessThanSignTok = pSymbol "vector<" SVectorLessThanSignTokSymbol $> iVectorLessThanSignTok

pVectorLeftSquareBracketTok :: Parser (MoveTerm VectorLeftSquareBracketTokL)
pVectorLeftSquareBracketTok = pSymbol "vector[" SVectorLeftSquareBracketTokSymbol $> iVectorLeftSquareBracketTok

pWhereTok :: Parser (MoveTerm WhereTokL)
pWhereTok = pSymbol "where" SWhereTokSymbol $> iWhereTok

pWhileTok :: Parser (MoveTerm WhileTokL)
pWhileTok = pSymbol "while" SWhileTokSymbol $> iWhileTok

pWithTok :: Parser (MoveTerm WithTokL)
pWithTok = pSymbol "with" SWithTokSymbol $> iWithTok

pLeftCurlyBracketTok :: Parser (MoveTerm LeftCurlyBracketTokL)
pLeftCurlyBracketTok = pSymbol "{" SLeftCurlyBracketTokSymbol $> iLeftCurlyBracketTok

pVerticalLineTok :: Parser (MoveTerm VerticalLineTokL)
pVerticalLineTok = pSymbol "|" SVerticalLineTokSymbol $> iVerticalLineTok

pVerticalLineVerticalLineTok :: Parser (MoveTerm VerticalLineVerticalLineTokL)
pVerticalLineVerticalLineTok = pSymbol "||" SVerticalLineVerticalLineTokSymbol $> iVerticalLineVerticalLineTok

pRightCurlyBracketTok :: Parser (MoveTerm RightCurlyBracketTokL)
pRightCurlyBracketTok = pSymbol "}" SRightCurlyBracketTokSymbol $> iRightCurlyBracketTok

pSourceFile :: Parser (MoveTerm SourceFileL)
pSourceFile = do
  _sym <- pSymbol "source_file" SSourceFileSymbol
  iSourceFile <$> pMany pModuleDefinition

pModuleDefinition :: Parser (MoveTerm ModuleDefinitionL)
pModuleDefinition = do
  _sym <- pSymbol "module_definition" SModuleDefinitionSymbol
  iModuleDefinition <$> pModuleTok <*> pModuleIdentity <*> pModuleBody

pModuleBody :: Parser (MoveTerm ModuleBodyL)
pModuleBody = do
  _sym <- pSymbol "module_body" SModuleBodySymbol
  iModuleBody <$> (pModuleBodyInternal0 _sym) <*> pMany (pModuleBodyInternal1 _sym) <*> pMaybe pRightCurlyBracketTok

pModuleBodyInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal0L)
pModuleBodyInternal0 _sym = do
  choice [ Megaparsec.try (pModuleBodyInternal0Semicolon _sym)
         , Megaparsec.try (pModuleBodyInternal0LeftCurlyBracket _sym)
         ]
  where
    pModuleBodyInternal0Semicolon :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal0L)
    pModuleBodyInternal0Semicolon _sym =
      iModuleBodyInternal0Semicolon <$> pSemicolonTok
    pModuleBodyInternal0LeftCurlyBracket :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal0L)
    pModuleBodyInternal0LeftCurlyBracket _sym =
      iModuleBodyInternal0LeftCurlyBracket <$> pLeftCurlyBracketTok

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
      iModuleBodyInternal1UseDeclaration <$> pUseDeclaration
    pModuleBodyInternal1FriendDeclaration :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1FriendDeclaration _sym =
      iModuleBodyInternal1FriendDeclaration <$> pFriendDeclaration
    pModuleBodyInternal1Constant :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1Constant _sym =
      iModuleBodyInternal1Constant <$> pConstant
    pModuleBodyInternal1FunctionItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1FunctionItem _sym =
      iModuleBodyInternal1FunctionItem <$> (pHiddenFunctionItem _sym)
    pModuleBodyInternal1StructItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1StructItem _sym =
      iModuleBodyInternal1StructItem <$> (pHiddenStructItem _sym)
    pModuleBodyInternal1EnumItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1EnumItem _sym =
      iModuleBodyInternal1EnumItem <$> (pHiddenEnumItem _sym)
    pModuleBodyInternal1SpecBlock :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1SpecBlock _sym =
      iModuleBodyInternal1SpecBlock <$> pSpecBlock

pHiddenEnumItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenEnumItemL)
pHiddenEnumItem _sym = do
  iHiddenEnumItem <$> pEnumDefinition

pEnumDefinition :: Parser (MoveTerm EnumDefinitionL)
pEnumDefinition = do
  _sym <- pSymbol "enum_definition" SEnumDefinitionSymbol
  iEnumDefinition <$> pMaybe pPublicTok <*> (pHiddenEnumSignature _sym) <*> pEnumVariants <*> pMaybe pPostfixAbilityDecls

pHiddenEnumSignature :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenEnumSignatureL)
pHiddenEnumSignature _sym = do
  iHiddenEnumSignature <$> pEnumTok <*> (pHiddenEnumIdentifier _sym) <*> pMaybe pTypeParameters <*> pMaybe pAbilityDecls

pHiddenEnumIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenEnumIdentifierL)
pHiddenEnumIdentifier _sym = do
  iHiddenEnumIdentifier <$> pIdentifier

pIdentifier :: Parser (MoveTerm IdentifierL)
pIdentifier = do
  _sym <- pSymbol "identifier" SIdentifierSymbol
  iIdentifier <$> pContent _sym

pAbilityDecls :: Parser (MoveTerm AbilityDeclsL)
pAbilityDecls = do
  _sym <- pSymbol "ability_decls" SAbilityDeclsSymbol
  iAbilityDecls <$> pHasTok <*> pSepBy pAbility pCommaTok

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
      iAbilityCopy <$> pCopyTok
    pAbilityDrop :: Cubix.TreeSitter.Token a -> Parser (MoveTerm AbilityL)
    pAbilityDrop _sym =
      iAbilityDrop <$> pDropTok
    pAbilityStore :: Cubix.TreeSitter.Token a -> Parser (MoveTerm AbilityL)
    pAbilityStore _sym =
      iAbilityStore <$> pStoreTok
    pAbilityKey :: Cubix.TreeSitter.Token a -> Parser (MoveTerm AbilityL)
    pAbilityKey _sym =
      iAbilityKey <$> pKeyTok

pTypeParameters :: Parser (MoveTerm TypeParametersL)
pTypeParameters = do
  _sym <- pSymbol "type_parameters" STypeParametersSymbol
  iTypeParameters <$> pBetween pLessThanSignTok pGreaterThanSignTok (pSepBy1 pTypeParameter pCommaTok)

pTypeParameter :: Parser (MoveTerm TypeParameterL)
pTypeParameter = do
  _sym <- pSymbol "type_parameter" STypeParameterSymbol
  iTypeParameter <$> pMaybe pDollarSignTok <*> pMaybe pPhantomTok <*> (pHiddenTypeParameterIdentifier _sym) <*> pMaybe (pPair pColonTok (pSepBy1 pAbility pPlusSignTok))

pHiddenTypeParameterIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeParameterIdentifierL)
pHiddenTypeParameterIdentifier _sym = do
  iHiddenTypeParameterIdentifier <$> pIdentifier

pEnumVariants :: Parser (MoveTerm EnumVariantsL)
pEnumVariants = do
  _sym <- pSymbol "enum_variants" SEnumVariantsSymbol
  iEnumVariants <$> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy pVariant pCommaTok)

pVariant :: Parser (MoveTerm VariantL)
pVariant = do
  _sym <- pSymbol "variant" SVariantSymbol
  iVariant <$> (pHiddenVariantIdentifier _sym) <*> pMaybe pDatatypeFields

pHiddenVariantIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenVariantIdentifierL)
pHiddenVariantIdentifier _sym = do
  iHiddenVariantIdentifier <$> pIdentifier

pDatatypeFields :: Parser (MoveTerm DatatypeFieldsL)
pDatatypeFields = do
  _sym <- pSymbol "datatype_fields" SDatatypeFieldsSymbol
  choice [ Megaparsec.try (pDatatypeFieldsPositionalFields _sym)
         , Megaparsec.try (pDatatypeFieldsNamedFields _sym)
         ]
  where
    pDatatypeFieldsPositionalFields :: Cubix.TreeSitter.Token a -> Parser (MoveTerm DatatypeFieldsL)
    pDatatypeFieldsPositionalFields _sym =
      iDatatypeFieldsPositionalFields <$> pPositionalFields
    pDatatypeFieldsNamedFields :: Cubix.TreeSitter.Token a -> Parser (MoveTerm DatatypeFieldsL)
    pDatatypeFieldsNamedFields _sym =
      iDatatypeFieldsNamedFields <$> pNamedFields

pNamedFields :: Parser (MoveTerm NamedFieldsL)
pNamedFields = do
  _sym <- pSymbol "named_fields" SNamedFieldsSymbol
  iNamedFields <$> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy pFieldAnnotation pCommaTok)

pFieldAnnotation :: Parser (MoveTerm FieldAnnotationL)
pFieldAnnotation = do
  _sym <- pSymbol "field_annotation" SFieldAnnotationSymbol
  iFieldAnnotation <$> (pHiddenFieldIdentifier _sym) <*> pColonTok <*> (pHiddenType _sym)

pHiddenFieldIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFieldIdentifierL)
pHiddenFieldIdentifier _sym = do
  iHiddenFieldIdentifier <$> pIdentifier

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
      iHiddenTypeApplyType <$> pApplyType
    pHiddenTypeRefType :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeL)
    pHiddenTypeRefType _sym =
      iHiddenTypeRefType <$> pRefType
    pHiddenTypeTupleType :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeL)
    pHiddenTypeTupleType _sym =
      iHiddenTypeTupleType <$> pTupleType
    pHiddenTypeFunctionType :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeL)
    pHiddenTypeFunctionType _sym =
      iHiddenTypeFunctionType <$> pFunctionType
    pHiddenTypePrimitiveType :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenTypeL)
    pHiddenTypePrimitiveType _sym =
      iHiddenTypePrimitiveType <$> pPrimitiveType

pApplyType :: Parser (MoveTerm ApplyTypeL)
pApplyType = do
  _sym <- pSymbol "apply_type" SApplyTypeSymbol
  iApplyType <$> pPair pModuleAccess (pMaybe pTypeArguments)

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
      iModuleAccess1 <$> pDollarSignTok <*> pIdentifier
    pModuleAccess2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess2 _sym =
      iModuleAccess2 <$> pCommercialAtTok <*> pIdentifier
    pModuleAccess3 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess3 _sym =
      iModuleAccess3 <$> pModuleIdentity <*> pColonColonTok <*> pIdentifier <*> pMaybe pTypeArguments <*> pColonColonTok <*> pIdentifier
    pModuleAccess4 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess4 _sym =
      iModuleAccess4 <$> pModuleIdentity <*> pColonColonTok <*> pIdentifier <*> pTypeArguments
    pModuleAccess5 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess5 _sym =
      iModuleAccess5 <$> pModuleIdentity <*> pMaybe pTypeArguments <*> pColonColonTok <*> pIdentifier
    pModuleAccess6 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess6 _sym =
      iModuleAccess6 <$> (pHiddenModuleIdentifier _sym) <*> pMaybe pTypeArguments <*> pColonColonTok <*> pIdentifier
    pModuleAccess7 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess7 _sym =
      iModuleAccess7 <$> pModuleIdentity <*> pMaybe pTypeArguments
    pModuleAccess8 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccess8 _sym =
      iModuleAccess8 <$> pIdentifier <*> pMaybe pTypeArguments
    pModuleAccessMember :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleAccessL)
    pModuleAccessMember _sym =
      iModuleAccessMember <$> (pHiddenReservedIdentifier _sym)

pHiddenModuleIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenModuleIdentifierL)
pHiddenModuleIdentifier _sym = do
  iHiddenModuleIdentifier <$> pIdentifier

pHiddenReservedIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReservedIdentifierL)
pHiddenReservedIdentifier _sym = do
  choice [ Megaparsec.try (pHiddenReservedIdentifierForall _sym)
         , Megaparsec.try (pHiddenReservedIdentifierExists _sym)
         ]
  where
    pHiddenReservedIdentifierForall :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReservedIdentifierL)
    pHiddenReservedIdentifierForall _sym =
      iHiddenReservedIdentifierForall <$> (pHiddenForall _sym)
    pHiddenReservedIdentifierExists :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReservedIdentifierL)
    pHiddenReservedIdentifierExists _sym =
      iHiddenReservedIdentifierExists <$> (pHiddenExists _sym)

pHiddenExists :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExistsL)
pHiddenExists _sym = do
  iHiddenExists <$> pExistsTok

pHiddenForall :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenForallL)
pHiddenForall _sym = do
  iHiddenForall <$> pForallTok

pModuleIdentity :: Parser (MoveTerm ModuleIdentityL)
pModuleIdentity = do
  _sym <- pSymbol "module_identity" SModuleIdentitySymbol
  iModuleIdentity <$> (pModuleIdentityInternal0 _sym) <*> pColonColonTok <*> (pHiddenModuleIdentifier _sym)

pModuleIdentityInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleIdentityInternal0L)
pModuleIdentityInternal0 _sym = do
  choice [ Megaparsec.try (pModuleIdentityInternal0NumLiteral _sym)
         , Megaparsec.try (pModuleIdentityInternal0ModuleIdentifier _sym)
         ]
  where
    pModuleIdentityInternal0NumLiteral :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleIdentityInternal0L)
    pModuleIdentityInternal0NumLiteral _sym =
      iModuleIdentityInternal0NumLiteral <$> pNumLiteral
    pModuleIdentityInternal0ModuleIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModuleIdentityInternal0L)
    pModuleIdentityInternal0ModuleIdentifier _sym =
      iModuleIdentityInternal0ModuleIdentifier <$> (pHiddenModuleIdentifier _sym)

pNumLiteral :: Parser (MoveTerm NumLiteralL)
pNumLiteral = do
  _sym <- pSymbol "num_literal" SNumLiteralSymbol
  iNumLiteral <$> pContent _sym <*> pMaybe (pNumLiteralInternal0 _sym)

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
      iNumLiteralInternal0U8 <$> pU8Tok
    pNumLiteralInternal0U16 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U16 _sym =
      iNumLiteralInternal0U16 <$> pU16Tok
    pNumLiteralInternal0U32 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U32 _sym =
      iNumLiteralInternal0U32 <$> pU32Tok
    pNumLiteralInternal0U64 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U64 _sym =
      iNumLiteralInternal0U64 <$> pU64Tok
    pNumLiteralInternal0U128 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U128 _sym =
      iNumLiteralInternal0U128 <$> pU128Tok
    pNumLiteralInternal0U256 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U256 _sym =
      iNumLiteralInternal0U256 <$> pU256Tok

pTypeArguments :: Parser (MoveTerm TypeArgumentsL)
pTypeArguments = do
  _sym <- pSymbol "type_arguments" STypeArgumentsSymbol
  iTypeArguments <$> pBetween pLessThanSignTok pGreaterThanSignTok (pSepBy1 (pHiddenType _sym) pCommaTok)

pFunctionType :: Parser (MoveTerm FunctionTypeL)
pFunctionType = do
  _sym <- pSymbol "function_type" SFunctionTypeSymbol
  iFunctionType <$> pFunctionTypeParameters <*> pMaybe (pPair pHyphenMinusGreaterThanSignTok (pHiddenType _sym))

pFunctionTypeParameters :: Parser (MoveTerm FunctionTypeParametersL)
pFunctionTypeParameters = do
  _sym <- pSymbol "function_type_parameters" SFunctionTypeParametersSymbol
  iFunctionTypeParameters <$> pBetween pVerticalLineTok pVerticalLineTok (pSepBy (pHiddenType _sym) pCommaTok)

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
      iPrimitiveTypeU8 <$> pU8Tok
    pPrimitiveTypeU16 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU16 _sym =
      iPrimitiveTypeU16 <$> pU16Tok
    pPrimitiveTypeU32 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU32 _sym =
      iPrimitiveTypeU32 <$> pU32Tok
    pPrimitiveTypeU64 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU64 _sym =
      iPrimitiveTypeU64 <$> pU64Tok
    pPrimitiveTypeU128 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU128 _sym =
      iPrimitiveTypeU128 <$> pU128Tok
    pPrimitiveTypeU256 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU256 _sym =
      iPrimitiveTypeU256 <$> pU256Tok
    pPrimitiveTypeBool :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeBool _sym =
      iPrimitiveTypeBool <$> pBoolTok
    pPrimitiveTypeAddress :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeAddress _sym =
      iPrimitiveTypeAddress <$> pAddressTok
    pPrimitiveTypeSigner :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeSigner _sym =
      iPrimitiveTypeSigner <$> pSignerTok
    pPrimitiveTypeBytearray :: Cubix.TreeSitter.Token a -> Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeBytearray _sym =
      iPrimitiveTypeBytearray <$> pBytearrayTok

pRefType :: Parser (MoveTerm RefTypeL)
pRefType = do
  _sym <- pSymbol "ref_type" SRefTypeSymbol
  iRefType <$> (pHiddenReference _sym) <*> (pHiddenType _sym)

pHiddenReference :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReferenceL)
pHiddenReference _sym = do
  choice [ Megaparsec.try (pHiddenReferenceImmRef _sym)
         , Megaparsec.try (pHiddenReferenceMutRef _sym)
         ]
  where
    pHiddenReferenceImmRef :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReferenceL)
    pHiddenReferenceImmRef _sym =
      iHiddenReferenceImmRef <$> pImmRef
    pHiddenReferenceMutRef :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenReferenceL)
    pHiddenReferenceMutRef _sym =
      iHiddenReferenceMutRef <$> pMutRef

pImmRef :: Parser (MoveTerm ImmRefL)
pImmRef = do
  _sym <- pSymbol "imm_ref" SImmRefSymbol
  iImmRef <$> pAmpersandTok

pMutRef :: Parser (MoveTerm MutRefL)
pMutRef = do
  _sym <- pSymbol "mut_ref" SMutRefSymbol
  iMutRef <$> pAmpersandTok <*> pMutTok

pTupleType :: Parser (MoveTerm TupleTypeL)
pTupleType = do
  _sym <- pSymbol "tuple_type" STupleTypeSymbol
  iTupleType <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy (pHiddenType _sym) pCommaTok)

pPositionalFields :: Parser (MoveTerm PositionalFieldsL)
pPositionalFields = do
  _sym <- pSymbol "positional_fields" SPositionalFieldsSymbol
  iPositionalFields <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy (pHiddenType _sym) pCommaTok)

pPostfixAbilityDecls :: Parser (MoveTerm PostfixAbilityDeclsL)
pPostfixAbilityDecls = do
  _sym <- pSymbol "postfix_ability_decls" SPostfixAbilityDeclsSymbol
  iPostfixAbilityDecls <$> pBetween pHasTok pSemicolonTok (pSepBy pAbility pCommaTok)

pHiddenFunctionItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionItemL)
pHiddenFunctionItem _sym = do
  choice [ Megaparsec.try (pHiddenFunctionItemNativeFunctionDefinition _sym)
         , Megaparsec.try (pHiddenFunctionItemMacroFunctionDefinition _sym)
         , Megaparsec.try (pHiddenFunctionItemFunctionDefinition _sym)
         ]
  where
    pHiddenFunctionItemNativeFunctionDefinition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionItemL)
    pHiddenFunctionItemNativeFunctionDefinition _sym =
      iHiddenFunctionItemNativeFunctionDefinition <$> pNativeFunctionDefinition
    pHiddenFunctionItemMacroFunctionDefinition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionItemL)
    pHiddenFunctionItemMacroFunctionDefinition _sym =
      iHiddenFunctionItemMacroFunctionDefinition <$> pMacroFunctionDefinition
    pHiddenFunctionItemFunctionDefinition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionItemL)
    pHiddenFunctionItemFunctionDefinition _sym =
      iHiddenFunctionItemFunctionDefinition <$> pFunctionDefinition

pFunctionDefinition :: Parser (MoveTerm FunctionDefinitionL)
pFunctionDefinition = do
  _sym <- pSymbol "function_definition" SFunctionDefinitionSymbol
  iFunctionDefinition <$> (pHiddenFunctionSignature _sym) <*> pBlock

pHiddenFunctionSignature :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionSignatureL)
pHiddenFunctionSignature _sym = do
  iHiddenFunctionSignature <$> pMaybe pModifier <*> pMaybe pModifier <*> pMaybe pModifier <*> pFunTok <*> (pHiddenFunctionIdentifier _sym) <*> pMaybe pTypeParameters <*> pFunctionParameters <*> pMaybe pRetType

pHiddenFunctionIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenFunctionIdentifierL)
pHiddenFunctionIdentifier _sym = do
  iHiddenFunctionIdentifier <$> pIdentifier

pFunctionParameters :: Parser (MoveTerm FunctionParametersL)
pFunctionParameters = do
  _sym <- pSymbol "function_parameters" SFunctionParametersSymbol
  iFunctionParameters <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy (pFunctionParametersInternal0 _sym) pCommaTok)

pFunctionParametersInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParametersInternal0L)
pFunctionParametersInternal0 _sym = do
  choice [ Megaparsec.try (pFunctionParametersInternal0MutFunctionParameter _sym)
         , Megaparsec.try (pFunctionParametersInternal0FunctionParameter _sym)
         ]
  where
    pFunctionParametersInternal0MutFunctionParameter :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParametersInternal0L)
    pFunctionParametersInternal0MutFunctionParameter _sym =
      iFunctionParametersInternal0MutFunctionParameter <$> pMutFunctionParameter
    pFunctionParametersInternal0FunctionParameter :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParametersInternal0L)
    pFunctionParametersInternal0FunctionParameter _sym =
      iFunctionParametersInternal0FunctionParameter <$> pFunctionParameter

pFunctionParameter :: Parser (MoveTerm FunctionParameterL)
pFunctionParameter = do
  _sym <- pSymbol "function_parameter" SFunctionParameterSymbol
  iFunctionParameter <$> (pFunctionParameterInternal0 _sym) <*> pColonTok <*> (pHiddenType _sym)

pFunctionParameterInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParameterInternal0L)
pFunctionParameterInternal0 _sym = do
  choice [ Megaparsec.try (pFunctionParameterInternal0Name _sym)
         , Megaparsec.try (pFunctionParameterInternal02 _sym)
         ]
  where
    pFunctionParameterInternal0Name :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParameterInternal0L)
    pFunctionParameterInternal0Name _sym =
      iFunctionParameterInternal0Name <$> (pHiddenVariableIdentifier _sym)
    pFunctionParameterInternal02 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FunctionParameterInternal0L)
    pFunctionParameterInternal02 _sym =
      iFunctionParameterInternal02 <$> pDollarSignTok <*> (pHiddenVariableIdentifier _sym)

pHiddenVariableIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenVariableIdentifierL)
pHiddenVariableIdentifier _sym = do
  iHiddenVariableIdentifier <$> pIdentifier

pMutFunctionParameter :: Parser (MoveTerm MutFunctionParameterL)
pMutFunctionParameter = do
  _sym <- pSymbol "mut_function_parameter" SMutFunctionParameterSymbol
  iMutFunctionParameter <$> pMutTok <*> pFunctionParameter

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
      iModifier1 <$> pPublicTok <*> pMaybe (pBetween pLeftParenthesisTok pRightParenthesisTok (pModifierInternal0 _sym))
    pModifierEntry :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModifierL)
    pModifierEntry _sym =
      iModifierEntry <$> pEntryTok
    pModifierNative :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModifierL)
    pModifierNative _sym =
      iModifierNative <$> pNativeTok

pModifierInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModifierInternal0L)
pModifierInternal0 _sym = do
  choice [ Megaparsec.try (pModifierInternal0Package _sym)
         , Megaparsec.try (pModifierInternal0Friend _sym)
         ]
  where
    pModifierInternal0Package :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModifierInternal0L)
    pModifierInternal0Package _sym =
      iModifierInternal0Package <$> pPackageTok
    pModifierInternal0Friend :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ModifierInternal0L)
    pModifierInternal0Friend _sym =
      iModifierInternal0Friend <$> pFriendTok

pRetType :: Parser (MoveTerm RetTypeL)
pRetType = do
  _sym <- pSymbol "ret_type" SRetTypeSymbol
  iRetType <$> pColonTok <*> (pHiddenType _sym)

pBlock :: Parser (MoveTerm BlockL)
pBlock = do
  _sym <- pSymbol "block" SBlockSymbol
  iBlock <$> pLeftCurlyBracketTok <*> pMany pUseDeclaration <*> pMany pBlockItem <*> pMaybe (pHiddenExpression _sym) <*> pRightCurlyBracketTok

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
      iHiddenExpressionCallExpression <$> pCallExpression
    pHiddenExpressionMacroCallExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionMacroCallExpression _sym =
      iHiddenExpressionMacroCallExpression <$> pMacroCallExpression
    pHiddenExpressionLambdaExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionLambdaExpression _sym =
      iHiddenExpressionLambdaExpression <$> pLambdaExpression
    pHiddenExpressionIfExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionIfExpression _sym =
      iHiddenExpressionIfExpression <$> pIfExpression
    pHiddenExpressionWhileExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionWhileExpression _sym =
      iHiddenExpressionWhileExpression <$> pWhileExpression
    pHiddenExpressionReturnExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionReturnExpression _sym =
      iHiddenExpressionReturnExpression <$> pReturnExpression
    pHiddenExpressionAbortExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionAbortExpression _sym =
      iHiddenExpressionAbortExpression <$> pAbortExpression
    pHiddenExpressionAssignExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionAssignExpression _sym =
      iHiddenExpressionAssignExpression <$> pAssignExpression
    pHiddenExpressionUnaryExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionUnaryExpression _sym =
      iHiddenExpressionUnaryExpression <$> (pHiddenUnaryExpression _sym)
    pHiddenExpressionBinaryExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionBinaryExpression _sym =
      iHiddenExpressionBinaryExpression <$> pBinaryExpression
    pHiddenExpressionCastExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionCastExpression _sym =
      iHiddenExpressionCastExpression <$> pCastExpression
    pHiddenExpressionQuantifierExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionQuantifierExpression _sym =
      iHiddenExpressionQuantifierExpression <$> pQuantifierExpression
    pHiddenExpressionMatchExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionMatchExpression _sym =
      iHiddenExpressionMatchExpression <$> pMatchExpression
    pHiddenExpressionVectorExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionVectorExpression _sym =
      iHiddenExpressionVectorExpression <$> pVectorExpression
    pHiddenExpressionLoopExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionLoopExpression _sym =
      iHiddenExpressionLoopExpression <$> pLoopExpression
    pHiddenExpressionIdentifiedExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionL)
    pHiddenExpressionIdentifiedExpression _sym =
      iHiddenExpressionIdentifiedExpression <$> pIdentifiedExpression

pHiddenUnaryExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionL)
pHiddenUnaryExpression _sym = do
  iHiddenUnaryExpression <$> (pHiddenUnaryExpressionInternal0 _sym)

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
      iHiddenUnaryExpressionInternal0UnaryExpression <$> pUnaryExpression
    pHiddenUnaryExpressionInternal0BorrowExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0BorrowExpression _sym =
      iHiddenUnaryExpressionInternal0BorrowExpression <$> pBorrowExpression
    pHiddenUnaryExpressionInternal0DereferenceExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0DereferenceExpression _sym =
      iHiddenUnaryExpressionInternal0DereferenceExpression <$> pDereferenceExpression
    pHiddenUnaryExpressionInternal0MoveOrCopyExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0MoveOrCopyExpression _sym =
      iHiddenUnaryExpressionInternal0MoveOrCopyExpression <$> pMoveOrCopyExpression
    pHiddenUnaryExpressionInternal0ExpressionTerm :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenUnaryExpressionInternal0L)
    pHiddenUnaryExpressionInternal0ExpressionTerm _sym =
      iHiddenUnaryExpressionInternal0ExpressionTerm <$> (pHiddenExpressionTerm _sym)

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
      iHiddenExpressionTermCallExpression <$> pCallExpression
    pHiddenExpressionTermBreakExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermBreakExpression _sym =
      iHiddenExpressionTermBreakExpression <$> pBreakExpression
    pHiddenExpressionTermContinueExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermContinueExpression _sym =
      iHiddenExpressionTermContinueExpression <$> pContinueExpression
    pHiddenExpressionTermNameExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermNameExpression _sym =
      iHiddenExpressionTermNameExpression <$> pNameExpression
    pHiddenExpressionTermMacroCallExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermMacroCallExpression _sym =
      iHiddenExpressionTermMacroCallExpression <$> pMacroCallExpression
    pHiddenExpressionTermPackExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermPackExpression _sym =
      iHiddenExpressionTermPackExpression <$> pPackExpression
    pHiddenExpressionTermLiteralValue :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermLiteralValue _sym =
      iHiddenExpressionTermLiteralValue <$> (pHiddenLiteralValue _sym)
    pHiddenExpressionTermUnitExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermUnitExpression _sym =
      iHiddenExpressionTermUnitExpression <$> pUnitExpression
    pHiddenExpressionTermExpressionList :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermExpressionList _sym =
      iHiddenExpressionTermExpressionList <$> pExpressionList
    pHiddenExpressionTermAnnotationExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermAnnotationExpression _sym =
      iHiddenExpressionTermAnnotationExpression <$> pAnnotationExpression
    pHiddenExpressionTermBlock :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermBlock _sym =
      iHiddenExpressionTermBlock <$> pBlock
    pHiddenExpressionTermSpecBlock :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermSpecBlock _sym =
      iHiddenExpressionTermSpecBlock <$> pSpecBlock
    pHiddenExpressionTermIfExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermIfExpression _sym =
      iHiddenExpressionTermIfExpression <$> pIfExpression
    pHiddenExpressionTermDotExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermDotExpression _sym =
      iHiddenExpressionTermDotExpression <$> pDotExpression
    pHiddenExpressionTermIndexExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermIndexExpression _sym =
      iHiddenExpressionTermIndexExpression <$> pIndexExpression
    pHiddenExpressionTermVectorExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermVectorExpression _sym =
      iHiddenExpressionTermVectorExpression <$> pVectorExpression
    pHiddenExpressionTermMatchExpression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenExpressionTermL)
    pHiddenExpressionTermMatchExpression _sym =
      iHiddenExpressionTermMatchExpression <$> pMatchExpression

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
      iHiddenLiteralValueAddressLiteral <$> pAddressLiteral
    pHiddenLiteralValueBoolLiteral :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueBoolLiteral _sym =
      iHiddenLiteralValueBoolLiteral <$> pBoolLiteral
    pHiddenLiteralValueNumLiteral :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueNumLiteral _sym =
      iHiddenLiteralValueNumLiteral <$> pNumLiteral
    pHiddenLiteralValueHexStringLiteral :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueHexStringLiteral _sym =
      iHiddenLiteralValueHexStringLiteral <$> pHexStringLiteral
    pHiddenLiteralValueByteStringLiteral :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenLiteralValueL)
    pHiddenLiteralValueByteStringLiteral _sym =
      iHiddenLiteralValueByteStringLiteral <$> pByteStringLiteral

pAddressLiteral :: Parser (MoveTerm AddressLiteralL)
pAddressLiteral = do
  _sym <- pSymbol "address_literal" SAddressLiteralSymbol
  iAddressLiteral <$> pContent _sym

pBoolLiteral :: Parser (MoveTerm BoolLiteralL)
pBoolLiteral = do
  _sym <- pSymbol "bool_literal" SBoolLiteralSymbol
  choice [ Megaparsec.try (pBoolLiteralTrue _sym)
         , Megaparsec.try (pBoolLiteralFalse _sym)
         ]
  where
    pBoolLiteralTrue :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BoolLiteralL)
    pBoolLiteralTrue _sym =
      iBoolLiteralTrue <$> pTrueTok
    pBoolLiteralFalse :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BoolLiteralL)
    pBoolLiteralFalse _sym =
      iBoolLiteralFalse <$> pFalseTok

pByteStringLiteral :: Parser (MoveTerm ByteStringLiteralL)
pByteStringLiteral = do
  _sym <- pSymbol "byte_string_literal" SByteStringLiteralSymbol
  iByteStringLiteral <$> pContent _sym

pHexStringLiteral :: Parser (MoveTerm HexStringLiteralL)
pHexStringLiteral = do
  _sym <- pSymbol "hex_string_literal" SHexStringLiteralSymbol
  iHexStringLiteral <$> pContent _sym

pAnnotationExpression :: Parser (MoveTerm AnnotationExpressionL)
pAnnotationExpression = do
  _sym <- pSymbol "annotation_expression" SAnnotationExpressionSymbol
  iAnnotationExpression <$> pBetween pLeftParenthesisTok pColonTok (pHiddenExpression _sym) <*> (pHiddenType _sym) <*> pRightParenthesisTok

pBreakExpression :: Parser (MoveTerm BreakExpressionL)
pBreakExpression = do
  _sym <- pSymbol "break_expression" SBreakExpressionSymbol
  iBreakExpression <$> pBreakTok <*> pMaybe pLabel <*> pMaybe (pHiddenExpression _sym)

pLabel :: Parser (MoveTerm LabelL)
pLabel = do
  _sym <- pSymbol "label" SLabelSymbol
  iLabel <$> pApostropheTok <*> pIdentifier

pCallExpression :: Parser (MoveTerm CallExpressionL)
pCallExpression = do
  _sym <- pSymbol "call_expression" SCallExpressionSymbol
  iCallExpression <$> pPair pNameExpression pArgList

pArgList :: Parser (MoveTerm ArgListL)
pArgList = do
  _sym <- pSymbol "arg_list" SArgListSymbol
  iArgList <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy (pHiddenExpression _sym) pCommaTok)

pNameExpression :: Parser (MoveTerm NameExpressionL)
pNameExpression = do
  _sym <- pSymbol "name_expression" SNameExpressionSymbol
  iNameExpression <$> pMaybe pColonColonTok <*> pModuleAccess

pContinueExpression :: Parser (MoveTerm ContinueExpressionL)
pContinueExpression = do
  _sym <- pSymbol "continue_expression" SContinueExpressionSymbol
  iContinueExpression <$> pContinueTok <*> pMaybe pLabel

pDotExpression :: Parser (MoveTerm DotExpressionL)
pDotExpression = do
  _sym <- pSymbol "dot_expression" SDotExpressionSymbol
  iDotExpression <$> pPair (pPair (pHiddenExpressionTerm _sym) pFullStopTok) (pHiddenExpressionTerm _sym)

pExpressionList :: Parser (MoveTerm ExpressionListL)
pExpressionList = do
  _sym <- pSymbol "expression_list" SExpressionListSymbol
  iExpressionList <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy1 (pHiddenExpression _sym) pCommaTok)

pIfExpression :: Parser (MoveTerm IfExpressionL)
pIfExpression = do
  _sym <- pSymbol "if_expression" SIfExpressionSymbol
  iIfExpression <$> pPair (pPair (pPair pIfTok (pBetween pLeftParenthesisTok pRightParenthesisTok (pHiddenExpression _sym))) (pHiddenExpression _sym)) (pMaybe (pPair pElseTok (pHiddenExpression _sym)))

pIndexExpression :: Parser (MoveTerm IndexExpressionL)
pIndexExpression = do
  _sym <- pSymbol "index_expression" SIndexExpressionSymbol
  iIndexExpression <$> pPair (pHiddenExpressionTerm _sym) (pBetween pLeftSquareBracketTok pRightSquareBracketTok (pSepBy (pHiddenExpression _sym) pCommaTok))

pMacroCallExpression :: Parser (MoveTerm MacroCallExpressionL)
pMacroCallExpression = do
  _sym <- pSymbol "macro_call_expression" SMacroCallExpressionSymbol
  iMacroCallExpression <$> pMacroModuleAccess <*> pMaybe pTypeArguments <*> pArgList

pMacroModuleAccess :: Parser (MoveTerm MacroModuleAccessL)
pMacroModuleAccess = do
  _sym <- pSymbol "macro_module_access" SMacroModuleAccessSymbol
  iMacroModuleAccess <$> pModuleAccess <*> pExclamationMarkTok

pMatchExpression :: Parser (MoveTerm MatchExpressionL)
pMatchExpression = do
  _sym <- pSymbol "match_expression" SMatchExpressionSymbol
  iMatchExpression <$> pMatchTok <*> pBetween pLeftParenthesisTok pRightParenthesisTok (pHiddenExpression _sym) <*> (pHiddenMatchBody _sym)

pHiddenMatchBody :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenMatchBodyL)
pHiddenMatchBody _sym = do
  iHiddenMatchBody <$> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy pMatchArm pCommaTok)

pMatchArm :: Parser (MoveTerm MatchArmL)
pMatchArm = do
  _sym <- pSymbol "match_arm" SMatchArmSymbol
  iMatchArm <$> pBindList <*> pMaybe pMatchCondition <*> pEqualsSignGreaterThanSignTok <*> (pHiddenExpression _sym)

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
      iBindListBind <$> (pHiddenBind _sym)
    pBindListCommaBindList :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindListL)
    pBindListCommaBindList _sym =
      iBindListCommaBindList <$> pCommaBindList
    pBindListOrBindList :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindListL)
    pBindListOrBindList _sym =
      iBindListOrBindList <$> pOrBindList

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
      iHiddenBindBindInternal0 <$> (pHiddenBindInternal0 _sym)
    pHiddenBindBindUnpack :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindL)
    pHiddenBindBindUnpack _sym =
      iHiddenBindBindUnpack <$> pBindUnpack
    pHiddenBindAtBind :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindL)
    pHiddenBindAtBind _sym =
      iHiddenBindAtBind <$> pAtBind
    pHiddenBindLiteralValue :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindL)
    pHiddenBindLiteralValue _sym =
      iHiddenBindLiteralValue <$> (pHiddenLiteralValue _sym)

pHiddenBindInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindInternal0L)
pHiddenBindInternal0 _sym = do
  choice [ Megaparsec.try (pHiddenBindInternal0MutBindVar _sym)
         , Megaparsec.try (pHiddenBindInternal0VariableIdentifier _sym)
         ]
  where
    pHiddenBindInternal0MutBindVar :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindInternal0L)
    pHiddenBindInternal0MutBindVar _sym =
      iHiddenBindInternal0MutBindVar <$> pMutBindVar
    pHiddenBindInternal0VariableIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenBindInternal0L)
    pHiddenBindInternal0VariableIdentifier _sym =
      iHiddenBindInternal0VariableIdentifier <$> (pHiddenVariableIdentifier _sym)

pMutBindVar :: Parser (MoveTerm MutBindVarL)
pMutBindVar = do
  _sym <- pSymbol "mut_bind_var" SMutBindVarSymbol
  iMutBindVar <$> pMutTok <*> (pHiddenVariableIdentifier _sym)

pAtBind :: Parser (MoveTerm AtBindL)
pAtBind = do
  _sym <- pSymbol "at_bind" SAtBindSymbol
  iAtBind <$> (pHiddenVariableIdentifier _sym) <*> pCommercialAtTok <*> pBindList

pBindUnpack :: Parser (MoveTerm BindUnpackL)
pBindUnpack = do
  _sym <- pSymbol "bind_unpack" SBindUnpackSymbol
  iBindUnpack <$> pNameExpression <*> pMaybe pBindFields

pBindFields :: Parser (MoveTerm BindFieldsL)
pBindFields = do
  _sym <- pSymbol "bind_fields" SBindFieldsSymbol
  choice [ Megaparsec.try (pBindFieldsBindPositionalFields _sym)
         , Megaparsec.try (pBindFieldsBindNamedFields _sym)
         ]
  where
    pBindFieldsBindPositionalFields :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindFieldsL)
    pBindFieldsBindPositionalFields _sym =
      iBindFieldsBindPositionalFields <$> pBindPositionalFields
    pBindFieldsBindNamedFields :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindFieldsL)
    pBindFieldsBindNamedFields _sym =
      iBindFieldsBindNamedFields <$> pBindNamedFields

pBindNamedFields :: Parser (MoveTerm BindNamedFieldsL)
pBindNamedFields = do
  _sym <- pSymbol "bind_named_fields" SBindNamedFieldsSymbol
  iBindNamedFields <$> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy (pBindNamedFieldsInternal0 _sym) pCommaTok)

pBindNamedFieldsInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindNamedFieldsInternal0L)
pBindNamedFieldsInternal0 _sym = do
  choice [ Megaparsec.try (pBindNamedFieldsInternal0BindField _sym)
         , Megaparsec.try (pBindNamedFieldsInternal0MutBindField _sym)
         ]
  where
    pBindNamedFieldsInternal0BindField :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindNamedFieldsInternal0L)
    pBindNamedFieldsInternal0BindField _sym =
      iBindNamedFieldsInternal0BindField <$> pBindField
    pBindNamedFieldsInternal0MutBindField :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindNamedFieldsInternal0L)
    pBindNamedFieldsInternal0MutBindField _sym =
      iBindNamedFieldsInternal0MutBindField <$> pMutBindField

pBindField :: Parser (MoveTerm BindFieldL)
pBindField = do
  _sym <- pSymbol "bind_field" SBindFieldSymbol
  choice [ Megaparsec.try (pBindField1 _sym)
         , Megaparsec.try (pBindFieldSpreadOperator _sym)
         ]
  where
    pBindField1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindFieldL)
    pBindField1 _sym =
      iBindField1 <$> pBindList <*> pMaybe (pPair pColonTok pBindList)
    pBindFieldSpreadOperator :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BindFieldL)
    pBindFieldSpreadOperator _sym =
      iBindFieldSpreadOperator <$> (pHiddenSpreadOperator _sym)

pHiddenSpreadOperator :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpreadOperatorL)
pHiddenSpreadOperator _sym = do
  iHiddenSpreadOperator <$> pFullStopFullStopTok

pMutBindField :: Parser (MoveTerm MutBindFieldL)
pMutBindField = do
  _sym <- pSymbol "mut_bind_field" SMutBindFieldSymbol
  iMutBindField <$> pMutTok <*> pBindField

pBindPositionalFields :: Parser (MoveTerm BindPositionalFieldsL)
pBindPositionalFields = do
  _sym <- pSymbol "bind_positional_fields" SBindPositionalFieldsSymbol
  iBindPositionalFields <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy (pBindNamedFieldsInternal0 _sym) pCommaTok)

pCommaBindList :: Parser (MoveTerm CommaBindListL)
pCommaBindList = do
  _sym <- pSymbol "comma_bind_list" SCommaBindListSymbol
  iCommaBindList <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy (pHiddenBind _sym) pCommaTok)

pOrBindList :: Parser (MoveTerm OrBindListL)
pOrBindList = do
  _sym <- pSymbol "or_bind_list" SOrBindListSymbol
  iOrBindList <$> pMaybe pLeftParenthesisTok <*> pSepBy1 (pPair (pPair (pMaybe pLeftParenthesisTok) (pHiddenBind _sym)) (pMaybe pRightParenthesisTok)) pVerticalLineTok <*> pMaybe pRightParenthesisTok

pMatchCondition :: Parser (MoveTerm MatchConditionL)
pMatchCondition = do
  _sym <- pSymbol "match_condition" SMatchConditionSymbol
  iMatchCondition <$> pIfTok <*> pBetween pLeftParenthesisTok pRightParenthesisTok (pHiddenExpression _sym)

pPackExpression :: Parser (MoveTerm PackExpressionL)
pPackExpression = do
  _sym <- pSymbol "pack_expression" SPackExpressionSymbol
  iPackExpression <$> pNameExpression <*> pFieldInitializeList

pFieldInitializeList :: Parser (MoveTerm FieldInitializeListL)
pFieldInitializeList = do
  _sym <- pSymbol "field_initialize_list" SFieldInitializeListSymbol
  iFieldInitializeList <$> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy pExpField pCommaTok)

pExpField :: Parser (MoveTerm ExpFieldL)
pExpField = do
  _sym <- pSymbol "exp_field" SExpFieldSymbol
  iExpField <$> (pHiddenFieldIdentifier _sym) <*> pMaybe (pPair pColonTok (pHiddenExpression _sym))

pSpecBlock :: Parser (MoveTerm SpecBlockL)
pSpecBlock = do
  _sym <- pSymbol "spec_block" SSpecBlockSymbol
  iSpecBlock <$> pSpecTok <*> (pSpecBlockInternal0 _sym)

pSpecBlockInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecBlockInternal0L)
pSpecBlockInternal0 _sym = do
  choice [ Megaparsec.try (pSpecBlockInternal01 _sym)
         , Megaparsec.try (pSpecBlockInternal0SpecFunction _sym)
         ]
  where
    pSpecBlockInternal01 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecBlockInternal0L)
    pSpecBlockInternal01 _sym =
      iSpecBlockInternal01 <$> pMaybe (pHiddenSpecBlockTarget _sym) <*> pSpecBody
    pSpecBlockInternal0SpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecBlockInternal0L)
    pSpecBlockInternal0SpecFunction _sym =
      iSpecBlockInternal0SpecFunction <$> (pHiddenSpecFunction _sym)

pHiddenSpecBlockTarget :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockTargetL)
pHiddenSpecBlockTarget _sym = do
  choice [ Megaparsec.try (pHiddenSpecBlockTargetIdentifier _sym)
         , Megaparsec.try (pHiddenSpecBlockTargetModule _sym)
         , Megaparsec.try (pHiddenSpecBlockTargetSpecBlockTargetSchema _sym)
         ]
  where
    pHiddenSpecBlockTargetIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockTargetL)
    pHiddenSpecBlockTargetIdentifier _sym =
      iHiddenSpecBlockTargetIdentifier <$> pIdentifier
    pHiddenSpecBlockTargetModule :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockTargetL)
    pHiddenSpecBlockTargetModule _sym =
      iHiddenSpecBlockTargetModule <$> pModuleTok
    pHiddenSpecBlockTargetSpecBlockTargetSchema :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockTargetL)
    pHiddenSpecBlockTargetSpecBlockTargetSchema _sym =
      iHiddenSpecBlockTargetSpecBlockTargetSchema <$> pSpecBlockTargetSchema

pSpecBlockTargetSchema :: Parser (MoveTerm SpecBlockTargetSchemaL)
pSpecBlockTargetSchema = do
  _sym <- pSymbol "spec_block_target_schema" SSpecBlockTargetSchemaSymbol
  iSpecBlockTargetSchema <$> pSchemaTok <*> (pHiddenStructIdentifier _sym) <*> pMaybe pTypeParameters

pHiddenStructIdentifier :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenStructIdentifierL)
pHiddenStructIdentifier _sym = do
  iHiddenStructIdentifier <$> pIdentifier

pHiddenSpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecFunctionL)
pHiddenSpecFunction _sym = do
  choice [ Megaparsec.try (pHiddenSpecFunctionNativeSpecFunction _sym)
         , Megaparsec.try (pHiddenSpecFunctionUsualSpecFunction _sym)
         , Megaparsec.try (pHiddenSpecFunctionUninterpretedSpecFunction _sym)
         ]
  where
    pHiddenSpecFunctionNativeSpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecFunctionL)
    pHiddenSpecFunctionNativeSpecFunction _sym =
      iHiddenSpecFunctionNativeSpecFunction <$> pNativeSpecFunction
    pHiddenSpecFunctionUsualSpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecFunctionL)
    pHiddenSpecFunctionUsualSpecFunction _sym =
      iHiddenSpecFunctionUsualSpecFunction <$> pUsualSpecFunction
    pHiddenSpecFunctionUninterpretedSpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecFunctionL)
    pHiddenSpecFunctionUninterpretedSpecFunction _sym =
      iHiddenSpecFunctionUninterpretedSpecFunction <$> pUninterpretedSpecFunction

pNativeSpecFunction :: Parser (MoveTerm NativeSpecFunctionL)
pNativeSpecFunction = do
  _sym <- pSymbol "native_spec_function" SNativeSpecFunctionSymbol
  iNativeSpecFunction <$> pNativeTok <*> pBetween pFunTok pSemicolonTok (pHiddenSpecFunctionSignature _sym)

pHiddenSpecFunctionSignature :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecFunctionSignatureL)
pHiddenSpecFunctionSignature _sym = do
  iHiddenSpecFunctionSignature <$> (pHiddenFunctionIdentifier _sym) <*> pMaybe pTypeParameters <*> pFunctionParameters <*> pRetType

pUninterpretedSpecFunction :: Parser (MoveTerm UninterpretedSpecFunctionL)
pUninterpretedSpecFunction = do
  _sym <- pSymbol "uninterpreted_spec_function" SUninterpretedSpecFunctionSymbol
  iUninterpretedSpecFunction <$> pBetween pFunTok pSemicolonTok (pHiddenSpecFunctionSignature _sym)

pUsualSpecFunction :: Parser (MoveTerm UsualSpecFunctionL)
pUsualSpecFunction = do
  _sym <- pSymbol "usual_spec_function" SUsualSpecFunctionSymbol
  iUsualSpecFunction <$> pFunTok <*> (pHiddenSpecFunctionSignature _sym) <*> pBlock

pSpecBody :: Parser (MoveTerm SpecBodyL)
pSpecBody = do
  _sym <- pSymbol "spec_body" SSpecBodySymbol
  iSpecBody <$> pLeftCurlyBracketTok <*> pMany pUseDeclaration <*> pMany (pHiddenSpecBlockMemeber _sym) <*> pRightCurlyBracketTok

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
      iHiddenSpecBlockMemeberSpecInvariant <$> pSpecInvariant
    pHiddenSpecBlockMemeberSpecFunction :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecFunction _sym =
      iHiddenSpecBlockMemeberSpecFunction <$> (pHiddenSpecFunction _sym)
    pHiddenSpecBlockMemeberSpecCondition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecCondition _sym =
      iHiddenSpecBlockMemeberSpecCondition <$> pSpecCondition
    pHiddenSpecBlockMemeberSpecInclude :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecInclude _sym =
      iHiddenSpecBlockMemeberSpecInclude <$> pSpecInclude
    pHiddenSpecBlockMemeberSpecApply :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecApply _sym =
      iHiddenSpecBlockMemeberSpecApply <$> pSpecApply
    pHiddenSpecBlockMemeberSpecPragma :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecPragma _sym =
      iHiddenSpecBlockMemeberSpecPragma <$> pSpecPragma
    pHiddenSpecBlockMemeberSpecVariable :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecVariable _sym =
      iHiddenSpecBlockMemeberSpecVariable <$> pSpecVariable
    pHiddenSpecBlockMemeberSpecLet :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecBlockMemeberL)
    pHiddenSpecBlockMemeberSpecLet _sym =
      iHiddenSpecBlockMemeberSpecLet <$> pSpecLet

pSpecApply :: Parser (MoveTerm SpecApplyL)
pSpecApply = do
  _sym <- pSymbol "spec_apply" SSpecApplySymbol
  iSpecApply <$> pBetween pApplyTok pToTok (pHiddenExpression _sym) <*> pSepBy1 pSpecApplyPattern pCommaTok <*> pMaybe (pPair pExceptTok (pSepBy1 pSpecApplyPattern pCommaTok)) <*> pSemicolonTok

pSpecApplyPattern :: Parser (MoveTerm SpecApplyPatternL)
pSpecApplyPattern = do
  _sym <- pSymbol "spec_apply_pattern" SSpecApplyPatternSymbol
  iSpecApplyPattern <$> pMaybe (pSpecApplyPatternInternal0 _sym) <*> pSpecApplyNamePattern <*> pMaybe pTypeParameters

pSpecApplyNamePattern :: Parser (MoveTerm SpecApplyNamePatternL)
pSpecApplyNamePattern = do
  _sym <- pSymbol "spec_apply_name_pattern" SSpecApplyNamePatternSymbol
  iSpecApplyNamePattern <$> pContent _sym

pSpecApplyPatternInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecApplyPatternInternal0L)
pSpecApplyPatternInternal0 _sym = do
  choice [ Megaparsec.try (pSpecApplyPatternInternal0Public _sym)
         , Megaparsec.try (pSpecApplyPatternInternal0Internal _sym)
         ]
  where
    pSpecApplyPatternInternal0Public :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecApplyPatternInternal0L)
    pSpecApplyPatternInternal0Public _sym =
      iSpecApplyPatternInternal0Public <$> pPublicTok
    pSpecApplyPatternInternal0Internal :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecApplyPatternInternal0L)
    pSpecApplyPatternInternal0Internal _sym =
      iSpecApplyPatternInternal0Internal <$> pInternalTok

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
      iSpecConditionSpecCondition <$> (pHiddenSpecCondition _sym)
    pSpecConditionSpecAbortIf :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecConditionL)
    pSpecConditionSpecAbortIf _sym =
      iSpecConditionSpecAbortIf <$> (pHiddenSpecAbortIf _sym)
    pSpecConditionSpecAbortWithOrModifies :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecConditionL)
    pSpecConditionSpecAbortWithOrModifies _sym =
      iSpecConditionSpecAbortWithOrModifies <$> (pHiddenSpecAbortWithOrModifies _sym)

pHiddenSpecAbortIf :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecAbortIfL)
pHiddenSpecAbortIf _sym = do
  iHiddenSpecAbortIf <$> pAbortsIfTok <*> pMaybe pConditionProperties <*> (pHiddenExpression _sym) <*> pMaybe (pPair pWithTok (pHiddenExpression _sym)) <*> pSemicolonTok

pConditionProperties :: Parser (MoveTerm ConditionPropertiesL)
pConditionProperties = do
  _sym <- pSymbol "condition_properties" SConditionPropertiesSymbol
  iConditionProperties <$> pBetween pLeftSquareBracketTok pRightSquareBracketTok (pSepBy pSpecProperty pCommaTok)

pSpecProperty :: Parser (MoveTerm SpecPropertyL)
pSpecProperty = do
  _sym <- pSymbol "spec_property" SSpecPropertySymbol
  iSpecProperty <$> pIdentifier <*> pMaybe (pPair pEqualsSignTok (pHiddenLiteralValue _sym))

pHiddenSpecAbortWithOrModifies :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecAbortWithOrModifiesL)
pHiddenSpecAbortWithOrModifies _sym = do
  iHiddenSpecAbortWithOrModifies <$> (pHiddenSpecAbortWithOrModifiesInternal0 _sym) <*> pMaybe pConditionProperties <*> pSepBy1 (pHiddenExpression _sym) pCommaTok <*> pSemicolonTok

pHiddenSpecAbortWithOrModifiesInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecAbortWithOrModifiesInternal0L)
pHiddenSpecAbortWithOrModifiesInternal0 _sym = do
  choice [ Megaparsec.try (pHiddenSpecAbortWithOrModifiesInternal0AbortsWith _sym)
         , Megaparsec.try (pHiddenSpecAbortWithOrModifiesInternal0Modifies _sym)
         ]
  where
    pHiddenSpecAbortWithOrModifiesInternal0AbortsWith :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecAbortWithOrModifiesInternal0L)
    pHiddenSpecAbortWithOrModifiesInternal0AbortsWith _sym =
      iHiddenSpecAbortWithOrModifiesInternal0AbortsWith <$> pAbortsWithTok
    pHiddenSpecAbortWithOrModifiesInternal0Modifies :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecAbortWithOrModifiesInternal0L)
    pHiddenSpecAbortWithOrModifiesInternal0Modifies _sym =
      iHiddenSpecAbortWithOrModifiesInternal0Modifies <$> pModifiesTok

pHiddenSpecCondition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionL)
pHiddenSpecCondition _sym = do
  iHiddenSpecCondition <$> (pHiddenSpecConditionInternal0 _sym) <*> pMaybe pConditionProperties <*> (pHiddenExpression _sym) <*> pSemicolonTok

pHiddenSpecConditionInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionInternal0L)
pHiddenSpecConditionInternal0 _sym = do
  choice [ Megaparsec.try (pHiddenSpecConditionInternal0Kind _sym)
         , Megaparsec.try (pHiddenSpecConditionInternal02 _sym)
         ]
  where
    pHiddenSpecConditionInternal0Kind :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionInternal0L)
    pHiddenSpecConditionInternal0Kind _sym =
      iHiddenSpecConditionInternal0Kind <$> (pHiddenSpecConditionKind _sym)
    pHiddenSpecConditionInternal02 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionInternal0L)
    pHiddenSpecConditionInternal02 _sym =
      iHiddenSpecConditionInternal02 <$> pRequiresTok <*> pMaybe pModuleTok

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
      iHiddenSpecConditionKindAssert <$> pAssertTok
    pHiddenSpecConditionKindAssume :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindAssume _sym =
      iHiddenSpecConditionKindAssume <$> pAssumeTok
    pHiddenSpecConditionKindDecreases :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindDecreases _sym =
      iHiddenSpecConditionKindDecreases <$> pDecreasesTok
    pHiddenSpecConditionKindEnsures :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindEnsures _sym =
      iHiddenSpecConditionKindEnsures <$> pEnsuresTok
    pHiddenSpecConditionKindSucceedsIf :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenSpecConditionKindL)
    pHiddenSpecConditionKindSucceedsIf _sym =
      iHiddenSpecConditionKindSucceedsIf <$> pSucceedsIfTok

pSpecInclude :: Parser (MoveTerm SpecIncludeL)
pSpecInclude = do
  _sym <- pSymbol "spec_include" SSpecIncludeSymbol
  iSpecInclude <$> pBetween pIncludeTok pSemicolonTok (pHiddenExpression _sym)

pSpecInvariant :: Parser (MoveTerm SpecInvariantL)
pSpecInvariant = do
  _sym <- pSymbol "spec_invariant" SSpecInvariantSymbol
  iSpecInvariant <$> pInvariantTok <*> pMaybe (pSpecInvariantInternal0 _sym) <*> pMaybe pConditionProperties <*> (pHiddenExpression _sym) <*> pSemicolonTok

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
      iSpecInvariantInternal0Update <$> pUpdateTok
    pSpecInvariantInternal0Pack :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Pack _sym =
      iSpecInvariantInternal0Pack <$> pPackTok
    pSpecInvariantInternal0Unpack :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Unpack _sym =
      iSpecInvariantInternal0Unpack <$> pUnpackTok
    pSpecInvariantInternal0Module :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Module _sym =
      iSpecInvariantInternal0Module <$> pModuleTok

pSpecLet :: Parser (MoveTerm SpecLetL)
pSpecLet = do
  _sym <- pSymbol "spec_let" SSpecLetSymbol
  iSpecLet <$> pLetTok <*> pMaybe pPostTok <*> pIdentifier <*> pBetween pEqualsSignTok pSemicolonTok (pHiddenExpression _sym)

pSpecPragma :: Parser (MoveTerm SpecPragmaL)
pSpecPragma = do
  _sym <- pSymbol "spec_pragma" SSpecPragmaSymbol
  iSpecPragma <$> pBetween pPragmaTok pSemicolonTok (pSepBy pSpecProperty pCommaTok)

pSpecVariable :: Parser (MoveTerm SpecVariableL)
pSpecVariable = do
  _sym <- pSymbol "spec_variable" SSpecVariableSymbol
  iSpecVariable <$> pMaybe (pSpecVariableInternal0 _sym) <*> pIdentifier <*> pMaybe pTypeParameters <*> pBetween pColonTok pSemicolonTok (pHiddenType _sym)

pSpecVariableInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecVariableInternal0L)
pSpecVariableInternal0 _sym = do
  choice [ Megaparsec.try (pSpecVariableInternal0Global _sym)
         , Megaparsec.try (pSpecVariableInternal0Local _sym)
         ]
  where
    pSpecVariableInternal0Global :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecVariableInternal0L)
    pSpecVariableInternal0Global _sym =
      iSpecVariableInternal0Global <$> pGlobalTok
    pSpecVariableInternal0Local :: Cubix.TreeSitter.Token a -> Parser (MoveTerm SpecVariableInternal0L)
    pSpecVariableInternal0Local _sym =
      iSpecVariableInternal0Local <$> pLocalTok

pUseDeclaration :: Parser (MoveTerm UseDeclarationL)
pUseDeclaration = do
  _sym <- pSymbol "use_declaration" SUseDeclarationSymbol
  iUseDeclaration <$> pMaybe pPublicTok <*> pBetween pUseTok pSemicolonTok (pUseDeclarationInternal0 _sym)

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
      iUseDeclarationInternal0UseFun <$> pUseFun
    pUseDeclarationInternal0UseModule :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseModule _sym =
      iUseDeclarationInternal0UseModule <$> pUseModule
    pUseDeclarationInternal0UseModuleMember :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseModuleMember _sym =
      iUseDeclarationInternal0UseModuleMember <$> pUseModuleMember
    pUseDeclarationInternal0UseModuleMembers :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseModuleMembers _sym =
      iUseDeclarationInternal0UseModuleMembers <$> pUseModuleMembers

pUseFun :: Parser (MoveTerm UseFunL)
pUseFun = do
  _sym <- pSymbol "use_fun" SUseFunSymbol
  iUseFun <$> pBetween pFunTok pAsTok pModuleAccess <*> pPair (pPair pModuleAccess pFullStopTok) (pHiddenFunctionIdentifier _sym)

pUseModule :: Parser (MoveTerm UseModuleL)
pUseModule = do
  _sym <- pSymbol "use_module" SUseModuleSymbol
  iUseModule <$> pModuleIdentity <*> pMaybe (pPair pAsTok (pHiddenModuleIdentifier _sym))

pUseModuleMember :: Parser (MoveTerm UseModuleMemberL)
pUseModuleMember = do
  _sym <- pSymbol "use_module_member" SUseModuleMemberSymbol
  iUseModuleMember <$> pModuleIdentity <*> pColonColonTok <*> pUseMember

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
      iUseMember1 <$> pIdentifier <*> pColonColonTok <*> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy1 pUseMember pCommaTok)
    pUseMember2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseMemberL)
    pUseMember2 _sym =
      iUseMember2 <$> pIdentifier <*> pColonColonTok <*> pIdentifier <*> pMaybe (pPair pAsTok pIdentifier)
    pUseMember3 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseMemberL)
    pUseMember3 _sym =
      iUseMember3 <$> pIdentifier <*> pMaybe (pPair pAsTok pIdentifier)

pUseModuleMembers :: Parser (MoveTerm UseModuleMembersL)
pUseModuleMembers = do
  _sym <- pSymbol "use_module_members" SUseModuleMembersSymbol
  choice [ Megaparsec.try (pUseModuleMembers1 _sym)
         , Megaparsec.try (pUseModuleMembers2 _sym)
         ]
  where
    pUseModuleMembers1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseModuleMembersL)
    pUseModuleMembers1 _sym =
      iUseModuleMembers1 <$> (pModuleIdentityInternal0 _sym) <*> pColonColonTok <*> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy1 pUseMember pCommaTok)
    pUseModuleMembers2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm UseModuleMembersL)
    pUseModuleMembers2 _sym =
      iUseModuleMembers2 <$> pModuleIdentity <*> pColonColonTok <*> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy1 pUseMember pCommaTok)

pUnitExpression :: Parser (MoveTerm UnitExpressionL)
pUnitExpression = do
  _sym <- pSymbol "unit_expression" SUnitExpressionSymbol
  iUnitExpression <$> pLeftParenthesisTok <*> pRightParenthesisTok

pVectorExpression :: Parser (MoveTerm VectorExpressionL)
pVectorExpression = do
  _sym <- pSymbol "vector_expression" SVectorExpressionSymbol
  iVectorExpression <$> (pVectorExpressionInternal0 _sym) <*> pSepBy (pHiddenExpression _sym) pCommaTok <*> pRightSquareBracketTok

pVectorExpressionInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm VectorExpressionInternal0L)
pVectorExpressionInternal0 _sym = do
  choice [ Megaparsec.try (pVectorExpressionInternal0VectorLeftSquareBracket _sym)
         , Megaparsec.try (pVectorExpressionInternal02 _sym)
         ]
  where
    pVectorExpressionInternal0VectorLeftSquareBracket :: Cubix.TreeSitter.Token a -> Parser (MoveTerm VectorExpressionInternal0L)
    pVectorExpressionInternal0VectorLeftSquareBracket _sym =
      iVectorExpressionInternal0VectorLeftSquareBracket <$> pVectorLeftSquareBracketTok
    pVectorExpressionInternal02 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm VectorExpressionInternal0L)
    pVectorExpressionInternal02 _sym =
      iVectorExpressionInternal02 <$> pBetween pVectorLessThanSignTok pGreaterThanSignTok (pSepBy1 (pHiddenType _sym) pCommaTok) <*> pLeftSquareBracketTok

pBorrowExpression :: Parser (MoveTerm BorrowExpressionL)
pBorrowExpression = do
  _sym <- pSymbol "borrow_expression" SBorrowExpressionSymbol
  iBorrowExpression <$> pPair (pHiddenReference _sym) (pHiddenExpression _sym)

pDereferenceExpression :: Parser (MoveTerm DereferenceExpressionL)
pDereferenceExpression = do
  _sym <- pSymbol "dereference_expression" SDereferenceExpressionSymbol
  iDereferenceExpression <$> pPair pAsteriskTok (pHiddenExpression _sym)

pMoveOrCopyExpression :: Parser (MoveTerm MoveOrCopyExpressionL)
pMoveOrCopyExpression = do
  _sym <- pSymbol "move_or_copy_expression" SMoveOrCopyExpressionSymbol
  iMoveOrCopyExpression <$> pPair (pMoveOrCopyExpressionInternal0 _sym) (pHiddenExpression _sym)

pMoveOrCopyExpressionInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm MoveOrCopyExpressionInternal0L)
pMoveOrCopyExpressionInternal0 _sym = do
  choice [ Megaparsec.try (pMoveOrCopyExpressionInternal0Move _sym)
         , Megaparsec.try (pMoveOrCopyExpressionInternal0Copy _sym)
         ]
  where
    pMoveOrCopyExpressionInternal0Move :: Cubix.TreeSitter.Token a -> Parser (MoveTerm MoveOrCopyExpressionInternal0L)
    pMoveOrCopyExpressionInternal0Move _sym =
      iMoveOrCopyExpressionInternal0Move <$> pMoveTok
    pMoveOrCopyExpressionInternal0Copy :: Cubix.TreeSitter.Token a -> Parser (MoveTerm MoveOrCopyExpressionInternal0L)
    pMoveOrCopyExpressionInternal0Copy _sym =
      iMoveOrCopyExpressionInternal0Copy <$> pCopyTok

pUnaryExpression :: Parser (MoveTerm UnaryExpressionL)
pUnaryExpression = do
  _sym <- pSymbol "unary_expression" SUnaryExpressionSymbol
  iUnaryExpression <$> pUnaryOp <*> (pHiddenExpression _sym)

pUnaryOp :: Parser (MoveTerm UnaryOpL)
pUnaryOp = do
  _sym <- pSymbol "unary_op" SUnaryOpSymbol
  iUnaryOp <$> pExclamationMarkTok

pAbortExpression :: Parser (MoveTerm AbortExpressionL)
pAbortExpression = do
  _sym <- pSymbol "abort_expression" SAbortExpressionSymbol
  iAbortExpression <$> pAbortTok <*> pMaybe (pHiddenExpression _sym)

pAssignExpression :: Parser (MoveTerm AssignExpressionL)
pAssignExpression = do
  _sym <- pSymbol "assign_expression" SAssignExpressionSymbol
  iAssignExpression <$> pPair (pPair (pHiddenUnaryExpression _sym) pEqualsSignTok) (pHiddenExpression _sym)

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
      iBinaryExpression1 <$> (pHiddenExpression _sym) <*> pEqualsSignEqualsSignGreaterThanSignTok <*> (pHiddenExpression _sym)
    pBinaryExpression2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression2 _sym =
      iBinaryExpression2 <$> (pHiddenExpression _sym) <*> pVerticalLineVerticalLineTok <*> (pHiddenExpression _sym)
    pBinaryExpression3 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression3 _sym =
      iBinaryExpression3 <$> (pHiddenExpression _sym) <*> pAmpersandAmpersandTok <*> (pHiddenExpression _sym)
    pBinaryExpression4 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression4 _sym =
      iBinaryExpression4 <$> (pHiddenExpression _sym) <*> pEqualsSignEqualsSignTok <*> (pHiddenExpression _sym)
    pBinaryExpression5 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression5 _sym =
      iBinaryExpression5 <$> (pHiddenExpression _sym) <*> pExclamationMarkEqualsSignTok <*> (pHiddenExpression _sym)
    pBinaryExpression6 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression6 _sym =
      iBinaryExpression6 <$> (pHiddenExpression _sym) <*> pLessThanSignTok <*> (pHiddenExpression _sym)
    pBinaryExpression7 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression7 _sym =
      iBinaryExpression7 <$> (pHiddenExpression _sym) <*> pGreaterThanSignTok <*> (pHiddenExpression _sym)
    pBinaryExpression8 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression8 _sym =
      iBinaryExpression8 <$> (pHiddenExpression _sym) <*> pLessThanSignEqualsSignTok <*> (pHiddenExpression _sym)
    pBinaryExpression9 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression9 _sym =
      iBinaryExpression9 <$> (pHiddenExpression _sym) <*> pGreaterThanSignEqualsSignTok <*> (pHiddenExpression _sym)
    pBinaryExpression10 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression10 _sym =
      iBinaryExpression10 <$> (pHiddenExpression _sym) <*> pFullStopFullStopTok <*> (pHiddenExpression _sym)
    pBinaryExpression11 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression11 _sym =
      iBinaryExpression11 <$> (pHiddenExpression _sym) <*> pVerticalLineTok <*> (pHiddenExpression _sym)
    pBinaryExpression12 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression12 _sym =
      iBinaryExpression12 <$> (pHiddenExpression _sym) <*> pCircumflexAccentTok <*> (pHiddenExpression _sym)
    pBinaryExpression13 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression13 _sym =
      iBinaryExpression13 <$> (pHiddenExpression _sym) <*> pAmpersandTok <*> (pHiddenExpression _sym)
    pBinaryExpression14 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression14 _sym =
      iBinaryExpression14 <$> (pHiddenExpression _sym) <*> pLessThanSignLessThanSignTok <*> (pHiddenExpression _sym)
    pBinaryExpression15 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression15 _sym =
      iBinaryExpression15 <$> (pHiddenExpression _sym) <*> pGreaterThanSignGreaterThanSignTok <*> (pHiddenExpression _sym)
    pBinaryExpression16 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression16 _sym =
      iBinaryExpression16 <$> (pHiddenExpression _sym) <*> pPlusSignTok <*> (pHiddenExpression _sym)
    pBinaryExpression17 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression17 _sym =
      iBinaryExpression17 <$> (pHiddenExpression _sym) <*> pHyphenMinusTok <*> (pHiddenExpression _sym)
    pBinaryExpression18 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression18 _sym =
      iBinaryExpression18 <$> (pHiddenExpression _sym) <*> pAsteriskTok <*> (pHiddenExpression _sym)
    pBinaryExpression19 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression19 _sym =
      iBinaryExpression19 <$> (pHiddenExpression _sym) <*> pSolidusTok <*> (pHiddenExpression _sym)
    pBinaryExpression20 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression20 _sym =
      iBinaryExpression20 <$> (pHiddenExpression _sym) <*> pPercentSignTok <*> (pHiddenExpression _sym)

pCastExpression :: Parser (MoveTerm CastExpressionL)
pCastExpression = do
  _sym <- pSymbol "cast_expression" SCastExpressionSymbol
  iCastExpression <$> pPair (pPair (pHiddenExpression _sym) pAsTok) (pHiddenType _sym)

pIdentifiedExpression :: Parser (MoveTerm IdentifiedExpressionL)
pIdentifiedExpression = do
  _sym <- pSymbol "identified_expression" SIdentifiedExpressionSymbol
  iIdentifiedExpression <$> pBlockIdentifier <*> (pHiddenExpression _sym)

pBlockIdentifier :: Parser (MoveTerm BlockIdentifierL)
pBlockIdentifier = do
  _sym <- pSymbol "block_identifier" SBlockIdentifierSymbol
  iBlockIdentifier <$> pLabel <*> pColonTok

pLambdaExpression :: Parser (MoveTerm LambdaExpressionL)
pLambdaExpression = do
  _sym <- pSymbol "lambda_expression" SLambdaExpressionSymbol
  iLambdaExpression <$> pLambdaBindings <*> pMaybe (pPair pHyphenMinusGreaterThanSignTok (pHiddenType _sym)) <*> (pHiddenExpression _sym)

pLambdaBindings :: Parser (MoveTerm LambdaBindingsL)
pLambdaBindings = do
  _sym <- pSymbol "lambda_bindings" SLambdaBindingsSymbol
  iLambdaBindings <$> pBetween pVerticalLineTok pVerticalLineTok (pSepBy pLambdaBinding pCommaTok)

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
      iLambdaBindingCommaBindList <$> pCommaBindList
    pLambdaBindingBind :: Cubix.TreeSitter.Token a -> Parser (MoveTerm LambdaBindingL)
    pLambdaBindingBind _sym =
      iLambdaBindingBind <$> (pHiddenBind _sym)
    pLambdaBinding3 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm LambdaBindingL)
    pLambdaBinding3 _sym =
      iLambdaBinding3 <$> (pHiddenBind _sym) <*> pMaybe (pPair pColonTok (pHiddenType _sym))

pLoopExpression :: Parser (MoveTerm LoopExpressionL)
pLoopExpression = do
  _sym <- pSymbol "loop_expression" SLoopExpressionSymbol
  iLoopExpression <$> pLoopTok <*> (pHiddenExpression _sym)

pQuantifierExpression :: Parser (MoveTerm QuantifierExpressionL)
pQuantifierExpression = do
  _sym <- pSymbol "quantifier_expression" SQuantifierExpressionSymbol
  iQuantifierExpression <$> pPair (pPair (pPair (pPair (pHiddenReservedIdentifier _sym) pQuantifierBindings) (pMaybe (pPair pWhereTok (pHiddenExpression _sym)))) pColonTok) (pHiddenExpression _sym)

pQuantifierBindings :: Parser (MoveTerm QuantifierBindingsL)
pQuantifierBindings = do
  _sym <- pSymbol "quantifier_bindings" SQuantifierBindingsSymbol
  iQuantifierBindings <$> pSepBy1 pQuantifierBinding pCommaTok

pQuantifierBinding :: Parser (MoveTerm QuantifierBindingL)
pQuantifierBinding = do
  _sym <- pSymbol "quantifier_binding" SQuantifierBindingSymbol
  choice [ Megaparsec.try (pQuantifierBinding1 _sym)
         , Megaparsec.try (pQuantifierBinding2 _sym)
         ]
  where
    pQuantifierBinding1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding1 _sym =
      iQuantifierBinding1 <$> pIdentifier <*> pColonTok <*> (pHiddenType _sym)
    pQuantifierBinding2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding2 _sym =
      iQuantifierBinding2 <$> pIdentifier <*> pInTok <*> (pHiddenExpression _sym)

pReturnExpression :: Parser (MoveTerm ReturnExpressionL)
pReturnExpression = do
  _sym <- pSymbol "return_expression" SReturnExpressionSymbol
  choice [ Megaparsec.try (pReturnExpression1 _sym)
         , Megaparsec.try (pReturnExpression2 _sym)
         ]
  where
    pReturnExpression1 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ReturnExpressionL)
    pReturnExpression1 _sym =
      iReturnExpression1 <$> pReturnTok <*> pMaybe pLabel <*> (pHiddenExpression _sym)
    pReturnExpression2 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm ReturnExpressionL)
    pReturnExpression2 _sym =
      iReturnExpression2 <$> pReturnTok <*> pMaybe pLabel

pWhileExpression :: Parser (MoveTerm WhileExpressionL)
pWhileExpression = do
  _sym <- pSymbol "while_expression" SWhileExpressionSymbol
  iWhileExpression <$> pWhileTok <*> pBetween pLeftParenthesisTok pRightParenthesisTok (pHiddenExpression _sym) <*> (pHiddenExpression _sym)

pBlockItem :: Parser (MoveTerm BlockItemL)
pBlockItem = do
  _sym <- pSymbol "block_item" SBlockItemSymbol
  iBlockItem <$> (pBlockItemInternal0 _sym) <*> pSemicolonTok

pBlockItemInternal0 :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BlockItemInternal0L)
pBlockItemInternal0 _sym = do
  choice [ Megaparsec.try (pBlockItemInternal0Expression _sym)
         , Megaparsec.try (pBlockItemInternal0LetStatement _sym)
         ]
  where
    pBlockItemInternal0Expression :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BlockItemInternal0L)
    pBlockItemInternal0Expression _sym =
      iBlockItemInternal0Expression <$> (pHiddenExpression _sym)
    pBlockItemInternal0LetStatement :: Cubix.TreeSitter.Token a -> Parser (MoveTerm BlockItemInternal0L)
    pBlockItemInternal0LetStatement _sym =
      iBlockItemInternal0LetStatement <$> pLetStatement

pLetStatement :: Parser (MoveTerm LetStatementL)
pLetStatement = do
  _sym <- pSymbol "let_statement" SLetStatementSymbol
  iLetStatement <$> pLetTok <*> pBindList <*> pMaybe (pPair pColonTok (pHiddenType _sym)) <*> pMaybe (pPair pEqualsSignTok (pHiddenExpression _sym))

pMacroFunctionDefinition :: Parser (MoveTerm MacroFunctionDefinitionL)
pMacroFunctionDefinition = do
  _sym <- pSymbol "macro_function_definition" SMacroFunctionDefinitionSymbol
  iMacroFunctionDefinition <$> pMaybe pModifier <*> pMacroTok <*> (pHiddenMacroSignature _sym) <*> pBlock

pHiddenMacroSignature :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenMacroSignatureL)
pHiddenMacroSignature _sym = do
  iHiddenMacroSignature <$> pMaybe pModifier <*> pFunTok <*> (pHiddenFunctionIdentifier _sym) <*> pMaybe pTypeParameters <*> pFunctionParameters <*> pMaybe pRetType

pNativeFunctionDefinition :: Parser (MoveTerm NativeFunctionDefinitionL)
pNativeFunctionDefinition = do
  _sym <- pSymbol "native_function_definition" SNativeFunctionDefinitionSymbol
  iNativeFunctionDefinition <$> (pHiddenFunctionSignature _sym) <*> pSemicolonTok

pHiddenStructItem :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenStructItemL)
pHiddenStructItem _sym = do
  choice [ Megaparsec.try (pHiddenStructItemNativeStructDefinition _sym)
         , Megaparsec.try (pHiddenStructItemStructDefinition _sym)
         ]
  where
    pHiddenStructItemNativeStructDefinition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenStructItemL)
    pHiddenStructItemNativeStructDefinition _sym =
      iHiddenStructItemNativeStructDefinition <$> pNativeStructDefinition
    pHiddenStructItemStructDefinition :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenStructItemL)
    pHiddenStructItemStructDefinition _sym =
      iHiddenStructItemStructDefinition <$> pStructDefinition

pNativeStructDefinition :: Parser (MoveTerm NativeStructDefinitionL)
pNativeStructDefinition = do
  _sym <- pSymbol "native_struct_definition" SNativeStructDefinitionSymbol
  iNativeStructDefinition <$> pMaybe pPublicTok <*> pBetween pNativeTok pSemicolonTok (pHiddenStructSignature _sym)

pHiddenStructSignature :: Cubix.TreeSitter.Token a -> Parser (MoveTerm HiddenStructSignatureL)
pHiddenStructSignature _sym = do
  iHiddenStructSignature <$> pStructTok <*> (pHiddenStructIdentifier _sym) <*> pMaybe pTypeParameters <*> pMaybe pAbilityDecls

pStructDefinition :: Parser (MoveTerm StructDefinitionL)
pStructDefinition = do
  _sym <- pSymbol "struct_definition" SStructDefinitionSymbol
  iStructDefinition <$> pMaybe pPublicTok <*> (pHiddenStructSignature _sym) <*> pDatatypeFields <*> pMaybe pPostfixAbilityDecls

pConstant :: Parser (MoveTerm ConstantL)
pConstant = do
  _sym <- pSymbol "constant" SConstantSymbol
  iConstant <$> pBetween pConstTok pColonTok pIdentifier <*> (pHiddenType _sym) <*> pBetween pEqualsSignTok pSemicolonTok (pHiddenExpression _sym)

pFriendDeclaration :: Parser (MoveTerm FriendDeclarationL)
pFriendDeclaration = do
  _sym <- pSymbol "friend_declaration" SFriendDeclarationSymbol
  iFriendDeclaration <$> pBetween pFriendTok pSemicolonTok pFriendAccess

pFriendAccess :: Parser (MoveTerm FriendAccessL)
pFriendAccess = do
  _sym <- pSymbol "friend_access" SFriendAccessSymbol
  choice [ Megaparsec.try (pFriendAccessLocalModule _sym)
         , Megaparsec.try (pFriendAccessFullyQualifiedModule _sym)
         ]
  where
    pFriendAccessLocalModule :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FriendAccessL)
    pFriendAccessLocalModule _sym =
      iFriendAccessLocalModule <$> pIdentifier
    pFriendAccessFullyQualifiedModule :: Cubix.TreeSitter.Token a -> Parser (MoveTerm FriendAccessL)
    pFriendAccessFullyQualifiedModule _sym =
      iFriendAccessFullyQualifiedModule <$> pModuleIdentity
