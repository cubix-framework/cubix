{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module Cubix.Language.SuiMove.ParsePretty where

import Control.Applicative.Combinators
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Comp.Multi (E (..))
import Data.Comp.Multi.Strategy.Classification (DynCase)
import Data.Foldable (foldrM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable)

import TreeSitter qualified as TS
import TreeSitter.SuiMove (tree_sitter_sui_move)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Cubix qualified as Megaparsec.Cubix
-- import Text.Megaparsec.Cubix
--   ( pMaybe, pPair, pSome, pMany
--   , pSepBy, pSepBy1, pBetween
--   , pSort, pContent
--   )

import Cubix.TreeSitter
import Cubix.Language.SuiMove.Modularized

import Text.Pretty.Simple

parse' :: ReaderT TreeSitterEnv IO SomeTerm
parse' = do
  filepath <- getFilePath
  source <- getSource
  pTable <- liftIO . mkParseTable =<< getLanguage
  rootNode <- liftIO . TS.treeRootNode =<< getTree
  syntax filepath source pTable rootNode

parse :: FilePath -> IO SomeTerm
parse path =
  runReaderT parse' =<<
    newTreeSitterEnv path tree_sitter_sui_move

syntax :: FilePath -> ByteString -> ParseTable -> TS.Node -> ReaderT TreeSitterEnv IO SomeTerm
syntax path source pTable = go
  where
    pContent = Megaparsec.Cubix.getContent source
    getParser sym = IM.lookup (fromIntegral sym) (unParseTable pTable)
    go :: TS.Node -> ReaderT TreeSitterEnv IO SomeTerm
    go root = do
      extra <- liftIO $ TS.nodeIsExtra root
      if extra
        then pure $ error ""
        else do
          range    <- liftIO $ nodeRange root
          span     <- liftIO $ nodeSpan path root
          symbolNo <- liftIO $ TS.nodeSymbol root
          -- liftIO $ traceIO =<< TS.nodeTypeAsString root
          case getParser symbolNo of
            Nothing -> do
              pPrintLightBg $ "Unrecognized symbol: " <> Prelude.show symbolNo
              pPrintLightBg $ "  at: " <> Prelude.show span
              liftIO (TS.nodeTypeAsString root) >>= pPrintLightBg
              error "no parse"

            Just p -> do
              childNo <- liftIO $ TS.nodeChildCount root
              -- pPrintLightBg $ "parsed sym: " <> show sym
              -- pPrintLightBg $ "child count: " <> show childNo

              let childNums = [0..childNo - 1]
                  content = pContent range

              children <- if childNo == 0
                then do
                  -- pPrintLightBg $ "parsed sym: " <> show sym
                  pure []
                else
                  mapM (go <=< (liftIO . TS.nodeChild root)) childNums
              case Megaparsec.runParser p path (Megaparsec.Cubix.Input content children) of
                Left err -> do
                  pPrintLightBg =<< liftIO (TS.nodeTypeAsString root)
                  pPrintLightBg children
                  error "no parse"
                Right item -> do
                  pPrintLightBg item
                  pure item

-- --------------------------------------------------------------------------------
-- -- Parser 
-- --------------------------------------------------------------------------------
type SomeTerm = E MoveTerm
type Parser t = Megaparsec.Cubix.Parser MoveSig t
type TermParser l = Parser (MoveTerm l)
type SomeTermParser = Parser (E MoveTerm)

-- reify types
pMaybe :: Typeable l => TermParser l -> TermParser (Maybe l)
pMaybe = Megaparsec.Cubix.pMaybe
{-# INLINABLE pMaybe #-}

pPair :: (Typeable l, Typeable l') => TermParser l -> TermParser l' -> TermParser (l, l')
pPair = Megaparsec.Cubix.pPair
{-# INLINABLE pPair #-}

pSome :: Typeable l => TermParser l -> TermParser [l]
pSome = Megaparsec.Cubix.pSome
{-# INLINABLE pSome #-}

pMany :: Typeable l => TermParser l -> TermParser [l]
pMany = Megaparsec.Cubix.pMany
{-# INLINABLE pMany #-}

pSepBy :: Typeable l => TermParser l -> TermParser sep -> TermParser [l]
pSepBy = Megaparsec.Cubix.pSepBy
{-# INLINABLE pSepBy #-}

pSepBy1 :: Typeable l => TermParser l -> TermParser sep -> TermParser [l]
pSepBy1 = Megaparsec.Cubix.pSepBy1
{-# INLINABLE pSepBy1 #-}

pBetween :: Typeable l => TermParser open -> TermParser close -> TermParser l -> TermParser l
pBetween = Megaparsec.Cubix.pBetween
{-# INLINABLE pBetween #-}

pSort :: forall l. DynCase MoveTerm l => NonEmpty Char -> TermParser l
pSort = Megaparsec.Cubix.pSort @l
{-# INLINABLE pSort #-}

pSort' :: forall l. DynCase MoveTerm l => Proxy l -> NonEmpty Char -> TermParser l
pSort' = Megaparsec.Cubix.pSort' 
{-# INLINABLE pSort' #-}

pContent :: Parser Text
pContent = Megaparsec.Cubix.pContent
{-# INLINABLE pContent #-}

pExclamationMarkTok :: TermParser ExclamationMarkTokL
pExclamationMarkTok = ExclamationMarkTok' <$ Megaparsec.eof

pExclamationMarkEqualsSignTok :: TermParser ExclamationMarkEqualsSignTokL
pExclamationMarkEqualsSignTok = ExclamationMarkEqualsSignTok' <$ Megaparsec.eof

pNumberSignLeftSquareBracketTok :: TermParser NumberSignLeftSquareBracketTokL
pNumberSignLeftSquareBracketTok = NumberSignLeftSquareBracketTok' <$ Megaparsec.eof

pDollarSignTok :: TermParser DollarSignTokL
pDollarSignTok = DollarSignTok' <$ Megaparsec.eof

pPercentSignTok :: TermParser PercentSignTokL
pPercentSignTok = PercentSignTok' <$ Megaparsec.eof

pAmpersandTok :: TermParser AmpersandTokL
pAmpersandTok = AmpersandTok' <$ Megaparsec.eof

pAmpersandAmpersandTok :: TermParser AmpersandAmpersandTokL
pAmpersandAmpersandTok = AmpersandAmpersandTok' <$ Megaparsec.eof

pApostropheTok :: TermParser ApostropheTokL
pApostropheTok = ApostropheTok' <$ Megaparsec.eof

pLeftParenthesisTok :: TermParser LeftParenthesisTokL
pLeftParenthesisTok = LeftParenthesisTok' <$ Megaparsec.eof

pRightParenthesisTok :: TermParser RightParenthesisTokL
pRightParenthesisTok = RightParenthesisTok' <$ Megaparsec.eof

pAsteriskTok :: TermParser AsteriskTokL
pAsteriskTok = AsteriskTok' <$ Megaparsec.eof

pPlusSignTok :: TermParser PlusSignTokL
pPlusSignTok = PlusSignTok' <$ Megaparsec.eof

pCommaTok :: TermParser CommaTokL
pCommaTok = CommaTok' <$ Megaparsec.eof

pHyphenMinusTok :: TermParser HyphenMinusTokL
pHyphenMinusTok = HyphenMinusTok' <$ Megaparsec.eof

pHyphenMinusGreaterThanSignTok :: TermParser HyphenMinusGreaterThanSignTokL
pHyphenMinusGreaterThanSignTok = HyphenMinusGreaterThanSignTok' <$ Megaparsec.eof

pFullStopTok :: TermParser FullStopTokL
pFullStopTok = FullStopTok' <$ Megaparsec.eof

pFullStopFullStopTok :: TermParser FullStopFullStopTokL
pFullStopFullStopTok = FullStopFullStopTok' <$ Megaparsec.eof

pSolidusTok :: TermParser SolidusTokL
pSolidusTok = SolidusTok' <$ Megaparsec.eof

pSolidusAsteriskTok :: TermParser SolidusAsteriskTokL
pSolidusAsteriskTok = SolidusAsteriskTok' <$ Megaparsec.eof

pSolidusSolidusTok :: TermParser SolidusSolidusTokL
pSolidusSolidusTok = SolidusSolidusTok' <$ Megaparsec.eof

pColonTok :: TermParser ColonTokL
pColonTok = ColonTok' <$ Megaparsec.eof

pColonColonTok :: TermParser ColonColonTokL
pColonColonTok = ColonColonTok' <$ Megaparsec.eof

pSemicolonTok :: TermParser SemicolonTokL
pSemicolonTok = SemicolonTok' <$ Megaparsec.eof

pLessThanSignTok :: TermParser LessThanSignTokL
pLessThanSignTok = LessThanSignTok' <$ Megaparsec.eof

pLessThanSignLessThanSignTok :: TermParser LessThanSignLessThanSignTokL
pLessThanSignLessThanSignTok = LessThanSignLessThanSignTok' <$ Megaparsec.eof

pLessThanSignEqualsSignTok :: TermParser LessThanSignEqualsSignTokL
pLessThanSignEqualsSignTok = LessThanSignEqualsSignTok' <$ Megaparsec.eof

pEqualsSignTok :: TermParser EqualsSignTokL
pEqualsSignTok = EqualsSignTok' <$ Megaparsec.eof

pEqualsSignEqualsSignTok :: TermParser EqualsSignEqualsSignTokL
pEqualsSignEqualsSignTok = EqualsSignEqualsSignTok' <$ Megaparsec.eof

pEqualsSignEqualsSignGreaterThanSignTok :: TermParser EqualsSignEqualsSignGreaterThanSignTokL
pEqualsSignEqualsSignGreaterThanSignTok = EqualsSignEqualsSignGreaterThanSignTok' <$ Megaparsec.eof

pEqualsSignGreaterThanSignTok :: TermParser EqualsSignGreaterThanSignTokL
pEqualsSignGreaterThanSignTok = EqualsSignGreaterThanSignTok' <$ Megaparsec.eof

pGreaterThanSignTok :: TermParser GreaterThanSignTokL
pGreaterThanSignTok = GreaterThanSignTok' <$ Megaparsec.eof

pGreaterThanSignEqualsSignTok :: TermParser GreaterThanSignEqualsSignTokL
pGreaterThanSignEqualsSignTok = GreaterThanSignEqualsSignTok' <$ Megaparsec.eof

pGreaterThanSignGreaterThanSignTok :: TermParser GreaterThanSignGreaterThanSignTokL
pGreaterThanSignGreaterThanSignTok = GreaterThanSignGreaterThanSignTok' <$ Megaparsec.eof

pCommercialAtTok :: TermParser CommercialAtTokL
pCommercialAtTok = CommercialAtTok' <$ Megaparsec.eof

pLeftSquareBracketTok :: TermParser LeftSquareBracketTokL
pLeftSquareBracketTok = LeftSquareBracketTok' <$ Megaparsec.eof

pRightSquareBracketTok :: TermParser RightSquareBracketTokL
pRightSquareBracketTok = RightSquareBracketTok' <$ Megaparsec.eof

pCircumflexAccentTok :: TermParser CircumflexAccentTokL
pCircumflexAccentTok = CircumflexAccentTok' <$ Megaparsec.eof

pAbortTok :: TermParser AbortTokL
pAbortTok = AbortTok' <$ Megaparsec.eof

pAbortsIfTok :: TermParser AbortsIfTokL
pAbortsIfTok = AbortsIfTok' <$ Megaparsec.eof

pAbortsWithTok :: TermParser AbortsWithTokL
pAbortsWithTok = AbortsWithTok' <$ Megaparsec.eof

pAddressTok :: TermParser AddressTokL
pAddressTok = AddressTok' <$ Megaparsec.eof

pApplyTok :: TermParser ApplyTokL
pApplyTok = ApplyTok' <$ Megaparsec.eof

pAsTok :: TermParser AsTokL
pAsTok = AsTok' <$ Megaparsec.eof

pAssertTok :: TermParser AssertTokL
pAssertTok = AssertTok' <$ Megaparsec.eof

pAssumeTok :: TermParser AssumeTokL
pAssumeTok = AssumeTok' <$ Megaparsec.eof

pBoolTok :: TermParser BoolTokL
pBoolTok = BoolTok' <$ Megaparsec.eof

pBreakTok :: TermParser BreakTokL
pBreakTok = BreakTok' <$ Megaparsec.eof

pBytearrayTok :: TermParser BytearrayTokL
pBytearrayTok = BytearrayTok' <$ Megaparsec.eof

pConstTok :: TermParser ConstTokL
pConstTok = ConstTok' <$ Megaparsec.eof

pContinueTok :: TermParser ContinueTokL
pContinueTok = ContinueTok' <$ Megaparsec.eof

pCopyTok :: TermParser CopyTokL
pCopyTok = CopyTok' <$ Megaparsec.eof

pDecreasesTok :: TermParser DecreasesTokL
pDecreasesTok = DecreasesTok' <$ Megaparsec.eof

pDropTok :: TermParser DropTokL
pDropTok = DropTok' <$ Megaparsec.eof

pElseTok :: TermParser ElseTokL
pElseTok = ElseTok' <$ Megaparsec.eof

pEnsuresTok :: TermParser EnsuresTokL
pEnsuresTok = EnsuresTok' <$ Megaparsec.eof

pEntryTok :: TermParser EntryTokL
pEntryTok = EntryTok' <$ Megaparsec.eof

pEnumTok :: TermParser EnumTokL
pEnumTok = EnumTok' <$ Megaparsec.eof

pExceptTok :: TermParser ExceptTokL
pExceptTok = ExceptTok' <$ Megaparsec.eof

pExistsTok :: TermParser ExistsTokL
pExistsTok = ExistsTok' <$ Megaparsec.eof

pFalseTok :: TermParser FalseTokL
pFalseTok = FalseTok' <$ Megaparsec.eof

pForallTok :: TermParser ForallTokL
pForallTok = ForallTok' <$ Megaparsec.eof

pFriendTok :: TermParser FriendTokL
pFriendTok = FriendTok' <$ Megaparsec.eof

pFunTok :: TermParser FunTokL
pFunTok = FunTok' <$ Megaparsec.eof

pGlobalTok :: TermParser GlobalTokL
pGlobalTok = GlobalTok' <$ Megaparsec.eof

pHasTok :: TermParser HasTokL
pHasTok = HasTok' <$ Megaparsec.eof

pIfTok :: TermParser IfTokL
pIfTok = IfTok' <$ Megaparsec.eof

pInTok :: TermParser InTokL
pInTok = InTok' <$ Megaparsec.eof

pIncludeTok :: TermParser IncludeTokL
pIncludeTok = IncludeTok' <$ Megaparsec.eof

pInternalTok :: TermParser InternalTokL
pInternalTok = InternalTok' <$ Megaparsec.eof

pInvariantTok :: TermParser InvariantTokL
pInvariantTok = InvariantTok' <$ Megaparsec.eof

pKeyTok :: TermParser KeyTokL
pKeyTok = KeyTok' <$ Megaparsec.eof

pLetTok :: TermParser LetTokL
pLetTok = LetTok' <$ Megaparsec.eof

pLocalTok :: TermParser LocalTokL
pLocalTok = LocalTok' <$ Megaparsec.eof

pLoopTok :: TermParser LoopTokL
pLoopTok = LoopTok' <$ Megaparsec.eof

pMacroTok :: TermParser MacroTokL
pMacroTok = MacroTok' <$ Megaparsec.eof

pMatchTok :: TermParser MatchTokL
pMatchTok = MatchTok' <$ Megaparsec.eof

pModifiesTok :: TermParser ModifiesTokL
pModifiesTok = ModifiesTok' <$ Megaparsec.eof

pModuleTok :: TermParser ModuleTokL
pModuleTok = ModuleTok' <$ Megaparsec.eof

pMoveTok :: TermParser MoveTokL
pMoveTok = MoveTok' <$ Megaparsec.eof

pMutTok :: TermParser MutTokL
pMutTok = MutTok' <$ Megaparsec.eof

pNativeTok :: TermParser NativeTokL
pNativeTok = NativeTok' <$ Megaparsec.eof

pPackTok :: TermParser PackTokL
pPackTok = PackTok' <$ Megaparsec.eof

pPackageTok :: TermParser PackageTokL
pPackageTok = PackageTok' <$ Megaparsec.eof

pPhantomTok :: TermParser PhantomTokL
pPhantomTok = PhantomTok' <$ Megaparsec.eof

pPostTok :: TermParser PostTokL
pPostTok = PostTok' <$ Megaparsec.eof

pPragmaTok :: TermParser PragmaTokL
pPragmaTok = PragmaTok' <$ Megaparsec.eof

pPublicTok :: TermParser PublicTokL
pPublicTok = PublicTok' <$ Megaparsec.eof

pRequiresTok :: TermParser RequiresTokL
pRequiresTok = RequiresTok' <$ Megaparsec.eof

pReturnTok :: TermParser ReturnTokL
pReturnTok = ReturnTok' <$ Megaparsec.eof

pSchemaTok :: TermParser SchemaTokL
pSchemaTok = SchemaTok' <$ Megaparsec.eof

pSignerTok :: TermParser SignerTokL
pSignerTok = SignerTok' <$ Megaparsec.eof

pSpecTok :: TermParser SpecTokL
pSpecTok = SpecTok' <$ Megaparsec.eof

pStoreTok :: TermParser StoreTokL
pStoreTok = StoreTok' <$ Megaparsec.eof

pStructTok :: TermParser StructTokL
pStructTok = StructTok' <$ Megaparsec.eof

pSucceedsIfTok :: TermParser SucceedsIfTokL
pSucceedsIfTok = SucceedsIfTok' <$ Megaparsec.eof

pToTok :: TermParser ToTokL
pToTok = ToTok' <$ Megaparsec.eof

pTrueTok :: TermParser TrueTokL
pTrueTok = TrueTok' <$ Megaparsec.eof

pU128Tok :: TermParser U128TokL
pU128Tok = U128Tok' <$ Megaparsec.eof

pU16Tok :: TermParser U16TokL
pU16Tok = U16Tok' <$ Megaparsec.eof

pU256Tok :: TermParser U256TokL
pU256Tok = U256Tok' <$ Megaparsec.eof

pU32Tok :: TermParser U32TokL
pU32Tok = U32Tok' <$ Megaparsec.eof

pU64Tok :: TermParser U64TokL
pU64Tok = U64Tok' <$ Megaparsec.eof

pU8Tok :: TermParser U8TokL
pU8Tok = U8Tok' <$ Megaparsec.eof

pUnpackTok :: TermParser UnpackTokL
pUnpackTok = UnpackTok' <$ Megaparsec.eof

pUpdateTok :: TermParser UpdateTokL
pUpdateTok = UpdateTok' <$ Megaparsec.eof

pUseTok :: TermParser UseTokL
pUseTok = UseTok' <$ Megaparsec.eof

pVectorLessThanSignTok :: TermParser VectorLessThanSignTokL
pVectorLessThanSignTok = VectorLessThanSignTok' <$ Megaparsec.eof

pVectorLeftSquareBracketTok :: TermParser VectorLeftSquareBracketTokL
pVectorLeftSquareBracketTok = VectorLeftSquareBracketTok' <$ Megaparsec.eof

pWhereTok :: TermParser WhereTokL
pWhereTok = WhereTok' <$ Megaparsec.eof

pWhileTok :: TermParser WhileTokL
pWhileTok = WhileTok' <$ Megaparsec.eof

pWithTok :: TermParser WithTokL
pWithTok = WithTok' <$ Megaparsec.eof

pLeftCurlyBracketTok :: TermParser LeftCurlyBracketTokL
pLeftCurlyBracketTok = LeftCurlyBracketTok' <$ Megaparsec.eof

pVerticalLineTok :: TermParser VerticalLineTokL
pVerticalLineTok = VerticalLineTok' <$ Megaparsec.eof

pVerticalLineVerticalLineTok :: TermParser VerticalLineVerticalLineTokL
pVerticalLineVerticalLineTok = VerticalLineVerticalLineTok' <$ Megaparsec.eof

pRightCurlyBracketTok :: TermParser RightCurlyBracketTokL
pRightCurlyBracketTok = RightCurlyBracketTok' <$ Megaparsec.eof

pSourceFile :: TermParser SourceFileL
pSourceFile =
  SourceFile' <$> pMany (pSort @ModuleDefinitionL "module_definition")

pModuleDefinition :: TermParser ModuleDefinitionL
pModuleDefinition =
  ModuleDefinition' <$> pSort @ModuleTokL "module_tok" <*> pSort @ModuleIdentityL "module_identity" <*> pSort @ModuleBodyL "module_body"

pModuleBody :: TermParser ModuleBodyL
pModuleBody =
  ModuleBody' <$> pSort @ModuleBodyInternal0L "module_body_internal0" <*> pMany (pSort @ModuleBodyInternal1L "module_body_internal1") <*> pMaybe (pSort @RightCurlyBracketTokL "}_tok")

pModuleBodyInternal0 :: TermParser ModuleBodyInternal0L
pModuleBodyInternal0 =
  choice [ Megaparsec.try pModuleBodyInternal0Semicolon
         , Megaparsec.try pModuleBodyInternal0LeftCurlyBracket
         ]
  where
    pModuleBodyInternal0Semicolon :: TermParser ModuleBodyInternal0L
    pModuleBodyInternal0Semicolon =
      ModuleBodyInternal0Semicolon' <$> pSort @SemicolonTokL ";_tok"
    pModuleBodyInternal0LeftCurlyBracket :: TermParser ModuleBodyInternal0L
    pModuleBodyInternal0LeftCurlyBracket =
      ModuleBodyInternal0LeftCurlyBracket' <$> pSort @LeftCurlyBracketTokL "{_tok"

pModuleBodyInternal1 :: TermParser ModuleBodyInternal1L
pModuleBodyInternal1 =
  choice [ Megaparsec.try pModuleBodyInternal1UseDeclaration
         , Megaparsec.try pModuleBodyInternal1FriendDeclaration
         , Megaparsec.try pModuleBodyInternal1Constant
         , Megaparsec.try pModuleBodyInternal1HidFunctionItem
         , Megaparsec.try pModuleBodyInternal1HidStructItem
         , Megaparsec.try pModuleBodyInternal1HidEnumItem
         , Megaparsec.try pModuleBodyInternal1SpecBlock
         ]
  where
    pModuleBodyInternal1UseDeclaration :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1UseDeclaration =
      ModuleBodyInternal1UseDeclaration' <$> pSort @UseDeclarationL "use_declaration"
    pModuleBodyInternal1FriendDeclaration :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1FriendDeclaration =
      ModuleBodyInternal1FriendDeclaration' <$> pSort @FriendDeclarationL "friend_declaration"
    pModuleBodyInternal1Constant :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1Constant =
      ModuleBodyInternal1Constant' <$> pSort @ConstantL "constant"
    pModuleBodyInternal1HidFunctionItem :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1HidFunctionItem =
      ModuleBodyInternal1HidFunctionItem' <$> pSort @HidFunctionItemL "hid_function_item"
    pModuleBodyInternal1HidStructItem :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1HidStructItem =
      ModuleBodyInternal1HidStructItem' <$> pSort @HidStructItemL "hid_struct_item"
    pModuleBodyInternal1HidEnumItem :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1HidEnumItem =
      ModuleBodyInternal1HidEnumItem' <$> pSort @HidEnumItemL "hid_enum_item"
    pModuleBodyInternal1SpecBlock :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1SpecBlock =
      ModuleBodyInternal1SpecBlock' <$> pSort @SpecBlockL "spec_block"

pConstant :: TermParser ConstantL
pConstant =
  Constant' <$> pBetween (pSort @ConstTokL "const_tok") (pSort @ColonTokL ":_tok") (pSort @IdentifierL "identifier") <*> pSort @HidTypeL "hid_type" <*> pBetween (pSort @EqualsSignTokL "=_tok") (pSort @SemicolonTokL ";_tok") (pSort @HidExpressionL "hid_expression")

pHidExpression :: TermParser HidExpressionL
pHidExpression =
  choice [ Megaparsec.try pHidExpressionCallExpression
         , Megaparsec.try pHidExpressionMacroCallExpression
         , Megaparsec.try pHidExpressionLambdaExpression
         , Megaparsec.try pHidExpressionIfExpression
         , Megaparsec.try pHidExpressionWhileExpression
         , Megaparsec.try pHidExpressionReturnExpression
         , Megaparsec.try pHidExpressionAbortExpression
         , Megaparsec.try pHidExpressionAssignExpression
         , Megaparsec.try pHidExpressionHidUnaryExpression
         , Megaparsec.try pHidExpressionBinaryExpression
         , Megaparsec.try pHidExpressionCastExpression
         , Megaparsec.try pHidExpressionQuantifierExpression
         , Megaparsec.try pHidExpressionMatchExpression
         , Megaparsec.try pHidExpressionVectorExpression
         , Megaparsec.try pHidExpressionLoopExpression
         , Megaparsec.try pHidExpressionIdentifiedExpression
         ]
  where
    pHidExpressionCallExpression :: TermParser HidExpressionL
    pHidExpressionCallExpression =
      HidExpressionCallExpression' <$> pSort @CallExpressionL "call_expression"
    pHidExpressionMacroCallExpression :: TermParser HidExpressionL
    pHidExpressionMacroCallExpression =
      HidExpressionMacroCallExpression' <$> pSort @MacroCallExpressionL "macro_call_expression"
    pHidExpressionLambdaExpression :: TermParser HidExpressionL
    pHidExpressionLambdaExpression =
      HidExpressionLambdaExpression' <$> pSort @LambdaExpressionL "lambda_expression"
    pHidExpressionIfExpression :: TermParser HidExpressionL
    pHidExpressionIfExpression =
      HidExpressionIfExpression' <$> pSort @IfExpressionL "if_expression"
    pHidExpressionWhileExpression :: TermParser HidExpressionL
    pHidExpressionWhileExpression =
      HidExpressionWhileExpression' <$> pSort @WhileExpressionL "while_expression"
    pHidExpressionReturnExpression :: TermParser HidExpressionL
    pHidExpressionReturnExpression =
      HidExpressionReturnExpression' <$> pSort @ReturnExpressionL "return_expression"
    pHidExpressionAbortExpression :: TermParser HidExpressionL
    pHidExpressionAbortExpression =
      HidExpressionAbortExpression' <$> pSort @AbortExpressionL "abort_expression"
    pHidExpressionAssignExpression :: TermParser HidExpressionL
    pHidExpressionAssignExpression =
      HidExpressionAssignExpression' <$> pSort @AssignExpressionL "assign_expression"
    pHidExpressionHidUnaryExpression :: TermParser HidExpressionL
    pHidExpressionHidUnaryExpression =
      HidExpressionHidUnaryExpression' <$> pSort @HidUnaryExpressionL "hid_unary_expression"
    pHidExpressionBinaryExpression :: TermParser HidExpressionL
    pHidExpressionBinaryExpression =
      HidExpressionBinaryExpression' <$> pSort @BinaryExpressionL "binary_expression"
    pHidExpressionCastExpression :: TermParser HidExpressionL
    pHidExpressionCastExpression =
      HidExpressionCastExpression' <$> pSort @CastExpressionL "cast_expression"
    pHidExpressionQuantifierExpression :: TermParser HidExpressionL
    pHidExpressionQuantifierExpression =
      HidExpressionQuantifierExpression' <$> pSort @QuantifierExpressionL "quantifier_expression"
    pHidExpressionMatchExpression :: TermParser HidExpressionL
    pHidExpressionMatchExpression =
      HidExpressionMatchExpression' <$> pSort @MatchExpressionL "match_expression"
    pHidExpressionVectorExpression :: TermParser HidExpressionL
    pHidExpressionVectorExpression =
      HidExpressionVectorExpression' <$> pSort @VectorExpressionL "vector_expression"
    pHidExpressionLoopExpression :: TermParser HidExpressionL
    pHidExpressionLoopExpression =
      HidExpressionLoopExpression' <$> pSort @LoopExpressionL "loop_expression"
    pHidExpressionIdentifiedExpression :: TermParser HidExpressionL
    pHidExpressionIdentifiedExpression =
      HidExpressionIdentifiedExpression' <$> pSort @IdentifiedExpressionL "identified_expression"

pAbortExpression :: TermParser AbortExpressionL
pAbortExpression =
  AbortExpression' <$> pSort @AbortTokL "abort_tok" <*> pMaybe (pSort @HidExpressionL "hid_expression")

pAssignExpression :: TermParser AssignExpressionL
pAssignExpression =
  AssignExpression' <$> pPair (pPair (pSort @HidUnaryExpressionL "hid_unary_expression") (pSort @EqualsSignTokL "=_tok")) (pSort @HidExpressionL "hid_expression")

pHidUnaryExpression :: TermParser HidUnaryExpressionL
pHidUnaryExpression =
  HidUnaryExpression' <$> pSort @HidUnaryExpressionInternal0L "hid_unary_expression_internal0"

pHidUnaryExpressionInternal0 :: TermParser HidUnaryExpressionInternal0L
pHidUnaryExpressionInternal0 =
  choice [ Megaparsec.try pHidUnaryExpressionInternal0UnaryExpression
         , Megaparsec.try pHidUnaryExpressionInternal0BorrowExpression
         , Megaparsec.try pHidUnaryExpressionInternal0DereferenceExpression
         , Megaparsec.try pHidUnaryExpressionInternal0MoveOrCopyExpression
         , Megaparsec.try pHidUnaryExpressionInternal0HidExpressionTerm
         ]
  where
    pHidUnaryExpressionInternal0UnaryExpression :: TermParser HidUnaryExpressionInternal0L
    pHidUnaryExpressionInternal0UnaryExpression =
      HidUnaryExpressionInternal0UnaryExpression' <$> pSort @UnaryExpressionL "unary_expression"
    pHidUnaryExpressionInternal0BorrowExpression :: TermParser HidUnaryExpressionInternal0L
    pHidUnaryExpressionInternal0BorrowExpression =
      HidUnaryExpressionInternal0BorrowExpression' <$> pSort @BorrowExpressionL "borrow_expression"
    pHidUnaryExpressionInternal0DereferenceExpression :: TermParser HidUnaryExpressionInternal0L
    pHidUnaryExpressionInternal0DereferenceExpression =
      HidUnaryExpressionInternal0DereferenceExpression' <$> pSort @DereferenceExpressionL "dereference_expression"
    pHidUnaryExpressionInternal0MoveOrCopyExpression :: TermParser HidUnaryExpressionInternal0L
    pHidUnaryExpressionInternal0MoveOrCopyExpression =
      HidUnaryExpressionInternal0MoveOrCopyExpression' <$> pSort @MoveOrCopyExpressionL "move_or_copy_expression"
    pHidUnaryExpressionInternal0HidExpressionTerm :: TermParser HidUnaryExpressionInternal0L
    pHidUnaryExpressionInternal0HidExpressionTerm =
      HidUnaryExpressionInternal0HidExpressionTerm' <$> pSort @HidExpressionTermL "hid_expression_term"

pBorrowExpression :: TermParser BorrowExpressionL
pBorrowExpression =
  BorrowExpression' <$> pPair (pSort @HidReferenceL "hid_reference") (pSort @HidExpressionL "hid_expression")

pHidReference :: TermParser HidReferenceL
pHidReference =
  choice [ Megaparsec.try pHidReferenceImmRef
         , Megaparsec.try pHidReferenceMutRef
         ]
  where
    pHidReferenceImmRef :: TermParser HidReferenceL
    pHidReferenceImmRef =
      HidReferenceImmRef' <$> pSort @ImmRefL "imm_ref"
    pHidReferenceMutRef :: TermParser HidReferenceL
    pHidReferenceMutRef =
      HidReferenceMutRef' <$> pSort @MutRefL "mut_ref"

pImmRef :: TermParser ImmRefL
pImmRef =
  ImmRef' <$> pSort @AmpersandTokL "&_tok"

pMutRef :: TermParser MutRefL
pMutRef =
  MutRef' <$> pSort @AmpersandTokL "&_tok" <*> pSort @MutTokL "mut_tok"

pDereferenceExpression :: TermParser DereferenceExpressionL
pDereferenceExpression =
  DereferenceExpression' <$> pPair (pSort @AsteriskTokL "*_tok") (pSort @HidExpressionL "hid_expression")

pHidExpressionTerm :: TermParser HidExpressionTermL
pHidExpressionTerm =
  choice [ Megaparsec.try pHidExpressionTermCallExpression
         , Megaparsec.try pHidExpressionTermBreakExpression
         , Megaparsec.try pHidExpressionTermContinueExpression
         , Megaparsec.try pHidExpressionTermNameExpression
         , Megaparsec.try pHidExpressionTermMacroCallExpression
         , Megaparsec.try pHidExpressionTermPackExpression
         , Megaparsec.try pHidExpressionTermHidLiteralValue
         , Megaparsec.try pHidExpressionTermUnitExpression
         , Megaparsec.try pHidExpressionTermExpressionList
         , Megaparsec.try pHidExpressionTermAnnotationExpression
         , Megaparsec.try pHidExpressionTermBlock
         , Megaparsec.try pHidExpressionTermSpecBlock
         , Megaparsec.try pHidExpressionTermIfExpression
         , Megaparsec.try pHidExpressionTermDotExpression
         , Megaparsec.try pHidExpressionTermIndexExpression
         , Megaparsec.try pHidExpressionTermVectorExpression
         , Megaparsec.try pHidExpressionTermMatchExpression
         ]
  where
    pHidExpressionTermCallExpression :: TermParser HidExpressionTermL
    pHidExpressionTermCallExpression =
      HidExpressionTermCallExpression' <$> pSort @CallExpressionL "call_expression"
    pHidExpressionTermBreakExpression :: TermParser HidExpressionTermL
    pHidExpressionTermBreakExpression =
      HidExpressionTermBreakExpression' <$> pSort @BreakExpressionL "break_expression"
    pHidExpressionTermContinueExpression :: TermParser HidExpressionTermL
    pHidExpressionTermContinueExpression =
      HidExpressionTermContinueExpression' <$> pSort @ContinueExpressionL "continue_expression"
    pHidExpressionTermNameExpression :: TermParser HidExpressionTermL
    pHidExpressionTermNameExpression =
      HidExpressionTermNameExpression' <$> pSort @NameExpressionL "name_expression"
    pHidExpressionTermMacroCallExpression :: TermParser HidExpressionTermL
    pHidExpressionTermMacroCallExpression =
      HidExpressionTermMacroCallExpression' <$> pSort @MacroCallExpressionL "macro_call_expression"
    pHidExpressionTermPackExpression :: TermParser HidExpressionTermL
    pHidExpressionTermPackExpression =
      HidExpressionTermPackExpression' <$> pSort @PackExpressionL "pack_expression"
    pHidExpressionTermHidLiteralValue :: TermParser HidExpressionTermL
    pHidExpressionTermHidLiteralValue =
      HidExpressionTermHidLiteralValue' <$> pSort @HidLiteralValueL "hid_literal_value"
    pHidExpressionTermUnitExpression :: TermParser HidExpressionTermL
    pHidExpressionTermUnitExpression =
      HidExpressionTermUnitExpression' <$> pSort @UnitExpressionL "unit_expression"
    pHidExpressionTermExpressionList :: TermParser HidExpressionTermL
    pHidExpressionTermExpressionList =
      HidExpressionTermExpressionList' <$> pSort @ExpressionListL "expression_list"
    pHidExpressionTermAnnotationExpression :: TermParser HidExpressionTermL
    pHidExpressionTermAnnotationExpression =
      HidExpressionTermAnnotationExpression' <$> pSort @AnnotationExpressionL "annotation_expression"
    pHidExpressionTermBlock :: TermParser HidExpressionTermL
    pHidExpressionTermBlock =
      HidExpressionTermBlock' <$> pSort @BlockL "block"
    pHidExpressionTermSpecBlock :: TermParser HidExpressionTermL
    pHidExpressionTermSpecBlock =
      HidExpressionTermSpecBlock' <$> pSort @SpecBlockL "spec_block"
    pHidExpressionTermIfExpression :: TermParser HidExpressionTermL
    pHidExpressionTermIfExpression =
      HidExpressionTermIfExpression' <$> pSort @IfExpressionL "if_expression"
    pHidExpressionTermDotExpression :: TermParser HidExpressionTermL
    pHidExpressionTermDotExpression =
      HidExpressionTermDotExpression' <$> pSort @DotExpressionL "dot_expression"
    pHidExpressionTermIndexExpression :: TermParser HidExpressionTermL
    pHidExpressionTermIndexExpression =
      HidExpressionTermIndexExpression' <$> pSort @IndexExpressionL "index_expression"
    pHidExpressionTermVectorExpression :: TermParser HidExpressionTermL
    pHidExpressionTermVectorExpression =
      HidExpressionTermVectorExpression' <$> pSort @VectorExpressionL "vector_expression"
    pHidExpressionTermMatchExpression :: TermParser HidExpressionTermL
    pHidExpressionTermMatchExpression =
      HidExpressionTermMatchExpression' <$> pSort @MatchExpressionL "match_expression"

pAnnotationExpression :: TermParser AnnotationExpressionL
pAnnotationExpression =
  AnnotationExpression' <$> pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @ColonTokL ":_tok") (pSort @HidExpressionL "hid_expression") <*> pSort @HidTypeL "hid_type" <*> pSort @RightParenthesisTokL ")_tok"

pHidType :: TermParser HidTypeL
pHidType =
  choice [ Megaparsec.try pHidTypeApplyType
         , Megaparsec.try pHidTypeRefType
         , Megaparsec.try pHidTypeTupleType
         , Megaparsec.try pHidTypeFunctionType
         , Megaparsec.try pHidTypePrimitiveType
         ]
  where
    pHidTypeApplyType :: TermParser HidTypeL
    pHidTypeApplyType =
      HidTypeApplyType' <$> pSort @ApplyTypeL "apply_type"
    pHidTypeRefType :: TermParser HidTypeL
    pHidTypeRefType =
      HidTypeRefType' <$> pSort @RefTypeL "ref_type"
    pHidTypeTupleType :: TermParser HidTypeL
    pHidTypeTupleType =
      HidTypeTupleType' <$> pSort @TupleTypeL "tuple_type"
    pHidTypeFunctionType :: TermParser HidTypeL
    pHidTypeFunctionType =
      HidTypeFunctionType' <$> pSort @FunctionTypeL "function_type"
    pHidTypePrimitiveType :: TermParser HidTypeL
    pHidTypePrimitiveType =
      HidTypePrimitiveType' <$> pSort @PrimitiveTypeL "primitive_type"

pApplyType :: TermParser ApplyTypeL
pApplyType =
  ApplyType' <$> pPair (pSort @ModuleAccessL "module_access") (pMaybe (pSort @TypeArgumentsL "type_arguments"))

pModuleAccess :: TermParser ModuleAccessL
pModuleAccess =
  choice [ Megaparsec.try pModuleAccess1
         , Megaparsec.try pModuleAccess2
         , Megaparsec.try pModuleAccess3
         , Megaparsec.try pModuleAccess4
         , Megaparsec.try pModuleAccess5
         , Megaparsec.try pModuleAccess6
         , Megaparsec.try pModuleAccess7
         , Megaparsec.try pModuleAccess8
         , Megaparsec.try pModuleAccessMember
         ]
  where
    pModuleAccess1 :: TermParser ModuleAccessL
    pModuleAccess1 =
      ModuleAccess1' <$> pSort @DollarSignTokL "$_tok" <*> pSort @IdentifierL "identifier"
    pModuleAccess2 :: TermParser ModuleAccessL
    pModuleAccess2 =
      ModuleAccess2' <$> pSort @CommercialAtTokL "@_tok" <*> pSort @IdentifierL "identifier"
    pModuleAccess3 :: TermParser ModuleAccessL
    pModuleAccess3 =
      ModuleAccess3' <$> pSort @ModuleIdentityL "module_identity" <*> pSort @ColonColonTokL "::_tok" <*> pSort @IdentifierL "identifier" <*> pMaybe (pSort @TypeArgumentsL "type_arguments") <*> pSort @ColonColonTokL "::_tok" <*> pSort @IdentifierL "identifier"
    pModuleAccess4 :: TermParser ModuleAccessL
    pModuleAccess4 =
      ModuleAccess4' <$> pSort @ModuleIdentityL "module_identity" <*> pSort @ColonColonTokL "::_tok" <*> pSort @IdentifierL "identifier" <*> pSort @TypeArgumentsL "type_arguments"
    pModuleAccess5 :: TermParser ModuleAccessL
    pModuleAccess5 =
      ModuleAccess5' <$> pSort @ModuleIdentityL "module_identity" <*> pMaybe (pSort @TypeArgumentsL "type_arguments") <*> pSort @ColonColonTokL "::_tok" <*> pSort @IdentifierL "identifier"
    pModuleAccess6 :: TermParser ModuleAccessL
    pModuleAccess6 =
      ModuleAccess6' <$> pSort @HidModuleIdentifierL "hid_module_identifier" <*> pMaybe (pSort @TypeArgumentsL "type_arguments") <*> pSort @ColonColonTokL "::_tok" <*> pSort @IdentifierL "identifier"
    pModuleAccess7 :: TermParser ModuleAccessL
    pModuleAccess7 =
      ModuleAccess7' <$> pSort @ModuleIdentityL "module_identity" <*> pMaybe (pSort @TypeArgumentsL "type_arguments")
    pModuleAccess8 :: TermParser ModuleAccessL
    pModuleAccess8 =
      ModuleAccess8' <$> pSort @IdentifierL "identifier" <*> pMaybe (pSort @TypeArgumentsL "type_arguments")
    pModuleAccessMember :: TermParser ModuleAccessL
    pModuleAccessMember =
      ModuleAccessMember' <$> pSort @HidReservedIdentifierL "hid_reserved_identifier"

pHidModuleIdentifier :: TermParser HidModuleIdentifierL
pHidModuleIdentifier =
  HidModuleIdentifier' <$> pSort @IdentifierL "identifier"

pIdentifier :: TermParser IdentifierL
pIdentifier =
  Identifier' <$> pContent

pHidReservedIdentifier :: TermParser HidReservedIdentifierL
pHidReservedIdentifier =
  choice [ Megaparsec.try pHidReservedIdentifierHidForall
         , Megaparsec.try pHidReservedIdentifierHidExists
         ]
  where
    pHidReservedIdentifierHidForall :: TermParser HidReservedIdentifierL
    pHidReservedIdentifierHidForall =
      HidReservedIdentifierHidForall' <$> pSort @HidForallL "hid_forall"
    pHidReservedIdentifierHidExists :: TermParser HidReservedIdentifierL
    pHidReservedIdentifierHidExists =
      HidReservedIdentifierHidExists' <$> pSort @HidExistsL "hid_exists"

pHidExists :: TermParser HidExistsL
pHidExists =
  HidExists' <$> pSort @ExistsTokL "exists_tok"

pHidForall :: TermParser HidForallL
pHidForall =
  HidForall' <$> pSort @ForallTokL "forall_tok"

pModuleIdentity :: TermParser ModuleIdentityL
pModuleIdentity =
  ModuleIdentity' <$> pSort @ModuleIdentityInternal0L "module_identity_internal0" <*> pSort @ColonColonTokL "::_tok" <*> pSort @HidModuleIdentifierL "hid_module_identifier"

pModuleIdentityInternal0 :: TermParser ModuleIdentityInternal0L
pModuleIdentityInternal0 =
  choice [ Megaparsec.try pModuleIdentityInternal0NumLiteral
         , Megaparsec.try pModuleIdentityInternal0HidModuleIdentifier
         ]
  where
    pModuleIdentityInternal0NumLiteral :: TermParser ModuleIdentityInternal0L
    pModuleIdentityInternal0NumLiteral =
      ModuleIdentityInternal0NumLiteral' <$> pSort @NumLiteralL "num_literal"
    pModuleIdentityInternal0HidModuleIdentifier :: TermParser ModuleIdentityInternal0L
    pModuleIdentityInternal0HidModuleIdentifier =
      ModuleIdentityInternal0HidModuleIdentifier' <$> pSort @HidModuleIdentifierL "hid_module_identifier"

pNumLiteral :: TermParser NumLiteralL
pNumLiteral =
  NumLiteral' <$> pContent <*> pMaybe (pSort @NumLiteralInternal0L "num_literal_internal0")

pNumLiteralInternal0 :: TermParser NumLiteralInternal0L
pNumLiteralInternal0 =
  choice [ Megaparsec.try pNumLiteralInternal0U8
         , Megaparsec.try pNumLiteralInternal0U16
         , Megaparsec.try pNumLiteralInternal0U32
         , Megaparsec.try pNumLiteralInternal0U64
         , Megaparsec.try pNumLiteralInternal0U128
         , Megaparsec.try pNumLiteralInternal0U256
         ]
  where
    pNumLiteralInternal0U8 :: TermParser NumLiteralInternal0L
    pNumLiteralInternal0U8 =
      NumLiteralInternal0U8' <$> pSort @U8TokL "u8_tok"
    pNumLiteralInternal0U16 :: TermParser NumLiteralInternal0L
    pNumLiteralInternal0U16 =
      NumLiteralInternal0U16' <$> pSort @U16TokL "u16_tok"
    pNumLiteralInternal0U32 :: TermParser NumLiteralInternal0L
    pNumLiteralInternal0U32 =
      NumLiteralInternal0U32' <$> pSort @U32TokL "u32_tok"
    pNumLiteralInternal0U64 :: TermParser NumLiteralInternal0L
    pNumLiteralInternal0U64 =
      NumLiteralInternal0U64' <$> pSort @U64TokL "u64_tok"
    pNumLiteralInternal0U128 :: TermParser NumLiteralInternal0L
    pNumLiteralInternal0U128 =
      NumLiteralInternal0U128' <$> pSort @U128TokL "u128_tok"
    pNumLiteralInternal0U256 :: TermParser NumLiteralInternal0L
    pNumLiteralInternal0U256 =
      NumLiteralInternal0U256' <$> pSort @U256TokL "u256_tok"

pTypeArguments :: TermParser TypeArgumentsL
pTypeArguments =
  TypeArguments' <$> pBetween (pSort @LessThanSignTokL "<_tok") (pSort @GreaterThanSignTokL ">_tok") (pSepBy1 (pSort @HidTypeL "hid_type") (pSort @CommaTokL ",_tok"))

pFunctionType :: TermParser FunctionTypeL
pFunctionType =
  FunctionType' <$> pSort @FunctionTypeParametersL "function_type_parameters" <*> pMaybe (pPair (pSort @HyphenMinusGreaterThanSignTokL "->_tok") (pSort @HidTypeL "hid_type"))

pFunctionTypeParameters :: TermParser FunctionTypeParametersL
pFunctionTypeParameters =
  FunctionTypeParameters' <$> pBetween (pSort @VerticalLineTokL "|_tok") (pSort @VerticalLineTokL "|_tok") (pSepBy (pSort @HidTypeL "hid_type") (pSort @CommaTokL ",_tok"))

pPrimitiveType :: TermParser PrimitiveTypeL
pPrimitiveType =
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
    pPrimitiveTypeU8 :: TermParser PrimitiveTypeL
    pPrimitiveTypeU8 =
      PrimitiveTypeU8' <$> pSort @U8TokL "u8_tok"
    pPrimitiveTypeU16 :: TermParser PrimitiveTypeL
    pPrimitiveTypeU16 =
      PrimitiveTypeU16' <$> pSort @U16TokL "u16_tok"
    pPrimitiveTypeU32 :: TermParser PrimitiveTypeL
    pPrimitiveTypeU32 =
      PrimitiveTypeU32' <$> pSort @U32TokL "u32_tok"
    pPrimitiveTypeU64 :: TermParser PrimitiveTypeL
    pPrimitiveTypeU64 =
      PrimitiveTypeU64' <$> pSort @U64TokL "u64_tok"
    pPrimitiveTypeU128 :: TermParser PrimitiveTypeL
    pPrimitiveTypeU128 =
      PrimitiveTypeU128' <$> pSort @U128TokL "u128_tok"
    pPrimitiveTypeU256 :: TermParser PrimitiveTypeL
    pPrimitiveTypeU256 =
      PrimitiveTypeU256' <$> pSort @U256TokL "u256_tok"
    pPrimitiveTypeBool :: TermParser PrimitiveTypeL
    pPrimitiveTypeBool =
      PrimitiveTypeBool' <$> pSort @BoolTokL "bool_tok"
    pPrimitiveTypeAddress :: TermParser PrimitiveTypeL
    pPrimitiveTypeAddress =
      PrimitiveTypeAddress' <$> pSort @AddressTokL "address_tok"
    pPrimitiveTypeSigner :: TermParser PrimitiveTypeL
    pPrimitiveTypeSigner =
      PrimitiveTypeSigner' <$> pSort @SignerTokL "signer_tok"
    pPrimitiveTypeBytearray :: TermParser PrimitiveTypeL
    pPrimitiveTypeBytearray =
      PrimitiveTypeBytearray' <$> pSort @BytearrayTokL "bytearray_tok"

pRefType :: TermParser RefTypeL
pRefType =
  RefType' <$> pSort @HidReferenceL "hid_reference" <*> pSort @HidTypeL "hid_type"

pTupleType :: TermParser TupleTypeL
pTupleType =
  TupleType' <$> pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSepBy (pSort @HidTypeL "hid_type") (pSort @CommaTokL ",_tok"))

pBlock :: TermParser BlockL
pBlock =
  Block' <$> pSort @LeftCurlyBracketTokL "{_tok" <*> pMany (pSort @UseDeclarationL "use_declaration") <*> pMany (pSort @BlockItemL "block_item") <*> pMaybe (pSort @HidExpressionL "hid_expression") <*> pSort @RightCurlyBracketTokL "}_tok"

pBlockItem :: TermParser BlockItemL
pBlockItem =
  BlockItem' <$> pSort @BlockItemInternal0L "block_item_internal0" <*> pSort @SemicolonTokL ";_tok"

pBlockItemInternal0 :: TermParser BlockItemInternal0L
pBlockItemInternal0 =
  choice [ Megaparsec.try pBlockItemInternal0HidExpression
         , Megaparsec.try pBlockItemInternal0LetStatement
         ]
  where
    pBlockItemInternal0HidExpression :: TermParser BlockItemInternal0L
    pBlockItemInternal0HidExpression =
      BlockItemInternal0HidExpression' <$> pSort @HidExpressionL "hid_expression"
    pBlockItemInternal0LetStatement :: TermParser BlockItemInternal0L
    pBlockItemInternal0LetStatement =
      BlockItemInternal0LetStatement' <$> pSort @LetStatementL "let_statement"

pLetStatement :: TermParser LetStatementL
pLetStatement =
  LetStatement' <$> pSort @LetTokL "let_tok" <*> pSort @BindListL "bind_list" <*> pMaybe (pPair (pSort @ColonTokL ":_tok") (pSort @HidTypeL "hid_type")) <*> pMaybe (pPair (pSort @EqualsSignTokL "=_tok") (pSort @HidExpressionL "hid_expression"))

pBindList :: TermParser BindListL
pBindList =
  choice [ Megaparsec.try pBindListHidBind
         , Megaparsec.try pBindListCommaBindList
         , Megaparsec.try pBindListOrBindList
         ]
  where
    pBindListHidBind :: TermParser BindListL
    pBindListHidBind =
      BindListHidBind' <$> pSort @HidBindL "hid_bind"
    pBindListCommaBindList :: TermParser BindListL
    pBindListCommaBindList =
      BindListCommaBindList' <$> pSort @CommaBindListL "comma_bind_list"
    pBindListOrBindList :: TermParser BindListL
    pBindListOrBindList =
      BindListOrBindList' <$> pSort @OrBindListL "or_bind_list"

pCommaBindList :: TermParser CommaBindListL
pCommaBindList =
  CommaBindList' <$> pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSepBy (pSort @HidBindL "hid_bind") (pSort @CommaTokL ",_tok"))

pHidBind :: TermParser HidBindL
pHidBind =
  choice [ Megaparsec.try pHidBindHidBindInternal0
         , Megaparsec.try pHidBindBindUnpack
         , Megaparsec.try pHidBindAtBind
         , Megaparsec.try pHidBindHidLiteralValue
         ]
  where
    pHidBindHidBindInternal0 :: TermParser HidBindL
    pHidBindHidBindInternal0 =
      HidBindHidBindInternal0' <$> pSort @HidBindInternal0L "hid_bind_internal0"
    pHidBindBindUnpack :: TermParser HidBindL
    pHidBindBindUnpack =
      HidBindBindUnpack' <$> pSort @BindUnpackL "bind_unpack"
    pHidBindAtBind :: TermParser HidBindL
    pHidBindAtBind =
      HidBindAtBind' <$> pSort @AtBindL "at_bind"
    pHidBindHidLiteralValue :: TermParser HidBindL
    pHidBindHidLiteralValue =
      HidBindHidLiteralValue' <$> pSort @HidLiteralValueL "hid_literal_value"

pAtBind :: TermParser AtBindL
pAtBind =
  AtBind' <$> pSort @HidVariableIdentifierL "hid_variable_identifier" <*> pSort @CommercialAtTokL "@_tok" <*> pSort @BindListL "bind_list"

pHidVariableIdentifier :: TermParser HidVariableIdentifierL
pHidVariableIdentifier =
  HidVariableIdentifier' <$> pSort @IdentifierL "identifier"

pBindUnpack :: TermParser BindUnpackL
pBindUnpack =
  BindUnpack' <$> pSort @NameExpressionL "name_expression" <*> pMaybe (pSort @BindFieldsL "bind_fields")

pBindFields :: TermParser BindFieldsL
pBindFields =
  choice [ Megaparsec.try pBindFieldsBindPositionalFields
         , Megaparsec.try pBindFieldsBindNamedFields
         ]
  where
    pBindFieldsBindPositionalFields :: TermParser BindFieldsL
    pBindFieldsBindPositionalFields =
      BindFieldsBindPositionalFields' <$> pSort @BindPositionalFieldsL "bind_positional_fields"
    pBindFieldsBindNamedFields :: TermParser BindFieldsL
    pBindFieldsBindNamedFields =
      BindFieldsBindNamedFields' <$> pSort @BindNamedFieldsL "bind_named_fields"

pBindNamedFields :: TermParser BindNamedFieldsL
pBindNamedFields =
  BindNamedFields' <$> pBetween (pSort @LeftCurlyBracketTokL "{_tok") (pSort @RightCurlyBracketTokL "}_tok") (pSepBy (pSort @BindNamedFieldsInternal0L "bind_named_fields_internal0") (pSort @CommaTokL ",_tok"))

pBindNamedFieldsInternal0 :: TermParser BindNamedFieldsInternal0L
pBindNamedFieldsInternal0 =
  choice [ Megaparsec.try pBindNamedFieldsInternal0BindField
         , Megaparsec.try pBindNamedFieldsInternal0MutBindField
         ]
  where
    pBindNamedFieldsInternal0BindField :: TermParser BindNamedFieldsInternal0L
    pBindNamedFieldsInternal0BindField =
      BindNamedFieldsInternal0BindField' <$> pSort @BindFieldL "bind_field"
    pBindNamedFieldsInternal0MutBindField :: TermParser BindNamedFieldsInternal0L
    pBindNamedFieldsInternal0MutBindField =
      BindNamedFieldsInternal0MutBindField' <$> pSort @MutBindFieldL "mut_bind_field"

pBindField :: TermParser BindFieldL
pBindField =
  choice [ Megaparsec.try pBindField1
         , Megaparsec.try pBindFieldHidSpreadOperator
         ]
  where
    pBindField1 :: TermParser BindFieldL
    pBindField1 =
      BindField1' <$> pSort @BindListL "bind_list" <*> pMaybe (pPair (pSort @ColonTokL ":_tok") (pSort @BindListL "bind_list"))
    pBindFieldHidSpreadOperator :: TermParser BindFieldL
    pBindFieldHidSpreadOperator =
      BindFieldHidSpreadOperator' <$> pSort @HidSpreadOperatorL "hid_spread_operator"

pHidSpreadOperator :: TermParser HidSpreadOperatorL
pHidSpreadOperator =
  HidSpreadOperator' <$> pSort @FullStopFullStopTokL ".._tok"

pMutBindField :: TermParser MutBindFieldL
pMutBindField =
  MutBindField' <$> pSort @MutTokL "mut_tok" <*> pSort @BindFieldL "bind_field"

pBindPositionalFields :: TermParser BindPositionalFieldsL
pBindPositionalFields =
  BindPositionalFields' <$> pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSepBy (pSort @BindNamedFieldsInternal0L "bind_named_fields_internal0") (pSort @CommaTokL ",_tok"))

pNameExpression :: TermParser NameExpressionL
pNameExpression =
  NameExpression' <$> pMaybe (pSort @ColonColonTokL "::_tok") <*> pSort @ModuleAccessL "module_access"

pHidBindInternal0 :: TermParser HidBindInternal0L
pHidBindInternal0 =
  choice [ Megaparsec.try pHidBindInternal0MutBindVar
         , Megaparsec.try pHidBindInternal0HidVariableIdentifier
         ]
  where
    pHidBindInternal0MutBindVar :: TermParser HidBindInternal0L
    pHidBindInternal0MutBindVar =
      HidBindInternal0MutBindVar' <$> pSort @MutBindVarL "mut_bind_var"
    pHidBindInternal0HidVariableIdentifier :: TermParser HidBindInternal0L
    pHidBindInternal0HidVariableIdentifier =
      HidBindInternal0HidVariableIdentifier' <$> pSort @HidVariableIdentifierL "hid_variable_identifier"

pMutBindVar :: TermParser MutBindVarL
pMutBindVar =
  MutBindVar' <$> pSort @MutTokL "mut_tok" <*> pSort @HidVariableIdentifierL "hid_variable_identifier"

pHidLiteralValue :: TermParser HidLiteralValueL
pHidLiteralValue =
  choice [ Megaparsec.try pHidLiteralValueAddressLiteral
         , Megaparsec.try pHidLiteralValueBoolLiteral
         , Megaparsec.try pHidLiteralValueNumLiteral
         , Megaparsec.try pHidLiteralValueHexStringLiteral
         , Megaparsec.try pHidLiteralValueByteStringLiteral
         ]
  where
    pHidLiteralValueAddressLiteral :: TermParser HidLiteralValueL
    pHidLiteralValueAddressLiteral =
      HidLiteralValueAddressLiteral' <$> pSort @AddressLiteralL "address_literal"
    pHidLiteralValueBoolLiteral :: TermParser HidLiteralValueL
    pHidLiteralValueBoolLiteral =
      HidLiteralValueBoolLiteral' <$> pSort @BoolLiteralL "bool_literal"
    pHidLiteralValueNumLiteral :: TermParser HidLiteralValueL
    pHidLiteralValueNumLiteral =
      HidLiteralValueNumLiteral' <$> pSort @NumLiteralL "num_literal"
    pHidLiteralValueHexStringLiteral :: TermParser HidLiteralValueL
    pHidLiteralValueHexStringLiteral =
      HidLiteralValueHexStringLiteral' <$> pSort @HexStringLiteralL "hex_string_literal"
    pHidLiteralValueByteStringLiteral :: TermParser HidLiteralValueL
    pHidLiteralValueByteStringLiteral =
      HidLiteralValueByteStringLiteral' <$> pSort @ByteStringLiteralL "byte_string_literal"

pAddressLiteral :: TermParser AddressLiteralL
pAddressLiteral =
  AddressLiteral' <$> pContent

pBoolLiteral :: TermParser BoolLiteralL
pBoolLiteral =
  choice [ Megaparsec.try pBoolLiteralTrue
         , Megaparsec.try pBoolLiteralFalse
         ]
  where
    pBoolLiteralTrue :: TermParser BoolLiteralL
    pBoolLiteralTrue =
      BoolLiteralTrue' <$> pSort @TrueTokL "true_tok"
    pBoolLiteralFalse :: TermParser BoolLiteralL
    pBoolLiteralFalse =
      BoolLiteralFalse' <$> pSort @FalseTokL "false_tok"

pByteStringLiteral :: TermParser ByteStringLiteralL
pByteStringLiteral =
  ByteStringLiteral' <$> pContent

pHexStringLiteral :: TermParser HexStringLiteralL
pHexStringLiteral =
  HexStringLiteral' <$> pContent

pOrBindList :: TermParser OrBindListL
pOrBindList =
  OrBindList' <$> pMaybe (pSort @LeftParenthesisTokL "(_tok") <*> pSepBy1 (pPair (pPair (pMaybe (pSort @LeftParenthesisTokL "(_tok")) (pSort @HidBindL "hid_bind")) (pMaybe (pSort @RightParenthesisTokL ")_tok"))) (pSort @VerticalLineTokL "|_tok") <*> pMaybe (pSort @RightParenthesisTokL ")_tok")

pUseDeclaration :: TermParser UseDeclarationL
pUseDeclaration =
  UseDeclaration' <$> pMaybe (pSort @PublicTokL "public_tok") <*> pBetween (pSort @UseTokL "use_tok") (pSort @SemicolonTokL ";_tok") (pSort @UseDeclarationInternal0L "use_declaration_internal0")

pUseDeclarationInternal0 :: TermParser UseDeclarationInternal0L
pUseDeclarationInternal0 =
  choice [ Megaparsec.try pUseDeclarationInternal0UseFun
         , Megaparsec.try pUseDeclarationInternal0UseModule
         , Megaparsec.try pUseDeclarationInternal0UseModuleMember
         , Megaparsec.try pUseDeclarationInternal0UseModuleMembers
         ]
  where
    pUseDeclarationInternal0UseFun :: TermParser UseDeclarationInternal0L
    pUseDeclarationInternal0UseFun =
      UseDeclarationInternal0UseFun' <$> pSort @UseFunL "use_fun"
    pUseDeclarationInternal0UseModule :: TermParser UseDeclarationInternal0L
    pUseDeclarationInternal0UseModule =
      UseDeclarationInternal0UseModule' <$> pSort @UseModuleL "use_module"
    pUseDeclarationInternal0UseModuleMember :: TermParser UseDeclarationInternal0L
    pUseDeclarationInternal0UseModuleMember =
      UseDeclarationInternal0UseModuleMember' <$> pSort @UseModuleMemberL "use_module_member"
    pUseDeclarationInternal0UseModuleMembers :: TermParser UseDeclarationInternal0L
    pUseDeclarationInternal0UseModuleMembers =
      UseDeclarationInternal0UseModuleMembers' <$> pSort @UseModuleMembersL "use_module_members"

pUseFun :: TermParser UseFunL
pUseFun =
  UseFun' <$> pBetween (pSort @FunTokL "fun_tok") (pSort @AsTokL "as_tok") (pSort @ModuleAccessL "module_access") <*> pPair (pPair (pSort @ModuleAccessL "module_access") (pSort @FullStopTokL "._tok")) (pSort @HidFunctionIdentifierL "hid_function_identifier")

pHidFunctionIdentifier :: TermParser HidFunctionIdentifierL
pHidFunctionIdentifier =
  HidFunctionIdentifier' <$> pSort @IdentifierL "identifier"

pUseModule :: TermParser UseModuleL
pUseModule =
  UseModule' <$> pSort @ModuleIdentityL "module_identity" <*> pMaybe (pPair (pSort @AsTokL "as_tok") (pSort @HidModuleIdentifierL "hid_module_identifier"))

pUseModuleMember :: TermParser UseModuleMemberL
pUseModuleMember =
  UseModuleMember' <$> pSort @ModuleIdentityL "module_identity" <*> pSort @ColonColonTokL "::_tok" <*> pSort @UseMemberL "use_member"

pUseMember :: TermParser UseMemberL
pUseMember =
  choice [ Megaparsec.try pUseMember1
         , Megaparsec.try pUseMember2
         , Megaparsec.try pUseMember3
         ]
  where
    pUseMember1 :: TermParser UseMemberL
    pUseMember1 =
      UseMember1' <$> pSort @IdentifierL "identifier" <*> pSort @ColonColonTokL "::_tok" <*> pBetween (pSort @LeftCurlyBracketTokL "{_tok") (pSort @RightCurlyBracketTokL "}_tok") (pSepBy1 (pSort @UseMemberL "use_member") (pSort @CommaTokL ",_tok"))
    pUseMember2 :: TermParser UseMemberL
    pUseMember2 =
      UseMember2' <$> pSort @IdentifierL "identifier" <*> pSort @ColonColonTokL "::_tok" <*> pSort @IdentifierL "identifier" <*> pMaybe (pPair (pSort @AsTokL "as_tok") (pSort @IdentifierL "identifier"))
    pUseMember3 :: TermParser UseMemberL
    pUseMember3 =
      UseMember3' <$> pSort @IdentifierL "identifier" <*> pMaybe (pPair (pSort @AsTokL "as_tok") (pSort @IdentifierL "identifier"))

pUseModuleMembers :: TermParser UseModuleMembersL
pUseModuleMembers =
  choice [ Megaparsec.try pUseModuleMembers1
         , Megaparsec.try pUseModuleMembers2
         ]
  where
    pUseModuleMembers1 :: TermParser UseModuleMembersL
    pUseModuleMembers1 =
      UseModuleMembers1' <$> pSort @ModuleIdentityInternal0L "module_identity_internal0" <*> pSort @ColonColonTokL "::_tok" <*> pBetween (pSort @LeftCurlyBracketTokL "{_tok") (pSort @RightCurlyBracketTokL "}_tok") (pSepBy1 (pSort @UseMemberL "use_member") (pSort @CommaTokL ",_tok"))
    pUseModuleMembers2 :: TermParser UseModuleMembersL
    pUseModuleMembers2 =
      UseModuleMembers2' <$> pSort @ModuleIdentityL "module_identity" <*> pSort @ColonColonTokL "::_tok" <*> pBetween (pSort @LeftCurlyBracketTokL "{_tok") (pSort @RightCurlyBracketTokL "}_tok") (pSepBy1 (pSort @UseMemberL "use_member") (pSort @CommaTokL ",_tok"))

pBreakExpression :: TermParser BreakExpressionL
pBreakExpression =
  BreakExpression' <$> pSort @BreakTokL "break_tok" <*> pMaybe (pSort @LabelL "label") <*> pMaybe (pSort @HidExpressionL "hid_expression")

pLabel :: TermParser LabelL
pLabel =
  Label' <$> pSort @ApostropheTokL "'_tok" <*> pSort @IdentifierL "identifier"

pCallExpression :: TermParser CallExpressionL
pCallExpression =
  CallExpression' <$> pPair (pSort @NameExpressionL "name_expression") (pSort @ArgListL "arg_list")

pArgList :: TermParser ArgListL
pArgList =
  ArgList' <$> pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSepBy (pSort @HidExpressionL "hid_expression") (pSort @CommaTokL ",_tok"))

pContinueExpression :: TermParser ContinueExpressionL
pContinueExpression =
  ContinueExpression' <$> pSort @ContinueTokL "continue_tok" <*> pMaybe (pSort @LabelL "label")

pDotExpression :: TermParser DotExpressionL
pDotExpression =
  DotExpression' <$> pPair (pPair (pSort @HidExpressionTermL "hid_expression_term") (pSort @FullStopTokL "._tok")) (pSort @HidExpressionTermL "hid_expression_term")

pExpressionList :: TermParser ExpressionListL
pExpressionList =
  ExpressionList' <$> pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSepBy1 (pSort @HidExpressionL "hid_expression") (pSort @CommaTokL ",_tok"))

pIfExpression :: TermParser IfExpressionL
pIfExpression =
  IfExpression' <$> pPair (pPair (pPair (pSort @IfTokL "if_tok") (pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSort @HidExpressionL "hid_expression"))) (pSort @HidExpressionL "hid_expression")) (pMaybe (pPair (pSort @ElseTokL "else_tok") (pSort @HidExpressionL "hid_expression")))

pIndexExpression :: TermParser IndexExpressionL
pIndexExpression =
  IndexExpression' <$> pPair (pSort @HidExpressionTermL "hid_expression_term") (pBetween (pSort @LeftSquareBracketTokL "[_tok") (pSort @RightSquareBracketTokL "]_tok") (pSepBy (pSort @HidExpressionL "hid_expression") (pSort @CommaTokL ",_tok")))

pMacroCallExpression :: TermParser MacroCallExpressionL
pMacroCallExpression =
  MacroCallExpression' <$> pSort @MacroModuleAccessL "macro_module_access" <*> pMaybe (pSort @TypeArgumentsL "type_arguments") <*> pSort @ArgListL "arg_list"

pMacroModuleAccess :: TermParser MacroModuleAccessL
pMacroModuleAccess =
  MacroModuleAccess' <$> pSort @ModuleAccessL "module_access" <*> pSort @ExclamationMarkTokL "!_tok"

pMatchExpression :: TermParser MatchExpressionL
pMatchExpression =
  MatchExpression' <$> pSort @MatchTokL "match_tok" <*> pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSort @HidExpressionL "hid_expression") <*> pSort @HidMatchBodyL "hid_match_body"

pHidMatchBody :: TermParser HidMatchBodyL
pHidMatchBody =
  HidMatchBody' <$> pBetween (pSort @LeftCurlyBracketTokL "{_tok") (pSort @RightCurlyBracketTokL "}_tok") (pSepBy (pSort @MatchArmL "match_arm") (pSort @CommaTokL ",_tok"))

pMatchArm :: TermParser MatchArmL
pMatchArm =
  MatchArm' <$> pSort @BindListL "bind_list" <*> pMaybe (pSort @MatchConditionL "match_condition") <*> pSort @EqualsSignGreaterThanSignTokL "=>_tok" <*> pSort @HidExpressionL "hid_expression"

pMatchCondition :: TermParser MatchConditionL
pMatchCondition =
  MatchCondition' <$> pSort @IfTokL "if_tok" <*> pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSort @HidExpressionL "hid_expression")

pPackExpression :: TermParser PackExpressionL
pPackExpression =
  PackExpression' <$> pSort @NameExpressionL "name_expression" <*> pSort @FieldInitializeListL "field_initialize_list"

pFieldInitializeList :: TermParser FieldInitializeListL
pFieldInitializeList =
  FieldInitializeList' <$> pBetween (pSort @LeftCurlyBracketTokL "{_tok") (pSort @RightCurlyBracketTokL "}_tok") (pSepBy (pSort @ExpFieldL "exp_field") (pSort @CommaTokL ",_tok"))

pExpField :: TermParser ExpFieldL
pExpField =
  ExpField' <$> pSort @HidFieldIdentifierL "hid_field_identifier" <*> pMaybe (pPair (pSort @ColonTokL ":_tok") (pSort @HidExpressionL "hid_expression"))

pHidFieldIdentifier :: TermParser HidFieldIdentifierL
pHidFieldIdentifier =
  HidFieldIdentifier' <$> pSort @IdentifierL "identifier"

pSpecBlock :: TermParser SpecBlockL
pSpecBlock =
  SpecBlock' <$> pSort @SpecTokL "spec_tok" <*> pSort @SpecBlockInternal0L "spec_block_internal0"

pSpecBlockInternal0 :: TermParser SpecBlockInternal0L
pSpecBlockInternal0 =
  choice [ Megaparsec.try pSpecBlockInternal01
         , Megaparsec.try pSpecBlockInternal0HidSpecFunction
         ]
  where
    pSpecBlockInternal01 :: TermParser SpecBlockInternal0L
    pSpecBlockInternal01 =
      SpecBlockInternal01' <$> pMaybe (pSort @HidSpecBlockTargetL "hid_spec_block_target") <*> pSort @SpecBodyL "spec_body"
    pSpecBlockInternal0HidSpecFunction :: TermParser SpecBlockInternal0L
    pSpecBlockInternal0HidSpecFunction =
      SpecBlockInternal0HidSpecFunction' <$> pSort @HidSpecFunctionL "hid_spec_function"

pHidSpecBlockTarget :: TermParser HidSpecBlockTargetL
pHidSpecBlockTarget =
  choice [ Megaparsec.try pHidSpecBlockTargetIdentifier
         , Megaparsec.try pHidSpecBlockTargetModule
         , Megaparsec.try pHidSpecBlockTargetSpecBlockTargetSchema
         ]
  where
    pHidSpecBlockTargetIdentifier :: TermParser HidSpecBlockTargetL
    pHidSpecBlockTargetIdentifier =
      HidSpecBlockTargetIdentifier' <$> pSort @IdentifierL "identifier"
    pHidSpecBlockTargetModule :: TermParser HidSpecBlockTargetL
    pHidSpecBlockTargetModule =
      HidSpecBlockTargetModule' <$> pSort @ModuleTokL "module_tok"
    pHidSpecBlockTargetSpecBlockTargetSchema :: TermParser HidSpecBlockTargetL
    pHidSpecBlockTargetSpecBlockTargetSchema =
      HidSpecBlockTargetSpecBlockTargetSchema' <$> pSort @SpecBlockTargetSchemaL "spec_block_target_schema"

pSpecBlockTargetSchema :: TermParser SpecBlockTargetSchemaL
pSpecBlockTargetSchema =
  SpecBlockTargetSchema' <$> pSort @SchemaTokL "schema_tok" <*> pSort @HidStructIdentifierL "hid_struct_identifier" <*> pMaybe (pSort @TypeParametersL "type_parameters")

pHidStructIdentifier :: TermParser HidStructIdentifierL
pHidStructIdentifier =
  HidStructIdentifier' <$> pSort @IdentifierL "identifier"

pTypeParameters :: TermParser TypeParametersL
pTypeParameters =
  TypeParameters' <$> pBetween (pSort @LessThanSignTokL "<_tok") (pSort @GreaterThanSignTokL ">_tok") (pSepBy1 (pSort @TypeParameterL "type_parameter") (pSort @CommaTokL ",_tok"))

pTypeParameter :: TermParser TypeParameterL
pTypeParameter =
  TypeParameter' <$> pMaybe (pSort @DollarSignTokL "$_tok") <*> pMaybe (pSort @PhantomTokL "phantom_tok") <*> pSort @HidTypeParameterIdentifierL "hid_type_parameter_identifier" <*> pMaybe (pPair (pSort @ColonTokL ":_tok") (pSepBy1 (pSort @AbilityL "ability") (pSort @PlusSignTokL "+_tok")))

pAbility :: TermParser AbilityL
pAbility =
  choice [ Megaparsec.try pAbilityCopy
         , Megaparsec.try pAbilityDrop
         , Megaparsec.try pAbilityStore
         , Megaparsec.try pAbilityKey
         ]
  where
    pAbilityCopy :: TermParser AbilityL
    pAbilityCopy =
      AbilityCopy' <$> pSort @CopyTokL "copy_tok"
    pAbilityDrop :: TermParser AbilityL
    pAbilityDrop =
      AbilityDrop' <$> pSort @DropTokL "drop_tok"
    pAbilityStore :: TermParser AbilityL
    pAbilityStore =
      AbilityStore' <$> pSort @StoreTokL "store_tok"
    pAbilityKey :: TermParser AbilityL
    pAbilityKey =
      AbilityKey' <$> pSort @KeyTokL "key_tok"

pHidTypeParameterIdentifier :: TermParser HidTypeParameterIdentifierL
pHidTypeParameterIdentifier =
  HidTypeParameterIdentifier' <$> pSort @IdentifierL "identifier"

pHidSpecFunction :: TermParser HidSpecFunctionL
pHidSpecFunction =
  choice [ Megaparsec.try pHidSpecFunctionNativeSpecFunction
         , Megaparsec.try pHidSpecFunctionUsualSpecFunction
         , Megaparsec.try pHidSpecFunctionUninterpretedSpecFunction
         ]
  where
    pHidSpecFunctionNativeSpecFunction :: TermParser HidSpecFunctionL
    pHidSpecFunctionNativeSpecFunction =
      HidSpecFunctionNativeSpecFunction' <$> pSort @NativeSpecFunctionL "native_spec_function"
    pHidSpecFunctionUsualSpecFunction :: TermParser HidSpecFunctionL
    pHidSpecFunctionUsualSpecFunction =
      HidSpecFunctionUsualSpecFunction' <$> pSort @UsualSpecFunctionL "usual_spec_function"
    pHidSpecFunctionUninterpretedSpecFunction :: TermParser HidSpecFunctionL
    pHidSpecFunctionUninterpretedSpecFunction =
      HidSpecFunctionUninterpretedSpecFunction' <$> pSort @UninterpretedSpecFunctionL "uninterpreted_spec_function"

pNativeSpecFunction :: TermParser NativeSpecFunctionL
pNativeSpecFunction =
  NativeSpecFunction' <$> pSort @NativeTokL "native_tok" <*> pBetween (pSort @FunTokL "fun_tok") (pSort @SemicolonTokL ";_tok") (pSort @HidSpecFunctionSignatureL "hid_spec_function_signature")

pHidSpecFunctionSignature :: TermParser HidSpecFunctionSignatureL
pHidSpecFunctionSignature =
  HidSpecFunctionSignature' <$> pSort @HidFunctionIdentifierL "hid_function_identifier" <*> pMaybe (pSort @TypeParametersL "type_parameters") <*> pSort @FunctionParametersL "function_parameters" <*> pSort @RetTypeL "ret_type"

pFunctionParameters :: TermParser FunctionParametersL
pFunctionParameters =
  FunctionParameters' <$> pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSepBy (pSort @FunctionParametersInternal0L "function_parameters_internal0") (pSort @CommaTokL ",_tok"))

pFunctionParametersInternal0 :: TermParser FunctionParametersInternal0L
pFunctionParametersInternal0 =
  choice [ Megaparsec.try pFunctionParametersInternal0MutFunctionParameter
         , Megaparsec.try pFunctionParametersInternal0FunctionParameter
         ]
  where
    pFunctionParametersInternal0MutFunctionParameter :: TermParser FunctionParametersInternal0L
    pFunctionParametersInternal0MutFunctionParameter =
      FunctionParametersInternal0MutFunctionParameter' <$> pSort @MutFunctionParameterL "mut_function_parameter"
    pFunctionParametersInternal0FunctionParameter :: TermParser FunctionParametersInternal0L
    pFunctionParametersInternal0FunctionParameter =
      FunctionParametersInternal0FunctionParameter' <$> pSort @FunctionParameterL "function_parameter"

pFunctionParameter :: TermParser FunctionParameterL
pFunctionParameter =
  FunctionParameter' <$> pSort @FunctionParameterInternal0L "function_parameter_internal0" <*> pSort @ColonTokL ":_tok" <*> pSort @HidTypeL "hid_type"

pFunctionParameterInternal0 :: TermParser FunctionParameterInternal0L
pFunctionParameterInternal0 =
  choice [ Megaparsec.try pFunctionParameterInternal0Name
         , Megaparsec.try pFunctionParameterInternal02
         ]
  where
    pFunctionParameterInternal0Name :: TermParser FunctionParameterInternal0L
    pFunctionParameterInternal0Name =
      FunctionParameterInternal0Name' <$> pSort @HidVariableIdentifierL "hid_variable_identifier"
    pFunctionParameterInternal02 :: TermParser FunctionParameterInternal0L
    pFunctionParameterInternal02 =
      FunctionParameterInternal02' <$> pSort @DollarSignTokL "$_tok" <*> pSort @HidVariableIdentifierL "hid_variable_identifier"

pMutFunctionParameter :: TermParser MutFunctionParameterL
pMutFunctionParameter =
  MutFunctionParameter' <$> pSort @MutTokL "mut_tok" <*> pSort @FunctionParameterL "function_parameter"

pRetType :: TermParser RetTypeL
pRetType =
  RetType' <$> pSort @ColonTokL ":_tok" <*> pSort @HidTypeL "hid_type"

pUninterpretedSpecFunction :: TermParser UninterpretedSpecFunctionL
pUninterpretedSpecFunction =
  UninterpretedSpecFunction' <$> pBetween (pSort @FunTokL "fun_tok") (pSort @SemicolonTokL ";_tok") (pSort @HidSpecFunctionSignatureL "hid_spec_function_signature")

pUsualSpecFunction :: TermParser UsualSpecFunctionL
pUsualSpecFunction =
  UsualSpecFunction' <$> pSort @FunTokL "fun_tok" <*> pSort @HidSpecFunctionSignatureL "hid_spec_function_signature" <*> pSort @BlockL "block"

pSpecBody :: TermParser SpecBodyL
pSpecBody =
  SpecBody' <$> pSort @LeftCurlyBracketTokL "{_tok" <*> pMany (pSort @UseDeclarationL "use_declaration") <*> pMany (pSort @HidSpecBlockMemeberL "hid_spec_block_memeber") <*> pSort @RightCurlyBracketTokL "}_tok"

pHidSpecBlockMemeber :: TermParser HidSpecBlockMemeberL
pHidSpecBlockMemeber =
  choice [ Megaparsec.try pHidSpecBlockMemeberSpecInvariant
         , Megaparsec.try pHidSpecBlockMemeberHidSpecFunction
         , Megaparsec.try pHidSpecBlockMemeberSpecCondition
         , Megaparsec.try pHidSpecBlockMemeberSpecInclude
         , Megaparsec.try pHidSpecBlockMemeberSpecApply
         , Megaparsec.try pHidSpecBlockMemeberSpecPragma
         , Megaparsec.try pHidSpecBlockMemeberSpecVariable
         , Megaparsec.try pHidSpecBlockMemeberSpecLet
         ]
  where
    pHidSpecBlockMemeberSpecInvariant :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecInvariant =
      HidSpecBlockMemeberSpecInvariant' <$> pSort @SpecInvariantL "spec_invariant"
    pHidSpecBlockMemeberHidSpecFunction :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberHidSpecFunction =
      HidSpecBlockMemeberHidSpecFunction' <$> pSort @HidSpecFunctionL "hid_spec_function"
    pHidSpecBlockMemeberSpecCondition :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecCondition =
      HidSpecBlockMemeberSpecCondition' <$> pSort @SpecConditionL "spec_condition"
    pHidSpecBlockMemeberSpecInclude :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecInclude =
      HidSpecBlockMemeberSpecInclude' <$> pSort @SpecIncludeL "spec_include"
    pHidSpecBlockMemeberSpecApply :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecApply =
      HidSpecBlockMemeberSpecApply' <$> pSort @SpecApplyL "spec_apply"
    pHidSpecBlockMemeberSpecPragma :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecPragma =
      HidSpecBlockMemeberSpecPragma' <$> pSort @SpecPragmaL "spec_pragma"
    pHidSpecBlockMemeberSpecVariable :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecVariable =
      HidSpecBlockMemeberSpecVariable' <$> pSort @SpecVariableL "spec_variable"
    pHidSpecBlockMemeberSpecLet :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecLet =
      HidSpecBlockMemeberSpecLet' <$> pSort @SpecLetL "spec_let"

pSpecApply :: TermParser SpecApplyL
pSpecApply =
  SpecApply' <$> pBetween (pSort @ApplyTokL "apply_tok") (pSort @ToTokL "to_tok") (pSort @HidExpressionL "hid_expression") <*> pSepBy1 (pSort @SpecApplyPatternL "spec_apply_pattern") (pSort @CommaTokL ",_tok") <*> pMaybe (pPair (pSort @ExceptTokL "except_tok") (pSepBy1 (pSort @SpecApplyPatternL "spec_apply_pattern") (pSort @CommaTokL ",_tok"))) <*> pSort @SemicolonTokL ";_tok"

pSpecApplyPattern :: TermParser SpecApplyPatternL
pSpecApplyPattern =
  SpecApplyPattern' <$> pMaybe (pSort @SpecApplyPatternInternal0L "spec_apply_pattern_internal0") <*> pSort @SpecApplyNamePatternL "spec_apply_name_pattern" <*> pMaybe (pSort @TypeParametersL "type_parameters")

pSpecApplyNamePattern :: TermParser SpecApplyNamePatternL
pSpecApplyNamePattern =
  SpecApplyNamePattern' <$> pContent

pSpecApplyPatternInternal0 :: TermParser SpecApplyPatternInternal0L
pSpecApplyPatternInternal0 =
  choice [ Megaparsec.try pSpecApplyPatternInternal0Public
         , Megaparsec.try pSpecApplyPatternInternal0Internal
         ]
  where
    pSpecApplyPatternInternal0Public :: TermParser SpecApplyPatternInternal0L
    pSpecApplyPatternInternal0Public =
      SpecApplyPatternInternal0Public' <$> pSort @PublicTokL "public_tok"
    pSpecApplyPatternInternal0Internal :: TermParser SpecApplyPatternInternal0L
    pSpecApplyPatternInternal0Internal =
      SpecApplyPatternInternal0Internal' <$> pSort @InternalTokL "internal_tok"

pSpecCondition :: TermParser SpecConditionL
pSpecCondition =
  choice [ Megaparsec.try pSpecConditionHidSpecCondition
         , Megaparsec.try pSpecConditionHidSpecAbortIf
         , Megaparsec.try pSpecConditionHidSpecAbortWithOrModifies
         ]
  where
    pSpecConditionHidSpecCondition :: TermParser SpecConditionL
    pSpecConditionHidSpecCondition =
      SpecConditionHidSpecCondition' <$> pSort @HidSpecConditionL "hid_spec_condition"
    pSpecConditionHidSpecAbortIf :: TermParser SpecConditionL
    pSpecConditionHidSpecAbortIf =
      SpecConditionHidSpecAbortIf' <$> pSort @HidSpecAbortIfL "hid_spec_abort_if"
    pSpecConditionHidSpecAbortWithOrModifies :: TermParser SpecConditionL
    pSpecConditionHidSpecAbortWithOrModifies =
      SpecConditionHidSpecAbortWithOrModifies' <$> pSort @HidSpecAbortWithOrModifiesL "hid_spec_abort_with_or_modifies"

pHidSpecAbortIf :: TermParser HidSpecAbortIfL
pHidSpecAbortIf =
  HidSpecAbortIf' <$> pSort @AbortsIfTokL "aborts_if_tok" <*> pMaybe (pSort @ConditionPropertiesL "condition_properties") <*> pSort @HidExpressionL "hid_expression" <*> pMaybe (pPair (pSort @WithTokL "with_tok") (pSort @HidExpressionL "hid_expression")) <*> pSort @SemicolonTokL ";_tok"

pConditionProperties :: TermParser ConditionPropertiesL
pConditionProperties =
  ConditionProperties' <$> pBetween (pSort @LeftSquareBracketTokL "[_tok") (pSort @RightSquareBracketTokL "]_tok") (pSepBy (pSort @SpecPropertyL "spec_property") (pSort @CommaTokL ",_tok"))

pSpecProperty :: TermParser SpecPropertyL
pSpecProperty =
  SpecProperty' <$> pSort @IdentifierL "identifier" <*> pMaybe (pPair (pSort @EqualsSignTokL "=_tok") (pSort @HidLiteralValueL "hid_literal_value"))

pHidSpecAbortWithOrModifies :: TermParser HidSpecAbortWithOrModifiesL
pHidSpecAbortWithOrModifies =
  HidSpecAbortWithOrModifies' <$> pSort @HidSpecAbortWithOrModifiesInternal0L "hid_spec_abort_with_or_modifies_internal0" <*> pMaybe (pSort @ConditionPropertiesL "condition_properties") <*> pSepBy1 (pSort @HidExpressionL "hid_expression") (pSort @CommaTokL ",_tok") <*> pSort @SemicolonTokL ";_tok"

pHidSpecAbortWithOrModifiesInternal0 :: TermParser HidSpecAbortWithOrModifiesInternal0L
pHidSpecAbortWithOrModifiesInternal0 =
  choice [ Megaparsec.try pHidSpecAbortWithOrModifiesInternal0AbortsWith
         , Megaparsec.try pHidSpecAbortWithOrModifiesInternal0Modifies
         ]
  where
    pHidSpecAbortWithOrModifiesInternal0AbortsWith :: TermParser HidSpecAbortWithOrModifiesInternal0L
    pHidSpecAbortWithOrModifiesInternal0AbortsWith =
      HidSpecAbortWithOrModifiesInternal0AbortsWith' <$> pSort @AbortsWithTokL "aborts_with_tok"
    pHidSpecAbortWithOrModifiesInternal0Modifies :: TermParser HidSpecAbortWithOrModifiesInternal0L
    pHidSpecAbortWithOrModifiesInternal0Modifies =
      HidSpecAbortWithOrModifiesInternal0Modifies' <$> pSort @ModifiesTokL "modifies_tok"

pHidSpecCondition :: TermParser HidSpecConditionL
pHidSpecCondition =
  HidSpecCondition' <$> pSort @HidSpecConditionInternal0L "hid_spec_condition_internal0" <*> pMaybe (pSort @ConditionPropertiesL "condition_properties") <*> pSort @HidExpressionL "hid_expression" <*> pSort @SemicolonTokL ";_tok"

pHidSpecConditionInternal0 :: TermParser HidSpecConditionInternal0L
pHidSpecConditionInternal0 =
  choice [ Megaparsec.try pHidSpecConditionInternal0Kind
         , Megaparsec.try pHidSpecConditionInternal02
         ]
  where
    pHidSpecConditionInternal0Kind :: TermParser HidSpecConditionInternal0L
    pHidSpecConditionInternal0Kind =
      HidSpecConditionInternal0Kind' <$> pSort @HidSpecConditionKindL "hid_spec_condition_kind"
    pHidSpecConditionInternal02 :: TermParser HidSpecConditionInternal0L
    pHidSpecConditionInternal02 =
      HidSpecConditionInternal02' <$> pSort @RequiresTokL "requires_tok" <*> pMaybe (pSort @ModuleTokL "module_tok")

pHidSpecConditionKind :: TermParser HidSpecConditionKindL
pHidSpecConditionKind =
  choice [ Megaparsec.try pHidSpecConditionKindAssert
         , Megaparsec.try pHidSpecConditionKindAssume
         , Megaparsec.try pHidSpecConditionKindDecreases
         , Megaparsec.try pHidSpecConditionKindEnsures
         , Megaparsec.try pHidSpecConditionKindSucceedsIf
         ]
  where
    pHidSpecConditionKindAssert :: TermParser HidSpecConditionKindL
    pHidSpecConditionKindAssert =
      HidSpecConditionKindAssert' <$> pSort @AssertTokL "assert_tok"
    pHidSpecConditionKindAssume :: TermParser HidSpecConditionKindL
    pHidSpecConditionKindAssume =
      HidSpecConditionKindAssume' <$> pSort @AssumeTokL "assume_tok"
    pHidSpecConditionKindDecreases :: TermParser HidSpecConditionKindL
    pHidSpecConditionKindDecreases =
      HidSpecConditionKindDecreases' <$> pSort @DecreasesTokL "decreases_tok"
    pHidSpecConditionKindEnsures :: TermParser HidSpecConditionKindL
    pHidSpecConditionKindEnsures =
      HidSpecConditionKindEnsures' <$> pSort @EnsuresTokL "ensures_tok"
    pHidSpecConditionKindSucceedsIf :: TermParser HidSpecConditionKindL
    pHidSpecConditionKindSucceedsIf =
      HidSpecConditionKindSucceedsIf' <$> pSort @SucceedsIfTokL "succeeds_if_tok"

pSpecInclude :: TermParser SpecIncludeL
pSpecInclude =
  SpecInclude' <$> pBetween (pSort @IncludeTokL "include_tok") (pSort @SemicolonTokL ";_tok") (pSort @HidExpressionL "hid_expression")

pSpecInvariant :: TermParser SpecInvariantL
pSpecInvariant =
  SpecInvariant' <$> pSort @InvariantTokL "invariant_tok" <*> pMaybe (pSort @SpecInvariantInternal0L "spec_invariant_internal0") <*> pMaybe (pSort @ConditionPropertiesL "condition_properties") <*> pSort @HidExpressionL "hid_expression" <*> pSort @SemicolonTokL ";_tok"

pSpecInvariantInternal0 :: TermParser SpecInvariantInternal0L
pSpecInvariantInternal0 =
  choice [ Megaparsec.try pSpecInvariantInternal0Update
         , Megaparsec.try pSpecInvariantInternal0Pack
         , Megaparsec.try pSpecInvariantInternal0Unpack
         , Megaparsec.try pSpecInvariantInternal0Module
         ]
  where
    pSpecInvariantInternal0Update :: TermParser SpecInvariantInternal0L
    pSpecInvariantInternal0Update =
      SpecInvariantInternal0Update' <$> pSort @UpdateTokL "update_tok"
    pSpecInvariantInternal0Pack :: TermParser SpecInvariantInternal0L
    pSpecInvariantInternal0Pack =
      SpecInvariantInternal0Pack' <$> pSort @PackTokL "pack_tok"
    pSpecInvariantInternal0Unpack :: TermParser SpecInvariantInternal0L
    pSpecInvariantInternal0Unpack =
      SpecInvariantInternal0Unpack' <$> pSort @UnpackTokL "unpack_tok"
    pSpecInvariantInternal0Module :: TermParser SpecInvariantInternal0L
    pSpecInvariantInternal0Module =
      SpecInvariantInternal0Module' <$> pSort @ModuleTokL "module_tok"

pSpecLet :: TermParser SpecLetL
pSpecLet =
  SpecLet' <$> pSort @LetTokL "let_tok" <*> pMaybe (pSort @PostTokL "post_tok") <*> pSort @IdentifierL "identifier" <*> pBetween (pSort @EqualsSignTokL "=_tok") (pSort @SemicolonTokL ";_tok") (pSort @HidExpressionL "hid_expression")

pSpecPragma :: TermParser SpecPragmaL
pSpecPragma =
  SpecPragma' <$> pBetween (pSort @PragmaTokL "pragma_tok") (pSort @SemicolonTokL ";_tok") (pSepBy (pSort @SpecPropertyL "spec_property") (pSort @CommaTokL ",_tok"))

pSpecVariable :: TermParser SpecVariableL
pSpecVariable =
  SpecVariable' <$> pMaybe (pSort @SpecVariableInternal0L "spec_variable_internal0") <*> pSort @IdentifierL "identifier" <*> pMaybe (pSort @TypeParametersL "type_parameters") <*> pBetween (pSort @ColonTokL ":_tok") (pSort @SemicolonTokL ";_tok") (pSort @HidTypeL "hid_type")

pSpecVariableInternal0 :: TermParser SpecVariableInternal0L
pSpecVariableInternal0 =
  choice [ Megaparsec.try pSpecVariableInternal0Global
         , Megaparsec.try pSpecVariableInternal0Local
         ]
  where
    pSpecVariableInternal0Global :: TermParser SpecVariableInternal0L
    pSpecVariableInternal0Global =
      SpecVariableInternal0Global' <$> pSort @GlobalTokL "global_tok"
    pSpecVariableInternal0Local :: TermParser SpecVariableInternal0L
    pSpecVariableInternal0Local =
      SpecVariableInternal0Local' <$> pSort @LocalTokL "local_tok"

pUnitExpression :: TermParser UnitExpressionL
pUnitExpression =
  UnitExpression' <$> pSort @LeftParenthesisTokL "(_tok" <*> pSort @RightParenthesisTokL ")_tok"

pVectorExpression :: TermParser VectorExpressionL
pVectorExpression =
  VectorExpression' <$> pSort @VectorExpressionInternal0L "vector_expression_internal0" <*> pSepBy (pSort @HidExpressionL "hid_expression") (pSort @CommaTokL ",_tok") <*> pSort @RightSquareBracketTokL "]_tok"

pVectorExpressionInternal0 :: TermParser VectorExpressionInternal0L
pVectorExpressionInternal0 =
  choice [ Megaparsec.try pVectorExpressionInternal0VectorLeftSquareBracket
         , Megaparsec.try pVectorExpressionInternal02
         ]
  where
    pVectorExpressionInternal0VectorLeftSquareBracket :: TermParser VectorExpressionInternal0L
    pVectorExpressionInternal0VectorLeftSquareBracket =
      VectorExpressionInternal0VectorLeftSquareBracket' <$> pSort @VectorLeftSquareBracketTokL "vector[_tok"
    pVectorExpressionInternal02 :: TermParser VectorExpressionInternal0L
    pVectorExpressionInternal02 =
      VectorExpressionInternal02' <$> pBetween (pSort @VectorLessThanSignTokL "vector<_tok") (pSort @GreaterThanSignTokL ">_tok") (pSepBy1 (pSort @HidTypeL "hid_type") (pSort @CommaTokL ",_tok")) <*> pSort @LeftSquareBracketTokL "[_tok"

pMoveOrCopyExpression :: TermParser MoveOrCopyExpressionL
pMoveOrCopyExpression =
  MoveOrCopyExpression' <$> pPair (pSort @MoveOrCopyExpressionInternal0L "move_or_copy_expression_internal0") (pSort @HidExpressionL "hid_expression")

pMoveOrCopyExpressionInternal0 :: TermParser MoveOrCopyExpressionInternal0L
pMoveOrCopyExpressionInternal0 =
  choice [ Megaparsec.try pMoveOrCopyExpressionInternal0Move
         , Megaparsec.try pMoveOrCopyExpressionInternal0Copy
         ]
  where
    pMoveOrCopyExpressionInternal0Move :: TermParser MoveOrCopyExpressionInternal0L
    pMoveOrCopyExpressionInternal0Move =
      MoveOrCopyExpressionInternal0Move' <$> pSort @MoveTokL "move_tok"
    pMoveOrCopyExpressionInternal0Copy :: TermParser MoveOrCopyExpressionInternal0L
    pMoveOrCopyExpressionInternal0Copy =
      MoveOrCopyExpressionInternal0Copy' <$> pSort @CopyTokL "copy_tok"

pUnaryExpression :: TermParser UnaryExpressionL
pUnaryExpression =
  UnaryExpression' <$> pSort @UnaryOpL "unary_op" <*> pSort @HidExpressionL "hid_expression"

pUnaryOp :: TermParser UnaryOpL
pUnaryOp =
  UnaryOp' <$> pSort @ExclamationMarkTokL "!_tok"

pBinaryExpression :: TermParser BinaryExpressionL
pBinaryExpression =
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
    pBinaryExpression1 :: TermParser BinaryExpressionL
    pBinaryExpression1 =
      BinaryExpression1' <$> pSort @HidExpressionL "hid_expression" <*> pSort @EqualsSignEqualsSignGreaterThanSignTokL "==>_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression2 :: TermParser BinaryExpressionL
    pBinaryExpression2 =
      BinaryExpression2' <$> pSort @HidExpressionL "hid_expression" <*> pSort @VerticalLineVerticalLineTokL "||_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression3 :: TermParser BinaryExpressionL
    pBinaryExpression3 =
      BinaryExpression3' <$> pSort @HidExpressionL "hid_expression" <*> pSort @AmpersandAmpersandTokL "&&_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression4 :: TermParser BinaryExpressionL
    pBinaryExpression4 =
      BinaryExpression4' <$> pSort @HidExpressionL "hid_expression" <*> pSort @EqualsSignEqualsSignTokL "==_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression5 :: TermParser BinaryExpressionL
    pBinaryExpression5 =
      BinaryExpression5' <$> pSort @HidExpressionL "hid_expression" <*> pSort @ExclamationMarkEqualsSignTokL "!=_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression6 :: TermParser BinaryExpressionL
    pBinaryExpression6 =
      BinaryExpression6' <$> pSort @HidExpressionL "hid_expression" <*> pSort @LessThanSignTokL "<_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression7 :: TermParser BinaryExpressionL
    pBinaryExpression7 =
      BinaryExpression7' <$> pSort @HidExpressionL "hid_expression" <*> pSort @GreaterThanSignTokL ">_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression8 :: TermParser BinaryExpressionL
    pBinaryExpression8 =
      BinaryExpression8' <$> pSort @HidExpressionL "hid_expression" <*> pSort @LessThanSignEqualsSignTokL "<=_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression9 :: TermParser BinaryExpressionL
    pBinaryExpression9 =
      BinaryExpression9' <$> pSort @HidExpressionL "hid_expression" <*> pSort @GreaterThanSignEqualsSignTokL ">=_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression10 :: TermParser BinaryExpressionL
    pBinaryExpression10 =
      BinaryExpression10' <$> pSort @HidExpressionL "hid_expression" <*> pSort @FullStopFullStopTokL ".._tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression11 :: TermParser BinaryExpressionL
    pBinaryExpression11 =
      BinaryExpression11' <$> pSort @HidExpressionL "hid_expression" <*> pSort @VerticalLineTokL "|_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression12 :: TermParser BinaryExpressionL
    pBinaryExpression12 =
      BinaryExpression12' <$> pSort @HidExpressionL "hid_expression" <*> pSort @CircumflexAccentTokL "^_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression13 :: TermParser BinaryExpressionL
    pBinaryExpression13 =
      BinaryExpression13' <$> pSort @HidExpressionL "hid_expression" <*> pSort @AmpersandTokL "&_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression14 :: TermParser BinaryExpressionL
    pBinaryExpression14 =
      BinaryExpression14' <$> pSort @HidExpressionL "hid_expression" <*> pSort @LessThanSignLessThanSignTokL "<<_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression15 :: TermParser BinaryExpressionL
    pBinaryExpression15 =
      BinaryExpression15' <$> pSort @HidExpressionL "hid_expression" <*> pSort @GreaterThanSignGreaterThanSignTokL ">>_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression16 :: TermParser BinaryExpressionL
    pBinaryExpression16 =
      BinaryExpression16' <$> pSort @HidExpressionL "hid_expression" <*> pSort @PlusSignTokL "+_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression17 :: TermParser BinaryExpressionL
    pBinaryExpression17 =
      BinaryExpression17' <$> pSort @HidExpressionL "hid_expression" <*> pSort @HyphenMinusTokL "-_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression18 :: TermParser BinaryExpressionL
    pBinaryExpression18 =
      BinaryExpression18' <$> pSort @HidExpressionL "hid_expression" <*> pSort @AsteriskTokL "*_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression19 :: TermParser BinaryExpressionL
    pBinaryExpression19 =
      BinaryExpression19' <$> pSort @HidExpressionL "hid_expression" <*> pSort @SolidusTokL "/_tok" <*> pSort @HidExpressionL "hid_expression"
    pBinaryExpression20 :: TermParser BinaryExpressionL
    pBinaryExpression20 =
      BinaryExpression20' <$> pSort @HidExpressionL "hid_expression" <*> pSort @PercentSignTokL "%_tok" <*> pSort @HidExpressionL "hid_expression"

pCastExpression :: TermParser CastExpressionL
pCastExpression =
  CastExpression' <$> pPair (pPair (pSort @HidExpressionL "hid_expression") (pSort @AsTokL "as_tok")) (pSort @HidTypeL "hid_type")

pIdentifiedExpression :: TermParser IdentifiedExpressionL
pIdentifiedExpression =
  IdentifiedExpression' <$> pSort @BlockIdentifierL "block_identifier" <*> pSort @HidExpressionL "hid_expression"

pBlockIdentifier :: TermParser BlockIdentifierL
pBlockIdentifier =
  BlockIdentifier' <$> pSort @LabelL "label" <*> pSort @ColonTokL ":_tok"

pLambdaExpression :: TermParser LambdaExpressionL
pLambdaExpression =
  LambdaExpression' <$> pSort @LambdaBindingsL "lambda_bindings" <*> pMaybe (pPair (pSort @HyphenMinusGreaterThanSignTokL "->_tok") (pSort @HidTypeL "hid_type")) <*> pSort @HidExpressionL "hid_expression"

pLambdaBindings :: TermParser LambdaBindingsL
pLambdaBindings =
  LambdaBindings' <$> pBetween (pSort @VerticalLineTokL "|_tok") (pSort @VerticalLineTokL "|_tok") (pSepBy (pSort @LambdaBindingL "lambda_binding") (pSort @CommaTokL ",_tok"))

pLambdaBinding :: TermParser LambdaBindingL
pLambdaBinding =
  choice [ Megaparsec.try pLambdaBindingCommaBindList
         , Megaparsec.try pLambdaBinding2
         ]
  where
    pLambdaBindingCommaBindList :: TermParser LambdaBindingL
    pLambdaBindingCommaBindList =
      LambdaBindingCommaBindList' <$> pSort @CommaBindListL "comma_bind_list"
    pLambdaBinding2 :: TermParser LambdaBindingL
    pLambdaBinding2 =
      LambdaBinding2' <$> pSort @HidBindL "hid_bind" <*> pMaybe (pPair (pSort @ColonTokL ":_tok") (pSort @HidTypeL "hid_type"))

pLoopExpression :: TermParser LoopExpressionL
pLoopExpression =
  LoopExpression' <$> pSort @LoopTokL "loop_tok" <*> pSort @HidExpressionL "hid_expression"

pQuantifierExpression :: TermParser QuantifierExpressionL
pQuantifierExpression =
  QuantifierExpression' <$> pPair (pPair (pPair (pPair (pSort @HidReservedIdentifierL "hid_reserved_identifier") (pSort @QuantifierBindingsL "quantifier_bindings")) (pMaybe (pPair (pSort @WhereTokL "where_tok") (pSort @HidExpressionL "hid_expression")))) (pSort @ColonTokL ":_tok")) (pSort @HidExpressionL "hid_expression")

pQuantifierBindings :: TermParser QuantifierBindingsL
pQuantifierBindings =
  QuantifierBindings' <$> pSepBy1 (pSort @QuantifierBindingL "quantifier_binding") (pSort @CommaTokL ",_tok")

pQuantifierBinding :: TermParser QuantifierBindingL
pQuantifierBinding =
  choice [ Megaparsec.try pQuantifierBinding1
         , Megaparsec.try pQuantifierBinding2
         ]
  where
    pQuantifierBinding1 :: TermParser QuantifierBindingL
    pQuantifierBinding1 =
      QuantifierBinding1' <$> pSort @IdentifierL "identifier" <*> pSort @ColonTokL ":_tok" <*> pSort @HidTypeL "hid_type"
    pQuantifierBinding2 :: TermParser QuantifierBindingL
    pQuantifierBinding2 =
      QuantifierBinding2' <$> pSort @IdentifierL "identifier" <*> pSort @InTokL "in_tok" <*> pSort @HidExpressionL "hid_expression"

pReturnExpression :: TermParser ReturnExpressionL
pReturnExpression =
  choice [ Megaparsec.try pReturnExpression1
         , Megaparsec.try pReturnExpression2
         ]
  where
    pReturnExpression1 :: TermParser ReturnExpressionL
    pReturnExpression1 =
      ReturnExpression1' <$> pSort @ReturnTokL "return_tok" <*> pMaybe (pSort @LabelL "label") <*> pSort @HidExpressionL "hid_expression"
    pReturnExpression2 :: TermParser ReturnExpressionL
    pReturnExpression2 =
      ReturnExpression2' <$> pSort @ReturnTokL "return_tok" <*> pMaybe (pSort @LabelL "label")

pWhileExpression :: TermParser WhileExpressionL
pWhileExpression =
  WhileExpression' <$> pSort @WhileTokL "while_tok" <*> pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSort @HidExpressionL "hid_expression") <*> pSort @HidExpressionL "hid_expression"

pFriendDeclaration :: TermParser FriendDeclarationL
pFriendDeclaration =
  FriendDeclaration' <$> pBetween (pSort @FriendTokL "friend_tok") (pSort @SemicolonTokL ";_tok") (pSort @FriendAccessL "friend_access")

pFriendAccess :: TermParser FriendAccessL
pFriendAccess =
  choice [ Megaparsec.try pFriendAccessLocalModule
         , Megaparsec.try pFriendAccessFullyQualifiedModule
         ]
  where
    pFriendAccessLocalModule :: TermParser FriendAccessL
    pFriendAccessLocalModule =
      FriendAccessLocalModule' <$> pSort @IdentifierL "identifier"
    pFriendAccessFullyQualifiedModule :: TermParser FriendAccessL
    pFriendAccessFullyQualifiedModule =
      FriendAccessFullyQualifiedModule' <$> pSort @ModuleIdentityL "module_identity"

pHidEnumItem :: TermParser HidEnumItemL
pHidEnumItem =
  HidEnumItem' <$> pSort @EnumDefinitionL "enum_definition"

pEnumDefinition :: TermParser EnumDefinitionL
pEnumDefinition =
  EnumDefinition' <$> pMaybe (pSort @PublicTokL "public_tok") <*> pSort @HidEnumSignatureL "hid_enum_signature" <*> pSort @EnumVariantsL "enum_variants" <*> pMaybe (pSort @PostfixAbilityDeclsL "postfix_ability_decls")

pEnumVariants :: TermParser EnumVariantsL
pEnumVariants =
  EnumVariants' <$> pBetween (pSort @LeftCurlyBracketTokL "{_tok") (pSort @RightCurlyBracketTokL "}_tok") (pSepBy (pSort @VariantL "variant") (pSort @CommaTokL ",_tok"))

pVariant :: TermParser VariantL
pVariant =
  Variant' <$> pSort @HidVariantIdentifierL "hid_variant_identifier" <*> pMaybe (pSort @DatatypeFieldsL "datatype_fields")

pDatatypeFields :: TermParser DatatypeFieldsL
pDatatypeFields =
  choice [ Megaparsec.try pDatatypeFieldsPositionalFields
         , Megaparsec.try pDatatypeFieldsNamedFields
         ]
  where
    pDatatypeFieldsPositionalFields :: TermParser DatatypeFieldsL
    pDatatypeFieldsPositionalFields =
      DatatypeFieldsPositionalFields' <$> pSort @PositionalFieldsL "positional_fields"
    pDatatypeFieldsNamedFields :: TermParser DatatypeFieldsL
    pDatatypeFieldsNamedFields =
      DatatypeFieldsNamedFields' <$> pSort @NamedFieldsL "named_fields"

pNamedFields :: TermParser NamedFieldsL
pNamedFields =
  NamedFields' <$> pBetween (pSort @LeftCurlyBracketTokL "{_tok") (pSort @RightCurlyBracketTokL "}_tok") (pSepBy (pSort @FieldAnnotationL "field_annotation") (pSort @CommaTokL ",_tok"))

pFieldAnnotation :: TermParser FieldAnnotationL
pFieldAnnotation =
  FieldAnnotation' <$> pSort @HidFieldIdentifierL "hid_field_identifier" <*> pSort @ColonTokL ":_tok" <*> pSort @HidTypeL "hid_type"

pPositionalFields :: TermParser PositionalFieldsL
pPositionalFields =
  PositionalFields' <$> pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSepBy (pSort @HidTypeL "hid_type") (pSort @CommaTokL ",_tok"))

pHidVariantIdentifier :: TermParser HidVariantIdentifierL
pHidVariantIdentifier =
  HidVariantIdentifier' <$> pSort @IdentifierL "identifier"

pHidEnumSignature :: TermParser HidEnumSignatureL
pHidEnumSignature =
  HidEnumSignature' <$> pSort @EnumTokL "enum_tok" <*> pSort @HidEnumIdentifierL "hid_enum_identifier" <*> pMaybe (pSort @TypeParametersL "type_parameters") <*> pMaybe (pSort @AbilityDeclsL "ability_decls")

pAbilityDecls :: TermParser AbilityDeclsL
pAbilityDecls =
  AbilityDecls' <$> pSort @HasTokL "has_tok" <*> pSepBy (pSort @AbilityL "ability") (pSort @CommaTokL ",_tok")

pHidEnumIdentifier :: TermParser HidEnumIdentifierL
pHidEnumIdentifier =
  HidEnumIdentifier' <$> pSort @IdentifierL "identifier"

pPostfixAbilityDecls :: TermParser PostfixAbilityDeclsL
pPostfixAbilityDecls =
  PostfixAbilityDecls' <$> pBetween (pSort @HasTokL "has_tok") (pSort @SemicolonTokL ";_tok") (pSepBy (pSort @AbilityL "ability") (pSort @CommaTokL ",_tok"))

pHidFunctionItem :: TermParser HidFunctionItemL
pHidFunctionItem =
  choice [ Megaparsec.try pHidFunctionItemNativeFunctionDefinition
         , Megaparsec.try pHidFunctionItemMacroFunctionDefinition
         , Megaparsec.try pHidFunctionItemFunctionDefinition
         ]
  where
    pHidFunctionItemNativeFunctionDefinition :: TermParser HidFunctionItemL
    pHidFunctionItemNativeFunctionDefinition =
      HidFunctionItemNativeFunctionDefinition' <$> pSort @NativeFunctionDefinitionL "native_function_definition"
    pHidFunctionItemMacroFunctionDefinition :: TermParser HidFunctionItemL
    pHidFunctionItemMacroFunctionDefinition =
      HidFunctionItemMacroFunctionDefinition' <$> pSort @MacroFunctionDefinitionL "macro_function_definition"
    pHidFunctionItemFunctionDefinition :: TermParser HidFunctionItemL
    pHidFunctionItemFunctionDefinition =
      HidFunctionItemFunctionDefinition' <$> pSort @FunctionDefinitionL "function_definition"

pFunctionDefinition :: TermParser FunctionDefinitionL
pFunctionDefinition =
  FunctionDefinition' <$> pSort @HidFunctionSignatureL "hid_function_signature" <*> pSort @BlockL "block"

pHidFunctionSignature :: TermParser HidFunctionSignatureL
pHidFunctionSignature =
  HidFunctionSignature' <$> pMaybe (pSort @ModifierL "modifier") <*> pMaybe (pSort @ModifierL "modifier") <*> pMaybe (pSort @ModifierL "modifier") <*> pSort @FunTokL "fun_tok" <*> pSort @HidFunctionIdentifierL "hid_function_identifier" <*> pMaybe (pSort @TypeParametersL "type_parameters") <*> pSort @FunctionParametersL "function_parameters" <*> pMaybe (pSort @RetTypeL "ret_type")

pModifier :: TermParser ModifierL
pModifier =
  choice [ Megaparsec.try pModifier1
         , Megaparsec.try pModifierEntry
         , Megaparsec.try pModifierNative
         ]
  where
    pModifier1 :: TermParser ModifierL
    pModifier1 =
      Modifier1' <$> pSort @PublicTokL "public_tok" <*> pMaybe (pBetween (pSort @LeftParenthesisTokL "(_tok") (pSort @RightParenthesisTokL ")_tok") (pSort @ModifierInternal0L "modifier_internal0"))
    pModifierEntry :: TermParser ModifierL
    pModifierEntry =
      ModifierEntry' <$> pSort @EntryTokL "entry_tok"
    pModifierNative :: TermParser ModifierL
    pModifierNative =
      ModifierNative' <$> pSort @NativeTokL "native_tok"

pModifierInternal0 :: TermParser ModifierInternal0L
pModifierInternal0 =
  choice [ Megaparsec.try pModifierInternal0Package
         , Megaparsec.try pModifierInternal0Friend
         ]
  where
    pModifierInternal0Package :: TermParser ModifierInternal0L
    pModifierInternal0Package =
      ModifierInternal0Package' <$> pSort @PackageTokL "package_tok"
    pModifierInternal0Friend :: TermParser ModifierInternal0L
    pModifierInternal0Friend =
      ModifierInternal0Friend' <$> pSort @FriendTokL "friend_tok"

pMacroFunctionDefinition :: TermParser MacroFunctionDefinitionL
pMacroFunctionDefinition =
  MacroFunctionDefinition' <$> pMaybe (pSort @ModifierL "modifier") <*> pSort @MacroTokL "macro_tok" <*> pSort @HidMacroSignatureL "hid_macro_signature" <*> pSort @BlockL "block"

pHidMacroSignature :: TermParser HidMacroSignatureL
pHidMacroSignature =
  HidMacroSignature' <$> pMaybe (pSort @ModifierL "modifier") <*> pSort @FunTokL "fun_tok" <*> pSort @HidFunctionIdentifierL "hid_function_identifier" <*> pMaybe (pSort @TypeParametersL "type_parameters") <*> pSort @FunctionParametersL "function_parameters" <*> pMaybe (pSort @RetTypeL "ret_type")

pNativeFunctionDefinition :: TermParser NativeFunctionDefinitionL
pNativeFunctionDefinition =
  NativeFunctionDefinition' <$> pSort @HidFunctionSignatureL "hid_function_signature" <*> pSort @SemicolonTokL ";_tok"

pHidStructItem :: TermParser HidStructItemL
pHidStructItem =
  choice [ Megaparsec.try pHidStructItemNativeStructDefinition
         , Megaparsec.try pHidStructItemStructDefinition
         ]
  where
    pHidStructItemNativeStructDefinition :: TermParser HidStructItemL
    pHidStructItemNativeStructDefinition =
      HidStructItemNativeStructDefinition' <$> pSort @NativeStructDefinitionL "native_struct_definition"
    pHidStructItemStructDefinition :: TermParser HidStructItemL
    pHidStructItemStructDefinition =
      HidStructItemStructDefinition' <$> pSort @StructDefinitionL "struct_definition"

pNativeStructDefinition :: TermParser NativeStructDefinitionL
pNativeStructDefinition =
  NativeStructDefinition' <$> pMaybe (pSort @PublicTokL "public_tok") <*> pBetween (pSort @NativeTokL "native_tok") (pSort @SemicolonTokL ";_tok") (pSort @HidStructSignatureL "hid_struct_signature")

pHidStructSignature :: TermParser HidStructSignatureL
pHidStructSignature =
  HidStructSignature' <$> pSort @StructTokL "struct_tok" <*> pSort @HidStructIdentifierL "hid_struct_identifier" <*> pMaybe (pSort @TypeParametersL "type_parameters") <*> pMaybe (pSort @AbilityDeclsL "ability_decls")

pStructDefinition :: TermParser StructDefinitionL
pStructDefinition =
  StructDefinition' <$> pMaybe (pSort @PublicTokL "public_tok") <*> pSort @HidStructSignatureL "hid_struct_signature" <*> pSort @DatatypeFieldsL "datatype_fields" <*> pMaybe (pSort @PostfixAbilityDeclsL "postfix_ability_decls")

--------------------------------------------------------------------------------
-- Parse Table
--------------------------------------------------------------------------------

newtype ParseTable = SymbolTable {unParseTable :: IntMap SomeTermParser}

symbolMap :: Map String SomeTermParser
symbolMap = Map.fromList
    [ ("source_file", E <$> pSourceFile)
    , ("module_definition", E <$> pModuleDefinition)
    , ("module_body", E <$> pModuleBody)
    , ("module_body_internal0", E <$> pModuleBodyInternal0)
    , ("module_body_internal1", E <$> pModuleBodyInternal1)
    , ("constant", E <$> pConstant)
    , ("hid_expression", E <$> pHidExpression)
    , ("abort_expression", E <$> pAbortExpression)
    , ("assign_expression", E <$> pAssignExpression)
    , ("hid_unary_expression", E <$> pHidUnaryExpression)
    , ("hid_unary_expression_internal0", E <$> pHidUnaryExpressionInternal0)
    , ("borrow_expression", E <$> pBorrowExpression)
    , ("hid_reference", E <$> pHidReference)
    , ("imm_ref", E <$> pImmRef)
    , ("mut_ref", E <$> pMutRef)
    , ("dereference_expression", E <$> pDereferenceExpression)
    , ("hid_expression_term", E <$> pHidExpressionTerm)
    , ("annotation_expression", E <$> pAnnotationExpression)
    , ("hid_type", E <$> pHidType)
    , ("apply_type", E <$> pApplyType)
    , ("module_access", E <$> pModuleAccess)
    , ("hid_module_identifier", E <$> pHidModuleIdentifier)
    , ("identifier", E <$> pIdentifier)
    , ("hid_reserved_identifier", E <$> pHidReservedIdentifier)
    , ("hid_exists", E <$> pHidExists)
    , ("hid_forall", E <$> pHidForall)
    , ("module_identity", E <$> pModuleIdentity)
    , ("module_identity_internal0", E <$> pModuleIdentityInternal0)
    , ("num_literal", E <$> pNumLiteral)
    , ("num_literal_internal0", E <$> pNumLiteralInternal0)
    , ("type_arguments", E <$> pTypeArguments)
    , ("function_type", E <$> pFunctionType)
    , ("function_type_parameters", E <$> pFunctionTypeParameters)
    , ("primitive_type", E <$> pPrimitiveType)
    , ("ref_type", E <$> pRefType)
    , ("tuple_type", E <$> pTupleType)
    , ("block", E <$> pBlock)
    , ("block_item", E <$> pBlockItem)
    , ("block_item_internal0", E <$> pBlockItemInternal0)
    , ("let_statement", E <$> pLetStatement)
    , ("bind_list", E <$> pBindList)
    , ("comma_bind_list", E <$> pCommaBindList)
    , ("hid_bind", E <$> pHidBind)
    , ("at_bind", E <$> pAtBind)
    , ("hid_variable_identifier", E <$> pHidVariableIdentifier)
    , ("bind_unpack", E <$> pBindUnpack)
    , ("bind_fields", E <$> pBindFields)
    , ("bind_named_fields", E <$> pBindNamedFields)
    , ("bind_named_fields_internal0", E <$> pBindNamedFieldsInternal0)
    , ("bind_field", E <$> pBindField)
    , ("hid_spread_operator", E <$> pHidSpreadOperator)
    , ("mut_bind_field", E <$> pMutBindField)
    , ("bind_positional_fields", E <$> pBindPositionalFields)
    , ("name_expression", E <$> pNameExpression)
    , ("hid_bind_internal0", E <$> pHidBindInternal0)
    , ("mut_bind_var", E <$> pMutBindVar)
    , ("hid_literal_value", E <$> pHidLiteralValue)
    , ("address_literal", E <$> pAddressLiteral)
    , ("bool_literal", E <$> pBoolLiteral)
    , ("byte_string_literal", E <$> pByteStringLiteral)
    , ("hex_string_literal", E <$> pHexStringLiteral)
    , ("or_bind_list", E <$> pOrBindList)
    , ("use_declaration", E <$> pUseDeclaration)
    , ("use_declaration_internal0", E <$> pUseDeclarationInternal0)
    , ("use_fun", E <$> pUseFun)
    , ("hid_function_identifier", E <$> pHidFunctionIdentifier)
    , ("use_module", E <$> pUseModule)
    , ("use_module_member", E <$> pUseModuleMember)
    , ("use_member", E <$> pUseMember)
    , ("use_module_members", E <$> pUseModuleMembers)
    , ("break_expression", E <$> pBreakExpression)
    , ("label", E <$> pLabel)
    , ("call_expression", E <$> pCallExpression)
    , ("arg_list", E <$> pArgList)
    , ("continue_expression", E <$> pContinueExpression)
    , ("dot_expression", E <$> pDotExpression)
    , ("expression_list", E <$> pExpressionList)
    , ("if_expression", E <$> pIfExpression)
    , ("index_expression", E <$> pIndexExpression)
    , ("macro_call_expression", E <$> pMacroCallExpression)
    , ("macro_module_access", E <$> pMacroModuleAccess)
    , ("match_expression", E <$> pMatchExpression)
    , ("hid_match_body", E <$> pHidMatchBody)
    , ("match_arm", E <$> pMatchArm)
    , ("match_condition", E <$> pMatchCondition)
    , ("pack_expression", E <$> pPackExpression)
    , ("field_initialize_list", E <$> pFieldInitializeList)
    , ("exp_field", E <$> pExpField)
    , ("hid_field_identifier", E <$> pHidFieldIdentifier)
    , ("spec_block", E <$> pSpecBlock)
    , ("spec_block_internal0", E <$> pSpecBlockInternal0)
    , ("hid_spec_block_target", E <$> pHidSpecBlockTarget)
    , ("spec_block_target_schema", E <$> pSpecBlockTargetSchema)
    , ("hid_struct_identifier", E <$> pHidStructIdentifier)
    , ("type_parameters", E <$> pTypeParameters)
    , ("type_parameter", E <$> pTypeParameter)
    , ("ability", E <$> pAbility)
    , ("hid_type_parameter_identifier", E <$> pHidTypeParameterIdentifier)
    , ("hid_spec_function", E <$> pHidSpecFunction)
    , ("native_spec_function", E <$> pNativeSpecFunction)
    , ("hid_spec_function_signature", E <$> pHidSpecFunctionSignature)
    , ("function_parameters", E <$> pFunctionParameters)
    , ("function_parameters_internal0", E <$> pFunctionParametersInternal0)
    , ("function_parameter", E <$> pFunctionParameter)
    , ("function_parameter_internal0", E <$> pFunctionParameterInternal0)
    , ("mut_function_parameter", E <$> pMutFunctionParameter)
    , ("ret_type", E <$> pRetType)
    , ("uninterpreted_spec_function", E <$> pUninterpretedSpecFunction)
    , ("usual_spec_function", E <$> pUsualSpecFunction)
    , ("spec_body", E <$> pSpecBody)
    , ("hid_spec_block_memeber", E <$> pHidSpecBlockMemeber)
    , ("spec_apply", E <$> pSpecApply)
    , ("spec_apply_pattern", E <$> pSpecApplyPattern)
    , ("spec_apply_name_pattern", E <$> pSpecApplyNamePattern)
    , ("spec_apply_pattern_internal0", E <$> pSpecApplyPatternInternal0)
    , ("spec_condition", E <$> pSpecCondition)
    , ("hid_spec_abort_if", E <$> pHidSpecAbortIf)
    , ("condition_properties", E <$> pConditionProperties)
    , ("spec_property", E <$> pSpecProperty)
    , ("hid_spec_abort_with_or_modifies", E <$> pHidSpecAbortWithOrModifies)
    , ("hid_spec_abort_with_or_modifies_internal0", E <$> pHidSpecAbortWithOrModifiesInternal0)
    , ("hid_spec_condition", E <$> pHidSpecCondition)
    , ("hid_spec_condition_internal0", E <$> pHidSpecConditionInternal0)
    , ("hid_spec_condition_kind", E <$> pHidSpecConditionKind)
    , ("spec_include", E <$> pSpecInclude)
    , ("spec_invariant", E <$> pSpecInvariant)
    , ("spec_invariant_internal0", E <$> pSpecInvariantInternal0)
    , ("spec_let", E <$> pSpecLet)
    , ("spec_pragma", E <$> pSpecPragma)
    , ("spec_variable", E <$> pSpecVariable)
    , ("spec_variable_internal0", E <$> pSpecVariableInternal0)
    , ("unit_expression", E <$> pUnitExpression)
    , ("vector_expression", E <$> pVectorExpression)
    , ("vector_expression_internal0", E <$> pVectorExpressionInternal0)
    , ("move_or_copy_expression", E <$> pMoveOrCopyExpression)
    , ("move_or_copy_expression_internal0", E <$> pMoveOrCopyExpressionInternal0)
    , ("unary_expression", E <$> pUnaryExpression)
    , ("unary_op", E <$> pUnaryOp)
    , ("binary_expression", E <$> pBinaryExpression)
    , ("cast_expression", E <$> pCastExpression)
    , ("identified_expression", E <$> pIdentifiedExpression)
    , ("block_identifier", E <$> pBlockIdentifier)
    , ("lambda_expression", E <$> pLambdaExpression)
    , ("lambda_bindings", E <$> pLambdaBindings)
    , ("lambda_binding", E <$> pLambdaBinding)
    , ("loop_expression", E <$> pLoopExpression)
    , ("quantifier_expression", E <$> pQuantifierExpression)
    , ("quantifier_bindings", E <$> pQuantifierBindings)
    , ("quantifier_binding", E <$> pQuantifierBinding)
    , ("return_expression", E <$> pReturnExpression)
    , ("while_expression", E <$> pWhileExpression)
    , ("friend_declaration", E <$> pFriendDeclaration)
    , ("friend_access", E <$> pFriendAccess)
    , ("hid_enum_item", E <$> pHidEnumItem)
    , ("enum_definition", E <$> pEnumDefinition)
    , ("enum_variants", E <$> pEnumVariants)
    , ("variant", E <$> pVariant)
    , ("datatype_fields", E <$> pDatatypeFields)
    , ("named_fields", E <$> pNamedFields)
    , ("field_annotation", E <$> pFieldAnnotation)
    , ("positional_fields", E <$> pPositionalFields)
    , ("hid_variant_identifier", E <$> pHidVariantIdentifier)
    , ("hid_enum_signature", E <$> pHidEnumSignature)
    , ("ability_decls", E <$> pAbilityDecls)
    , ("hid_enum_identifier", E <$> pHidEnumIdentifier)
    , ("postfix_ability_decls", E <$> pPostfixAbilityDecls)
    , ("hid_function_item", E <$> pHidFunctionItem)
    , ("function_definition", E <$> pFunctionDefinition)
    , ("hid_function_signature", E <$> pHidFunctionSignature)
    , ("modifier", E <$> pModifier)
    , ("modifier_internal0", E <$> pModifierInternal0)
    , ("macro_function_definition", E <$> pMacroFunctionDefinition)
    , ("hid_macro_signature", E <$> pHidMacroSignature)
    , ("native_function_definition", E <$> pNativeFunctionDefinition)
    , ("hid_struct_item", E <$> pHidStructItem)
    , ("native_struct_definition", E <$> pNativeStructDefinition)
    , ("hid_struct_signature", E <$> pHidStructSignature)
    , ("struct_definition", E <$> pStructDefinition)
    , ("!", E <$> pExclamationMarkTok)
    , ("!=", E <$> pExclamationMarkEqualsSignTok)
    , ("#[", E <$> pNumberSignLeftSquareBracketTok)
    , ("$", E <$> pDollarSignTok)
    , ("%", E <$> pPercentSignTok)
    , ("&", E <$> pAmpersandTok)
    , ("&&", E <$> pAmpersandAmpersandTok)
    , ("'", E <$> pApostropheTok)
    , ("(", E <$> pLeftParenthesisTok)
    , (")", E <$> pRightParenthesisTok)
    , ("*", E <$> pAsteriskTok)
    , ("+", E <$> pPlusSignTok)
    , (",", E <$> pCommaTok)
    , ("-", E <$> pHyphenMinusTok)
    , ("->", E <$> pHyphenMinusGreaterThanSignTok)
    , (".", E <$> pFullStopTok)
    , ("..", E <$> pFullStopFullStopTok)
    , ("/", E <$> pSolidusTok)
    , ("/*", E <$> pSolidusAsteriskTok)
    , ("//", E <$> pSolidusSolidusTok)
    , (":", E <$> pColonTok)
    , ("::", E <$> pColonColonTok)
    , (";", E <$> pSemicolonTok)
    , ("<", E <$> pLessThanSignTok)
    , ("<<", E <$> pLessThanSignLessThanSignTok)
    , ("<=", E <$> pLessThanSignEqualsSignTok)
    , ("=", E <$> pEqualsSignTok)
    , ("==", E <$> pEqualsSignEqualsSignTok)
    , ("==>", E <$> pEqualsSignEqualsSignGreaterThanSignTok)
    , ("=>", E <$> pEqualsSignGreaterThanSignTok)
    , (">", E <$> pGreaterThanSignTok)
    , (">=", E <$> pGreaterThanSignEqualsSignTok)
    , (">>", E <$> pGreaterThanSignGreaterThanSignTok)
    , ("@", E <$> pCommercialAtTok)
    , ("[", E <$> pLeftSquareBracketTok)
    , ("]", E <$> pRightSquareBracketTok)
    , ("^", E <$> pCircumflexAccentTok)
    , ("abort", E <$> pAbortTok)
    , ("aborts_if", E <$> pAbortsIfTok)
    , ("aborts_with", E <$> pAbortsWithTok)
    , ("address", E <$> pAddressTok)
    , ("apply", E <$> pApplyTok)
    , ("as", E <$> pAsTok)
    , ("assert", E <$> pAssertTok)
    , ("assume", E <$> pAssumeTok)
    , ("bool", E <$> pBoolTok)
    , ("break", E <$> pBreakTok)
    , ("bytearray", E <$> pBytearrayTok)
    , ("const", E <$> pConstTok)
    , ("continue", E <$> pContinueTok)
    , ("copy", E <$> pCopyTok)
    , ("decreases", E <$> pDecreasesTok)
    , ("drop", E <$> pDropTok)
    , ("else", E <$> pElseTok)
    , ("ensures", E <$> pEnsuresTok)
    , ("entry", E <$> pEntryTok)
    , ("enum", E <$> pEnumTok)
    , ("except", E <$> pExceptTok)
    , ("exists", E <$> pExistsTok)
    , ("false", E <$> pFalseTok)
    , ("forall", E <$> pForallTok)
    , ("friend", E <$> pFriendTok)
    , ("fun", E <$> pFunTok)
    , ("global", E <$> pGlobalTok)
    , ("has", E <$> pHasTok)
    , ("if", E <$> pIfTok)
    , ("in", E <$> pInTok)
    , ("include", E <$> pIncludeTok)
    , ("internal", E <$> pInternalTok)
    , ("invariant", E <$> pInvariantTok)
    , ("key", E <$> pKeyTok)
    , ("let", E <$> pLetTok)
    , ("local", E <$> pLocalTok)
    , ("loop", E <$> pLoopTok)
    , ("macro", E <$> pMacroTok)
    , ("match", E <$> pMatchTok)
    , ("modifies", E <$> pModifiesTok)
    , ("module", E <$> pModuleTok)
    , ("move", E <$> pMoveTok)
    , ("mut", E <$> pMutTok)
    , ("native", E <$> pNativeTok)
    , ("pack", E <$> pPackTok)
    , ("package", E <$> pPackageTok)
    , ("phantom", E <$> pPhantomTok)
    , ("post", E <$> pPostTok)
    , ("pragma", E <$> pPragmaTok)
    , ("public", E <$> pPublicTok)
    , ("requires", E <$> pRequiresTok)
    , ("return", E <$> pReturnTok)
    , ("schema", E <$> pSchemaTok)
    , ("signer", E <$> pSignerTok)
    , ("spec", E <$> pSpecTok)
    , ("store", E <$> pStoreTok)
    , ("struct", E <$> pStructTok)
    , ("succeeds_if", E <$> pSucceedsIfTok)
    , ("to", E <$> pToTok)
    , ("true", E <$> pTrueTok)
    , ("u128", E <$> pU128Tok)
    , ("u16", E <$> pU16Tok)
    , ("u256", E <$> pU256Tok)
    , ("u32", E <$> pU32Tok)
    , ("u64", E <$> pU64Tok)
    , ("u8", E <$> pU8Tok)
    , ("unpack", E <$> pUnpackTok)
    , ("update", E <$> pUpdateTok)
    , ("use", E <$> pUseTok)
    , ("vector<", E <$> pVectorLessThanSignTok)
    , ("vector[", E <$> pVectorLeftSquareBracketTok)
    , ("where", E <$> pWhereTok)
    , ("while", E <$> pWhileTok)
    , ("with", E <$> pWithTok)
    , ("{", E <$> pLeftCurlyBracketTok)
    , ("|", E <$> pVerticalLineTok)
    , ("||", E <$> pVerticalLineVerticalLineTok)
    , ("}", E <$> pRightCurlyBracketTok)
    ]

mkParseTable :: TS.Language -> IO ParseTable
mkParseTable lang = do
  count <- fromIntegral <$> TS.languageSymbolCount lang
  SymbolTable <$> foldrM
    (\id acc -> do
      symName <- TS.languageSymbolName lang id
      let mSymSing = Map.lookup (Char8.unpack symName) symbolMap
      pure (maybe acc (flip (IM.insert (fromIntegral id)) acc) mSymSing)
    )
    (IM.empty :: IntMap SomeTermParser)
    [0..count - 1]
