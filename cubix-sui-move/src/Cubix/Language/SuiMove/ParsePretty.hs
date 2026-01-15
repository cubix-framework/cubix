{-# LANGUAGE OverloadedStrings #-}
module Cubix.Language.SuiMove.ParsePretty where

import Control.Applicative.Combinators
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Comp.Multi (Cxt, E (..), NoHole, Sum, K)
import Data.Foldable (foldrM)
import Data.Functor ((<$), ($>))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Text
import Data.Type.Equality (type (:~:) (..), type (:~~:) (..))
import Data.Typeable (Typeable)

import TreeSitter qualified as TS
import TreeSitter.SuiMove (tree_sitter_sui_move)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.TreeSitter qualified as Megaparsec.TreeSitter
import Text.Megaparsec.Cubix qualified as Megaparsec.Cubix
import Text.Megaparsec.Cubix
  ( pMaybe, pPair, pSome, pMany
  , pSepBy, pSepBy1, pBetween
  , pSort, pContent
  )

import Cubix.Language.Info
  ( SourcePos (..)
  , SourceSpan (..)
  , SourceRange (..)
  , rangeLength
  )
import Cubix.TreeSitter
import Cubix.Language.SuiMove.Modularized

import Text.Pretty.Simple

parse' :: ReaderT (TreeSitterEnv SomeSymbolSing) IO SomeTerm
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

getContent :: ByteString -> SourceRange -> ByteString
getContent src range =
  let len = rangeLength range
      start = _rangeStart range
  in ByteString.take len . ByteString.drop start $ src

syntax :: FilePath -> ByteString -> ParseTable -> TS.Node -> ReaderT (TreeSitterEnv SomeSymbolSing) IO SomeTerm
syntax path source pTable = go
  where
    pContent = getContent source
    getParser sym = IM.lookup (fromIntegral sym) (unParseTable pTable)
    go :: TS.Node -> ReaderT (TreeSitterEnv SomeSymbolSing) IO SomeTerm
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
              case Megaparsec.runParser p path (Megaparsec.Cubix.Tok content <$> children) of
                Left err -> do
                  error "no parse"
                Right item -> do
                  pPrintLightBg item
                  pure item

-- --------------------------------------------------------------------------------
-- -- Parser 
-- --------------------------------------------------------------------------------
type SomeTerm = E MoveTerm
type Parser t = Megaparsec.Cubix.Parser NoHole (Sum MoveSig) (K ()) t
type TermParser l = Parser (MoveTerm l)
type SomeTermParser = Parser (E MoveTerm)

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
  SourceFile' <$> pMany pModuleDefinition

pModuleDefinition :: TermParser ModuleDefinitionL
pModuleDefinition =
  ModuleDefinition' <$> pModuleTok <*> pModuleIdentity <*> pModuleBody

pModuleBody :: TermParser ModuleBodyL
pModuleBody =
  ModuleBody' <$> (pModuleBodyInternal0) <*> pMany (pModuleBodyInternal1) <*> pMaybe pRightCurlyBracketTok

pModuleBodyInternal0 :: TermParser ModuleBodyInternal0L
pModuleBodyInternal0 =
  choice [ Megaparsec.try pModuleBodyInternal0Semicolon
         , Megaparsec.try pModuleBodyInternal0LeftCurlyBracket
         ]
  where
    pModuleBodyInternal0Semicolon :: TermParser ModuleBodyInternal0L
    pModuleBodyInternal0Semicolon =
      ModuleBodyInternal0Semicolon' <$> pSemicolonTok
    pModuleBodyInternal0LeftCurlyBracket :: TermParser ModuleBodyInternal0L
    pModuleBodyInternal0LeftCurlyBracket =
      ModuleBodyInternal0LeftCurlyBracket' <$> pLeftCurlyBracketTok

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
      ModuleBodyInternal1UseDeclaration' <$> pUseDeclaration
    pModuleBodyInternal1FriendDeclaration :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1FriendDeclaration =
      ModuleBodyInternal1FriendDeclaration' <$> pFriendDeclaration
    pModuleBodyInternal1Constant :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1Constant =
      ModuleBodyInternal1Constant' <$> pConstant
    pModuleBodyInternal1HidFunctionItem :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1HidFunctionItem =
      ModuleBodyInternal1HidFunctionItem' <$> pHidFunctionItem
    pModuleBodyInternal1HidStructItem :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1HidStructItem =
      ModuleBodyInternal1HidStructItem' <$> pHidStructItem
    pModuleBodyInternal1HidEnumItem :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1HidEnumItem =
      ModuleBodyInternal1HidEnumItem' <$> pHidEnumItem
    pModuleBodyInternal1SpecBlock :: TermParser ModuleBodyInternal1L
    pModuleBodyInternal1SpecBlock =
      ModuleBodyInternal1SpecBlock' <$> pSpecBlock

pConstant :: TermParser ConstantL
pConstant =
  Constant' <$> pBetween pConstTok pColonTok pIdentifier <*> pHidType <*> pBetween pEqualsSignTok pSemicolonTok pHidExpression

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
      HidExpressionCallExpression' <$> pCallExpression
    pHidExpressionMacroCallExpression :: TermParser HidExpressionL
    pHidExpressionMacroCallExpression =
      HidExpressionMacroCallExpression' <$> pMacroCallExpression
    pHidExpressionLambdaExpression :: TermParser HidExpressionL
    pHidExpressionLambdaExpression =
      HidExpressionLambdaExpression' <$> pLambdaExpression
    pHidExpressionIfExpression :: TermParser HidExpressionL
    pHidExpressionIfExpression =
      HidExpressionIfExpression' <$> pIfExpression
    pHidExpressionWhileExpression :: TermParser HidExpressionL
    pHidExpressionWhileExpression =
      HidExpressionWhileExpression' <$> pWhileExpression
    pHidExpressionReturnExpression :: TermParser HidExpressionL
    pHidExpressionReturnExpression =
      HidExpressionReturnExpression' <$> pReturnExpression
    pHidExpressionAbortExpression :: TermParser HidExpressionL
    pHidExpressionAbortExpression =
      HidExpressionAbortExpression' <$> pAbortExpression
    pHidExpressionAssignExpression :: TermParser HidExpressionL
    pHidExpressionAssignExpression =
      HidExpressionAssignExpression' <$> pAssignExpression
    pHidExpressionHidUnaryExpression :: TermParser HidExpressionL
    pHidExpressionHidUnaryExpression =
      HidExpressionHidUnaryExpression' <$> pHidUnaryExpression
    pHidExpressionBinaryExpression :: TermParser HidExpressionL
    pHidExpressionBinaryExpression =
      HidExpressionBinaryExpression' <$> pBinaryExpression
    pHidExpressionCastExpression :: TermParser HidExpressionL
    pHidExpressionCastExpression =
      HidExpressionCastExpression' <$> pCastExpression
    pHidExpressionQuantifierExpression :: TermParser HidExpressionL
    pHidExpressionQuantifierExpression =
      HidExpressionQuantifierExpression' <$> pQuantifierExpression
    pHidExpressionMatchExpression :: TermParser HidExpressionL
    pHidExpressionMatchExpression =
      HidExpressionMatchExpression' <$> pMatchExpression
    pHidExpressionVectorExpression :: TermParser HidExpressionL
    pHidExpressionVectorExpression =
      HidExpressionVectorExpression' <$> pVectorExpression
    pHidExpressionLoopExpression :: TermParser HidExpressionL
    pHidExpressionLoopExpression =
      HidExpressionLoopExpression' <$> pLoopExpression
    pHidExpressionIdentifiedExpression :: TermParser HidExpressionL
    pHidExpressionIdentifiedExpression =
      HidExpressionIdentifiedExpression' <$> pIdentifiedExpression

pAbortExpression :: TermParser AbortExpressionL
pAbortExpression =
  AbortExpression' <$> pAbortTok <*> pMaybe pHidExpression

pAssignExpression :: TermParser AssignExpressionL
pAssignExpression =
  AssignExpression' <$> pPair (pPair pHidUnaryExpression pEqualsSignTok) pHidExpression

pHidUnaryExpression :: TermParser HidUnaryExpressionL
pHidUnaryExpression =
  HidUnaryExpression' <$> (pHidUnaryExpressionInternal0)

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
      HidUnaryExpressionInternal0UnaryExpression' <$> pUnaryExpression
    pHidUnaryExpressionInternal0BorrowExpression :: TermParser HidUnaryExpressionInternal0L
    pHidUnaryExpressionInternal0BorrowExpression =
      HidUnaryExpressionInternal0BorrowExpression' <$> pBorrowExpression
    pHidUnaryExpressionInternal0DereferenceExpression :: TermParser HidUnaryExpressionInternal0L
    pHidUnaryExpressionInternal0DereferenceExpression =
      HidUnaryExpressionInternal0DereferenceExpression' <$> pDereferenceExpression
    pHidUnaryExpressionInternal0MoveOrCopyExpression :: TermParser HidUnaryExpressionInternal0L
    pHidUnaryExpressionInternal0MoveOrCopyExpression =
      HidUnaryExpressionInternal0MoveOrCopyExpression' <$> pMoveOrCopyExpression
    pHidUnaryExpressionInternal0HidExpressionTerm :: TermParser HidUnaryExpressionInternal0L
    pHidUnaryExpressionInternal0HidExpressionTerm =
      HidUnaryExpressionInternal0HidExpressionTerm' <$> pHidExpressionTerm

pBorrowExpression :: TermParser BorrowExpressionL
pBorrowExpression =
  BorrowExpression' <$> pPair pHidReference pHidExpression

pHidReference :: TermParser HidReferenceL
pHidReference =
  choice [ Megaparsec.try pHidReferenceImmRef
         , Megaparsec.try pHidReferenceMutRef
         ]
  where
    pHidReferenceImmRef :: TermParser HidReferenceL
    pHidReferenceImmRef =
      HidReferenceImmRef' <$> pImmRef
    pHidReferenceMutRef :: TermParser HidReferenceL
    pHidReferenceMutRef =
      HidReferenceMutRef' <$> pMutRef

pImmRef :: TermParser ImmRefL
pImmRef =
  ImmRef' <$> pAmpersandTok

pMutRef :: TermParser MutRefL
pMutRef =
  MutRef' <$> pAmpersandTok <*> pMutTok

pDereferenceExpression :: TermParser DereferenceExpressionL
pDereferenceExpression =
  DereferenceExpression' <$> pPair pAsteriskTok pHidExpression

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
      HidExpressionTermCallExpression' <$> pCallExpression
    pHidExpressionTermBreakExpression :: TermParser HidExpressionTermL
    pHidExpressionTermBreakExpression =
      HidExpressionTermBreakExpression' <$> pBreakExpression
    pHidExpressionTermContinueExpression :: TermParser HidExpressionTermL
    pHidExpressionTermContinueExpression =
      HidExpressionTermContinueExpression' <$> pContinueExpression
    pHidExpressionTermNameExpression :: TermParser HidExpressionTermL
    pHidExpressionTermNameExpression =
      HidExpressionTermNameExpression' <$> pNameExpression
    pHidExpressionTermMacroCallExpression :: TermParser HidExpressionTermL
    pHidExpressionTermMacroCallExpression =
      HidExpressionTermMacroCallExpression' <$> pMacroCallExpression
    pHidExpressionTermPackExpression :: TermParser HidExpressionTermL
    pHidExpressionTermPackExpression =
      HidExpressionTermPackExpression' <$> pPackExpression
    pHidExpressionTermHidLiteralValue :: TermParser HidExpressionTermL
    pHidExpressionTermHidLiteralValue =
      HidExpressionTermHidLiteralValue' <$> pHidLiteralValue
    pHidExpressionTermUnitExpression :: TermParser HidExpressionTermL
    pHidExpressionTermUnitExpression =
      HidExpressionTermUnitExpression' <$> pUnitExpression
    pHidExpressionTermExpressionList :: TermParser HidExpressionTermL
    pHidExpressionTermExpressionList =
      HidExpressionTermExpressionList' <$> pExpressionList
    pHidExpressionTermAnnotationExpression :: TermParser HidExpressionTermL
    pHidExpressionTermAnnotationExpression =
      HidExpressionTermAnnotationExpression' <$> pAnnotationExpression
    pHidExpressionTermBlock :: TermParser HidExpressionTermL
    pHidExpressionTermBlock =
      HidExpressionTermBlock' <$> pBlock
    pHidExpressionTermSpecBlock :: TermParser HidExpressionTermL
    pHidExpressionTermSpecBlock =
      HidExpressionTermSpecBlock' <$> pSpecBlock
    pHidExpressionTermIfExpression :: TermParser HidExpressionTermL
    pHidExpressionTermIfExpression =
      HidExpressionTermIfExpression' <$> pIfExpression
    pHidExpressionTermDotExpression :: TermParser HidExpressionTermL
    pHidExpressionTermDotExpression =
      HidExpressionTermDotExpression' <$> pDotExpression
    pHidExpressionTermIndexExpression :: TermParser HidExpressionTermL
    pHidExpressionTermIndexExpression =
      HidExpressionTermIndexExpression' <$> pIndexExpression
    pHidExpressionTermVectorExpression :: TermParser HidExpressionTermL
    pHidExpressionTermVectorExpression =
      HidExpressionTermVectorExpression' <$> pVectorExpression
    pHidExpressionTermMatchExpression :: TermParser HidExpressionTermL
    pHidExpressionTermMatchExpression =
      HidExpressionTermMatchExpression' <$> pMatchExpression

pAnnotationExpression :: TermParser AnnotationExpressionL
pAnnotationExpression =
  AnnotationExpression' <$> pBetween pLeftParenthesisTok pColonTok pHidExpression <*> pHidType <*> pRightParenthesisTok

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
      HidTypeApplyType' <$> pApplyType
    pHidTypeRefType :: TermParser HidTypeL
    pHidTypeRefType =
      HidTypeRefType' <$> pRefType
    pHidTypeTupleType :: TermParser HidTypeL
    pHidTypeTupleType =
      HidTypeTupleType' <$> pTupleType
    pHidTypeFunctionType :: TermParser HidTypeL
    pHidTypeFunctionType =
      HidTypeFunctionType' <$> pFunctionType
    pHidTypePrimitiveType :: TermParser HidTypeL
    pHidTypePrimitiveType =
      HidTypePrimitiveType' <$> pPrimitiveType

pApplyType :: TermParser ApplyTypeL
pApplyType =
  ApplyType' <$> pPair pModuleAccess (pMaybe pTypeArguments)

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
      ModuleAccess1' <$> pDollarSignTok <*> pIdentifier
    pModuleAccess2 :: TermParser ModuleAccessL
    pModuleAccess2 =
      ModuleAccess2' <$> pCommercialAtTok <*> pIdentifier
    pModuleAccess3 :: TermParser ModuleAccessL
    pModuleAccess3 =
      ModuleAccess3' <$> pModuleIdentity <*> pColonColonTok <*> pIdentifier <*> pMaybe pTypeArguments <*> pColonColonTok <*> pIdentifier
    pModuleAccess4 :: TermParser ModuleAccessL
    pModuleAccess4 =
      ModuleAccess4' <$> pModuleIdentity <*> pColonColonTok <*> pIdentifier <*> pTypeArguments
    pModuleAccess5 :: TermParser ModuleAccessL
    pModuleAccess5 =
      ModuleAccess5' <$> pModuleIdentity <*> pMaybe pTypeArguments <*> pColonColonTok <*> pIdentifier
    pModuleAccess6 :: TermParser ModuleAccessL
    pModuleAccess6 =
      ModuleAccess6' <$> pHidModuleIdentifier <*> pMaybe pTypeArguments <*> pColonColonTok <*> pIdentifier
    pModuleAccess7 :: TermParser ModuleAccessL
    pModuleAccess7 =
      ModuleAccess7' <$> pModuleIdentity <*> pMaybe pTypeArguments
    pModuleAccess8 :: TermParser ModuleAccessL
    pModuleAccess8 =
      ModuleAccess8' <$> pIdentifier <*> pMaybe pTypeArguments
    pModuleAccessMember :: TermParser ModuleAccessL
    pModuleAccessMember =
      ModuleAccessMember' <$> pHidReservedIdentifier

pHidModuleIdentifier :: TermParser HidModuleIdentifierL
pHidModuleIdentifier =
  HidModuleIdentifier' <$> pIdentifier

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
      HidReservedIdentifierHidForall' <$> pHidForall
    pHidReservedIdentifierHidExists :: TermParser HidReservedIdentifierL
    pHidReservedIdentifierHidExists =
      HidReservedIdentifierHidExists' <$> pHidExists

pHidExists :: TermParser HidExistsL
pHidExists =
  HidExists' <$> pExistsTok

pHidForall :: TermParser HidForallL
pHidForall =
  HidForall' <$> pForallTok

pModuleIdentity :: TermParser ModuleIdentityL
pModuleIdentity =
  ModuleIdentity' <$> (pModuleIdentityInternal0) <*> pColonColonTok <*> pHidModuleIdentifier

pModuleIdentityInternal0 :: TermParser ModuleIdentityInternal0L
pModuleIdentityInternal0 =
  choice [ Megaparsec.try pModuleIdentityInternal0NumLiteral
         , Megaparsec.try pModuleIdentityInternal0HidModuleIdentifier
         ]
  where
    pModuleIdentityInternal0NumLiteral :: TermParser ModuleIdentityInternal0L
    pModuleIdentityInternal0NumLiteral =
      ModuleIdentityInternal0NumLiteral' <$> pNumLiteral
    pModuleIdentityInternal0HidModuleIdentifier :: TermParser ModuleIdentityInternal0L
    pModuleIdentityInternal0HidModuleIdentifier =
      ModuleIdentityInternal0HidModuleIdentifier' <$> pHidModuleIdentifier

pNumLiteral :: TermParser NumLiteralL
pNumLiteral =
  NumLiteral' <$> pContent <*> pMaybe (pNumLiteralInternal0)

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
      NumLiteralInternal0U8' <$> pU8Tok
    pNumLiteralInternal0U16 :: TermParser NumLiteralInternal0L
    pNumLiteralInternal0U16 =
      NumLiteralInternal0U16' <$> pU16Tok
    pNumLiteralInternal0U32 :: TermParser NumLiteralInternal0L
    pNumLiteralInternal0U32 =
      NumLiteralInternal0U32' <$> pU32Tok
    pNumLiteralInternal0U64 :: TermParser NumLiteralInternal0L
    pNumLiteralInternal0U64 =
      NumLiteralInternal0U64' <$> pU64Tok
    pNumLiteralInternal0U128 :: TermParser NumLiteralInternal0L
    pNumLiteralInternal0U128 =
      NumLiteralInternal0U128' <$> pU128Tok
    pNumLiteralInternal0U256 :: TermParser NumLiteralInternal0L
    pNumLiteralInternal0U256 =
      NumLiteralInternal0U256' <$> pU256Tok

pTypeArguments :: TermParser TypeArgumentsL
pTypeArguments =
  TypeArguments' <$> pBetween pLessThanSignTok pGreaterThanSignTok (pSepBy1 pHidType pCommaTok)

pFunctionType :: TermParser FunctionTypeL
pFunctionType =
  FunctionType' <$> pFunctionTypeParameters <*> pMaybe (pPair pHyphenMinusGreaterThanSignTok pHidType)

pFunctionTypeParameters :: TermParser FunctionTypeParametersL
pFunctionTypeParameters =
  FunctionTypeParameters' <$> pBetween pVerticalLineTok pVerticalLineTok (pSepBy pHidType pCommaTok)

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
      PrimitiveTypeU8' <$> pU8Tok
    pPrimitiveTypeU16 :: TermParser PrimitiveTypeL
    pPrimitiveTypeU16 =
      PrimitiveTypeU16' <$> pU16Tok
    pPrimitiveTypeU32 :: TermParser PrimitiveTypeL
    pPrimitiveTypeU32 =
      PrimitiveTypeU32' <$> pU32Tok
    pPrimitiveTypeU64 :: TermParser PrimitiveTypeL
    pPrimitiveTypeU64 =
      PrimitiveTypeU64' <$> pU64Tok
    pPrimitiveTypeU128 :: TermParser PrimitiveTypeL
    pPrimitiveTypeU128 =
      PrimitiveTypeU128' <$> pU128Tok
    pPrimitiveTypeU256 :: TermParser PrimitiveTypeL
    pPrimitiveTypeU256 =
      PrimitiveTypeU256' <$> pU256Tok
    pPrimitiveTypeBool :: TermParser PrimitiveTypeL
    pPrimitiveTypeBool =
      PrimitiveTypeBool' <$> pBoolTok
    pPrimitiveTypeAddress :: TermParser PrimitiveTypeL
    pPrimitiveTypeAddress =
      PrimitiveTypeAddress' <$> pAddressTok
    pPrimitiveTypeSigner :: TermParser PrimitiveTypeL
    pPrimitiveTypeSigner =
      PrimitiveTypeSigner' <$> pSignerTok
    pPrimitiveTypeBytearray :: TermParser PrimitiveTypeL
    pPrimitiveTypeBytearray =
      PrimitiveTypeBytearray' <$> pBytearrayTok

pRefType :: TermParser RefTypeL
pRefType =
  RefType' <$> pHidReference <*> pHidType

pTupleType :: TermParser TupleTypeL
pTupleType =
  TupleType' <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy pHidType pCommaTok)

pBlock :: TermParser BlockL
pBlock =
  Block' <$> pLeftCurlyBracketTok <*> pMany pUseDeclaration <*> pMany pBlockItem <*> pMaybe pHidExpression <*> pRightCurlyBracketTok

pBlockItem :: TermParser BlockItemL
pBlockItem =
  BlockItem' <$> (pBlockItemInternal0) <*> pSemicolonTok

pBlockItemInternal0 :: TermParser BlockItemInternal0L
pBlockItemInternal0 =
  choice [ Megaparsec.try pBlockItemInternal0HidExpression
         , Megaparsec.try pBlockItemInternal0LetStatement
         ]
  where
    pBlockItemInternal0HidExpression :: TermParser BlockItemInternal0L
    pBlockItemInternal0HidExpression =
      BlockItemInternal0HidExpression' <$> pHidExpression
    pBlockItemInternal0LetStatement :: TermParser BlockItemInternal0L
    pBlockItemInternal0LetStatement =
      BlockItemInternal0LetStatement' <$> pLetStatement

pLetStatement :: TermParser LetStatementL
pLetStatement =
  LetStatement' <$> pLetTok <*> pBindList <*> pMaybe (pPair pColonTok pHidType) <*> pMaybe (pPair pEqualsSignTok pHidExpression)

pBindList :: TermParser BindListL
pBindList =
  choice [ Megaparsec.try pBindListHidBind
         , Megaparsec.try pBindListCommaBindList
         , Megaparsec.try pBindListOrBindList
         ]
  where
    pBindListHidBind :: TermParser BindListL
    pBindListHidBind =
      BindListHidBind' <$> pHidBind
    pBindListCommaBindList :: TermParser BindListL
    pBindListCommaBindList =
      BindListCommaBindList' <$> pCommaBindList
    pBindListOrBindList :: TermParser BindListL
    pBindListOrBindList =
      BindListOrBindList' <$> pOrBindList

pCommaBindList :: TermParser CommaBindListL
pCommaBindList =
  CommaBindList' <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy pHidBind pCommaTok)

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
      HidBindHidBindInternal0' <$> (pHidBindInternal0)
    pHidBindBindUnpack :: TermParser HidBindL
    pHidBindBindUnpack =
      HidBindBindUnpack' <$> pBindUnpack
    pHidBindAtBind :: TermParser HidBindL
    pHidBindAtBind =
      HidBindAtBind' <$> pAtBind
    pHidBindHidLiteralValue :: TermParser HidBindL
    pHidBindHidLiteralValue =
      HidBindHidLiteralValue' <$> pHidLiteralValue

pAtBind :: TermParser AtBindL
pAtBind =
  AtBind' <$> pHidVariableIdentifier <*> pCommercialAtTok <*> pBindList

pHidVariableIdentifier :: TermParser HidVariableIdentifierL
pHidVariableIdentifier =
  HidVariableIdentifier' <$> pIdentifier

pBindUnpack :: TermParser BindUnpackL
pBindUnpack =
  BindUnpack' <$> pNameExpression <*> pMaybe pBindFields

pBindFields :: TermParser BindFieldsL
pBindFields =
  choice [ Megaparsec.try pBindFieldsBindPositionalFields
         , Megaparsec.try pBindFieldsBindNamedFields
         ]
  where
    pBindFieldsBindPositionalFields :: TermParser BindFieldsL
    pBindFieldsBindPositionalFields =
      BindFieldsBindPositionalFields' <$> pBindPositionalFields
    pBindFieldsBindNamedFields :: TermParser BindFieldsL
    pBindFieldsBindNamedFields =
      BindFieldsBindNamedFields' <$> pBindNamedFields

pBindNamedFields :: TermParser BindNamedFieldsL
pBindNamedFields =
  BindNamedFields' <$> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy (pBindNamedFieldsInternal0) pCommaTok)

pBindNamedFieldsInternal0 :: TermParser BindNamedFieldsInternal0L
pBindNamedFieldsInternal0 =
  choice [ Megaparsec.try pBindNamedFieldsInternal0BindField
         , Megaparsec.try pBindNamedFieldsInternal0MutBindField
         ]
  where
    pBindNamedFieldsInternal0BindField :: TermParser BindNamedFieldsInternal0L
    pBindNamedFieldsInternal0BindField =
      BindNamedFieldsInternal0BindField' <$> pBindField
    pBindNamedFieldsInternal0MutBindField :: TermParser BindNamedFieldsInternal0L
    pBindNamedFieldsInternal0MutBindField =
      BindNamedFieldsInternal0MutBindField' <$> pMutBindField

pBindField :: TermParser BindFieldL
pBindField =
  choice [ Megaparsec.try pBindField1
         , Megaparsec.try pBindFieldHidSpreadOperator
         ]
  where
    pBindField1 :: TermParser BindFieldL
    pBindField1 =
      BindField1' <$> pBindList <*> pMaybe (pPair pColonTok pBindList)
    pBindFieldHidSpreadOperator :: TermParser BindFieldL
    pBindFieldHidSpreadOperator =
      BindFieldHidSpreadOperator' <$> pHidSpreadOperator

pHidSpreadOperator :: TermParser HidSpreadOperatorL
pHidSpreadOperator =
  HidSpreadOperator' <$> pFullStopFullStopTok

pMutBindField :: TermParser MutBindFieldL
pMutBindField =
  MutBindField' <$> pMutTok <*> pBindField

pBindPositionalFields :: TermParser BindPositionalFieldsL
pBindPositionalFields =
  BindPositionalFields' <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy (pBindNamedFieldsInternal0) pCommaTok)

pNameExpression :: TermParser NameExpressionL
pNameExpression =
  NameExpression' <$> pMaybe pColonColonTok <*> pModuleAccess

pHidBindInternal0 :: TermParser HidBindInternal0L
pHidBindInternal0 =
  choice [ Megaparsec.try pHidBindInternal0MutBindVar
         , Megaparsec.try pHidBindInternal0HidVariableIdentifier
         ]
  where
    pHidBindInternal0MutBindVar :: TermParser HidBindInternal0L
    pHidBindInternal0MutBindVar =
      HidBindInternal0MutBindVar' <$> pMutBindVar
    pHidBindInternal0HidVariableIdentifier :: TermParser HidBindInternal0L
    pHidBindInternal0HidVariableIdentifier =
      HidBindInternal0HidVariableIdentifier' <$> pHidVariableIdentifier

pMutBindVar :: TermParser MutBindVarL
pMutBindVar =
  MutBindVar' <$> pMutTok <*> pHidVariableIdentifier

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
      HidLiteralValueAddressLiteral' <$> pAddressLiteral
    pHidLiteralValueBoolLiteral :: TermParser HidLiteralValueL
    pHidLiteralValueBoolLiteral =
      HidLiteralValueBoolLiteral' <$> pBoolLiteral
    pHidLiteralValueNumLiteral :: TermParser HidLiteralValueL
    pHidLiteralValueNumLiteral =
      HidLiteralValueNumLiteral' <$> pNumLiteral
    pHidLiteralValueHexStringLiteral :: TermParser HidLiteralValueL
    pHidLiteralValueHexStringLiteral =
      HidLiteralValueHexStringLiteral' <$> pHexStringLiteral
    pHidLiteralValueByteStringLiteral :: TermParser HidLiteralValueL
    pHidLiteralValueByteStringLiteral =
      HidLiteralValueByteStringLiteral' <$> pByteStringLiteral

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
      BoolLiteralTrue' <$> pTrueTok
    pBoolLiteralFalse :: TermParser BoolLiteralL
    pBoolLiteralFalse =
      BoolLiteralFalse' <$> pFalseTok

pByteStringLiteral :: TermParser ByteStringLiteralL
pByteStringLiteral =
  ByteStringLiteral' <$> pContent

pHexStringLiteral :: TermParser HexStringLiteralL
pHexStringLiteral =
  HexStringLiteral' <$> pContent

pOrBindList :: TermParser OrBindListL
pOrBindList =
  OrBindList' <$> pMaybe pLeftParenthesisTok <*> pSepBy1 (pPair (pPair (pMaybe pLeftParenthesisTok) pHidBind) (pMaybe pRightParenthesisTok)) pVerticalLineTok <*> pMaybe pRightParenthesisTok

pUseDeclaration :: TermParser UseDeclarationL
pUseDeclaration =
  UseDeclaration' <$> pMaybe pPublicTok <*> pBetween pUseTok pSemicolonTok (pUseDeclarationInternal0)

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
      UseDeclarationInternal0UseFun' <$> pUseFun
    pUseDeclarationInternal0UseModule :: TermParser UseDeclarationInternal0L
    pUseDeclarationInternal0UseModule =
      UseDeclarationInternal0UseModule' <$> pUseModule
    pUseDeclarationInternal0UseModuleMember :: TermParser UseDeclarationInternal0L
    pUseDeclarationInternal0UseModuleMember =
      UseDeclarationInternal0UseModuleMember' <$> pUseModuleMember
    pUseDeclarationInternal0UseModuleMembers :: TermParser UseDeclarationInternal0L
    pUseDeclarationInternal0UseModuleMembers =
      UseDeclarationInternal0UseModuleMembers' <$> pUseModuleMembers

pUseFun :: TermParser UseFunL
pUseFun =
  UseFun' <$> pBetween pFunTok pAsTok pModuleAccess <*> pPair (pPair pModuleAccess pFullStopTok) pHidFunctionIdentifier

pHidFunctionIdentifier :: TermParser HidFunctionIdentifierL
pHidFunctionIdentifier =
  HidFunctionIdentifier' <$> pIdentifier

pUseModule :: TermParser UseModuleL
pUseModule =
  UseModule' <$> pModuleIdentity <*> pMaybe (pPair pAsTok pHidModuleIdentifier)

pUseModuleMember :: TermParser UseModuleMemberL
pUseModuleMember =
  UseModuleMember' <$> pModuleIdentity <*> pColonColonTok <*> pUseMember

pUseMember :: TermParser UseMemberL
pUseMember =
  choice [ Megaparsec.try pUseMember1
         , Megaparsec.try pUseMember2
         , Megaparsec.try pUseMember3
         ]
  where
    pUseMember1 :: TermParser UseMemberL
    pUseMember1 =
      UseMember1' <$> pIdentifier <*> pColonColonTok <*> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy1 pUseMember pCommaTok)
    pUseMember2 :: TermParser UseMemberL
    pUseMember2 =
      UseMember2' <$> pIdentifier <*> pColonColonTok <*> pIdentifier <*> pMaybe (pPair pAsTok pIdentifier)
    pUseMember3 :: TermParser UseMemberL
    pUseMember3 =
      UseMember3' <$> pIdentifier <*> pMaybe (pPair pAsTok pIdentifier)

pUseModuleMembers :: TermParser UseModuleMembersL
pUseModuleMembers =
  choice [ Megaparsec.try pUseModuleMembers1
         , Megaparsec.try pUseModuleMembers2
         ]
  where
    pUseModuleMembers1 :: TermParser UseModuleMembersL
    pUseModuleMembers1 =
      UseModuleMembers1' <$> (pModuleIdentityInternal0) <*> pColonColonTok <*> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy1 pUseMember pCommaTok)
    pUseModuleMembers2 :: TermParser UseModuleMembersL
    pUseModuleMembers2 =
      UseModuleMembers2' <$> pModuleIdentity <*> pColonColonTok <*> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy1 pUseMember pCommaTok)

pBreakExpression :: TermParser BreakExpressionL
pBreakExpression =
  BreakExpression' <$> pBreakTok <*> pMaybe pLabel <*> pMaybe pHidExpression

pLabel :: TermParser LabelL
pLabel =
  Label' <$> pApostropheTok <*> pIdentifier

pCallExpression :: TermParser CallExpressionL
pCallExpression =
  CallExpression' <$> pPair pNameExpression pArgList

pArgList :: TermParser ArgListL
pArgList =
  ArgList' <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy pHidExpression pCommaTok)

pContinueExpression :: TermParser ContinueExpressionL
pContinueExpression =
  ContinueExpression' <$> pContinueTok <*> pMaybe pLabel

pDotExpression :: TermParser DotExpressionL
pDotExpression =
  DotExpression' <$> pPair (pPair pHidExpressionTerm pFullStopTok) pHidExpressionTerm

pExpressionList :: TermParser ExpressionListL
pExpressionList =
  ExpressionList' <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy1 pHidExpression pCommaTok)

pIfExpression :: TermParser IfExpressionL
pIfExpression =
  IfExpression' <$> pPair (pPair (pPair pIfTok (pBetween pLeftParenthesisTok pRightParenthesisTok pHidExpression)) pHidExpression) (pMaybe (pPair pElseTok pHidExpression))

pIndexExpression :: TermParser IndexExpressionL
pIndexExpression =
  IndexExpression' <$> pPair pHidExpressionTerm (pBetween pLeftSquareBracketTok pRightSquareBracketTok (pSepBy pHidExpression pCommaTok))

pMacroCallExpression :: TermParser MacroCallExpressionL
pMacroCallExpression =
  MacroCallExpression' <$> pMacroModuleAccess <*> pMaybe pTypeArguments <*> pArgList

pMacroModuleAccess :: TermParser MacroModuleAccessL
pMacroModuleAccess =
  MacroModuleAccess' <$> pModuleAccess <*> pExclamationMarkTok

pMatchExpression :: TermParser MatchExpressionL
pMatchExpression =
  MatchExpression' <$> pMatchTok <*> pBetween pLeftParenthesisTok pRightParenthesisTok pHidExpression <*> pHidMatchBody

pHidMatchBody :: TermParser HidMatchBodyL
pHidMatchBody =
  HidMatchBody' <$> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy pMatchArm pCommaTok)

pMatchArm :: TermParser MatchArmL
pMatchArm =
  MatchArm' <$> pBindList <*> pMaybe pMatchCondition <*> pEqualsSignGreaterThanSignTok <*> pHidExpression

pMatchCondition :: TermParser MatchConditionL
pMatchCondition =
  MatchCondition' <$> pIfTok <*> pBetween pLeftParenthesisTok pRightParenthesisTok pHidExpression

pPackExpression :: TermParser PackExpressionL
pPackExpression =
  PackExpression' <$> pNameExpression <*> pFieldInitializeList

pFieldInitializeList :: TermParser FieldInitializeListL
pFieldInitializeList =
  FieldInitializeList' <$> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy pExpField pCommaTok)

pExpField :: TermParser ExpFieldL
pExpField =
  ExpField' <$> pHidFieldIdentifier <*> pMaybe (pPair pColonTok pHidExpression)

pHidFieldIdentifier :: TermParser HidFieldIdentifierL
pHidFieldIdentifier =
  HidFieldIdentifier' <$> pIdentifier

pSpecBlock :: TermParser SpecBlockL
pSpecBlock =
  SpecBlock' <$> pSpecTok <*> (pSpecBlockInternal0)

pSpecBlockInternal0 :: TermParser SpecBlockInternal0L
pSpecBlockInternal0 =
  choice [ Megaparsec.try pSpecBlockInternal01
         , Megaparsec.try pSpecBlockInternal0HidSpecFunction
         ]
  where
    pSpecBlockInternal01 :: TermParser SpecBlockInternal0L
    pSpecBlockInternal01 =
      SpecBlockInternal01' <$> pMaybe pHidSpecBlockTarget <*> pSpecBody
    pSpecBlockInternal0HidSpecFunction :: TermParser SpecBlockInternal0L
    pSpecBlockInternal0HidSpecFunction =
      SpecBlockInternal0HidSpecFunction' <$> pHidSpecFunction

pHidSpecBlockTarget :: TermParser HidSpecBlockTargetL
pHidSpecBlockTarget =
  choice [ Megaparsec.try pHidSpecBlockTargetIdentifier
         , Megaparsec.try pHidSpecBlockTargetModule
         , Megaparsec.try pHidSpecBlockTargetSpecBlockTargetSchema
         ]
  where
    pHidSpecBlockTargetIdentifier :: TermParser HidSpecBlockTargetL
    pHidSpecBlockTargetIdentifier =
      HidSpecBlockTargetIdentifier' <$> pIdentifier
    pHidSpecBlockTargetModule :: TermParser HidSpecBlockTargetL
    pHidSpecBlockTargetModule =
      HidSpecBlockTargetModule' <$> pModuleTok
    pHidSpecBlockTargetSpecBlockTargetSchema :: TermParser HidSpecBlockTargetL
    pHidSpecBlockTargetSpecBlockTargetSchema =
      HidSpecBlockTargetSpecBlockTargetSchema' <$> pSpecBlockTargetSchema

pSpecBlockTargetSchema :: TermParser SpecBlockTargetSchemaL
pSpecBlockTargetSchema =
  SpecBlockTargetSchema' <$> pSchemaTok <*> pHidStructIdentifier <*> pMaybe pTypeParameters

pHidStructIdentifier :: TermParser HidStructIdentifierL
pHidStructIdentifier =
  HidStructIdentifier' <$> pIdentifier

pTypeParameters :: TermParser TypeParametersL
pTypeParameters =
  TypeParameters' <$> pBetween pLessThanSignTok pGreaterThanSignTok (pSepBy1 pTypeParameter pCommaTok)

pTypeParameter :: TermParser TypeParameterL
pTypeParameter =
  TypeParameter' <$> pMaybe pDollarSignTok <*> pMaybe pPhantomTok <*> pHidTypeParameterIdentifier <*> pMaybe (pPair pColonTok (pSepBy1 pAbility pPlusSignTok))

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
      AbilityCopy' <$> pCopyTok
    pAbilityDrop :: TermParser AbilityL
    pAbilityDrop =
      AbilityDrop' <$> pDropTok
    pAbilityStore :: TermParser AbilityL
    pAbilityStore =
      AbilityStore' <$> pStoreTok
    pAbilityKey :: TermParser AbilityL
    pAbilityKey =
      AbilityKey' <$> pKeyTok

pHidTypeParameterIdentifier :: TermParser HidTypeParameterIdentifierL
pHidTypeParameterIdentifier =
  HidTypeParameterIdentifier' <$> pIdentifier

pHidSpecFunction :: TermParser HidSpecFunctionL
pHidSpecFunction =
  choice [ Megaparsec.try pHidSpecFunctionNativeSpecFunction
         , Megaparsec.try pHidSpecFunctionUsualSpecFunction
         , Megaparsec.try pHidSpecFunctionUninterpretedSpecFunction
         ]
  where
    pHidSpecFunctionNativeSpecFunction :: TermParser HidSpecFunctionL
    pHidSpecFunctionNativeSpecFunction =
      HidSpecFunctionNativeSpecFunction' <$> pNativeSpecFunction
    pHidSpecFunctionUsualSpecFunction :: TermParser HidSpecFunctionL
    pHidSpecFunctionUsualSpecFunction =
      HidSpecFunctionUsualSpecFunction' <$> pUsualSpecFunction
    pHidSpecFunctionUninterpretedSpecFunction :: TermParser HidSpecFunctionL
    pHidSpecFunctionUninterpretedSpecFunction =
      HidSpecFunctionUninterpretedSpecFunction' <$> pUninterpretedSpecFunction

pNativeSpecFunction :: TermParser NativeSpecFunctionL
pNativeSpecFunction =
  NativeSpecFunction' <$> pNativeTok <*> pBetween pFunTok pSemicolonTok pHidSpecFunctionSignature

pHidSpecFunctionSignature :: TermParser HidSpecFunctionSignatureL
pHidSpecFunctionSignature =
  HidSpecFunctionSignature' <$> pHidFunctionIdentifier <*> pMaybe pTypeParameters <*> pFunctionParameters <*> pRetType

pFunctionParameters :: TermParser FunctionParametersL
pFunctionParameters =
  FunctionParameters' <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy (pFunctionParametersInternal0) pCommaTok)

pFunctionParametersInternal0 :: TermParser FunctionParametersInternal0L
pFunctionParametersInternal0 =
  choice [ Megaparsec.try pFunctionParametersInternal0MutFunctionParameter
         , Megaparsec.try pFunctionParametersInternal0FunctionParameter
         ]
  where
    pFunctionParametersInternal0MutFunctionParameter :: TermParser FunctionParametersInternal0L
    pFunctionParametersInternal0MutFunctionParameter =
      FunctionParametersInternal0MutFunctionParameter' <$> pMutFunctionParameter
    pFunctionParametersInternal0FunctionParameter :: TermParser FunctionParametersInternal0L
    pFunctionParametersInternal0FunctionParameter =
      FunctionParametersInternal0FunctionParameter' <$> pFunctionParameter

pFunctionParameter :: TermParser FunctionParameterL
pFunctionParameter =
  FunctionParameter' <$> (pFunctionParameterInternal0) <*> pColonTok <*> pHidType

pFunctionParameterInternal0 :: TermParser FunctionParameterInternal0L
pFunctionParameterInternal0 =
  choice [ Megaparsec.try pFunctionParameterInternal0Name
         , Megaparsec.try pFunctionParameterInternal02
         ]
  where
    pFunctionParameterInternal0Name :: TermParser FunctionParameterInternal0L
    pFunctionParameterInternal0Name =
      FunctionParameterInternal0Name' <$> pHidVariableIdentifier
    pFunctionParameterInternal02 :: TermParser FunctionParameterInternal0L
    pFunctionParameterInternal02 =
      FunctionParameterInternal02' <$> pDollarSignTok <*> pHidVariableIdentifier

pMutFunctionParameter :: TermParser MutFunctionParameterL
pMutFunctionParameter =
  MutFunctionParameter' <$> pMutTok <*> pFunctionParameter

pRetType :: TermParser RetTypeL
pRetType =
  RetType' <$> pColonTok <*> pHidType

pUninterpretedSpecFunction :: TermParser UninterpretedSpecFunctionL
pUninterpretedSpecFunction =
  UninterpretedSpecFunction' <$> pBetween pFunTok pSemicolonTok pHidSpecFunctionSignature

pUsualSpecFunction :: TermParser UsualSpecFunctionL
pUsualSpecFunction =
  UsualSpecFunction' <$> pFunTok <*> pHidSpecFunctionSignature <*> pBlock

pSpecBody :: TermParser SpecBodyL
pSpecBody =
  SpecBody' <$> pLeftCurlyBracketTok <*> pMany pUseDeclaration <*> pMany pHidSpecBlockMemeber <*> pRightCurlyBracketTok

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
      HidSpecBlockMemeberSpecInvariant' <$> pSpecInvariant
    pHidSpecBlockMemeberHidSpecFunction :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberHidSpecFunction =
      HidSpecBlockMemeberHidSpecFunction' <$> pHidSpecFunction
    pHidSpecBlockMemeberSpecCondition :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecCondition =
      HidSpecBlockMemeberSpecCondition' <$> pSpecCondition
    pHidSpecBlockMemeberSpecInclude :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecInclude =
      HidSpecBlockMemeberSpecInclude' <$> pSpecInclude
    pHidSpecBlockMemeberSpecApply :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecApply =
      HidSpecBlockMemeberSpecApply' <$> pSpecApply
    pHidSpecBlockMemeberSpecPragma :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecPragma =
      HidSpecBlockMemeberSpecPragma' <$> pSpecPragma
    pHidSpecBlockMemeberSpecVariable :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecVariable =
      HidSpecBlockMemeberSpecVariable' <$> pSpecVariable
    pHidSpecBlockMemeberSpecLet :: TermParser HidSpecBlockMemeberL
    pHidSpecBlockMemeberSpecLet =
      HidSpecBlockMemeberSpecLet' <$> pSpecLet

pSpecApply :: TermParser SpecApplyL
pSpecApply =
  SpecApply' <$> pBetween pApplyTok pToTok pHidExpression <*> pSepBy1 pSpecApplyPattern pCommaTok <*> pMaybe (pPair pExceptTok (pSepBy1 pSpecApplyPattern pCommaTok)) <*> pSemicolonTok

pSpecApplyPattern :: TermParser SpecApplyPatternL
pSpecApplyPattern =
  SpecApplyPattern' <$> pMaybe (pSpecApplyPatternInternal0) <*> pSpecApplyNamePattern <*> pMaybe pTypeParameters

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
      SpecApplyPatternInternal0Public' <$> pPublicTok
    pSpecApplyPatternInternal0Internal :: TermParser SpecApplyPatternInternal0L
    pSpecApplyPatternInternal0Internal =
      SpecApplyPatternInternal0Internal' <$> pInternalTok

pSpecCondition :: TermParser SpecConditionL
pSpecCondition =
  choice [ Megaparsec.try pSpecConditionHidSpecCondition
         , Megaparsec.try pSpecConditionHidSpecAbortIf
         , Megaparsec.try pSpecConditionHidSpecAbortWithOrModifies
         ]
  where
    pSpecConditionHidSpecCondition :: TermParser SpecConditionL
    pSpecConditionHidSpecCondition =
      SpecConditionHidSpecCondition' <$> pHidSpecCondition
    pSpecConditionHidSpecAbortIf :: TermParser SpecConditionL
    pSpecConditionHidSpecAbortIf =
      SpecConditionHidSpecAbortIf' <$> pHidSpecAbortIf
    pSpecConditionHidSpecAbortWithOrModifies :: TermParser SpecConditionL
    pSpecConditionHidSpecAbortWithOrModifies =
      SpecConditionHidSpecAbortWithOrModifies' <$> pHidSpecAbortWithOrModifies

pHidSpecAbortIf :: TermParser HidSpecAbortIfL
pHidSpecAbortIf =
  HidSpecAbortIf' <$> pAbortsIfTok <*> pMaybe pConditionProperties <*> pHidExpression <*> pMaybe (pPair pWithTok pHidExpression) <*> pSemicolonTok

pConditionProperties :: TermParser ConditionPropertiesL
pConditionProperties =
  ConditionProperties' <$> pBetween pLeftSquareBracketTok pRightSquareBracketTok (pSepBy pSpecProperty pCommaTok)

pSpecProperty :: TermParser SpecPropertyL
pSpecProperty =
  SpecProperty' <$> pIdentifier <*> pMaybe (pPair pEqualsSignTok pHidLiteralValue)

pHidSpecAbortWithOrModifies :: TermParser HidSpecAbortWithOrModifiesL
pHidSpecAbortWithOrModifies =
  HidSpecAbortWithOrModifies' <$> (pHidSpecAbortWithOrModifiesInternal0) <*> pMaybe pConditionProperties <*> pSepBy1 pHidExpression pCommaTok <*> pSemicolonTok

pHidSpecAbortWithOrModifiesInternal0 :: TermParser HidSpecAbortWithOrModifiesInternal0L
pHidSpecAbortWithOrModifiesInternal0 =
  choice [ Megaparsec.try pHidSpecAbortWithOrModifiesInternal0AbortsWith
         , Megaparsec.try pHidSpecAbortWithOrModifiesInternal0Modifies
         ]
  where
    pHidSpecAbortWithOrModifiesInternal0AbortsWith :: TermParser HidSpecAbortWithOrModifiesInternal0L
    pHidSpecAbortWithOrModifiesInternal0AbortsWith =
      HidSpecAbortWithOrModifiesInternal0AbortsWith' <$> pAbortsWithTok
    pHidSpecAbortWithOrModifiesInternal0Modifies :: TermParser HidSpecAbortWithOrModifiesInternal0L
    pHidSpecAbortWithOrModifiesInternal0Modifies =
      HidSpecAbortWithOrModifiesInternal0Modifies' <$> pModifiesTok

pHidSpecCondition :: TermParser HidSpecConditionL
pHidSpecCondition =
  HidSpecCondition' <$> (pHidSpecConditionInternal0) <*> pMaybe pConditionProperties <*> pHidExpression <*> pSemicolonTok

pHidSpecConditionInternal0 :: TermParser HidSpecConditionInternal0L
pHidSpecConditionInternal0 =
  choice [ Megaparsec.try pHidSpecConditionInternal0Kind
         , Megaparsec.try pHidSpecConditionInternal02
         ]
  where
    pHidSpecConditionInternal0Kind :: TermParser HidSpecConditionInternal0L
    pHidSpecConditionInternal0Kind =
      HidSpecConditionInternal0Kind' <$> pHidSpecConditionKind
    pHidSpecConditionInternal02 :: TermParser HidSpecConditionInternal0L
    pHidSpecConditionInternal02 =
      HidSpecConditionInternal02' <$> pRequiresTok <*> pMaybe pModuleTok

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
      HidSpecConditionKindAssert' <$> pAssertTok
    pHidSpecConditionKindAssume :: TermParser HidSpecConditionKindL
    pHidSpecConditionKindAssume =
      HidSpecConditionKindAssume' <$> pAssumeTok
    pHidSpecConditionKindDecreases :: TermParser HidSpecConditionKindL
    pHidSpecConditionKindDecreases =
      HidSpecConditionKindDecreases' <$> pDecreasesTok
    pHidSpecConditionKindEnsures :: TermParser HidSpecConditionKindL
    pHidSpecConditionKindEnsures =
      HidSpecConditionKindEnsures' <$> pEnsuresTok
    pHidSpecConditionKindSucceedsIf :: TermParser HidSpecConditionKindL
    pHidSpecConditionKindSucceedsIf =
      HidSpecConditionKindSucceedsIf' <$> pSucceedsIfTok

pSpecInclude :: TermParser SpecIncludeL
pSpecInclude =
  SpecInclude' <$> pBetween pIncludeTok pSemicolonTok pHidExpression

pSpecInvariant :: TermParser SpecInvariantL
pSpecInvariant =
  SpecInvariant' <$> pInvariantTok <*> pMaybe (pSpecInvariantInternal0) <*> pMaybe pConditionProperties <*> pHidExpression <*> pSemicolonTok

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
      SpecInvariantInternal0Update' <$> pUpdateTok
    pSpecInvariantInternal0Pack :: TermParser SpecInvariantInternal0L
    pSpecInvariantInternal0Pack =
      SpecInvariantInternal0Pack' <$> pPackTok
    pSpecInvariantInternal0Unpack :: TermParser SpecInvariantInternal0L
    pSpecInvariantInternal0Unpack =
      SpecInvariantInternal0Unpack' <$> pUnpackTok
    pSpecInvariantInternal0Module :: TermParser SpecInvariantInternal0L
    pSpecInvariantInternal0Module =
      SpecInvariantInternal0Module' <$> pModuleTok

pSpecLet :: TermParser SpecLetL
pSpecLet =
  SpecLet' <$> pLetTok <*> pMaybe pPostTok <*> pIdentifier <*> pBetween pEqualsSignTok pSemicolonTok pHidExpression

pSpecPragma :: TermParser SpecPragmaL
pSpecPragma =
  SpecPragma' <$> pBetween pPragmaTok pSemicolonTok (pSepBy pSpecProperty pCommaTok)

pSpecVariable :: TermParser SpecVariableL
pSpecVariable =
  SpecVariable' <$> pMaybe (pSpecVariableInternal0) <*> pIdentifier <*> pMaybe pTypeParameters <*> pBetween pColonTok pSemicolonTok pHidType

pSpecVariableInternal0 :: TermParser SpecVariableInternal0L
pSpecVariableInternal0 =
  choice [ Megaparsec.try pSpecVariableInternal0Global
         , Megaparsec.try pSpecVariableInternal0Local
         ]
  where
    pSpecVariableInternal0Global :: TermParser SpecVariableInternal0L
    pSpecVariableInternal0Global =
      SpecVariableInternal0Global' <$> pGlobalTok
    pSpecVariableInternal0Local :: TermParser SpecVariableInternal0L
    pSpecVariableInternal0Local =
      SpecVariableInternal0Local' <$> pLocalTok

pUnitExpression :: TermParser UnitExpressionL
pUnitExpression =
  UnitExpression' <$> pLeftParenthesisTok <*> pRightParenthesisTok

pVectorExpression :: TermParser VectorExpressionL
pVectorExpression =
  VectorExpression' <$> (pVectorExpressionInternal0) <*> pSepBy pHidExpression pCommaTok <*> pRightSquareBracketTok

pVectorExpressionInternal0 :: TermParser VectorExpressionInternal0L
pVectorExpressionInternal0 =
  choice [ Megaparsec.try pVectorExpressionInternal0VectorLeftSquareBracket
         , Megaparsec.try pVectorExpressionInternal02
         ]
  where
    pVectorExpressionInternal0VectorLeftSquareBracket :: TermParser VectorExpressionInternal0L
    pVectorExpressionInternal0VectorLeftSquareBracket =
      VectorExpressionInternal0VectorLeftSquareBracket' <$> pVectorLeftSquareBracketTok
    pVectorExpressionInternal02 :: TermParser VectorExpressionInternal0L
    pVectorExpressionInternal02 =
      VectorExpressionInternal02' <$> pBetween pVectorLessThanSignTok pGreaterThanSignTok (pSepBy1 pHidType pCommaTok) <*> pLeftSquareBracketTok

pMoveOrCopyExpression :: TermParser MoveOrCopyExpressionL
pMoveOrCopyExpression =
  MoveOrCopyExpression' <$> pPair (pMoveOrCopyExpressionInternal0) pHidExpression

pMoveOrCopyExpressionInternal0 :: TermParser MoveOrCopyExpressionInternal0L
pMoveOrCopyExpressionInternal0 =
  choice [ Megaparsec.try pMoveOrCopyExpressionInternal0Move
         , Megaparsec.try pMoveOrCopyExpressionInternal0Copy
         ]
  where
    pMoveOrCopyExpressionInternal0Move :: TermParser MoveOrCopyExpressionInternal0L
    pMoveOrCopyExpressionInternal0Move =
      MoveOrCopyExpressionInternal0Move' <$> pMoveTok
    pMoveOrCopyExpressionInternal0Copy :: TermParser MoveOrCopyExpressionInternal0L
    pMoveOrCopyExpressionInternal0Copy =
      MoveOrCopyExpressionInternal0Copy' <$> pCopyTok

pUnaryExpression :: TermParser UnaryExpressionL
pUnaryExpression =
  UnaryExpression' <$> pUnaryOp <*> pHidExpression

pUnaryOp :: TermParser UnaryOpL
pUnaryOp =
  UnaryOp' <$> pExclamationMarkTok

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
      BinaryExpression1' <$> pHidExpression <*> pEqualsSignEqualsSignGreaterThanSignTok <*> pHidExpression
    pBinaryExpression2 :: TermParser BinaryExpressionL
    pBinaryExpression2 =
      BinaryExpression2' <$> pHidExpression <*> pVerticalLineVerticalLineTok <*> pHidExpression
    pBinaryExpression3 :: TermParser BinaryExpressionL
    pBinaryExpression3 =
      BinaryExpression3' <$> pHidExpression <*> pAmpersandAmpersandTok <*> pHidExpression
    pBinaryExpression4 :: TermParser BinaryExpressionL
    pBinaryExpression4 =
      BinaryExpression4' <$> pHidExpression <*> pEqualsSignEqualsSignTok <*> pHidExpression
    pBinaryExpression5 :: TermParser BinaryExpressionL
    pBinaryExpression5 =
      BinaryExpression5' <$> pHidExpression <*> pExclamationMarkEqualsSignTok <*> pHidExpression
    pBinaryExpression6 :: TermParser BinaryExpressionL
    pBinaryExpression6 =
      BinaryExpression6' <$> pHidExpression <*> pLessThanSignTok <*> pHidExpression
    pBinaryExpression7 :: TermParser BinaryExpressionL
    pBinaryExpression7 =
      BinaryExpression7' <$> pHidExpression <*> pGreaterThanSignTok <*> pHidExpression
    pBinaryExpression8 :: TermParser BinaryExpressionL
    pBinaryExpression8 =
      BinaryExpression8' <$> pHidExpression <*> pLessThanSignEqualsSignTok <*> pHidExpression
    pBinaryExpression9 :: TermParser BinaryExpressionL
    pBinaryExpression9 =
      BinaryExpression9' <$> pHidExpression <*> pGreaterThanSignEqualsSignTok <*> pHidExpression
    pBinaryExpression10 :: TermParser BinaryExpressionL
    pBinaryExpression10 =
      BinaryExpression10' <$> pHidExpression <*> pFullStopFullStopTok <*> pHidExpression
    pBinaryExpression11 :: TermParser BinaryExpressionL
    pBinaryExpression11 =
      BinaryExpression11' <$> pHidExpression <*> pVerticalLineTok <*> pHidExpression
    pBinaryExpression12 :: TermParser BinaryExpressionL
    pBinaryExpression12 =
      BinaryExpression12' <$> pHidExpression <*> pCircumflexAccentTok <*> pHidExpression
    pBinaryExpression13 :: TermParser BinaryExpressionL
    pBinaryExpression13 =
      BinaryExpression13' <$> pHidExpression <*> pAmpersandTok <*> pHidExpression
    pBinaryExpression14 :: TermParser BinaryExpressionL
    pBinaryExpression14 =
      BinaryExpression14' <$> pHidExpression <*> pLessThanSignLessThanSignTok <*> pHidExpression
    pBinaryExpression15 :: TermParser BinaryExpressionL
    pBinaryExpression15 =
      BinaryExpression15' <$> pHidExpression <*> pGreaterThanSignGreaterThanSignTok <*> pHidExpression
    pBinaryExpression16 :: TermParser BinaryExpressionL
    pBinaryExpression16 =
      BinaryExpression16' <$> pHidExpression <*> pPlusSignTok <*> pHidExpression
    pBinaryExpression17 :: TermParser BinaryExpressionL
    pBinaryExpression17 =
      BinaryExpression17' <$> pHidExpression <*> pHyphenMinusTok <*> pHidExpression
    pBinaryExpression18 :: TermParser BinaryExpressionL
    pBinaryExpression18 =
      BinaryExpression18' <$> pHidExpression <*> pAsteriskTok <*> pHidExpression
    pBinaryExpression19 :: TermParser BinaryExpressionL
    pBinaryExpression19 =
      BinaryExpression19' <$> pHidExpression <*> pSolidusTok <*> pHidExpression
    pBinaryExpression20 :: TermParser BinaryExpressionL
    pBinaryExpression20 =
      BinaryExpression20' <$> pHidExpression <*> pPercentSignTok <*> pHidExpression

pCastExpression :: TermParser CastExpressionL
pCastExpression =
  CastExpression' <$> pPair (pPair pHidExpression pAsTok) pHidType

pIdentifiedExpression :: TermParser IdentifiedExpressionL
pIdentifiedExpression =
  IdentifiedExpression' <$> pBlockIdentifier <*> pHidExpression

pBlockIdentifier :: TermParser BlockIdentifierL
pBlockIdentifier =
  BlockIdentifier' <$> pLabel <*> pColonTok

pLambdaExpression :: TermParser LambdaExpressionL
pLambdaExpression =
  LambdaExpression' <$> pLambdaBindings <*> pMaybe (pPair pHyphenMinusGreaterThanSignTok pHidType) <*> pHidExpression

pLambdaBindings :: TermParser LambdaBindingsL
pLambdaBindings =
  LambdaBindings' <$> pBetween pVerticalLineTok pVerticalLineTok (pSepBy pLambdaBinding pCommaTok)

pLambdaBinding :: TermParser LambdaBindingL
pLambdaBinding =
  choice [ Megaparsec.try pLambdaBindingCommaBindList
         , Megaparsec.try pLambdaBinding2
         ]
  where
    pLambdaBindingCommaBindList :: TermParser LambdaBindingL
    pLambdaBindingCommaBindList =
      LambdaBindingCommaBindList' <$> pCommaBindList
    pLambdaBinding2 :: TermParser LambdaBindingL
    pLambdaBinding2 =
      LambdaBinding2' <$> pHidBind <*> pMaybe (pPair pColonTok pHidType)

pLoopExpression :: TermParser LoopExpressionL
pLoopExpression =
  LoopExpression' <$> pLoopTok <*> pHidExpression

pQuantifierExpression :: TermParser QuantifierExpressionL
pQuantifierExpression =
  QuantifierExpression' <$> pPair (pPair (pPair (pPair pHidReservedIdentifier pQuantifierBindings) (pMaybe (pPair pWhereTok pHidExpression))) pColonTok) pHidExpression

pQuantifierBindings :: TermParser QuantifierBindingsL
pQuantifierBindings =
  QuantifierBindings' <$> pSepBy1 pQuantifierBinding pCommaTok

pQuantifierBinding :: TermParser QuantifierBindingL
pQuantifierBinding =
  choice [ Megaparsec.try pQuantifierBinding1
         , Megaparsec.try pQuantifierBinding2
         ]
  where
    pQuantifierBinding1 :: TermParser QuantifierBindingL
    pQuantifierBinding1 =
      QuantifierBinding1' <$> pIdentifier <*> pColonTok <*> pHidType
    pQuantifierBinding2 :: TermParser QuantifierBindingL
    pQuantifierBinding2 =
      QuantifierBinding2' <$> pIdentifier <*> pInTok <*> pHidExpression

pReturnExpression :: TermParser ReturnExpressionL
pReturnExpression =
  choice [ Megaparsec.try pReturnExpression1
         , Megaparsec.try pReturnExpression2
         ]
  where
    pReturnExpression1 :: TermParser ReturnExpressionL
    pReturnExpression1 =
      ReturnExpression1' <$> pReturnTok <*> pMaybe pLabel <*> pHidExpression
    pReturnExpression2 :: TermParser ReturnExpressionL
    pReturnExpression2 =
      ReturnExpression2' <$> pReturnTok <*> pMaybe pLabel

pWhileExpression :: TermParser WhileExpressionL
pWhileExpression =
  WhileExpression' <$> pWhileTok <*> pBetween pLeftParenthesisTok pRightParenthesisTok pHidExpression <*> pHidExpression

pFriendDeclaration :: TermParser FriendDeclarationL
pFriendDeclaration =
  FriendDeclaration' <$> pBetween pFriendTok pSemicolonTok pFriendAccess

pFriendAccess :: TermParser FriendAccessL
pFriendAccess =
  choice [ Megaparsec.try pFriendAccessLocalModule
         , Megaparsec.try pFriendAccessFullyQualifiedModule
         ]
  where
    pFriendAccessLocalModule :: TermParser FriendAccessL
    pFriendAccessLocalModule =
      FriendAccessLocalModule' <$> pIdentifier
    pFriendAccessFullyQualifiedModule :: TermParser FriendAccessL
    pFriendAccessFullyQualifiedModule =
      FriendAccessFullyQualifiedModule' <$> pModuleIdentity

pHidEnumItem :: TermParser HidEnumItemL
pHidEnumItem =
  HidEnumItem' <$> pEnumDefinition

pEnumDefinition :: TermParser EnumDefinitionL
pEnumDefinition =
  EnumDefinition' <$> pMaybe pPublicTok <*> pHidEnumSignature <*> pEnumVariants <*> pMaybe pPostfixAbilityDecls

pEnumVariants :: TermParser EnumVariantsL
pEnumVariants =
  EnumVariants' <$> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy pVariant pCommaTok)

pVariant :: TermParser VariantL
pVariant =
  Variant' <$> pHidVariantIdentifier <*> pMaybe pDatatypeFields

pDatatypeFields :: TermParser DatatypeFieldsL
pDatatypeFields =
  choice [ Megaparsec.try pDatatypeFieldsPositionalFields
         , Megaparsec.try pDatatypeFieldsNamedFields
         ]
  where
    pDatatypeFieldsPositionalFields :: TermParser DatatypeFieldsL
    pDatatypeFieldsPositionalFields =
      DatatypeFieldsPositionalFields' <$> pPositionalFields
    pDatatypeFieldsNamedFields :: TermParser DatatypeFieldsL
    pDatatypeFieldsNamedFields =
      DatatypeFieldsNamedFields' <$> pNamedFields

pNamedFields :: TermParser NamedFieldsL
pNamedFields =
  NamedFields' <$> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy pFieldAnnotation pCommaTok)

pFieldAnnotation :: TermParser FieldAnnotationL
pFieldAnnotation =
  FieldAnnotation' <$> pHidFieldIdentifier <*> pColonTok <*> pHidType

pPositionalFields :: TermParser PositionalFieldsL
pPositionalFields =
  PositionalFields' <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy pHidType pCommaTok)

pHidVariantIdentifier :: TermParser HidVariantIdentifierL
pHidVariantIdentifier =
  HidVariantIdentifier' <$> pIdentifier

pHidEnumSignature :: TermParser HidEnumSignatureL
pHidEnumSignature =
  HidEnumSignature' <$> pEnumTok <*> pHidEnumIdentifier <*> pMaybe pTypeParameters <*> pMaybe pAbilityDecls

pAbilityDecls :: TermParser AbilityDeclsL
pAbilityDecls =
  AbilityDecls' <$> pHasTok <*> pSepBy pAbility pCommaTok

pHidEnumIdentifier :: TermParser HidEnumIdentifierL
pHidEnumIdentifier =
  HidEnumIdentifier' <$> pIdentifier

pPostfixAbilityDecls :: TermParser PostfixAbilityDeclsL
pPostfixAbilityDecls =
  PostfixAbilityDecls' <$> pBetween pHasTok pSemicolonTok (pSepBy pAbility pCommaTok)

pHidFunctionItem :: TermParser HidFunctionItemL
pHidFunctionItem =
  choice [ Megaparsec.try pHidFunctionItemNativeFunctionDefinition
         , Megaparsec.try pHidFunctionItemMacroFunctionDefinition
         , Megaparsec.try pHidFunctionItemFunctionDefinition
         ]
  where
    pHidFunctionItemNativeFunctionDefinition :: TermParser HidFunctionItemL
    pHidFunctionItemNativeFunctionDefinition =
      HidFunctionItemNativeFunctionDefinition' <$> pNativeFunctionDefinition
    pHidFunctionItemMacroFunctionDefinition :: TermParser HidFunctionItemL
    pHidFunctionItemMacroFunctionDefinition =
      HidFunctionItemMacroFunctionDefinition' <$> pMacroFunctionDefinition
    pHidFunctionItemFunctionDefinition :: TermParser HidFunctionItemL
    pHidFunctionItemFunctionDefinition =
      HidFunctionItemFunctionDefinition' <$> pFunctionDefinition

pFunctionDefinition :: TermParser FunctionDefinitionL
pFunctionDefinition =
  FunctionDefinition' <$> pHidFunctionSignature <*> pBlock

pHidFunctionSignature :: TermParser HidFunctionSignatureL
pHidFunctionSignature =
  HidFunctionSignature' <$> pMaybe pModifier <*> pMaybe pModifier <*> pMaybe pModifier <*> pFunTok <*> pHidFunctionIdentifier <*> pMaybe pTypeParameters <*> pFunctionParameters <*> pMaybe pRetType

pModifier :: TermParser ModifierL
pModifier =
  choice [ Megaparsec.try pModifier1
         , Megaparsec.try pModifierEntry
         , Megaparsec.try pModifierNative
         ]
  where
    pModifier1 :: TermParser ModifierL
    pModifier1 =
      Modifier1' <$> pPublicTok <*> pMaybe (pBetween pLeftParenthesisTok pRightParenthesisTok (pModifierInternal0))
    pModifierEntry :: TermParser ModifierL
    pModifierEntry =
      ModifierEntry' <$> pEntryTok
    pModifierNative :: TermParser ModifierL
    pModifierNative =
      ModifierNative' <$> pNativeTok

pModifierInternal0 :: TermParser ModifierInternal0L
pModifierInternal0 =
  choice [ Megaparsec.try pModifierInternal0Package
         , Megaparsec.try pModifierInternal0Friend
         ]
  where
    pModifierInternal0Package :: TermParser ModifierInternal0L
    pModifierInternal0Package =
      ModifierInternal0Package' <$> pPackageTok
    pModifierInternal0Friend :: TermParser ModifierInternal0L
    pModifierInternal0Friend =
      ModifierInternal0Friend' <$> pFriendTok

pMacroFunctionDefinition :: TermParser MacroFunctionDefinitionL
pMacroFunctionDefinition =
  MacroFunctionDefinition' <$> pMaybe pModifier <*> pMacroTok <*> pHidMacroSignature <*> pBlock

pHidMacroSignature :: TermParser HidMacroSignatureL
pHidMacroSignature =
  HidMacroSignature' <$> pMaybe pModifier <*> pFunTok <*> pHidFunctionIdentifier <*> pMaybe pTypeParameters <*> pFunctionParameters <*> pMaybe pRetType

pNativeFunctionDefinition :: TermParser NativeFunctionDefinitionL
pNativeFunctionDefinition =
  NativeFunctionDefinition' <$> pHidFunctionSignature <*> pSemicolonTok

pHidStructItem :: TermParser HidStructItemL
pHidStructItem =
  choice [ Megaparsec.try pHidStructItemNativeStructDefinition
         , Megaparsec.try pHidStructItemStructDefinition
         ]
  where
    pHidStructItemNativeStructDefinition :: TermParser HidStructItemL
    pHidStructItemNativeStructDefinition =
      HidStructItemNativeStructDefinition' <$> pNativeStructDefinition
    pHidStructItemStructDefinition :: TermParser HidStructItemL
    pHidStructItemStructDefinition =
      HidStructItemStructDefinition' <$> pStructDefinition

pNativeStructDefinition :: TermParser NativeStructDefinitionL
pNativeStructDefinition =
  NativeStructDefinition' <$> pMaybe pPublicTok <*> pBetween pNativeTok pSemicolonTok pHidStructSignature

pHidStructSignature :: TermParser HidStructSignatureL
pHidStructSignature =
  HidStructSignature' <$> pStructTok <*> pHidStructIdentifier <*> pMaybe pTypeParameters <*> pMaybe pAbilityDecls

pStructDefinition :: TermParser StructDefinitionL
pStructDefinition =
  StructDefinition' <$> pMaybe pPublicTok <*> pHidStructSignature <*> pDatatypeFields <*> pMaybe pPostfixAbilityDecls

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
