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
type SomeTerm = E ${grammarName}Term
type ${grammarName}Parser t = Parser NoHole (Sum ${grammarName}Sig) (K ()) t
type ${grammarName}TermParser l = ${grammarName}Parser (${grammarName}Term l)
type SomeTermParser = Parser (E ${grammarName}Term)

pExclamationMarkTok :: TermParser ExclamationMarkTokL
pExclamationMarkTok = pure ExclamationMarkTok' <* Megaparsec.eof

pExclamationMarkEqualsSignTok :: TermParser ExclamationMarkEqualsSignTokL
pExclamationMarkEqualsSignTok = pure ExclamationMarkEqualsSignTok' <* Megaparsec.eof

pNumberSignLeftSquareBracketTok :: TermParser NumberSignLeftSquareBracketTokL
pNumberSignLeftSquareBracketTok = pure NumberSignLeftSquareBracketTok' <* Megaparsec.eof

pDollarSignTok :: TermParser DollarSignTokL
pDollarSignTok = pure DollarSignTok' <* Megaparsec.eof

pPercentSignTok :: TermParser PercentSignTokL
pPercentSignTok = pure PercentSignTok' <* Megaparsec.eof

pAmpersandTok :: TermParser AmpersandTokL
pAmpersandTok = pure AmpersandTok' <* Megaparsec.eof

pAmpersandAmpersandTok :: TermParser AmpersandAmpersandTokL
pAmpersandAmpersandTok = pure AmpersandAmpersandTok' <* Megaparsec.eof

pApostropheTok :: TermParser ApostropheTokL
pApostropheTok = pure ApostropheTok' <* Megaparsec.eof

pLeftParenthesisTok :: TermParser LeftParenthesisTokL
pLeftParenthesisTok = pure LeftParenthesisTok' <* Megaparsec.eof

pRightParenthesisTok :: TermParser RightParenthesisTokL
pRightParenthesisTok = pure RightParenthesisTok' <* Megaparsec.eof

pAsteriskTok :: TermParser AsteriskTokL
pAsteriskTok = pure AsteriskTok' <* Megaparsec.eof

pPlusSignTok :: TermParser PlusSignTokL
pPlusSignTok = pure PlusSignTok' <* Megaparsec.eof

pCommaTok :: TermParser CommaTokL
pCommaTok = pure CommaTok' <* Megaparsec.eof

pHyphenMinusTok :: TermParser HyphenMinusTokL
pHyphenMinusTok = pure HyphenMinusTok' <* Megaparsec.eof

pHyphenMinusGreaterThanSignTok :: TermParser HyphenMinusGreaterThanSignTokL
pHyphenMinusGreaterThanSignTok = pure HyphenMinusGreaterThanSignTok' <* Megaparsec.eof

pFullStopTok :: TermParser FullStopTokL
pFullStopTok = pure FullStopTok' <* Megaparsec.eof

pFullStopFullStopTok :: TermParser FullStopFullStopTokL
pFullStopFullStopTok = pure FullStopFullStopTok' <* Megaparsec.eof

pSolidusTok :: TermParser SolidusTokL
pSolidusTok = pure SolidusTok' <* Megaparsec.eof

pSolidusAsteriskTok :: TermParser SolidusAsteriskTokL
pSolidusAsteriskTok = pure SolidusAsteriskTok' <* Megaparsec.eof

pSolidusSolidusTok :: TermParser SolidusSolidusTokL
pSolidusSolidusTok = pure SolidusSolidusTok' <* Megaparsec.eof

pColonTok :: TermParser ColonTokL
pColonTok = pure ColonTok' <* Megaparsec.eof

pColonColonTok :: TermParser ColonColonTokL
pColonColonTok = pure ColonColonTok' <* Megaparsec.eof

pSemicolonTok :: TermParser SemicolonTokL
pSemicolonTok = pure SemicolonTok' <* Megaparsec.eof

pLessThanSignTok :: TermParser LessThanSignTokL
pLessThanSignTok = pure LessThanSignTok' <* Megaparsec.eof

pLessThanSignLessThanSignTok :: TermParser LessThanSignLessThanSignTokL
pLessThanSignLessThanSignTok = pure LessThanSignLessThanSignTok' <* Megaparsec.eof

pLessThanSignEqualsSignTok :: TermParser LessThanSignEqualsSignTokL
pLessThanSignEqualsSignTok = pure LessThanSignEqualsSignTok' <* Megaparsec.eof

pEqualsSignTok :: TermParser EqualsSignTokL
pEqualsSignTok = pure EqualsSignTok' <* Megaparsec.eof

pEqualsSignEqualsSignTok :: TermParser EqualsSignEqualsSignTokL
pEqualsSignEqualsSignTok = pure EqualsSignEqualsSignTok' <* Megaparsec.eof

pEqualsSignEqualsSignGreaterThanSignTok :: TermParser EqualsSignEqualsSignGreaterThanSignTokL
pEqualsSignEqualsSignGreaterThanSignTok = pure EqualsSignEqualsSignGreaterThanSignTok' <* Megaparsec.eof

pEqualsSignGreaterThanSignTok :: TermParser EqualsSignGreaterThanSignTokL
pEqualsSignGreaterThanSignTok = pure EqualsSignGreaterThanSignTok' <* Megaparsec.eof

pGreaterThanSignTok :: TermParser GreaterThanSignTokL
pGreaterThanSignTok = pure GreaterThanSignTok' <* Megaparsec.eof

pGreaterThanSignEqualsSignTok :: TermParser GreaterThanSignEqualsSignTokL
pGreaterThanSignEqualsSignTok = pure GreaterThanSignEqualsSignTok' <* Megaparsec.eof

pGreaterThanSignGreaterThanSignTok :: TermParser GreaterThanSignGreaterThanSignTokL
pGreaterThanSignGreaterThanSignTok = pure GreaterThanSignGreaterThanSignTok' <* Megaparsec.eof

pCommercialAtTok :: TermParser CommercialAtTokL
pCommercialAtTok = pure CommercialAtTok' <* Megaparsec.eof

pLeftSquareBracketTok :: TermParser LeftSquareBracketTokL
pLeftSquareBracketTok = pure LeftSquareBracketTok' <* Megaparsec.eof

pRightSquareBracketTok :: TermParser RightSquareBracketTokL
pRightSquareBracketTok = pure RightSquareBracketTok' <* Megaparsec.eof

pCircumflexAccentTok :: TermParser CircumflexAccentTokL
pCircumflexAccentTok = pure CircumflexAccentTok' <* Megaparsec.eof

pAbortTok :: TermParser AbortTokL
pAbortTok = pure AbortTok' <* Megaparsec.eof

pAbortsIfTok :: TermParser AbortsIfTokL
pAbortsIfTok = pure AbortsIfTok' <* Megaparsec.eof

pAbortsWithTok :: TermParser AbortsWithTokL
pAbortsWithTok = pure AbortsWithTok' <* Megaparsec.eof

pAddressTok :: TermParser AddressTokL
pAddressTok = pure AddressTok' <* Megaparsec.eof

pApplyTok :: TermParser ApplyTokL
pApplyTok = pure ApplyTok' <* Megaparsec.eof

pAsTok :: TermParser AsTokL
pAsTok = pure AsTok' <* Megaparsec.eof

pAssertTok :: TermParser AssertTokL
pAssertTok = pure AssertTok' <* Megaparsec.eof

pAssumeTok :: TermParser AssumeTokL
pAssumeTok = pure AssumeTok' <* Megaparsec.eof

pBoolTok :: TermParser BoolTokL
pBoolTok = pure BoolTok' <* Megaparsec.eof

pBreakTok :: TermParser BreakTokL
pBreakTok = pure BreakTok' <* Megaparsec.eof

pBytearrayTok :: TermParser BytearrayTokL
pBytearrayTok = pure BytearrayTok' <* Megaparsec.eof

pConstTok :: TermParser ConstTokL
pConstTok = pure ConstTok' <* Megaparsec.eof

pContinueTok :: TermParser ContinueTokL
pContinueTok = pure ContinueTok' <* Megaparsec.eof

pCopyTok :: TermParser CopyTokL
pCopyTok = pure CopyTok' <* Megaparsec.eof

pDecreasesTok :: TermParser DecreasesTokL
pDecreasesTok = pure DecreasesTok' <* Megaparsec.eof

pDropTok :: TermParser DropTokL
pDropTok = pure DropTok' <* Megaparsec.eof

pElseTok :: TermParser ElseTokL
pElseTok = pure ElseTok' <* Megaparsec.eof

pEnsuresTok :: TermParser EnsuresTokL
pEnsuresTok = pure EnsuresTok' <* Megaparsec.eof

pEntryTok :: TermParser EntryTokL
pEntryTok = pure EntryTok' <* Megaparsec.eof

pEnumTok :: TermParser EnumTokL
pEnumTok = pure EnumTok' <* Megaparsec.eof

pExceptTok :: TermParser ExceptTokL
pExceptTok = pure ExceptTok' <* Megaparsec.eof

pExistsTok :: TermParser ExistsTokL
pExistsTok = pure ExistsTok' <* Megaparsec.eof

pFalseTok :: TermParser FalseTokL
pFalseTok = pure FalseTok' <* Megaparsec.eof

pForallTok :: TermParser ForallTokL
pForallTok = pure ForallTok' <* Megaparsec.eof

pFriendTok :: TermParser FriendTokL
pFriendTok = pure FriendTok' <* Megaparsec.eof

pFunTok :: TermParser FunTokL
pFunTok = pure FunTok' <* Megaparsec.eof

pGlobalTok :: TermParser GlobalTokL
pGlobalTok = pure GlobalTok' <* Megaparsec.eof

pHasTok :: TermParser HasTokL
pHasTok = pure HasTok' <* Megaparsec.eof

pIfTok :: TermParser IfTokL
pIfTok = pure IfTok' <* Megaparsec.eof

pInTok :: TermParser InTokL
pInTok = pure InTok' <* Megaparsec.eof

pIncludeTok :: TermParser IncludeTokL
pIncludeTok = pure IncludeTok' <* Megaparsec.eof

pInternalTok :: TermParser InternalTokL
pInternalTok = pure InternalTok' <* Megaparsec.eof

pInvariantTok :: TermParser InvariantTokL
pInvariantTok = pure InvariantTok' <* Megaparsec.eof

pKeyTok :: TermParser KeyTokL
pKeyTok = pure KeyTok' <* Megaparsec.eof

pLetTok :: TermParser LetTokL
pLetTok = pure LetTok' <* Megaparsec.eof

pLocalTok :: TermParser LocalTokL
pLocalTok = pure LocalTok' <* Megaparsec.eof

pLoopTok :: TermParser LoopTokL
pLoopTok = pure LoopTok' <* Megaparsec.eof

pMacroTok :: TermParser MacroTokL
pMacroTok = pure MacroTok' <* Megaparsec.eof

pMatchTok :: TermParser MatchTokL
pMatchTok = pure MatchTok' <* Megaparsec.eof

pModifiesTok :: TermParser ModifiesTokL
pModifiesTok = pure ModifiesTok' <* Megaparsec.eof

pModuleTok :: TermParser ModuleTokL
pModuleTok = pure ModuleTok' <* Megaparsec.eof

pMoveTok :: TermParser MoveTokL
pMoveTok = pure MoveTok' <* Megaparsec.eof

pMutTok :: TermParser MutTokL
pMutTok = pure MutTok' <* Megaparsec.eof

pNativeTok :: TermParser NativeTokL
pNativeTok = pure NativeTok' <* Megaparsec.eof

pPackTok :: TermParser PackTokL
pPackTok = pure PackTok' <* Megaparsec.eof

pPackageTok :: TermParser PackageTokL
pPackageTok = pure PackageTok' <* Megaparsec.eof

pPhantomTok :: TermParser PhantomTokL
pPhantomTok = pure PhantomTok' <* Megaparsec.eof

pPostTok :: TermParser PostTokL
pPostTok = pure PostTok' <* Megaparsec.eof

pPragmaTok :: TermParser PragmaTokL
pPragmaTok = pure PragmaTok' <* Megaparsec.eof

pPublicTok :: TermParser PublicTokL
pPublicTok = pure PublicTok' <* Megaparsec.eof

pRequiresTok :: TermParser RequiresTokL
pRequiresTok = pure RequiresTok' <* Megaparsec.eof

pReturnTok :: TermParser ReturnTokL
pReturnTok = pure ReturnTok' <* Megaparsec.eof

pSchemaTok :: TermParser SchemaTokL
pSchemaTok = pure SchemaTok' <* Megaparsec.eof

pSignerTok :: TermParser SignerTokL
pSignerTok = pure SignerTok' <* Megaparsec.eof

pSpecTok :: TermParser SpecTokL
pSpecTok = pure SpecTok' <* Megaparsec.eof

pStoreTok :: TermParser StoreTokL
pStoreTok = pure StoreTok' <* Megaparsec.eof

pStructTok :: TermParser StructTokL
pStructTok = pure StructTok' <* Megaparsec.eof

pSucceedsIfTok :: TermParser SucceedsIfTokL
pSucceedsIfTok = pure SucceedsIfTok' <* Megaparsec.eof

pToTok :: TermParser ToTokL
pToTok = pure ToTok' <* Megaparsec.eof

pTrueTok :: TermParser TrueTokL
pTrueTok = pure TrueTok' <* Megaparsec.eof

pU128Tok :: TermParser U128TokL
pU128Tok = pure U128Tok' <* Megaparsec.eof

pU16Tok :: TermParser U16TokL
pU16Tok = pure U16Tok' <* Megaparsec.eof

pU256Tok :: TermParser U256TokL
pU256Tok = pure U256Tok' <* Megaparsec.eof

pU32Tok :: TermParser U32TokL
pU32Tok = pure U32Tok' <* Megaparsec.eof

pU64Tok :: TermParser U64TokL
pU64Tok = pure U64Tok' <* Megaparsec.eof

pU8Tok :: TermParser U8TokL
pU8Tok = pure U8Tok' <* Megaparsec.eof

pUnpackTok :: TermParser UnpackTokL
pUnpackTok = pure UnpackTok' <* Megaparsec.eof

pUpdateTok :: TermParser UpdateTokL
pUpdateTok = pure UpdateTok' <* Megaparsec.eof

pUseTok :: TermParser UseTokL
pUseTok = pure UseTok' <* Megaparsec.eof

pVectorLessThanSignTok :: TermParser VectorLessThanSignTokL
pVectorLessThanSignTok = pure VectorLessThanSignTok' <* Megaparsec.eof

pVectorLeftSquareBracketTok :: TermParser VectorLeftSquareBracketTokL
pVectorLeftSquareBracketTok = pure VectorLeftSquareBracketTok' <* Megaparsec.eof

pWhereTok :: TermParser WhereTokL
pWhereTok = pure WhereTok' <* Megaparsec.eof

pWhileTok :: TermParser WhileTokL
pWhileTok = pure WhileTok' <* Megaparsec.eof

pWithTok :: TermParser WithTokL
pWithTok = pure WithTok' <* Megaparsec.eof

pLeftCurlyBracketTok :: TermParser LeftCurlyBracketTokL
pLeftCurlyBracketTok = pure LeftCurlyBracketTok' <* Megaparsec.eof

pVerticalLineTok :: TermParser VerticalLineTokL
pVerticalLineTok = pure VerticalLineTok' <* Megaparsec.eof

pVerticalLineVerticalLineTok :: TermParser VerticalLineVerticalLineTokL
pVerticalLineVerticalLineTok = pure VerticalLineVerticalLineTok' <* Megaparsec.eof

pRightCurlyBracketTok :: TermParser RightCurlyBracketTokL
pRightCurlyBracketTok = pure RightCurlyBracketTok' <* Megaparsec.eof

pSourceFile :: TermParser SourceFileL
pSourceFile =
  SourceFile' <$> pMany pModuleDefinition

pModuleDefinition :: TermParser ModuleDefinitionL
pModuleDefinition =
  ModuleDefinition' <$> pModuleTok <*> pModuleIdentity <*> pModuleBody

pModuleBody :: TermParser ModuleBodyL
pModuleBody =
  ModuleBody' <$> (pModuleBodyInternal0 _sym) <*> pMany (pModuleBodyInternal1 _sym) <*> pMaybe pRightCurlyBracketTok

pModuleBodyInternal0 :: TermParser ModuleBodyInternal0L
pModuleBodyInternal0 =
  choice [ Megaparsec.try pModuleBodyInternal0Semicolon
         , Megaparsec.try pModuleBodyInternal0LeftCurlyBracket
         ]
  where
    pModuleBodyInternal0Semicolon :: Parser (MoveTerm ModuleBodyInternal0L)
    pModuleBodyInternal0Semicolon =
      ModuleBodyInternal0Semicolon' <$> pSemicolonTok
    pModuleBodyInternal0LeftCurlyBracket :: Parser (MoveTerm ModuleBodyInternal0L)
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
    pModuleBodyInternal1UseDeclaration :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1UseDeclaration =
      ModuleBodyInternal1UseDeclaration' <$> pUseDeclaration
    pModuleBodyInternal1FriendDeclaration :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1FriendDeclaration =
      ModuleBodyInternal1FriendDeclaration' <$> pFriendDeclaration
    pModuleBodyInternal1Constant :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1Constant =
      ModuleBodyInternal1Constant' <$> pConstant
    pModuleBodyInternal1HidFunctionItem :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1HidFunctionItem =
      ModuleBodyInternal1HidFunctionItem' <$> pHidFunctionItem
    pModuleBodyInternal1HidStructItem :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1HidStructItem =
      ModuleBodyInternal1HidStructItem' <$> pHidStructItem
    pModuleBodyInternal1HidEnumItem :: Parser (MoveTerm ModuleBodyInternal1L)
    pModuleBodyInternal1HidEnumItem =
      ModuleBodyInternal1HidEnumItem' <$> pHidEnumItem
    pModuleBodyInternal1SpecBlock :: Parser (MoveTerm ModuleBodyInternal1L)
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
    pHidExpressionCallExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionCallExpression =
      HidExpressionCallExpression' <$> pCallExpression
    pHidExpressionMacroCallExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionMacroCallExpression =
      HidExpressionMacroCallExpression' <$> pMacroCallExpression
    pHidExpressionLambdaExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionLambdaExpression =
      HidExpressionLambdaExpression' <$> pLambdaExpression
    pHidExpressionIfExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionIfExpression =
      HidExpressionIfExpression' <$> pIfExpression
    pHidExpressionWhileExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionWhileExpression =
      HidExpressionWhileExpression' <$> pWhileExpression
    pHidExpressionReturnExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionReturnExpression =
      HidExpressionReturnExpression' <$> pReturnExpression
    pHidExpressionAbortExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionAbortExpression =
      HidExpressionAbortExpression' <$> pAbortExpression
    pHidExpressionAssignExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionAssignExpression =
      HidExpressionAssignExpression' <$> pAssignExpression
    pHidExpressionHidUnaryExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionHidUnaryExpression =
      HidExpressionHidUnaryExpression' <$> pHidUnaryExpression
    pHidExpressionBinaryExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionBinaryExpression =
      HidExpressionBinaryExpression' <$> pBinaryExpression
    pHidExpressionCastExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionCastExpression =
      HidExpressionCastExpression' <$> pCastExpression
    pHidExpressionQuantifierExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionQuantifierExpression =
      HidExpressionQuantifierExpression' <$> pQuantifierExpression
    pHidExpressionMatchExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionMatchExpression =
      HidExpressionMatchExpression' <$> pMatchExpression
    pHidExpressionVectorExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionVectorExpression =
      HidExpressionVectorExpression' <$> pVectorExpression
    pHidExpressionLoopExpression :: Parser (MoveTerm HidExpressionL)
    pHidExpressionLoopExpression =
      HidExpressionLoopExpression' <$> pLoopExpression
    pHidExpressionIdentifiedExpression :: Parser (MoveTerm HidExpressionL)
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
  HidUnaryExpression' <$> (pHidUnaryExpressionInternal0 _sym)

pHidUnaryExpressionInternal0 :: TermParser HidUnaryExpressionInternal0L
pHidUnaryExpressionInternal0 =
  choice [ Megaparsec.try pHidUnaryExpressionInternal0UnaryExpression
         , Megaparsec.try pHidUnaryExpressionInternal0BorrowExpression
         , Megaparsec.try pHidUnaryExpressionInternal0DereferenceExpression
         , Megaparsec.try pHidUnaryExpressionInternal0MoveOrCopyExpression
         , Megaparsec.try pHidUnaryExpressionInternal0HidExpressionTerm
         ]
  where
    pHidUnaryExpressionInternal0UnaryExpression :: Parser (MoveTerm HidUnaryExpressionInternal0L)
    pHidUnaryExpressionInternal0UnaryExpression =
      HidUnaryExpressionInternal0UnaryExpression' <$> pUnaryExpression
    pHidUnaryExpressionInternal0BorrowExpression :: Parser (MoveTerm HidUnaryExpressionInternal0L)
    pHidUnaryExpressionInternal0BorrowExpression =
      HidUnaryExpressionInternal0BorrowExpression' <$> pBorrowExpression
    pHidUnaryExpressionInternal0DereferenceExpression :: Parser (MoveTerm HidUnaryExpressionInternal0L)
    pHidUnaryExpressionInternal0DereferenceExpression =
      HidUnaryExpressionInternal0DereferenceExpression' <$> pDereferenceExpression
    pHidUnaryExpressionInternal0MoveOrCopyExpression :: Parser (MoveTerm HidUnaryExpressionInternal0L)
    pHidUnaryExpressionInternal0MoveOrCopyExpression =
      HidUnaryExpressionInternal0MoveOrCopyExpression' <$> pMoveOrCopyExpression
    pHidUnaryExpressionInternal0HidExpressionTerm :: Parser (MoveTerm HidUnaryExpressionInternal0L)
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
    pHidReferenceImmRef :: Parser (MoveTerm HidReferenceL)
    pHidReferenceImmRef =
      HidReferenceImmRef' <$> pImmRef
    pHidReferenceMutRef :: Parser (MoveTerm HidReferenceL)
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
    pHidExpressionTermCallExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermCallExpression =
      HidExpressionTermCallExpression' <$> pCallExpression
    pHidExpressionTermBreakExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermBreakExpression =
      HidExpressionTermBreakExpression' <$> pBreakExpression
    pHidExpressionTermContinueExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermContinueExpression =
      HidExpressionTermContinueExpression' <$> pContinueExpression
    pHidExpressionTermNameExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermNameExpression =
      HidExpressionTermNameExpression' <$> pNameExpression
    pHidExpressionTermMacroCallExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermMacroCallExpression =
      HidExpressionTermMacroCallExpression' <$> pMacroCallExpression
    pHidExpressionTermPackExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermPackExpression =
      HidExpressionTermPackExpression' <$> pPackExpression
    pHidExpressionTermHidLiteralValue :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermHidLiteralValue =
      HidExpressionTermHidLiteralValue' <$> pHidLiteralValue
    pHidExpressionTermUnitExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermUnitExpression =
      HidExpressionTermUnitExpression' <$> pUnitExpression
    pHidExpressionTermExpressionList :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermExpressionList =
      HidExpressionTermExpressionList' <$> pExpressionList
    pHidExpressionTermAnnotationExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermAnnotationExpression =
      HidExpressionTermAnnotationExpression' <$> pAnnotationExpression
    pHidExpressionTermBlock :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermBlock =
      HidExpressionTermBlock' <$> pBlock
    pHidExpressionTermSpecBlock :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermSpecBlock =
      HidExpressionTermSpecBlock' <$> pSpecBlock
    pHidExpressionTermIfExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermIfExpression =
      HidExpressionTermIfExpression' <$> pIfExpression
    pHidExpressionTermDotExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermDotExpression =
      HidExpressionTermDotExpression' <$> pDotExpression
    pHidExpressionTermIndexExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermIndexExpression =
      HidExpressionTermIndexExpression' <$> pIndexExpression
    pHidExpressionTermVectorExpression :: Parser (MoveTerm HidExpressionTermL)
    pHidExpressionTermVectorExpression =
      HidExpressionTermVectorExpression' <$> pVectorExpression
    pHidExpressionTermMatchExpression :: Parser (MoveTerm HidExpressionTermL)
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
    pHidTypeApplyType :: Parser (MoveTerm HidTypeL)
    pHidTypeApplyType =
      HidTypeApplyType' <$> pApplyType
    pHidTypeRefType :: Parser (MoveTerm HidTypeL)
    pHidTypeRefType =
      HidTypeRefType' <$> pRefType
    pHidTypeTupleType :: Parser (MoveTerm HidTypeL)
    pHidTypeTupleType =
      HidTypeTupleType' <$> pTupleType
    pHidTypeFunctionType :: Parser (MoveTerm HidTypeL)
    pHidTypeFunctionType =
      HidTypeFunctionType' <$> pFunctionType
    pHidTypePrimitiveType :: Parser (MoveTerm HidTypeL)
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
    pModuleAccess1 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess1 =
      ModuleAccess1' <$> pDollarSignTok <*> pIdentifier
    pModuleAccess2 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess2 =
      ModuleAccess2' <$> pCommercialAtTok <*> pIdentifier
    pModuleAccess3 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess3 =
      ModuleAccess3' <$> pModuleIdentity <*> pColonColonTok <*> pIdentifier <*> pMaybe pTypeArguments <*> pColonColonTok <*> pIdentifier
    pModuleAccess4 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess4 =
      ModuleAccess4' <$> pModuleIdentity <*> pColonColonTok <*> pIdentifier <*> pTypeArguments
    pModuleAccess5 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess5 =
      ModuleAccess5' <$> pModuleIdentity <*> pMaybe pTypeArguments <*> pColonColonTok <*> pIdentifier
    pModuleAccess6 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess6 =
      ModuleAccess6' <$> pHidModuleIdentifier <*> pMaybe pTypeArguments <*> pColonColonTok <*> pIdentifier
    pModuleAccess7 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess7 =
      ModuleAccess7' <$> pModuleIdentity <*> pMaybe pTypeArguments
    pModuleAccess8 :: Parser (MoveTerm ModuleAccessL)
    pModuleAccess8 =
      ModuleAccess8' <$> pIdentifier <*> pMaybe pTypeArguments
    pModuleAccessMember :: Parser (MoveTerm ModuleAccessL)
    pModuleAccessMember =
      ModuleAccessMember' <$> pHidReservedIdentifier

pHidModuleIdentifier :: TermParser HidModuleIdentifierL
pHidModuleIdentifier =
  HidModuleIdentifier' <$> pIdentifier

pIdentifier :: TermParser IdentifierL
pIdentifier =
  Identifier' <$> pContent _sym

pHidReservedIdentifier :: TermParser HidReservedIdentifierL
pHidReservedIdentifier =
  choice [ Megaparsec.try pHidReservedIdentifierHidForall
         , Megaparsec.try pHidReservedIdentifierHidExists
         ]
  where
    pHidReservedIdentifierHidForall :: Parser (MoveTerm HidReservedIdentifierL)
    pHidReservedIdentifierHidForall =
      HidReservedIdentifierHidForall' <$> pHidForall
    pHidReservedIdentifierHidExists :: Parser (MoveTerm HidReservedIdentifierL)
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
  ModuleIdentity' <$> (pModuleIdentityInternal0 _sym) <*> pColonColonTok <*> pHidModuleIdentifier

pModuleIdentityInternal0 :: TermParser ModuleIdentityInternal0L
pModuleIdentityInternal0 =
  choice [ Megaparsec.try pModuleIdentityInternal0NumLiteral
         , Megaparsec.try pModuleIdentityInternal0HidModuleIdentifier
         ]
  where
    pModuleIdentityInternal0NumLiteral :: Parser (MoveTerm ModuleIdentityInternal0L)
    pModuleIdentityInternal0NumLiteral =
      ModuleIdentityInternal0NumLiteral' <$> pNumLiteral
    pModuleIdentityInternal0HidModuleIdentifier :: Parser (MoveTerm ModuleIdentityInternal0L)
    pModuleIdentityInternal0HidModuleIdentifier =
      ModuleIdentityInternal0HidModuleIdentifier' <$> pHidModuleIdentifier

pNumLiteral :: TermParser NumLiteralL
pNumLiteral =
  NumLiteral' <$> pContent _sym <*> pMaybe (pNumLiteralInternal0 _sym)

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
    pNumLiteralInternal0U8 :: Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U8 =
      NumLiteralInternal0U8' <$> pU8Tok
    pNumLiteralInternal0U16 :: Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U16 =
      NumLiteralInternal0U16' <$> pU16Tok
    pNumLiteralInternal0U32 :: Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U32 =
      NumLiteralInternal0U32' <$> pU32Tok
    pNumLiteralInternal0U64 :: Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U64 =
      NumLiteralInternal0U64' <$> pU64Tok
    pNumLiteralInternal0U128 :: Parser (MoveTerm NumLiteralInternal0L)
    pNumLiteralInternal0U128 =
      NumLiteralInternal0U128' <$> pU128Tok
    pNumLiteralInternal0U256 :: Parser (MoveTerm NumLiteralInternal0L)
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
    pPrimitiveTypeU8 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU8 =
      PrimitiveTypeU8' <$> pU8Tok
    pPrimitiveTypeU16 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU16 =
      PrimitiveTypeU16' <$> pU16Tok
    pPrimitiveTypeU32 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU32 =
      PrimitiveTypeU32' <$> pU32Tok
    pPrimitiveTypeU64 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU64 =
      PrimitiveTypeU64' <$> pU64Tok
    pPrimitiveTypeU128 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU128 =
      PrimitiveTypeU128' <$> pU128Tok
    pPrimitiveTypeU256 :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeU256 =
      PrimitiveTypeU256' <$> pU256Tok
    pPrimitiveTypeBool :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeBool =
      PrimitiveTypeBool' <$> pBoolTok
    pPrimitiveTypeAddress :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeAddress =
      PrimitiveTypeAddress' <$> pAddressTok
    pPrimitiveTypeSigner :: Parser (MoveTerm PrimitiveTypeL)
    pPrimitiveTypeSigner =
      PrimitiveTypeSigner' <$> pSignerTok
    pPrimitiveTypeBytearray :: Parser (MoveTerm PrimitiveTypeL)
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
  BlockItem' <$> (pBlockItemInternal0 _sym) <*> pSemicolonTok

pBlockItemInternal0 :: TermParser BlockItemInternal0L
pBlockItemInternal0 =
  choice [ Megaparsec.try pBlockItemInternal0HidExpression
         , Megaparsec.try pBlockItemInternal0LetStatement
         ]
  where
    pBlockItemInternal0HidExpression :: Parser (MoveTerm BlockItemInternal0L)
    pBlockItemInternal0HidExpression =
      BlockItemInternal0HidExpression' <$> pHidExpression
    pBlockItemInternal0LetStatement :: Parser (MoveTerm BlockItemInternal0L)
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
    pBindListHidBind :: Parser (MoveTerm BindListL)
    pBindListHidBind =
      BindListHidBind' <$> pHidBind
    pBindListCommaBindList :: Parser (MoveTerm BindListL)
    pBindListCommaBindList =
      BindListCommaBindList' <$> pCommaBindList
    pBindListOrBindList :: Parser (MoveTerm BindListL)
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
    pHidBindHidBindInternal0 :: Parser (MoveTerm HidBindL)
    pHidBindHidBindInternal0 =
      HidBindHidBindInternal0' <$> (pHidBindInternal0 _sym)
    pHidBindBindUnpack :: Parser (MoveTerm HidBindL)
    pHidBindBindUnpack =
      HidBindBindUnpack' <$> pBindUnpack
    pHidBindAtBind :: Parser (MoveTerm HidBindL)
    pHidBindAtBind =
      HidBindAtBind' <$> pAtBind
    pHidBindHidLiteralValue :: Parser (MoveTerm HidBindL)
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
    pBindFieldsBindPositionalFields :: Parser (MoveTerm BindFieldsL)
    pBindFieldsBindPositionalFields =
      BindFieldsBindPositionalFields' <$> pBindPositionalFields
    pBindFieldsBindNamedFields :: Parser (MoveTerm BindFieldsL)
    pBindFieldsBindNamedFields =
      BindFieldsBindNamedFields' <$> pBindNamedFields

pBindNamedFields :: TermParser BindNamedFieldsL
pBindNamedFields =
  BindNamedFields' <$> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy (pBindNamedFieldsInternal0 _sym) pCommaTok)

pBindNamedFieldsInternal0 :: TermParser BindNamedFieldsInternal0L
pBindNamedFieldsInternal0 =
  choice [ Megaparsec.try pBindNamedFieldsInternal0BindField
         , Megaparsec.try pBindNamedFieldsInternal0MutBindField
         ]
  where
    pBindNamedFieldsInternal0BindField :: Parser (MoveTerm BindNamedFieldsInternal0L)
    pBindNamedFieldsInternal0BindField =
      BindNamedFieldsInternal0BindField' <$> pBindField
    pBindNamedFieldsInternal0MutBindField :: Parser (MoveTerm BindNamedFieldsInternal0L)
    pBindNamedFieldsInternal0MutBindField =
      BindNamedFieldsInternal0MutBindField' <$> pMutBindField

pBindField :: TermParser BindFieldL
pBindField =
  choice [ Megaparsec.try pBindField1
         , Megaparsec.try pBindFieldHidSpreadOperator
         ]
  where
    pBindField1 :: Parser (MoveTerm BindFieldL)
    pBindField1 =
      BindField1' <$> pBindList <*> pMaybe (pPair pColonTok pBindList)
    pBindFieldHidSpreadOperator :: Parser (MoveTerm BindFieldL)
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
  BindPositionalFields' <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy (pBindNamedFieldsInternal0 _sym) pCommaTok)

pNameExpression :: TermParser NameExpressionL
pNameExpression =
  NameExpression' <$> pMaybe pColonColonTok <*> pModuleAccess

pHidBindInternal0 :: TermParser HidBindInternal0L
pHidBindInternal0 =
  choice [ Megaparsec.try pHidBindInternal0MutBindVar
         , Megaparsec.try pHidBindInternal0HidVariableIdentifier
         ]
  where
    pHidBindInternal0MutBindVar :: Parser (MoveTerm HidBindInternal0L)
    pHidBindInternal0MutBindVar =
      HidBindInternal0MutBindVar' <$> pMutBindVar
    pHidBindInternal0HidVariableIdentifier :: Parser (MoveTerm HidBindInternal0L)
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
    pHidLiteralValueAddressLiteral :: Parser (MoveTerm HidLiteralValueL)
    pHidLiteralValueAddressLiteral =
      HidLiteralValueAddressLiteral' <$> pAddressLiteral
    pHidLiteralValueBoolLiteral :: Parser (MoveTerm HidLiteralValueL)
    pHidLiteralValueBoolLiteral =
      HidLiteralValueBoolLiteral' <$> pBoolLiteral
    pHidLiteralValueNumLiteral :: Parser (MoveTerm HidLiteralValueL)
    pHidLiteralValueNumLiteral =
      HidLiteralValueNumLiteral' <$> pNumLiteral
    pHidLiteralValueHexStringLiteral :: Parser (MoveTerm HidLiteralValueL)
    pHidLiteralValueHexStringLiteral =
      HidLiteralValueHexStringLiteral' <$> pHexStringLiteral
    pHidLiteralValueByteStringLiteral :: Parser (MoveTerm HidLiteralValueL)
    pHidLiteralValueByteStringLiteral =
      HidLiteralValueByteStringLiteral' <$> pByteStringLiteral

pAddressLiteral :: TermParser AddressLiteralL
pAddressLiteral =
  AddressLiteral' <$> pContent _sym

pBoolLiteral :: TermParser BoolLiteralL
pBoolLiteral =
  choice [ Megaparsec.try pBoolLiteralTrue
         , Megaparsec.try pBoolLiteralFalse
         ]
  where
    pBoolLiteralTrue :: Parser (MoveTerm BoolLiteralL)
    pBoolLiteralTrue =
      BoolLiteralTrue' <$> pTrueTok
    pBoolLiteralFalse :: Parser (MoveTerm BoolLiteralL)
    pBoolLiteralFalse =
      BoolLiteralFalse' <$> pFalseTok

pByteStringLiteral :: TermParser ByteStringLiteralL
pByteStringLiteral =
  ByteStringLiteral' <$> pContent _sym

pHexStringLiteral :: TermParser HexStringLiteralL
pHexStringLiteral =
  HexStringLiteral' <$> pContent _sym

pOrBindList :: TermParser OrBindListL
pOrBindList =
  OrBindList' <$> pMaybe pLeftParenthesisTok <*> pSepBy1 (pPair (pPair (pMaybe pLeftParenthesisTok) pHidBind) (pMaybe pRightParenthesisTok)) pVerticalLineTok <*> pMaybe pRightParenthesisTok

pUseDeclaration :: TermParser UseDeclarationL
pUseDeclaration =
  UseDeclaration' <$> pMaybe pPublicTok <*> pBetween pUseTok pSemicolonTok (pUseDeclarationInternal0 _sym)

pUseDeclarationInternal0 :: TermParser UseDeclarationInternal0L
pUseDeclarationInternal0 =
  choice [ Megaparsec.try pUseDeclarationInternal0UseFun
         , Megaparsec.try pUseDeclarationInternal0UseModule
         , Megaparsec.try pUseDeclarationInternal0UseModuleMember
         , Megaparsec.try pUseDeclarationInternal0UseModuleMembers
         ]
  where
    pUseDeclarationInternal0UseFun :: Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseFun =
      UseDeclarationInternal0UseFun' <$> pUseFun
    pUseDeclarationInternal0UseModule :: Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseModule =
      UseDeclarationInternal0UseModule' <$> pUseModule
    pUseDeclarationInternal0UseModuleMember :: Parser (MoveTerm UseDeclarationInternal0L)
    pUseDeclarationInternal0UseModuleMember =
      UseDeclarationInternal0UseModuleMember' <$> pUseModuleMember
    pUseDeclarationInternal0UseModuleMembers :: Parser (MoveTerm UseDeclarationInternal0L)
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
    pUseMember1 :: Parser (MoveTerm UseMemberL)
    pUseMember1 =
      UseMember1' <$> pIdentifier <*> pColonColonTok <*> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy1 pUseMember pCommaTok)
    pUseMember2 :: Parser (MoveTerm UseMemberL)
    pUseMember2 =
      UseMember2' <$> pIdentifier <*> pColonColonTok <*> pIdentifier <*> pMaybe (pPair pAsTok pIdentifier)
    pUseMember3 :: Parser (MoveTerm UseMemberL)
    pUseMember3 =
      UseMember3' <$> pIdentifier <*> pMaybe (pPair pAsTok pIdentifier)

pUseModuleMembers :: TermParser UseModuleMembersL
pUseModuleMembers =
  choice [ Megaparsec.try pUseModuleMembers1
         , Megaparsec.try pUseModuleMembers2
         ]
  where
    pUseModuleMembers1 :: Parser (MoveTerm UseModuleMembersL)
    pUseModuleMembers1 =
      UseModuleMembers1' <$> (pModuleIdentityInternal0 _sym) <*> pColonColonTok <*> pBetween pLeftCurlyBracketTok pRightCurlyBracketTok (pSepBy1 pUseMember pCommaTok)
    pUseModuleMembers2 :: Parser (MoveTerm UseModuleMembersL)
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
  SpecBlock' <$> pSpecTok <*> (pSpecBlockInternal0 _sym)

pSpecBlockInternal0 :: TermParser SpecBlockInternal0L
pSpecBlockInternal0 =
  choice [ Megaparsec.try pSpecBlockInternal01
         , Megaparsec.try pSpecBlockInternal0HidSpecFunction
         ]
  where
    pSpecBlockInternal01 :: Parser (MoveTerm SpecBlockInternal0L)
    pSpecBlockInternal01 =
      SpecBlockInternal01' <$> pMaybe pHidSpecBlockTarget <*> pSpecBody
    pSpecBlockInternal0HidSpecFunction :: Parser (MoveTerm SpecBlockInternal0L)
    pSpecBlockInternal0HidSpecFunction =
      SpecBlockInternal0HidSpecFunction' <$> pHidSpecFunction

pHidSpecBlockTarget :: TermParser HidSpecBlockTargetL
pHidSpecBlockTarget =
  choice [ Megaparsec.try pHidSpecBlockTargetIdentifier
         , Megaparsec.try pHidSpecBlockTargetModule
         , Megaparsec.try pHidSpecBlockTargetSpecBlockTargetSchema
         ]
  where
    pHidSpecBlockTargetIdentifier :: Parser (MoveTerm HidSpecBlockTargetL)
    pHidSpecBlockTargetIdentifier =
      HidSpecBlockTargetIdentifier' <$> pIdentifier
    pHidSpecBlockTargetModule :: Parser (MoveTerm HidSpecBlockTargetL)
    pHidSpecBlockTargetModule =
      HidSpecBlockTargetModule' <$> pModuleTok
    pHidSpecBlockTargetSpecBlockTargetSchema :: Parser (MoveTerm HidSpecBlockTargetL)
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
    pAbilityCopy :: Parser (MoveTerm AbilityL)
    pAbilityCopy =
      AbilityCopy' <$> pCopyTok
    pAbilityDrop :: Parser (MoveTerm AbilityL)
    pAbilityDrop =
      AbilityDrop' <$> pDropTok
    pAbilityStore :: Parser (MoveTerm AbilityL)
    pAbilityStore =
      AbilityStore' <$> pStoreTok
    pAbilityKey :: Parser (MoveTerm AbilityL)
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
    pHidSpecFunctionNativeSpecFunction :: Parser (MoveTerm HidSpecFunctionL)
    pHidSpecFunctionNativeSpecFunction =
      HidSpecFunctionNativeSpecFunction' <$> pNativeSpecFunction
    pHidSpecFunctionUsualSpecFunction :: Parser (MoveTerm HidSpecFunctionL)
    pHidSpecFunctionUsualSpecFunction =
      HidSpecFunctionUsualSpecFunction' <$> pUsualSpecFunction
    pHidSpecFunctionUninterpretedSpecFunction :: Parser (MoveTerm HidSpecFunctionL)
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
  FunctionParameters' <$> pBetween pLeftParenthesisTok pRightParenthesisTok (pSepBy (pFunctionParametersInternal0 _sym) pCommaTok)

pFunctionParametersInternal0 :: TermParser FunctionParametersInternal0L
pFunctionParametersInternal0 =
  choice [ Megaparsec.try pFunctionParametersInternal0MutFunctionParameter
         , Megaparsec.try pFunctionParametersInternal0FunctionParameter
         ]
  where
    pFunctionParametersInternal0MutFunctionParameter :: Parser (MoveTerm FunctionParametersInternal0L)
    pFunctionParametersInternal0MutFunctionParameter =
      FunctionParametersInternal0MutFunctionParameter' <$> pMutFunctionParameter
    pFunctionParametersInternal0FunctionParameter :: Parser (MoveTerm FunctionParametersInternal0L)
    pFunctionParametersInternal0FunctionParameter =
      FunctionParametersInternal0FunctionParameter' <$> pFunctionParameter

pFunctionParameter :: TermParser FunctionParameterL
pFunctionParameter =
  FunctionParameter' <$> (pFunctionParameterInternal0 _sym) <*> pColonTok <*> pHidType

pFunctionParameterInternal0 :: TermParser FunctionParameterInternal0L
pFunctionParameterInternal0 =
  choice [ Megaparsec.try pFunctionParameterInternal0Name
         , Megaparsec.try pFunctionParameterInternal02
         ]
  where
    pFunctionParameterInternal0Name :: Parser (MoveTerm FunctionParameterInternal0L)
    pFunctionParameterInternal0Name =
      FunctionParameterInternal0Name' <$> pHidVariableIdentifier
    pFunctionParameterInternal02 :: Parser (MoveTerm FunctionParameterInternal0L)
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
    pHidSpecBlockMemeberSpecInvariant :: Parser (MoveTerm HidSpecBlockMemeberL)
    pHidSpecBlockMemeberSpecInvariant =
      HidSpecBlockMemeberSpecInvariant' <$> pSpecInvariant
    pHidSpecBlockMemeberHidSpecFunction :: Parser (MoveTerm HidSpecBlockMemeberL)
    pHidSpecBlockMemeberHidSpecFunction =
      HidSpecBlockMemeberHidSpecFunction' <$> pHidSpecFunction
    pHidSpecBlockMemeberSpecCondition :: Parser (MoveTerm HidSpecBlockMemeberL)
    pHidSpecBlockMemeberSpecCondition =
      HidSpecBlockMemeberSpecCondition' <$> pSpecCondition
    pHidSpecBlockMemeberSpecInclude :: Parser (MoveTerm HidSpecBlockMemeberL)
    pHidSpecBlockMemeberSpecInclude =
      HidSpecBlockMemeberSpecInclude' <$> pSpecInclude
    pHidSpecBlockMemeberSpecApply :: Parser (MoveTerm HidSpecBlockMemeberL)
    pHidSpecBlockMemeberSpecApply =
      HidSpecBlockMemeberSpecApply' <$> pSpecApply
    pHidSpecBlockMemeberSpecPragma :: Parser (MoveTerm HidSpecBlockMemeberL)
    pHidSpecBlockMemeberSpecPragma =
      HidSpecBlockMemeberSpecPragma' <$> pSpecPragma
    pHidSpecBlockMemeberSpecVariable :: Parser (MoveTerm HidSpecBlockMemeberL)
    pHidSpecBlockMemeberSpecVariable =
      HidSpecBlockMemeberSpecVariable' <$> pSpecVariable
    pHidSpecBlockMemeberSpecLet :: Parser (MoveTerm HidSpecBlockMemeberL)
    pHidSpecBlockMemeberSpecLet =
      HidSpecBlockMemeberSpecLet' <$> pSpecLet

pSpecApply :: TermParser SpecApplyL
pSpecApply =
  SpecApply' <$> pBetween pApplyTok pToTok pHidExpression <*> pSepBy1 pSpecApplyPattern pCommaTok <*> pMaybe (pPair pExceptTok (pSepBy1 pSpecApplyPattern pCommaTok)) <*> pSemicolonTok

pSpecApplyPattern :: TermParser SpecApplyPatternL
pSpecApplyPattern =
  SpecApplyPattern' <$> pMaybe (pSpecApplyPatternInternal0 _sym) <*> pSpecApplyNamePattern <*> pMaybe pTypeParameters

pSpecApplyNamePattern :: TermParser SpecApplyNamePatternL
pSpecApplyNamePattern =
  SpecApplyNamePattern' <$> pContent _sym

pSpecApplyPatternInternal0 :: TermParser SpecApplyPatternInternal0L
pSpecApplyPatternInternal0 =
  choice [ Megaparsec.try pSpecApplyPatternInternal0Public
         , Megaparsec.try pSpecApplyPatternInternal0Internal
         ]
  where
    pSpecApplyPatternInternal0Public :: Parser (MoveTerm SpecApplyPatternInternal0L)
    pSpecApplyPatternInternal0Public =
      SpecApplyPatternInternal0Public' <$> pPublicTok
    pSpecApplyPatternInternal0Internal :: Parser (MoveTerm SpecApplyPatternInternal0L)
    pSpecApplyPatternInternal0Internal =
      SpecApplyPatternInternal0Internal' <$> pInternalTok

pSpecCondition :: TermParser SpecConditionL
pSpecCondition =
  choice [ Megaparsec.try pSpecConditionHidSpecCondition
         , Megaparsec.try pSpecConditionHidSpecAbortIf
         , Megaparsec.try pSpecConditionHidSpecAbortWithOrModifies
         ]
  where
    pSpecConditionHidSpecCondition :: Parser (MoveTerm SpecConditionL)
    pSpecConditionHidSpecCondition =
      SpecConditionHidSpecCondition' <$> pHidSpecCondition
    pSpecConditionHidSpecAbortIf :: Parser (MoveTerm SpecConditionL)
    pSpecConditionHidSpecAbortIf =
      SpecConditionHidSpecAbortIf' <$> pHidSpecAbortIf
    pSpecConditionHidSpecAbortWithOrModifies :: Parser (MoveTerm SpecConditionL)
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
  HidSpecAbortWithOrModifies' <$> (pHidSpecAbortWithOrModifiesInternal0 _sym) <*> pMaybe pConditionProperties <*> pSepBy1 pHidExpression pCommaTok <*> pSemicolonTok

pHidSpecAbortWithOrModifiesInternal0 :: TermParser HidSpecAbortWithOrModifiesInternal0L
pHidSpecAbortWithOrModifiesInternal0 =
  choice [ Megaparsec.try pHidSpecAbortWithOrModifiesInternal0AbortsWith
         , Megaparsec.try pHidSpecAbortWithOrModifiesInternal0Modifies
         ]
  where
    pHidSpecAbortWithOrModifiesInternal0AbortsWith :: Parser (MoveTerm HidSpecAbortWithOrModifiesInternal0L)
    pHidSpecAbortWithOrModifiesInternal0AbortsWith =
      HidSpecAbortWithOrModifiesInternal0AbortsWith' <$> pAbortsWithTok
    pHidSpecAbortWithOrModifiesInternal0Modifies :: Parser (MoveTerm HidSpecAbortWithOrModifiesInternal0L)
    pHidSpecAbortWithOrModifiesInternal0Modifies =
      HidSpecAbortWithOrModifiesInternal0Modifies' <$> pModifiesTok

pHidSpecCondition :: TermParser HidSpecConditionL
pHidSpecCondition =
  HidSpecCondition' <$> (pHidSpecConditionInternal0 _sym) <*> pMaybe pConditionProperties <*> pHidExpression <*> pSemicolonTok

pHidSpecConditionInternal0 :: TermParser HidSpecConditionInternal0L
pHidSpecConditionInternal0 =
  choice [ Megaparsec.try pHidSpecConditionInternal0Kind
         , Megaparsec.try pHidSpecConditionInternal02
         ]
  where
    pHidSpecConditionInternal0Kind :: Parser (MoveTerm HidSpecConditionInternal0L)
    pHidSpecConditionInternal0Kind =
      HidSpecConditionInternal0Kind' <$> pHidSpecConditionKind
    pHidSpecConditionInternal02 :: Parser (MoveTerm HidSpecConditionInternal0L)
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
    pHidSpecConditionKindAssert :: Parser (MoveTerm HidSpecConditionKindL)
    pHidSpecConditionKindAssert =
      HidSpecConditionKindAssert' <$> pAssertTok
    pHidSpecConditionKindAssume :: Parser (MoveTerm HidSpecConditionKindL)
    pHidSpecConditionKindAssume =
      HidSpecConditionKindAssume' <$> pAssumeTok
    pHidSpecConditionKindDecreases :: Parser (MoveTerm HidSpecConditionKindL)
    pHidSpecConditionKindDecreases =
      HidSpecConditionKindDecreases' <$> pDecreasesTok
    pHidSpecConditionKindEnsures :: Parser (MoveTerm HidSpecConditionKindL)
    pHidSpecConditionKindEnsures =
      HidSpecConditionKindEnsures' <$> pEnsuresTok
    pHidSpecConditionKindSucceedsIf :: Parser (MoveTerm HidSpecConditionKindL)
    pHidSpecConditionKindSucceedsIf =
      HidSpecConditionKindSucceedsIf' <$> pSucceedsIfTok

pSpecInclude :: TermParser SpecIncludeL
pSpecInclude =
  SpecInclude' <$> pBetween pIncludeTok pSemicolonTok pHidExpression

pSpecInvariant :: TermParser SpecInvariantL
pSpecInvariant =
  SpecInvariant' <$> pInvariantTok <*> pMaybe (pSpecInvariantInternal0 _sym) <*> pMaybe pConditionProperties <*> pHidExpression <*> pSemicolonTok

pSpecInvariantInternal0 :: TermParser SpecInvariantInternal0L
pSpecInvariantInternal0 =
  choice [ Megaparsec.try pSpecInvariantInternal0Update
         , Megaparsec.try pSpecInvariantInternal0Pack
         , Megaparsec.try pSpecInvariantInternal0Unpack
         , Megaparsec.try pSpecInvariantInternal0Module
         ]
  where
    pSpecInvariantInternal0Update :: Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Update =
      SpecInvariantInternal0Update' <$> pUpdateTok
    pSpecInvariantInternal0Pack :: Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Pack =
      SpecInvariantInternal0Pack' <$> pPackTok
    pSpecInvariantInternal0Unpack :: Parser (MoveTerm SpecInvariantInternal0L)
    pSpecInvariantInternal0Unpack =
      SpecInvariantInternal0Unpack' <$> pUnpackTok
    pSpecInvariantInternal0Module :: Parser (MoveTerm SpecInvariantInternal0L)
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
  SpecVariable' <$> pMaybe (pSpecVariableInternal0 _sym) <*> pIdentifier <*> pMaybe pTypeParameters <*> pBetween pColonTok pSemicolonTok pHidType

pSpecVariableInternal0 :: TermParser SpecVariableInternal0L
pSpecVariableInternal0 =
  choice [ Megaparsec.try pSpecVariableInternal0Global
         , Megaparsec.try pSpecVariableInternal0Local
         ]
  where
    pSpecVariableInternal0Global :: Parser (MoveTerm SpecVariableInternal0L)
    pSpecVariableInternal0Global =
      SpecVariableInternal0Global' <$> pGlobalTok
    pSpecVariableInternal0Local :: Parser (MoveTerm SpecVariableInternal0L)
    pSpecVariableInternal0Local =
      SpecVariableInternal0Local' <$> pLocalTok

pUnitExpression :: TermParser UnitExpressionL
pUnitExpression =
  UnitExpression' <$> pLeftParenthesisTok <*> pRightParenthesisTok

pVectorExpression :: TermParser VectorExpressionL
pVectorExpression =
  VectorExpression' <$> (pVectorExpressionInternal0 _sym) <*> pSepBy pHidExpression pCommaTok <*> pRightSquareBracketTok

pVectorExpressionInternal0 :: TermParser VectorExpressionInternal0L
pVectorExpressionInternal0 =
  choice [ Megaparsec.try pVectorExpressionInternal0VectorLeftSquareBracket
         , Megaparsec.try pVectorExpressionInternal02
         ]
  where
    pVectorExpressionInternal0VectorLeftSquareBracket :: Parser (MoveTerm VectorExpressionInternal0L)
    pVectorExpressionInternal0VectorLeftSquareBracket =
      VectorExpressionInternal0VectorLeftSquareBracket' <$> pVectorLeftSquareBracketTok
    pVectorExpressionInternal02 :: Parser (MoveTerm VectorExpressionInternal0L)
    pVectorExpressionInternal02 =
      VectorExpressionInternal02' <$> pBetween pVectorLessThanSignTok pGreaterThanSignTok (pSepBy1 pHidType pCommaTok) <*> pLeftSquareBracketTok

pMoveOrCopyExpression :: TermParser MoveOrCopyExpressionL
pMoveOrCopyExpression =
  MoveOrCopyExpression' <$> pPair (pMoveOrCopyExpressionInternal0 _sym) pHidExpression

pMoveOrCopyExpressionInternal0 :: TermParser MoveOrCopyExpressionInternal0L
pMoveOrCopyExpressionInternal0 =
  choice [ Megaparsec.try pMoveOrCopyExpressionInternal0Move
         , Megaparsec.try pMoveOrCopyExpressionInternal0Copy
         ]
  where
    pMoveOrCopyExpressionInternal0Move :: Parser (MoveTerm MoveOrCopyExpressionInternal0L)
    pMoveOrCopyExpressionInternal0Move =
      MoveOrCopyExpressionInternal0Move' <$> pMoveTok
    pMoveOrCopyExpressionInternal0Copy :: Parser (MoveTerm MoveOrCopyExpressionInternal0L)
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
    pBinaryExpression1 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression1 =
      BinaryExpression1' <$> pHidExpression <*> pEqualsSignEqualsSignGreaterThanSignTok <*> pHidExpression
    pBinaryExpression2 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression2 =
      BinaryExpression2' <$> pHidExpression <*> pVerticalLineVerticalLineTok <*> pHidExpression
    pBinaryExpression3 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression3 =
      BinaryExpression3' <$> pHidExpression <*> pAmpersandAmpersandTok <*> pHidExpression
    pBinaryExpression4 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression4 =
      BinaryExpression4' <$> pHidExpression <*> pEqualsSignEqualsSignTok <*> pHidExpression
    pBinaryExpression5 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression5 =
      BinaryExpression5' <$> pHidExpression <*> pExclamationMarkEqualsSignTok <*> pHidExpression
    pBinaryExpression6 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression6 =
      BinaryExpression6' <$> pHidExpression <*> pLessThanSignTok <*> pHidExpression
    pBinaryExpression7 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression7 =
      BinaryExpression7' <$> pHidExpression <*> pGreaterThanSignTok <*> pHidExpression
    pBinaryExpression8 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression8 =
      BinaryExpression8' <$> pHidExpression <*> pLessThanSignEqualsSignTok <*> pHidExpression
    pBinaryExpression9 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression9 =
      BinaryExpression9' <$> pHidExpression <*> pGreaterThanSignEqualsSignTok <*> pHidExpression
    pBinaryExpression10 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression10 =
      BinaryExpression10' <$> pHidExpression <*> pFullStopFullStopTok <*> pHidExpression
    pBinaryExpression11 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression11 =
      BinaryExpression11' <$> pHidExpression <*> pVerticalLineTok <*> pHidExpression
    pBinaryExpression12 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression12 =
      BinaryExpression12' <$> pHidExpression <*> pCircumflexAccentTok <*> pHidExpression
    pBinaryExpression13 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression13 =
      BinaryExpression13' <$> pHidExpression <*> pAmpersandTok <*> pHidExpression
    pBinaryExpression14 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression14 =
      BinaryExpression14' <$> pHidExpression <*> pLessThanSignLessThanSignTok <*> pHidExpression
    pBinaryExpression15 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression15 =
      BinaryExpression15' <$> pHidExpression <*> pGreaterThanSignGreaterThanSignTok <*> pHidExpression
    pBinaryExpression16 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression16 =
      BinaryExpression16' <$> pHidExpression <*> pPlusSignTok <*> pHidExpression
    pBinaryExpression17 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression17 =
      BinaryExpression17' <$> pHidExpression <*> pHyphenMinusTok <*> pHidExpression
    pBinaryExpression18 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression18 =
      BinaryExpression18' <$> pHidExpression <*> pAsteriskTok <*> pHidExpression
    pBinaryExpression19 :: Parser (MoveTerm BinaryExpressionL)
    pBinaryExpression19 =
      BinaryExpression19' <$> pHidExpression <*> pSolidusTok <*> pHidExpression
    pBinaryExpression20 :: Parser (MoveTerm BinaryExpressionL)
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
    pLambdaBindingCommaBindList :: Parser (MoveTerm LambdaBindingL)
    pLambdaBindingCommaBindList =
      LambdaBindingCommaBindList' <$> pCommaBindList
    pLambdaBinding2 :: Parser (MoveTerm LambdaBindingL)
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
    pQuantifierBinding1 :: Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding1 =
      QuantifierBinding1' <$> pIdentifier <*> pColonTok <*> pHidType
    pQuantifierBinding2 :: Parser (MoveTerm QuantifierBindingL)
    pQuantifierBinding2 =
      QuantifierBinding2' <$> pIdentifier <*> pInTok <*> pHidExpression

pReturnExpression :: TermParser ReturnExpressionL
pReturnExpression =
  choice [ Megaparsec.try pReturnExpression1
         , Megaparsec.try pReturnExpression2
         ]
  where
    pReturnExpression1 :: Parser (MoveTerm ReturnExpressionL)
    pReturnExpression1 =
      ReturnExpression1' <$> pReturnTok <*> pMaybe pLabel <*> pHidExpression
    pReturnExpression2 :: Parser (MoveTerm ReturnExpressionL)
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
    pFriendAccessLocalModule :: Parser (MoveTerm FriendAccessL)
    pFriendAccessLocalModule =
      FriendAccessLocalModule' <$> pIdentifier
    pFriendAccessFullyQualifiedModule :: Parser (MoveTerm FriendAccessL)
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
    pDatatypeFieldsPositionalFields :: Parser (MoveTerm DatatypeFieldsL)
    pDatatypeFieldsPositionalFields =
      DatatypeFieldsPositionalFields' <$> pPositionalFields
    pDatatypeFieldsNamedFields :: Parser (MoveTerm DatatypeFieldsL)
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
    pHidFunctionItemNativeFunctionDefinition :: Parser (MoveTerm HidFunctionItemL)
    pHidFunctionItemNativeFunctionDefinition =
      HidFunctionItemNativeFunctionDefinition' <$> pNativeFunctionDefinition
    pHidFunctionItemMacroFunctionDefinition :: Parser (MoveTerm HidFunctionItemL)
    pHidFunctionItemMacroFunctionDefinition =
      HidFunctionItemMacroFunctionDefinition' <$> pMacroFunctionDefinition
    pHidFunctionItemFunctionDefinition :: Parser (MoveTerm HidFunctionItemL)
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
    pModifier1 :: Parser (MoveTerm ModifierL)
    pModifier1 =
      Modifier1' <$> pPublicTok <*> pMaybe (pBetween pLeftParenthesisTok pRightParenthesisTok (pModifierInternal0 _sym))
    pModifierEntry :: Parser (MoveTerm ModifierL)
    pModifierEntry =
      ModifierEntry' <$> pEntryTok
    pModifierNative :: Parser (MoveTerm ModifierL)
    pModifierNative =
      ModifierNative' <$> pNativeTok

pModifierInternal0 :: TermParser ModifierInternal0L
pModifierInternal0 =
  choice [ Megaparsec.try pModifierInternal0Package
         , Megaparsec.try pModifierInternal0Friend
         ]
  where
    pModifierInternal0Package :: Parser (MoveTerm ModifierInternal0L)
    pModifierInternal0Package =
      ModifierInternal0Package' <$> pPackageTok
    pModifierInternal0Friend :: Parser (MoveTerm ModifierInternal0L)
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
    pHidStructItemNativeStructDefinition :: Parser (MoveTerm HidStructItemL)
    pHidStructItemNativeStructDefinition =
      HidStructItemNativeStructDefinition' <$> pNativeStructDefinition
    pHidStructItemStructDefinition :: Parser (MoveTerm HidStructItemL)
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
