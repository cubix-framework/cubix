module Cubix.Language.SuiMove.Parse where

import Control.Monad ((<=<), unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Strategy.Classification (DynCase, caseE)
import Data.Foldable (foldrM)
import Data.Functor ((<$))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Type.Equality (type (:~:) (..), type (:~~:) (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IORef (IORef, newIORef, readIORef)
import Streaming (Stream, Of (..), effect, wrap)
import Streaming.Prelude qualified as Streaming
import TreeSitter qualified as TS
import TreeSitter.SuiMove (tree_sitter_sui_move)

import Cubix.Language.Info
  ( SourcePos (..)
  , SourceSpan (..)
  , SourceRange (..)
  , rangeLength
  )
import Prelude hiding (span)
import Cubix.TreeSitter
import Cubix.Language.SuiMove.Modularized
import Cubix.Language.Parametric.Syntax
import Data.Comp.Multi (Cxt, NoHole, Sum, K)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Cubix

import Debug.Trace
import Text.Pretty.Simple

type SomeMoveTerm = E MoveTerm
type MoveParser t = Parser NoHole (Sum MoveSig) (K ()) t
type MoveTermParser l = MoveParser (MoveTerm l)
type SomeMoveTermParser = MoveParser (E MoveTerm)

pBoolLiteral :: MoveTermParser BoolLiteralL
pBoolLiteral = Megaparsec.choice
  [ BoolLiteralFalse' <$> pSort @FalseTokL "false"
  , BoolLiteralTrue' <$> pSort @TrueTokL "true"
  ] <* Megaparsec.eof

pTrueTok :: MoveTermParser TrueTokL
pTrueTok = TrueTok' <$ Megaparsec.eof

pFalseTok :: MoveTermParser FalseTokL
pFalseTok = FalseTok' <$ Megaparsec.eof

pIdentifier :: MoveTermParser IdentifierL
pIdentifier = Identifier' <$> pContent <* Megaparsec.eof

newtype ParseTable = ParseTable {unParseTable :: IntMap SomeMoveTermParser}

symbolMap :: Map String SomeMoveTermParser
symbolMap = Map.fromList
  [ ("bool_literal", E <$> pBoolLiteral)
  , ("identifier", E <$> pIdentifier)
  , ("false", E <$> pFalseTok)
  , ("true", E <$> pTrueTok)
  ]

mkParseTable :: TS.Language -> IO ParseTable
mkParseTable lang = do
  count <- fromIntegral <$> TS.languageSymbolCount lang
  ParseTable <$> foldrM
    (\id acc -> do
      symName <- TS.languageSymbolName lang id
      let mSymSing = Map.lookup (Char8.unpack symName) symbolMap
      pure (maybe acc (flip (IM.insert (fromIntegral id)) acc) mSymSing)
    )
    (IM.empty :: IntMap SomeMoveTermParser)
    [0..count - 1]

parse :: FilePath -> IO SomeMoveTerm
parse path =
  runReaderT parse' =<<
    newTreeSitterEnv path tree_sitter_sui_move

parse' :: ReaderT (TreeSitterEnv SomeSymbolSing) IO SomeMoveTerm
parse' = do
  filepath <- getFilePath
  source <- getSource
  pTable <- liftIO . mkParseTable =<< getLanguage
  rootNode <- liftIO . TS.treeRootNode =<< getTree
  syntax filepath source pTable rootNode

getContent :: ByteString -> SourceRange -> ByteString
getContent src range =
  let len = rangeLength range
      start = _rangeStart range
  in ByteString.take len . ByteString.drop start $ src

syntax :: FilePath -> ByteString -> ParseTable -> TS.Node -> ReaderT (TreeSitterEnv SomeSymbolSing) IO SomeMoveTerm
syntax path source pTable = go
  where
    pContent = getContent source
    getParser sym = IM.lookup (fromIntegral sym) (unParseTable pTable)
    go :: TS.Node -> ReaderT (TreeSitterEnv SomeSymbolSing) IO SomeMoveTerm
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
              -- pPrintLightBg $ "Unrecognized symbol: " <> show symbolNo
              -- pPrintLightBg $ "  at: " <> show span
              -- liftIO (TS.nodeTypeAsString root) >>= pPrintLightBg
              -- error "no parse"
              childNo <- liftIO $ TS.nodeChildCount root
              if childNo == 0
                then do
                  pure $ E $ NumLiteral' "Test" Nothing'
                else
                  head <$> mapM (go <=< (liftIO . TS.nodeChild root)) [0..childNo - 1]

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
              case Megaparsec.runParser p path (Tok content <$> children) of
                Left err -> do
                  error "no parse"
                Right item -> do
                  pPrintLightBg item
                  pure item
