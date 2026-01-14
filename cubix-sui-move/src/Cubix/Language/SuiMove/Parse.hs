module Cubix.Language.SuiMove.Parse where

import Control.Monad ((<=<), unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)
import Data.ByteString qualified as BS
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Strategy.Classification (DynCase, caseE)
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
  , mkSourceSpan
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
pTrueTok = pure TrueTok' <* Megaparsec.eof

pTrueTokE :: MoveParser (E MoveTerm)
pTrueTokE = E <$> pTrueTok

pFalseTok :: MoveTermParser FalseTokL
pFalseTok = pure FalseTok' <* Megaparsec.eof

pIdentifier :: MoveTermParser IdentifierL
pIdentifier = Identifier' <$> pContent <* Megaparsec.eof
  
parse :: FilePath -> IO SomeMoveTerm
parse path =
  runReaderT parse' =<<
    newTreeSitterEnv path tree_sitter_sui_move (fmap unSymbolTable . mkSymbolTable)

parse' :: ReaderT (TreeSitterEnv SomeSymbolSing) IO SomeMoveTerm
parse' = do
  filepath <- getFilePath
  rootNode <- liftIO . TS.treeRootNode =<< getTree
  syntax filepath rootNode

getContent :: ByteString -> Range -> Text
getContent src range =
  let len = rangeLength range
      start = _rangeStart rang
      chunk = ByteString.take len . ByteString.drop start
  in Text.Encoding.decodeUtf8With Text.Encoding.lenientDecode (chunk src)

syntax :: FilePath -> ByteString -> TS.Node -> ReaderT (TreeSitterEnv SomeSymbolSing) IO SomeMoveTerm
syntax path content = go
  where
    pContent = getContent content
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
          msym <- getSymbol symbolNo
          case msym of
            Nothing -> do
              pPrintLightBg $ "Unrecognized symbol: " <> show symbolNo
              pPrintLightBg $ "  at: " <> show span
              liftIO (TS.nodeTypeAsString root) >>= pPrintLightBg
              error "no parse"

            Just (SomeSymbolSing _ sym) -> do
              -- pPrintLightBg $ "parsed sym: " <> show sym
              childNo <- liftIO $ TS.nodeChildCount root
              -- pPrintLightBg $ "child count: " <> show childNo
              let p :: MoveParser SomeMoveTerm
                  p = case decSymbolSing sym SBoolLiteralSymbol of
                        Just (Refl, HRefl) -> E <$> pBoolLiteral
                        Nothing ->
                          case decSymbolSing sym STrueTokSymbol of
                            Just (Refl, HRefl) -> fmap E pTrueTok
                            Nothing -> case decSymbolSing sym SFalseTokSymbol of
                              Just (Refl, HRefl) -> fmap E pFalseTok
                              Nothing -> case decSymbolSing sym SIdentifierSymbol of
                                Just (Refl, HRefl) -> fmap E pIdentifier
                                Nothing -> E <$> pure (NumLiteral' "10" Nothing')
                  childNums = [0..childNo - 1]
              children <- if childNo == 0
                then do
                  -- pPrintLightBg $ "parsed sym: " <> show sym
                  pure []
                else
                  mapM (go <=< (liftIO . TS.nodeChild root)) childNums
              case Megaparsec.runParser p path children of
                Left err -> do                  
                  error "no parse"
                Right item -> do
                  pPrintLightBg item
                  pure item
