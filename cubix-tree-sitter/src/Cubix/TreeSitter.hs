{-# LANGUAGE RecordWildCards #-}
module Cubix.TreeSitter where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Trans.Reader (ReaderT, ask, asks)
import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import TreeSitter qualified as TS (Language, Node, Parser, Point (..), Tree, nodeEndByte, nodeEndPoint, nodeHasError, nodeStartByte, nodeStartPoint, parserLanguage, parserNew, parserParseByteString, parserSetLanguage, treeRootNode, unsafeLanguageDelete, unsafeParserDelete, unsafeToLanguage)

import Cubix.Language.Info (SourcePos (..), SourceRange (..), SourceSpan (..), mkSourceSpan)

nodeRange :: TS.Node -> IO SourceRange
nodeRange node = do
  start <- TS.nodeStartByte node
  end <- TS.nodeEndByte node
  pure $ SourceRange (fromIntegral start) (fromIntegral end)

pointToPos :: FilePath -> TS.Point -> SourcePos
pointToPos path (TS.Point line column) = SourcePos path (fromIntegral line) (fromIntegral column)

pointToPos' :: TS.Point -> (Int, Int)
pointToPos' (TS.Point line column) = (fromIntegral line, fromIntegral column)

nodeSpan :: FilePath -> ByteString -> TS.Node -> IO SourceSpan
nodeSpan path source node = do
  start <- pointToPos' <$> TS.nodeStartPoint node
  end <- pointToPos' <$> TS.nodeEndPoint node
  pure $ mkSourceSpan path
    (toSourcePos source start True)
    (toSourcePos source end False)
  where
    toSourcePos src (row, byteCol) isStart =
      let displayCol = byteColumnToDisplayColumn src row byteCol
      in (row + 1, displayCol + if isStart then 1 else 0)

byteColumnToDisplayColumn :: ByteString -> Int -> Int -> Int
byteColumnToDisplayColumn src row byteCol =
  T.foldl' advance 1 (TE.decodeUtf8With lenientDecode prefix) - 1
  where
    line = case drop row (BS.split 0x0a src) of
      l : _ -> l
      []    -> BS.empty
    prefix = BS.take byteCol line

    advance col '\t' = ((col - 1) `div` tabWidth + 1) * tabWidth + 1
    advance col _    = col + 1

    tabWidth = 8

withLanguage :: (MonadMask m, MonadIO m) => IO (ConstPtr lang) -> (TS.Language -> m a) -> m a
withLanguage getLang = bracket
  (liftIO $ TS.unsafeToLanguage =<< getLang)
  (liftIO . TS.unsafeLanguageDelete)

withParser :: (MonadMask m, MonadIO m) => TS.Language -> (TS.Parser -> m a) -> m a
withParser lang = bracket
  (do
    parser  <- liftIO TS.parserNew
    success <- liftIO $ TS.parserSetLanguage parser lang
    unless success $
      error "failed to set parser language"
    pure parser)
  (liftIO . TS.unsafeParserDelete)  

newParser :: IO (ConstPtr lang) -> IO TS.Parser
newParser getLang = do
  parser  <- liftIO TS.parserNew
  language <- TS.unsafeToLanguage =<< getLang
  success <- liftIO $ TS.parserSetLanguage parser language
  unless success $
    error "failed to set parser language"
  pure parser

data TreeSitterEnv = TreeSitterEnv
  { tsParser :: IORef TS.Parser
  , tsLanguage :: IORef TS.Language
  , filepath :: FilePath
  , tsTree :: IORef TS.Tree
  , source :: ByteString
  }

newTreeSitterEnv
  :: FilePath
  -> IO (ConstPtr lang)
  -> IO (Maybe TreeSitterEnv)
newTreeSitterEnv filepath getLang = do
  parser <- newParser getLang
  tsParser <- newIORef parser

  language <- TS.parserLanguage parser
  tsLanguage <- newIORef language

  source <- BS.readFile filepath
  mTree <- liftIO $ TS.parserParseByteString parser Nothing source
  case mTree of
    Nothing -> pure Nothing
    Just tree -> do
      root <- TS.treeRootNode tree
      hasError <- TS.nodeHasError root
      if hasError
        then pure Nothing
        else do
          tsTree <- liftIO $ newIORef tree

          pure $ Just TreeSitterEnv {..}

getParser :: ReaderT TreeSitterEnv IO TS.Parser
getParser = liftIO . readIORef . tsParser =<< ask
{-# INLINEABLE getParser #-}

getLanguage :: ReaderT TreeSitterEnv IO TS.Language
getLanguage = liftIO . readIORef . tsLanguage =<< ask
{-# INLINEABLE getLanguage #-}

getTree :: ReaderT TreeSitterEnv IO TS.Tree
getTree = liftIO . readIORef . tsTree =<< ask
{-# INLINEABLE getTree #-}

getFilePath :: ReaderT TreeSitterEnv IO FilePath
getFilePath = asks filepath
{-# INLINEABLE getFilePath #-}

getSource :: ReaderT TreeSitterEnv IO ByteString
getSource = asks source
{-# INLINEABLE getSource #-}
