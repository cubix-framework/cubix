{-# LANGUAGE RecordWildCards #-}
module Cubix.TreeSitter where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS (readFile)
import Data.IORef (IORef, newIORef, readIORef)

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Trans.Reader (ReaderT, ask, asks)
import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import TreeSitter qualified as TS (Language, Node, Parser, Point (..), Tree, nodeEndByte, nodeEndPoint, nodeStartByte, nodeStartPoint, parserLanguage, parserNew, parserParseByteString, parserSetLanguage, unsafeLanguageDelete, unsafeParserDelete, unsafeToLanguage)

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

nodeSpan :: FilePath -> TS.Node -> IO SourceSpan
nodeSpan path node = do
  start <- pointToPos' <$> TS.nodeStartPoint node
  end <- pointToPos' <$> TS.nodeEndPoint node
  pure $ mkSourceSpan path start end

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
  -> IO TreeSitterEnv
newTreeSitterEnv filepath getLang = do
  parser <- newParser getLang
  tsParser <- newIORef parser

  language <- TS.parserLanguage parser
  tsLanguage <- newIORef language

  source <- BS.readFile filepath
  mTree <- liftIO $ TS.parserParseByteString parser Nothing source
  tree <- maybe (error "failed to parse the program") pure mTree
  tsTree <- liftIO $ newIORef tree

  pure $ TreeSitterEnv {..}

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
