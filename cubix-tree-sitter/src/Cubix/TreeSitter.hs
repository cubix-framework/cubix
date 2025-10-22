{-# LANGUAGE RecordWildCards #-}
module Cubix.TreeSitter where

import Data.ByteString qualified as BS
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, ask, asks)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IORef (IORef, newIORef, readIORef)
import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Streaming (Stream, Of (..), effect, wrap)
import Streaming.Prelude qualified as Streaming
import TreeSitter qualified as TS

import Cubix.Language.Info
  ( SourcePos (..)
  , SourceSpan (..)
  , SourceRange (..)
  , mkSourceSpan
  )
import Prelude hiding (span)

data Token a = MkToken
  { tokenValue :: !a
  , tokenSpan  :: !SourceSpan
  , tokenRange :: !SourceRange
  } deriving (Show, Functor)

-- Two tokens are equal if their grammar symbol is the same,
-- no matter the location
instance Eq a => Eq (Token a) where
  (==) l r = tokenValue l == tokenValue r

-- Work around getting Ord for ts generated symbol
instance Eq a => Ord (Token a) where
  compare a b = compare (tokenSpan a) (tokenSpan b)

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

nodes :: TS.Node -> Stream (Of TS.Node) IO ()
nodes = go
  where
    go :: TS.Node -> Stream (Of TS.Node) IO ()
    go root = wrap (root :> children root)

    children :: TS.Node -> Stream (Of TS.Node) IO ()
    children n = effect $ do
      childNo <- TS.nodeChildCount n
      let childNums = if childNo == 0 then [] else [0..childNo - 1]
          childs = Streaming.mapMaybeM (\num -> do
            child <- TS.nodeChild n num
            isNull <- TS.nodeIsNull child
            if isNull
              then do
                putStrLn $ "null child: " <> show num <> " of: " <> show childNo
                pure Nothing
              else pure $ Just child)
            $ Streaming.each childNums
      pure $ Streaming.for childs go

significantNodes :: TS.Node -> Stream (Of TS.Node) IO ()
significantNodes = Streaming.filterM (fmap not . TS.nodeIsExtra) . nodes
{-# INLINE significantNodes #-}

-- symbols :: TS.Node -> Stream (Of String) IO ()
-- symbols = Streaming.mapM TS.nodeGrammarTypeAsString . significantNodes

type TokenStream a m r = Stream (Of (Token a)) m r

annotate :: FilePath -> TS.Node -> IO (Token TS.Node)
annotate path node = MkToken node <$> nodeSpan path node <*> nodeRange node
{-# INLINE annotate #-}

annotated :: FilePath -> TS.Node -> TokenStream TS.Node IO ()
annotated path = Streaming.mapM (annotate path) . significantNodes
{-# INLINE annotated #-}

annotatedSymbols :: FilePath -> TS.Node -> TokenStream String IO ()
annotatedSymbols path node = flip Streaming.mapM (annotated path node) $ \tok -> do
  name <- liftIO (TS.nodeGrammarTypeAsString (tokenValue tok))
  pure tok{ tokenValue = name }

symbols :: FilePath -> TS.Node -> TokenStream TS.Symbol (ReaderT (TreeSitterEnv sym) IO) ()
symbols path = go
  where
    go :: TS.Node -> TokenStream TS.Symbol (ReaderT (TreeSitterEnv sym) IO) ()
    go root = effect $ do
      extra <- liftIO $ TS.nodeIsExtra root
      when extra $ pure ()
      range  <- liftIO $ nodeRange root
      span   <- liftIO $ nodeSpan path root
      symbol <- liftIO $ TS.nodeSymbol root
      let tok = MkToken symbol span range
      childNo <- liftIO $ TS.nodeChildCount root
      let childNums = if childNo == 0 then [] else [0..childNo - 1]
          childs = Streaming.mapM (liftIO . TS.nodeChild root)
            $ Streaming.each childNums
      pure $ wrap (tok :> Streaming.for childs go)

    -- children :: TS.Node -> TokenStream TS.Symbol IO ()
    -- children n = effect $ do
    --   childNo <- TS.nodeChildCount n
    --   let childNums = if childNo == 0 then [] else [0..childNo - 1]
    --       childs = Streaming.mapM (TS.nodeChild n)
    --         $ Streaming.each childNums
    --   pure $ Streaming.for childs go

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

data TreeSitterEnv sym = TreeSitterEnv
  { tsParser :: IORef TS.Parser
  , tsLanguage :: IORef TS.Language
  , tsSymbolTable :: !(IntMap sym)
  , filepath :: FilePath
  , tsTree :: IORef TS.Tree
  , source :: BS.ByteString
  }

newTreeSitterEnv
  :: FilePath
  -> IO (ConstPtr lang)
  -> (TS.Language -> IO (IntMap sym))
  -> IO (TreeSitterEnv sym)
newTreeSitterEnv filepath getLang mkSymbolTable = do
  parser <- newParser getLang
  tsParser <- newIORef parser

  language <- TS.parserLanguage parser
  tsLanguage <- newIORef language

  tsSymbolTable <- mkSymbolTable language

  source <- BS.readFile filepath
  mTree <- liftIO $ TS.parserParseByteString parser Nothing source
  tree <- maybe (error "failed to parse the program") pure mTree
  tsTree <- liftIO $ newIORef tree

  pure $ TreeSitterEnv {..}

getParser :: ReaderT (TreeSitterEnv sym) IO TS.Parser
getParser = liftIO . readIORef . tsParser =<< ask
{-# INLINEABLE getParser #-}

getSymbolTable :: ReaderT (TreeSitterEnv sym) IO (IntMap sym)
getSymbolTable = asks tsSymbolTable
{-# INLINEABLE getSymbolTable #-}

getTree :: ReaderT (TreeSitterEnv sym) IO TS.Tree
getTree = liftIO . readIORef . tsTree =<< ask
{-# INLINEABLE getTree #-}

getSymbol :: Integral a => a -> ReaderT (TreeSitterEnv sym) IO (Maybe sym)
getSymbol symbol = IM.lookup (fromIntegral symbol) <$> getSymbolTable
{-# INLINEABLE getSymbol #-}

getFilePath :: ReaderT (TreeSitterEnv sym) IO FilePath
getFilePath = asks filepath
{-# INLINEABLE getFilePath #-}

getSource :: ReaderT (TreeSitterEnv sym) IO BS.ByteString
getSource = asks source
{-# INLINEABLE getSource #-}

