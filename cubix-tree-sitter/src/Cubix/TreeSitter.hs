module Cubix.TreeSitter where

import Data.ByteString qualified as BS
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource (..))
import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Streaming (Stream, Of (..))
import Streaming.Prelude qualified as Streaming
import Streaming.Extra qualified as Streaming
import TreeSitter qualified as TS

import Cubix.Language.Info
  ( SourcePos (..)
  , SourceSpan (..)
  , SourceRange (..)
  )

data Token a = MkToken
  { tokenValue :: !a
  , tokenSpan :: {-# UNPACK #-} !SourceSpan
  , tokenRange :: {-# UNPACK #-} !SourceRange
  }
  deriving (Ord, Show)

-- Two tokens are equal if their grammar symbol is the same,
-- no matter the location
instance Eq a => Eq (Token a) where
  (==) l r = tokenValue l == tokenValue r

nodeRange :: TS.Node -> IO SourceRange
nodeRange node = do
  start <- TS.nodeStartByte node
  end <- TS.nodeEndByte node
  pure $ SourceRange (fromIntegral start) (fromIntegral end)

pointToPos :: FilePath -> TS.Point -> SourcePos
pointToPos path (TS.Point line column) = SourcePos path (fromIntegral line) (fromIntegral column)

nodeSpan :: FilePath -> TS.Node -> IO SourceSpan
nodeSpan path node = do
  start <- pointToPos path <$> TS.nodeStartPoint node
  end <- pointToPos path <$> TS.nodeEndPoint node
  pure $ SourceSpan start end

nodes :: MonadIO m => TS.TreeCursor -> Stream (Of TS.Node) m ()
nodes = go
  where
    go treeCursor = do
      node <- liftIO $ TS.treeCursorCurrentNode treeCursor
      Streaming.yield node

      hasChildren <-
        liftIO $ TS.treeCursorGotoFirstChild treeCursor
      when hasChildren $ do
        go treeCursor
        _ <- liftIO $ TS.treeCursorGotoParent treeCursor
        pure ()

      hasSiblings <-
        liftIO $ TS.treeCursorGotoNextSibling treeCursor
      when hasSiblings $
        go treeCursor

-- | without comments and newline tokens
significantNodes :: MonadIO m => TS.TreeCursor -> Stream (Of TS.Node) m ()
significantNodes = Streaming.filterM (liftIO . fmap not . TS.nodeIsExtra) . nodes

symbols :: MonadIO m => TS.TreeCursor -> Stream (Of String) m ()
symbols = Streaming.mapM (liftIO . TS.nodeGrammarTypeAsString) . significantNodes

type TokenStream a m r = Stream (Of (Token a)) m r

annotate :: FilePath -> TS.Node -> IO (Token TS.Node)
annotate path node = MkToken node <$> nodeSpan path node <*> nodeRange node

annotated :: MonadIO m => FilePath -> TS.TreeCursor -> TokenStream TS.Node m ()
annotated path = Streaming.mapM (liftIO . annotate path) . significantNodes

annotatedSymbols :: MonadIO m => FilePath -> TS.TreeCursor -> TokenStream String m ()
annotatedSymbols path treeCursor = flip Streaming.mapM (annotated path treeCursor) $ \tok -> do
  name <- liftIO (TS.nodeGrammarTypeAsString (tokenValue tok))
  pure tok{ tokenValue = name }

lexer :: MonadResource m => IO (ConstPtr lang) -> FilePath -> TokenStream TS.Node m ()
lexer getLang file = Streaming.bracket
  (do
    parser  <- TS.parserNew
    lang    <- TS.unsafeToLanguage =<< getLang
    success <- TS.parserSetLanguage parser lang
    unless success $
      error "failed to set parser language"

    input <- BS.readFile file
    maybeTree <- TS.parserParseByteString parser Nothing input
    tree <- maybe (error "failed to parse the program") pure maybeTree
    rootNode <- TS.treeRootNode tree
    treeCursor <- TS.treeCursorNew rootNode

    pure (lang, parser, treeCursor))
  (\(lang, parser, _) -> do
    TS.unsafeParserDelete parser
    TS.unsafeLanguageDelete lang)

  (\(_, _, treeCursor) ->
    annotated file treeCursor)
