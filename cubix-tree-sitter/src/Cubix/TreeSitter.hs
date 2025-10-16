module Cubix.TreeSitter where

import Control.Monad (unless)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO (..))
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
          childs = Streaming.mapM (TS.nodeChild n)
            $ Streaming.each childNums
      pure $ Streaming.for childs go
{-# INLINE nodes #-}

significantNodes :: TS.Node -> Stream (Of TS.Node) IO ()
significantNodes = Streaming.filterM (fmap not . TS.nodeIsError) . nodes
{-# INLINE significantNodes #-}

symbols :: TS.Node -> Stream (Of String) IO ()
symbols = Streaming.mapM TS.nodeGrammarTypeAsString . significantNodes

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
