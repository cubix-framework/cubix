module Cubix.Language.SuiMove.ParsePretty where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (MonadPlus (..), forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, modify')
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IORef qualified as IORef
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Type.Equality (type (:~:) (..), type (:~~:) (..))

import TreeSitter (NodeId (..), Range (..))
import TreeSitter qualified as TS
import TreeSitter.SuiMove (tree_sitter_sui_move)

import Cubix.ParsePretty
import Cubix.Language.SuiMove.Modularized

parse :: FilePath -> IO (Maybe (MoveTerm SourceFileL))
parse path = do
  -- Create the parser
  parser <- TS.parserNew
  -- Set the language
  languageMove <- TS.unsafeToLanguage =<< tree_sitter_sui_move
  success <- TS.parserSetLanguage parser languageMove
  unless success $
    error "failed to set parser language"
  -- Assert the parser has no logger
  hasLogger <- TS.parserHasLogger parser
  when hasLogger $
    error "parser has logger"
  -- Set the logger
  let logState1 = []
  logStateRef <- IORef.newIORef logState1
  let logFun _logType msg =
        IORef.atomicModifyIORef' logStateRef $ \logState ->
          (BSC.unpack msg : logState, ())
  TS.parserSetLogger parser logFun

  -- TODO: TS.parserParse with explicit input function for streaming?
  input <- BS.readFile path
  maybeTree <- TS.parserParseByteString parser Nothing input
  tree <- maybe (error "failed to parse the program") pure maybeTree
  rootNode <- TS.treeRootNode tree
  rootNodeString <- TS.showNodeAsString rootNode

  symbolTable <- mkSymbolTable languageMove
  
  mAst <- liftIO (parseAst symbolTable tree)
  logState <- IORef.readIORef logStateRef
  -- forM_ logState putStrLn
  TS.unsafeParserDelete parser
  TS.unsafeLanguageDelete languageMove
  pure mAst

-- --------------------------------------------------------------------------------
-- -- Node Cache
-- --------------------------------------------------------------------------------

-- newtype AstCache = AstCache {unAstCache :: IntMap SomeNode}
--   deriving (Show, Semigroup, Monoid)


-- --------------------------------------------------------------------------------
-- -- Parser Class
-- --------------------------------------------------------------------------------

-- class HasParser a where
--   p :: P a

-- instance HasParser NodeId where
--   p :: P NodeId
--   p = TS.nodeId <$> getCurrentNode

-- instance HasParser (ChildList '[]) where
--   p :: P (ChildList '[])
--   p = pure Nil

-- instance (HasParser a) => HasParser (ChildList (a ': '[])) where
--   p :: P (ChildList '[a])
--   p = Cons <$> p <*> pure Nil

-- instance (HasParser a, HasParser (ChildList (b ': bs))) => HasParser (ChildList (a ': b ': bs)) where
--   p :: P (ChildList (a ': b ': bs))
--   p = Cons <$> p <* gotoNextNamedSibling <*> p

-- instance (HasParser (ChildList as)) => HasParser (Children as) where
--   p :: P (Children as)
--   p = Children <$> (gotoFirstNamedChild *> p <* gotoParent)

-- instance (HasParser a) => HasParser (Maybe a) where
--   p :: P (Maybe a)
--   p = optional p

instance (HasParser a) => HasParser [a] where
  p :: P [a]
  p = pPostFence p gotoNextNamedSibling

-- instance (HasParser a) => HasParser (NonEmpty a) where
--   p :: P (NonEmpty a)
--   p = pPostFence1 p gotoNextNamedSibling

-- instance HasParser Range where
--   p :: P Range
--   p = liftIO . TS.nodeRange =<< getCurrentNode

pPostFence :: P a -> P () -> P [a]
pPostFence post fence = postFence
 where
  fencePost = fence *> postFence <|> pure []
  postFence = ((:) <$> post <*> fencePost) <|> pure []

pPostFence1 :: P a -> P () -> P (NonEmpty a)
pPostFence1 post fence = postFence
 where
  fencePost = fence *> pPostFence post fence <|> pure []
  postFence = (:|) <$> post <*> fencePost


-- --------------------------------------------------------------------------------
-- -- Node Parser
-- --------------------------------------------------------------------------------

-- instance (KnownSort sort) => HasParser (Node sort) where
--   p :: P (Node sort)
--   p = pNode

-- pNode :: (KnownSort sort) => P (Node sort)
-- pNode =
--   pSomeNode >>= \someNode@(SomeNode isReal content) -> do
--     let symbol = nodeContentToSymbol content
--     case isReal of
--       RegularIsReal ->
--         pure $
--           case decSortSing (symbolToSort symbol) sortSing of
--             Just prf -> Node (RegularWellSorted prf) content
--             Nothing -> SortMismatch someNode
--       AuxiliaryIsReal -> pure $ Node AuxiliaryWellSorted content

-- instance HasParser SomeNode where
--   p :: P SomeNode
--   p = pSomeNode

-- p :: P (MoveTerm l)
-- p = _


-- pSomeNode :: P SomeNode
-- pSomeNode = cacheSomeNode =<< pSomeNode' =<< getCurrentNode
--  where
--   pSomeNode' :: TS.Node -> P SomeNode
--   pSomeNode' node = tryCache <|> tryParse
--    where
--     tryCache = do
--       liftIO (TS.nodeHasChanges node) >>= \nodeHasChanges ->
--         if nodeHasChanges then mzero else findOldSomeNodeInCache
--     tryParse = do
--       getSymbol node >>= \(SomeSymbolSing isReal symbol) ->
--         SomeNode isReal <$> pNodeContent symbol
