module Cubix.Language.SuiMove.ParsePretty where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (MonadPlus (..), forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, modify')
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Resource (runResourceT)
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
import Streaming qualified
import Streaming.Prelude qualified as Streaming

import TreeSitter (NodeId (..), Range (..))
import TreeSitter qualified as TS
import TreeSitter.SuiMove (tree_sitter_sui_move)

import Cubix.ParsePretty
import Cubix.Language.SuiMove.Modularized
import Cubix.TreeSitter

parse :: FilePath -> IO (Maybe (MoveTerm SourceFileL))
parse path = runResourceT $
  withLanguage tree_sitter_sui_move $ \lang -> do
  Streaming.print
    $ Streaming.mapM (liftIO . TS.nodeGrammarTypeAsString . tokenValue)
    $ lexer lang path

  pure undefined
