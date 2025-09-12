module Streaming.Extra where

import Streaming qualified
import Streaming.Internal (Stream (..))
import Control.Monad.Trans.Resource (MonadResource (..), allocate, release)

bracket
  :: (Functor f, MonadResource m)
  => IO a                -- ^ Initializer
  -> (a -> IO ())        -- ^ Finalizer
  -> (a -> Stream f m b) -- ^ Action
  -> Stream f m b
bracket alloc free inside = do
  (key, seed) <- Streaming.lift (allocate alloc free)
  clean key (inside seed)
  where
    clean key = loop where
      loop str = case str of
        Return r -> Effect (release key >> return (Return r))
        Effect m -> Effect (fmap loop m)
        Step f   -> Step (fmap loop f)
{-#INLINABLE bracket #-}

