-- | Running a generated tree-sitter parser over files, for the command-line
-- drivers and parse tests of the generated language packages.
module Cubix.TreeSitter.ParseFiles (parseFiles) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM)

-- | Parse each file, printing one line per file and a summary, and pass
-- each term to the callback. Returns whether every file parsed.
parseFiles
  :: Show t
  => (FilePath -> IO (Maybe t))
  -> (t -> IO ())
  -> [FilePath]
  -> IO Bool
parseFiles parse onTerm files = do
  results <- forM files (parseFile parse onTerm)

  let parsed = length (filter id results)
  putStrLn (show parsed <> "/" <> show (length results) <> " parsed")

  pure (and results)

-- | Parse one file, printing whether it parsed, and pass the term to the
-- callback. Returns whether the file parsed.
--
-- The generated parsers build terms lazily and report failures by
-- throwing, so the term is forced (by showing it) to surface them here.
parseFile
  :: Show t
  => (FilePath -> IO (Maybe t))
  -> (t -> IO ())
  -> FilePath
  -> IO Bool
parseFile parse onTerm file = do
  result <- try (parse file >>= traverse (evaluate . force))

  case result of
    Left (err :: SomeException) -> do
      putStrLn ("FAIL " <> file <> ": " <> takeWhile (/= '\n') (displayException err))
      pure False

    Right Nothing -> do
      putStrLn ("FAIL " <> file <> ": tree-sitter reported a syntax error")
      pure False

    Right (Just term) -> do
      putStrLn ("OK   " <> file)
      onTerm term
      pure True
  where
    force t = length (show t) `seq` t
