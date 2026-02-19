module TransRoundtripSpec (spec) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)

import Test.Hspec (Spec, describe, expectationFailure, it, runIO)

import TreeSitter.SuiMove (tree_sitter_sui_move)

import Cubix.Language.SuiMove.IPS (translate, untranslate)
import Cubix.Language.SuiMove.RawParse (parse)

-- | Parse a Move file, translate to IPS, untranslate back, and verify equality
roundtripTest :: FilePath -> IO ()
roundtripTest filepath = do
  parsed <- parse filepath tree_sitter_sui_move
  case parsed of
    Nothing -> expectationFailure $ "Failed to parse file: " ++ filepath
    Just orig -> do
      let roundtripped = untranslate . translate $ orig
      if orig == roundtripped
        then return ()
        else expectationFailure $ "Roundtrip failed for: " ++ filepath

-- | Recursively collect all .move files under a directory
getMoveFiles :: FilePath -> IO [FilePath]
getMoveFiles dir = do
  entries <- listDirectory dir
  fmap concat $ forM entries $ \entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then getMoveFiles path
      else if takeExtension path == ".move"
           then return [path]
           else return []

spec :: Spec
spec = describe "Trans/Untrans Roundtrip Tests" $ do
  files <- runIO $ getMoveFiles "../sui-move-test-corpus"
  caps  <- runIO getNumCapabilities

  it ("found " ++ show (length files) ++ " .move files; running on " ++ show caps ++ " capabilities") $
    if null files
      then expectationFailure "No .move files found in ../sui-move-test-corpus"
      else return ()

  it "all files roundtrip correctly (parallel)" $
    forConcurrently_ files roundtripTest
