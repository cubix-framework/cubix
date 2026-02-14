module TransRoundtripSpec (spec) where

import Control.Monad (filterM, forM_, unless)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)

import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldSatisfy)

import TreeSitter.SuiMove (getTestDir, tree_sitter_sui_move)

import Cubix.Language.SuiMove.IPS (translate, untranslate)
import Cubix.Language.SuiMove.ParsePretty (parse)

-- | Parse a Move file, translate to IPS, untranslate back, and verify equality
roundtripTest :: FilePath -> IO ()
roundtripTest filepath = do
  parsed <- parse filepath tree_sitter_sui_move
  case parsed of
    Nothing -> expectationFailure $ "Failed to parse file: " ++ filepath
    Just orig -> do
      let roundtripped = untranslate . translate $ orig
      unless (orig == roundtripped) $
        expectationFailure $ "Roundtrip failed for: " ++ filepath

-- | Get all .move files from the test directory
getMoveTestFiles :: IO [FilePath]
getMoveTestFiles = do
  testDir <- getTestDir
  files <- listDirectory testDir
  let moveFiles = filter (\f -> takeExtension f == ".move") files
  let fullPaths = map (testDir </>) moveFiles
  filterM doesFileExist fullPaths

spec :: Spec
spec = describe "Trans/Untrans Roundtrip Tests" $ do
  
  it "should have test files available" $ do
    files <- getMoveTestFiles
    files `shouldSatisfy` (not . null)
  
  describe "translate . untranslate should preserve AST" $ do
    runIO getMoveTestFiles >>= \files ->
      forM_ files $ \file ->
        it ("roundtrips correctly for " ++ file) $ do
          roundtripTest file
