module TransRoundtripSpec (spec) where

import Test.Hspec
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)
import Control.Monad (filterM, forM_, unless)

import TreeSitter.SuiMove (tree_sitter_sui_move, getTestDir)
import Cubix.Language.SuiMove.ParsePretty (parse)
import Cubix.Language.SuiMove.IPS (translate, untranslate)

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
