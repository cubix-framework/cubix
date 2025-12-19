module BannedConstructorsSpec (spec) where

import Control.Monad (filterM, forM_, when )
import Control.Monad.Identity ( Identity(..) )
import Data.Monoid ( Any(..) )
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)
import Test.Hspec

import Data.Comp.Multi
import Data.Comp.Multi.Strategic

import TreeSitter.SuiMove (getTestDir)
import Cubix.Language.SuiMove.ParsePretty (parse)
import Cubix.Language.SuiMove.IPS (translate, MSuiMoveTerm)
import Cubix.Language.SuiMove.Modularized qualified as M

-----------------------------------------------------------------------
-- Banned constructor detection
-----------------------------------------------------------------------

hasBanned' :: Translate MSuiMoveTerm l Any
hasBanned' (project -> Just (M.Identifier {})) = pure $ Any True
hasBanned' (project -> Just (M.BinaryExpression4 {})) = pure $ Any True
hasBanned' _ = pure $ Any False

hasBanned :: MSuiMoveTerm :=> Bool
hasBanned = getAny . runIdentity . foldtdT hasBanned'

getMoveTestFiles :: IO [FilePath]
getMoveTestFiles = do
  testDir <- getTestDir
  files <- listDirectory testDir
  let moveFiles = filter (\f -> takeExtension f == ".move") files
  let fullPaths = map (testDir </>) moveFiles
  filterM doesFileExist fullPaths

bannedConstructorsTest :: FilePath -> IO ()
bannedConstructorsTest filepath = do
  parsed <- parse filepath
  case parsed of
    Nothing -> expectationFailure $ "Failed to parse file: " ++ filepath
    Just orig -> do
      let ips = translate orig
      when (hasBanned ips) $
        expectationFailure $ "Found banned constructor in: " ++ filepath

spec :: Spec
spec = describe "Banned Constructors Tests" $ do

  it "should have test files available" $ do
    files <- getMoveTestFiles
    files `shouldSatisfy` (not . null)

  describe "IPS translation should not contain banned constructors" $ do
    runIO getMoveTestFiles >>= \files ->
      forM_ files $ \file ->
        it ("has no banned constructors in " ++ file) $ do
          bannedConstructorsTest file
