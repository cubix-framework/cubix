{-# LANGUAGE TemplateHaskell #-}

module BannedConstructorsSpec (spec) where

import Control.Monad (filterM, forM_, when)
import Control.Monad.Identity (Identity(..))
import Data.Monoid (Any(..))
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)

import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldSatisfy)

import Data.Comp.Multi (project)
import Data.Comp.Multi.Strategic (foldtdT)

import TreeSitter.SuiMove (getTestDir, tree_sitter_sui_move)

import Cubix.Language.SuiMove.IPS (MSuiMoveTerm, translate)
import Cubix.Language.SuiMove.ParsePretty (parse)

import BannedConstructorTH (bannedCons, mkHasBanned)

$(mkHasBanned bannedCons)

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
  parsed <- parse filepath tree_sitter_sui_move
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
