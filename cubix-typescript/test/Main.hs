-- | Every sample program parses completely: each tree-sitter node's
-- children are all accounted for by the generated parser.
module Main (main) where

import Control.Monad (unless)
import Data.List (sort)
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import Cubix.TreeSitter.ParseFiles (parseFiles)
import TreeSitter.TypeScript (tree_sitter_typescript)

import Cubix.Language.TypeScript.RawParse qualified as RawParse

samples :: FilePath
samples = "test/samples"

main :: IO ()
main = do
  files <- fmap (samples </>) . sort <$> listDirectory samples
  ok <- parseFiles (`RawParse.parse` tree_sitter_typescript) (const (pure ())) files
  unless ok exitFailure
