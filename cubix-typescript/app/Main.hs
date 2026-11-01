-- | Parse TypeScript files with the generated tree-sitter parser into
-- Modularized terms, reporting one line per file. With @--print@, also
-- pretty-print each term (annotations stripped).
module Main (main) where

import Control.Monad (unless)
import Data.List (partition)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Text.Pretty.Simple (pPrintLightBg)

import Cubix.TreeSitter.ParseFiles (parseFiles)
import Data.Comp.Multi (stripA)
import TreeSitter.TypeScript (tree_sitter_typescript)

import Cubix.Language.TypeScript.RawParse qualified as RawParse

main :: IO ()
main = do
  (flags, files) <- partition (== "--print") <$> getArgs
  let onTerm = if null flags then const (pure ()) else pPrintLightBg . stripA
  ok <- parseFiles (`RawParse.parse` tree_sitter_typescript) onTerm files
  unless ok exitFailure
