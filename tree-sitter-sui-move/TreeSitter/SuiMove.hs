module TreeSitter.SuiMove
  ( tree_sitter_move
  , getNodeTypesPath
  , getTestDir
  ) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_sui_move

foreign import ccall unsafe
  "vendor/tree-sitter-move/external-crates/move/tooling/tree-sitter/src/parser.c"
  tree_sitter_move :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName
  "vendor/tree-sitter-move/external-crates/move/tooling/tree-sitter/src/node-types.json"

getTestDir :: IO FilePath
getTestDir = getDataFileName
  "vendor/tree-sitter-move/external-crates/move/tooling/tree-sitter/tests"
