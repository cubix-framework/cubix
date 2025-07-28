{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TreeSitter.SuiMove
  ( tree_sitter_sui_move
  , getNodeTypesPath
  , getTestDir
  ) where

import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Paths_tree_sitter_sui_move (getDataFileName)

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName
  "vendor/tree-sitter-move/external-crates/move/tooling/tree-sitter/src/node-types.json"

getTestDir :: IO FilePath
getTestDir = getDataFileName
  "vendor/tree-sitter-move/external-crates/move/tooling/tree-sitter/tests"

data
  {-# CTYPE "tree-sitter-move.h" "TSLanguage" #-}
  TSLanguage

foreign import capi unsafe "tree-sitter-move.h tree_sitter_move"
  tree_sitter_sui_move ::
    IO (ConstPtr TSLanguage)
