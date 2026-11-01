{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TreeSitter.Python
  ( tree_sitter_python
  ) where

import Foreign.C.ConstPtr.Compat (ConstPtr (..))

data
  {-# CTYPE "tree_sitter/tree-sitter-python.h" "TSLanguage" #-}
  TSLanguage

foreign import capi unsafe "tree_sitter/tree-sitter-python.h tree_sitter_python"
  tree_sitter_python ::
    IO (ConstPtr TSLanguage)
