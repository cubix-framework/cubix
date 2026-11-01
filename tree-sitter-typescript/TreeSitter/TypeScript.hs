{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TreeSitter.TypeScript
  ( tree_sitter_typescript
  ) where

import Foreign.C.ConstPtr.Compat (ConstPtr (..))

data
  {-# CTYPE "tree_sitter/tree-sitter-typescript.h" "TSLanguage" #-}
  TSLanguage

foreign import capi unsafe "tree_sitter/tree-sitter-typescript.h tree_sitter_typescript"
  tree_sitter_typescript ::
    IO (ConstPtr TSLanguage)
