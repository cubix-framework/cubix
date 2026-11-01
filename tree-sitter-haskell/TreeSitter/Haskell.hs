{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TreeSitter.Haskell
  ( tree_sitter_haskell
  ) where

import Foreign.C.ConstPtr.Compat (ConstPtr (..))

data
  {-# CTYPE "tree_sitter/tree-sitter-haskell.h" "TSLanguage" #-}
  TSLanguage

foreign import capi unsafe "tree_sitter/tree-sitter-haskell.h tree_sitter_haskell"
  tree_sitter_haskell ::
    IO (ConstPtr TSLanguage)
