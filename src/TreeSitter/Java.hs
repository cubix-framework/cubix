{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- | Raw FFI binding to the vendored tree-sitter-java grammar, matching
-- the @TreeSitter.SuiMove@ convention.
--
-- The grammar's @parser.c@ is compiled into the @cubix@ library (see
-- @cubix.cabal@ @c-sources@). It exports a single public function,
-- @tree_sitter_java@, which returns a pointer to the @TSLanguage@ table
-- describing the Java grammar.
--
-- All other @ts_*@ symbols in @parser.c@ are file-static, so this
-- module does not collide with the tree-sitter runtime supplied by
-- @hs-tree-sitter@.
module TreeSitter.Java
  ( tree_sitter_java
  ) where

import Foreign.C.ConstPtr.Compat (ConstPtr (..))

-- | Opaque tree-sitter language handle. Only used to type-tag the
-- @ConstPtr@ returned by 'tree_sitter_java'.
data
  {-# CTYPE "tree_sitter/tree-sitter-java.h" "TSLanguage" #-}
  TSLanguage

foreign import capi unsafe "tree_sitter/tree-sitter-java.h tree_sitter_java"
  tree_sitter_java :: IO (ConstPtr TSLanguage)
