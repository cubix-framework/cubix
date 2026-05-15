{-# OPTIONS_HADDOCK hide #-}

module Cubix.Language.C.Parse (
  parse
) where

import Language.C
import Language.C.System.GCC


parse :: FilePath -> IO (Either String CTranslUnit)
parse path = do
  -- On macOS, stdlib.h includes Objective-C @__BLOCKS__@ syntax that
  -- language-c can't parse. We disable that branch via a command-line
  -- @-U@ rather than prepending @#undef __BLOCKS__@ to a temp copy of
  -- the file: that way the @#line@ markers cpp emits continue to point
  -- at the original path with the original line numbers, so AST
  -- positions align with the bytes on disk.
  let gcc = newGCC "gcc"
  res <- parseCFile gcc Nothing ["-U__BLOCKS__"] path
  case res of
    Left errors -> return $ Left $ show errors
    Right tree  -> return $ Right tree
