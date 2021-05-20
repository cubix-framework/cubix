{-# OPTIONS_HADDOCK hide #-}

module Cubix.Language.C.Parse (
  parse
) where

import Language.C
import Language.C.System.GCC

import System.IO ( hClose, hPutStrLn )
import System.IO.Temp ( withSystemTempFile )


parse :: FilePath -> IO (Either String CTranslUnit)
parse path = withSystemTempFile "parseTmp.c" $ \tmp h -> do
                         contents <- readFile path

                         -- On Mac, stdlib.h includes some Objective-C syntax (oh Clang...),
                         -- which breaks the language-c parser. This directive kills the offending parts
                         hPutStrLn h $ "#undef __BLOCKS__\n" ++ contents
                         hClose h


                         let gcc = newGCC "gcc"
                         res <- parseCFile gcc Nothing [] tmp

                         case res of
                           Left errors -> return $ Left $ show errors
                           Right tree  -> return $ Right tree
