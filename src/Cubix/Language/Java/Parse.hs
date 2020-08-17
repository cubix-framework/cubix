{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Provides Java parser via the javaparser-to-hs bridge
module Cubix.Language.Java.Parse
  (
    parse
  ) where

import Control.Monad ( liftM )

import Language.Java.Parser ( compilationUnit, parser )
import Language.Java.Syntax

import System.Exit ( ExitCode(..) )
import System.IO ( hClose, hPutStrLn, stderr )
import System.IO.Temp ( withSystemTempFile )
import System.Process ( system )


parse :: FilePath -> IO (Either String CompilationUnit)
parse path = withSystemTempFile "parse" $ \tmp h -> do
                         hClose h
                         exitCode <- system $ "java -jar javaparser-to-hs.jar " ++ (show path) ++ " " ++ (show tmp)
                         case exitCode of
                           ExitFailure _ -> do hPutStrLn stderr "Java parser failed; using fallback parser"
                                               parseFallback path
                           ExitSuccess   -> liftM (Right . read) $ readFile tmp
                           

parseFallback :: FilePath -> IO (Either String CompilationUnit)
parseFallback path = do
  contents <- readFile path
  case parser compilationUnit contents of
    Left err -> return $ Left $ "parse error: " ++ show err
    Right x  -> do hPutStrLn stderr "Fallback parser succeeded"
                   return $ Right x
