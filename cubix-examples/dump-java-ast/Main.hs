-- | Walks a directory of .java files, parses each one via the cubix
-- tree-sitter Java parser, and writes a single @.ast@ file per input
-- under the given output directory, containing 'show' of the resulting
-- 'J.CompilationUnit'.
--
-- Output filenames mirror the input's relative path with @/@ replaced
-- by @__@, so they are flat under @<outdir>@ and stable across runs.
-- Diff-friendly: @diff -r@ between two output dirs flags any AST
-- divergence between two builds of the parser.
module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Data.List         (sort)
import System.Directory  (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit       (exitFailure)
import System.FilePath   ((</>), takeExtension)
import System.IO         (hPutStrLn, stderr)

import Cubix.Language.Java.Parse qualified as JParse

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outDir, corpus] -> do
      createDirectoryIfMissing True outDir
      files <- gatherJava corpus
      hPutStrLn stderr $ "found " <> show (length files) <> " .java files"
      mapM_ (dumpOne outDir) files
    _ -> do
      hPutStrLn stderr "usage: dump-java-ast <outDir> <corpusRoot>"
      exitFailure

gatherJava :: FilePath -> IO [FilePath]
gatherJava p = do
  isDir  <- doesDirectoryExist p
  isFile <- doesFileExist p
  if isFile
    then pure (if takeExtension p == ".java" then [p] else [])
    else if isDir
           then do
             children <- listDirectory p
             concat <$> mapM (gatherJava . (p </>)) (sort children)
           else pure []

-- | Flatten path/with/slashes.java to path__with__slashes.java.ast under outDir.
outNameFor :: FilePath -> FilePath -> FilePath
outNameFor outDir path = outDir </> (replaceSlash path <> ".ast")
  where
    replaceSlash = map (\c -> if c == '/' then '_' else c)

dumpOne :: FilePath -> FilePath -> IO ()
dumpOne outDir path = do
  r <- try @SomeException (JParse.parse path)
  let body = case r of
        Left  e         -> "EXCEPTION: " <> displayException e
        Right (Left  e) -> "PARSE_FAIL: " <> e
        Right (Right c) -> show c
  writeFile (outNameFor outDir path) body
