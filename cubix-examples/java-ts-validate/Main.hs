{-# LANGUAGE TypeApplications #-}

-- | Validation harness for the tree-sitter Java parser.
--
-- Walks one or more directories (or files) and parses every @.java@ file
-- found, reporting per-file outcome and a final tally. By default
-- exercises both the raw tree-sitter parser ('JParse.parseFile') and the
-- IPS roundtrip ('parseFile' \@'MJavaSig' followed by 'pretty').
--
-- Usage:
--   cabal run java-ts-validate -- corpora/java-semantics/tests
--
-- The K-Java corpus is the canonical target; clone it with
--   git clone --depth 1 https://github.com/kframework/java-semantics.git corpora/java-semantics
-- and pass its @tests/@ subdirectory.
module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose, hPutStr)
import System.Process (system)

import Language.Java.Syntax (CompilationUnit)

import Data.Comp.Multi (stripA)

import Cubix.Language.Java.Parametric.Common (MJavaSig)
import Cubix.Language.Java.Parametric.Full qualified as JFull
import Cubix.Language.Java.Parse qualified as JParse
import Cubix.ParsePretty (parseFile, pretty)

main :: IO ()
main = do
  rawArgs <- getArgs
  let (vsJar, args) = case rawArgs of
        ("--vs-jar" : rest) -> (True,  rest)
        _                   -> (False, rawArgs)
  when (null args) $ do
    hPutStrLn stderr "usage: java-ts-validate [--vs-jar] <path> [path ...]"
    hPutStrLn stderr "  Each <path> may be a .java file or a directory walked recursively."
    hPutStrLn stderr "  --vs-jar adds an exact-equality check against javaparser-to-hs.jar."
    hPutStrLn stderr "          (requires javaparser-to-hs.jar + javaparser-1.0.8.jar +"
    hPutStrLn stderr "           commons-lang3-3.2-SNAPSHOT.jar in the working directory and"
    hPutStrLn stderr "           a JDK on PATH)"
    exitFailure
  files <- concat <$> mapM gatherJava args
  putStrLn $ "Found " <> show (length files) <> " .java files"
  counts <- newCounters
  mapM_ (validateOne vsJar counts) files
  printSummary vsJar counts
  -- Exit non-zero so CI / shell scripts can gate parser changes on this
  -- tool. @cJarMissing@ is intentionally *not* in the failure set:
  -- those are files the JAR oracle itself can't parse (legitimate
  -- K-Java edge cases), so they're informational rather than a parser
  -- regression.
  pFail <- readIORef (cParseFail counts)
  pErr  <- readIORef (cParseErr counts)
  rBad  <- readIORef (cRoundtripBad counts)
  jDiff <- if vsJar then readIORef (cJarDiff counts) else pure 0
  let failures = pFail + pErr + rBad + jDiff
  when (failures > 0) $ exitWith (ExitFailure 1)

-- | Recursively collect @.java@ files (regular files only, excluding the
-- bogus ".java"-suffixed directories the K-Java corpus contains).
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

data Counters = Counters
  { cParseOk      :: IORef Int
  , cParseFail    :: IORef Int
  , cParseErr     :: IORef Int
  , cRoundtripOk  :: IORef Int
  , cRoundtripBad :: IORef Int
  , cJarMatch     :: IORef Int
  , cJarDiff      :: IORef Int
  , cJarMissing   :: IORef Int  -- JAR rejected (or unavailable) for this file
  , cFirstDiff    :: IORef (Maybe (FilePath, String, String))
  }

newCounters :: IO Counters
newCounters = Counters
  <$> newIORef 0 <*> newIORef 0 <*> newIORef 0
  <*> newIORef 0 <*> newIORef 0
  <*> newIORef 0 <*> newIORef 0 <*> newIORef 0
  <*> newIORef Nothing

bump :: IORef Int -> IO ()
bump r = modifyIORef' r (+ 1)

validateOne :: Bool -> Counters -> FilePath -> IO ()
validateOne vsJar ctrs path = do
  -- Phase 1: raw tree-sitter parse to J.CompilationUnit.
  rawRes <- try @SomeException (JParse.parseFile path)
  case rawRes of
    Left e -> do
      bump (cParseErr ctrs)
      putStrLn $ "PARSE_ERR " <> path <> " : " <> displayException e
    Right Nothing -> do
      bump (cParseFail ctrs)
      putStrLn $ "PARSE_FAIL " <> path
    Right (Just tsTerm) -> do
      bump (cParseOk ctrs)
      let tsCU = JFull.untranslate (stripA tsTerm)
      -- Phase 2: full IPS roundtrip via parseFile/pretty.
      doRoundtrip ctrs path
      -- Phase 3 (optional): exact equality against the JAR.
      when vsJar (compareToJar ctrs path tsCU)

-- | Run the legacy javaparser-to-hs.jar shellout and compare the
-- 'J.CompilationUnit' it produces with the tree-sitter parser's output.
-- Files where the JAR fails (e.g. its known hex/octal-literal bug) are
-- recorded as JAR_NA rather than counted against the parser.
compareToJar :: Counters -> FilePath -> CompilationUnit -> IO ()
compareToJar ctrs path tsCU = withSystemTempFile "ts-vs-jar.hs" $ \tmp h -> do
  hClose h
  let cmd = "java -cp 'javaparser-to-hs.jar:javaparser-1.0.8.jar:commons-lang3-3.2-SNAPSHOT.jar'"
         <> " JavaparserToHS " <> show path <> " " <> show tmp
         <> " > /dev/null 2>&1"
  ec <- system cmd
  case ec of
    ExitFailure _ -> do
      bump (cJarMissing ctrs)
      putStrLn ("JAR_NA " <> path)
    ExitSuccess -> do
      r <- try @SomeException (readFile tmp)
      case r of
        Left e -> do
          bump (cJarMissing ctrs)
          putStrLn ("JAR_READ_ERR " <> path <> " : " <> displayException e)
        Right s -> do
          rd <- try @SomeException (let !cu = read s :: CompilationUnit in pure cu)
          case rd of
            Left e -> do
              bump (cJarMissing ctrs)
              putStrLn ("JAR_PARSE_OUT_ERR " <> path <> " : " <> displayException e)
            Right jarCU
              | jarCU == tsCU -> do
                  bump (cJarMatch ctrs)
                  putStrLn ("JAR_MATCH " <> path)
              | otherwise -> do
                  bump (cJarDiff ctrs)
                  putStrLn ("JAR_DIFF " <> path)
                  recordFirstDiff ctrs path jarCU tsCU

recordFirstDiff :: Counters -> FilePath -> CompilationUnit -> CompilationUnit -> IO ()
recordFirstDiff ctrs path jarCU tsCU = do
  cur <- readIORef (cFirstDiff ctrs)
  case cur of
    Just _  -> pure ()
    Nothing -> modifyIORef' (cFirstDiff ctrs)
                  (\_ -> Just (path, show jarCU, show tsCU))

doRoundtrip :: Counters -> FilePath -> IO ()
doRoundtrip ctrs path = do
  r <- try @SomeException $ do
    Just term <- parseFile @MJavaSig path
    let s1 = pretty @MJavaSig term
    withSystemTempFile "rt.java" $ \tmp h -> do
      hPutStr h s1
      hClose h
      Just term2 <- parseFile @MJavaSig tmp
      let s2 = pretty @MJavaSig term2
      pure (s1 == s2)
  case r of
    Right True  -> bump (cRoundtripOk ctrs) >> putStrLn ("ROUNDTRIP_OK " <> path)
    Right False -> bump (cRoundtripBad ctrs) >> putStrLn ("ROUNDTRIP_DIFF " <> path)
    Left e      -> bump (cRoundtripBad ctrs) >> putStrLn ("ROUNDTRIP_ERR " <> path <> " : " <> displayException e)

printSummary :: Bool -> Counters -> IO ()
printSummary vsJar ctrs = do
  pOk     <- readIORef (cParseOk ctrs)
  pFail   <- readIORef (cParseFail ctrs)
  pErr    <- readIORef (cParseErr ctrs)
  rOk     <- readIORef (cRoundtripOk ctrs)
  rBad    <- readIORef (cRoundtripBad ctrs)
  putStrLn ""
  putStrLn "=== Summary ==="
  putStrLn $ "Tree-sitter parse:   " <> show pOk   <> " OK"
  putStrLn $ "                     " <> show pFail <> " tree-sitter rejected"
  putStrLn $ "                     " <> show pErr  <> " converter exception"
  putStrLn $ "IPS pretty/reparse:  " <> show rOk   <> " stable"
  putStrLn $ "                     " <> show rBad  <> " divergent"
  when vsJar $ do
    jM   <- readIORef (cJarMatch ctrs)
    jD   <- readIORef (cJarDiff  ctrs)
    jN   <- readIORef (cJarMissing ctrs)
    fd   <- readIORef (cFirstDiff ctrs)
    putStrLn $ "vs JAR oracle:       " <> show jM <> " exact match"
    putStrLn $ "                     " <> show jD <> " differ"
    putStrLn $ "                     " <> show jN <> " JAR rejected (skipped)"
    case fd of
      Nothing            -> pure ()
      Just (p, jar, ts)  -> do
        putStrLn ""
        putStrLn ("--- first divergent file: " <> p)
        putStrLn "--- JAR (oracle):"
        putStrLn (truncate' 800 jar)
        putStrLn "--- tree-sitter:"
        putStrLn (truncate' 800 ts)
  where
    truncate' n s = let (a, b) = splitAt n s
                    in if null b then a else a <> "  ...(truncated)"
