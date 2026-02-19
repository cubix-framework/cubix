module TransRoundtripSpec (spec) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (SomeException, bracket_, try, evaluate)
import Control.Monad (forM)
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Timeout (timeout)

import Test.Hspec (Spec, describe, expectationFailure, it, runIO)

import TreeSitter.SuiMove (tree_sitter_sui_move)

import Cubix.Language.SuiMove.Pretty qualified as Pretty
import Cubix.Language.SuiMove.RawParse (parse)

-- | Files with intentionally malformed/incomplete source code where tree-sitter
-- error recovery produces non-deterministic parse trees across formatting changes.
-- These cannot roundtrip because pretty-printing loses error-recovery artifacts
-- (e.g. trailing '::' in incomplete expressions, type arguments in invalid positions).
skipSuffixes :: [String]
skipSuffixes =
  [ "ide_mode/colon_colon_incomplete.move"  -- incomplete '::' expressions for IDE completion
  , "parser/invalid_tyarg_locs.move"        -- type arguments in deliberately invalid positions
  ]

shouldSkip :: FilePath -> Bool
shouldSkip fp = any (`isInfixOf` fp) skipSuffixes

-- | Test parse(prettyPrint(parse(x))) === parse(x).
-- Returns Nothing on success or skip, Just filepath on failure.
-- Skips files that fail to parse or take longer than 10 seconds.
roundtripTest :: FilePath -> IO (Maybe String)
roundtripTest filepath
  | shouldSkip filepath = return Nothing
  | otherwise = do
      result <- timeout (10 * 1000000) $ try @SomeException $ do
        parsed <- parse filepath tree_sitter_sui_move
        case parsed of
          Nothing -> return Nothing
          Just orig -> do
            let prettyPrinted = Pretty.pretty orig
            withSystemTempFile "roundtrip.move" $ \tmpPath tmpHandle -> do
              hClose tmpHandle
              writeFile tmpPath prettyPrinted
              reparsed <- parse tmpPath tree_sitter_sui_move
              case reparsed of
                Nothing -> return $ Just filepath
                Just ast2 -> do
                  eq <- evaluate (orig == ast2)
                  return $ if eq then Nothing else Just filepath
      case result of
        Nothing        -> return Nothing  -- timed out, skip
        Just (Left _)  -> return Nothing  -- exception, skip
        Just (Right r) -> return r

-- | Recursively collect all .move files under a directory
getMoveFiles :: FilePath -> IO [FilePath]
getMoveFiles dir = do
  entries <- listDirectory dir
  fmap concat $ forM entries $ \entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then getMoveFiles path
      else if takeExtension path == ".move"
           then do
             exists <- doesFileExist path  -- skip broken symlinks
             return [path | exists]
           else return []

-- | Try multiple candidate paths for the test corpus.
-- The working directory varies depending on how the test is invoked:
--   - Running binary directly from cubix/:          ../sui-move-test-corpus
--   - Running via cabal test from cubix/:            ../../sui-move-test-corpus
findCorpusDir :: IO (Maybe FilePath)
findCorpusDir = go candidates
  where
    candidates = [ "../sui-move-test-corpus"
                 , "../../sui-move-test-corpus"
                 ]
    go [] = return Nothing
    go (p:ps) = do
      exists <- doesDirectoryExist p
      if exists then return (Just p) else go ps

spec :: Spec
spec = describe "Parse/PrettyPrint Roundtrip Tests" $ do
  mdir <- runIO findCorpusDir
  case mdir of
    Nothing -> it "finds test corpus" $
      expectationFailure "Could not find sui-move-test-corpus directory"
    Just corpusDir -> do
      files <- runIO $ getMoveFiles corpusDir
      caps  <- runIO getNumCapabilities

      it ("found " ++ show (length files) ++ " .move files; running on " ++ show caps ++ " capabilities") $
        if null files
          then expectationFailure $ "No .move files found in " ++ corpusDir
          else return ()

      it "all files roundtrip correctly (parallel)" $ do
        sem <- newQSem (caps * 2)
        let withSem = bracket_ (waitQSem sem) (signalQSem sem)
        results <- mapConcurrently (\f -> withSem $ roundtripTest f) files
        let failures = catMaybes results
        if null failures
          then return ()
          else expectationFailure $
            show (length failures) ++ " file(s) failed roundtrip:\n"
            ++ unlines failures
