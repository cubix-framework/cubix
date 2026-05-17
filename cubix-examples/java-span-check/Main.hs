-- | Standalone span-sanity check. Walks a directory of .java files,
-- parses each one via 'JParse.parseFile', and verifies:
--
--   (1) /Containment/: for every (parent, direct-child) pair where
--       both layers carry a 'SourceSpan', the parent's span encloses
--       the child's.
--
--   (2) /Root coverage/: the root 'CompilationUnit' span starts at
--       @(1, 1)@ and ends at the last non-empty line/column of the
--       source file.
--
-- Reports per-file violations and a final tally. Exits non-zero on any
-- failure.
module Main (main) where

import Control.Exception (SomeException, try)
import Data.IORef        (IORef, newIORef, modifyIORef', readIORef)
import Data.List         (sort)
import Data.Text         (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory  (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit       (exitFailure, exitSuccess)
import System.FilePath   ((</>), takeExtension)
import System.IO         (hPutStrLn, stderr)

import Data.Comp.Multi             (E (..), HFoldable)
import Data.Comp.Multi.Generic     qualified as Generic
import Data.Comp.Multi.HFoldable   (hfoldMap)
import Data.Comp.Multi.HFunctor    (HFunctor)
import Data.Comp.Multi.Term        (Cxt (..))

import Cubix.Language.Info             (SourcePos (..), SourceSpan (..))
import Cubix.Language.Java.Parse qualified as JParse
import Cubix.Sin.Compdata.Annotation   (getAnn)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [root] -> do
      files <- gatherJava root
      hPutStrLn stderr $ "checking " <> show (length files) <> " files"
      bad <- newIORef (0 :: Int)
      mapM_ (checkOne bad) files
      n <- readIORef bad
      if n == 0
        then do
          hPutStrLn stderr $
            "OK: " <> show (length files) <> " files, no span violations"
          exitSuccess
        else do
          hPutStrLn stderr $ "FAIL: " <> show n <> " file(s) with violations"
          exitFailure
    _ -> do
      hPutStrLn stderr "usage: java-span-check <corpusRoot>"
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

checkOne :: IORef Int -> FilePath -> IO ()
checkOne bad path = do
  r <- try @SomeException (JParse.parseFile path)
  case r of
    Left  e -> reportBad path ("exception: " <> show e)
    Right Nothing -> reportBad path "tree-sitter parse failed"
    Right (Just t) -> do
      let vs = genericContainmentViolations getAnn t
      txt <- TIO.readFile path
      case getAnn t of
        Nothing -> reportBad path "root has no source span"
        Just rootSp
          | isDegenerateSpan rootSp ->
              reportBad path ("degenerate root span: " <> showSpan rootSp)
          | otherwise ->
              case rootCoverage txt rootSp of
                Just msg -> reportBad path ("root coverage: " <> msg)
                Nothing  -> case vs of
                  [] -> pure ()
                  _  -> reportBad path $ "containment:\n  "
                        <> unlines (take 3 (map showViolation vs))
  where
    reportBad p msg = do
      hPutStrLn stderr (p <> ":\n  " <> msg)
      modifyIORef' bad (+ 1)

-- ----------------------------------------------------------------------
-- Span helpers (copied from Cubix.Language.Parametric.SourcePos.Test,
-- which lives under test/ and is not in the library).

data Violation = Violation !SourceSpan !SourceSpan

showViolation :: Violation -> String
showViolation (Violation p c) =
  "  parent " <> showSpan p <> " does not contain " <> showSpan c

-- | Walk an annotated term and yield (parent, child) pairs whose span
-- containment fails.
genericContainmentViolations
  :: (HFunctor f, HFoldable f)
  => (forall i. Cxt h f a i -> Maybe SourceSpan)
  -> Cxt h f a l
  -> [Violation]
genericContainmentViolations getSp root =
  [ Violation pSp cSp
  | E parent <- Generic.subterms root
  , Just pSp <- [getSp parent]
  , not (isDegenerateSpan pSp)
  , E child  <- directChildren parent
  , Just cSp <- [getSp child]
  , not (isDegenerateSpan cSp)
  , not (pSp `contains` cSp)
  ]

directChildren :: HFoldable f => Cxt h f a l -> [E (Cxt h f a)]
directChildren (Term t) = hfoldMap (\c -> [E c]) t
directChildren (Hole _) = []

contains :: SourceSpan -> SourceSpan -> Bool
contains (SourceSpan ps pe) (SourceSpan cs ce) =
  posLE ps cs && posLE ce pe
  where
    posLE (SourcePos _ r1 c1) (SourcePos _ r2 c2) = (r1, c1) <= (r2, c2)

isDegenerateSpan :: SourceSpan -> Bool
isDegenerateSpan (SourceSpan s e) = posLT e s
  where
    posLT (SourcePos _ r1 c1) (SourcePos _ r2 c2) = (r1, c1) < (r2, c2)

-- | Verify the root span encloses the file's actual content. The start
-- must be at or before the first non-blank line/column; the end must
-- be at or after the last non-blank line. Returns 'Just msg' on
-- mismatch.
rootCoverage :: Text -> SourceSpan -> Maybe String
rootCoverage src (SourceSpan (SourcePos _ sr sc) (SourcePos _ er _))
  | sr < 1 || sc < 1 =
      Just $ "start " <> show (sr, sc) <> " is not 1-based"
  | (sr, sc) > (firstNonEmptyLine, firstNonBlankCol) =
      Just $ "start " <> show (sr, sc)
           <> " starts after first content "
           <> show (firstNonEmptyLine, firstNonBlankCol)
  | er < lastNonEmptyLine =
      Just $ "end-row " <> show er
           <> " < last non-empty line " <> show lastNonEmptyLine
  | otherwise = Nothing
  where
    ls = T.lines src
    nonEmptyIdx =
      [i | (i, l) <- zip [1..] ls, not (T.null (T.strip l))]
    firstNonEmptyLine = case nonEmptyIdx of { (x:_) -> x; _ -> 1 }
    lastNonEmptyLine  = case nonEmptyIdx of { [] -> 1; xs -> last xs }
    firstNonBlankCol  =
      case drop (firstNonEmptyLine - 1) ls of
        (l:_) ->
          1 + T.length (T.takeWhile (\c -> c == ' ' || c == '\t') l)
        _     -> 1

showSpan :: SourceSpan -> String
showSpan (SourceSpan (SourcePos _ r1 c1) (SourcePos _ r2 c2)) =
  show (r1, c1) <> "-" <> show (r2, c2)
