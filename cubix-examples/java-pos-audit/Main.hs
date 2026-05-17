{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Audit harness for the tree-sitter Java parser's source-position
-- annotations. For each input file, parses to a Cubix Full-layer term
-- preserving 'SourceSpan' annotations, samples N (default 50) subterms
-- uniformly at random with a fixed seed, slices the original file at
-- each sampled node's span, and emits a Markdown table comparing
-- constructor name + reported span + extracted source text.
--
-- The output is intended for independent human review: each row asks
-- "does the slice look like a valid <Constructor>?".
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (Handle, IOMode (WriteMode), hPutStrLn, stderr, withFile)
import System.Random (StdGen, mkStdGen, randomR)

import Data.Comp.Multi (AnnTerm, E (..))
import Data.Comp.Multi.Annotation ((:&:) (..))
import Data.Comp.Multi.ConstrName (ConstrNameHF (..))
import Data.Comp.Multi.Generic qualified as Generic
import Data.Comp.Multi.Ops (caseCxt)
import Data.Comp.Multi.Term (Cxt (Term))

import Cubix.Language.Info (SourcePos (..), SourceSpan (..))
import Cubix.Language.Java.Parametric.Full (JavaSig)
import Cubix.Language.Java.Parse qualified as JParse

main :: IO ()
main = do
  args <- getArgs
  case args of
    (outFile : files@(_:_)) ->
      withFile outFile WriteMode $ \h -> do
        hPutStrLn h "# Java parser source-position audit"
        hPutStrLn h ""
        hPutStrLn h "Parser: cubix tree-sitter Java parser (Cubix.Language.Java.Parse)."
        hPutStrLn h "Method: parseFile preserves Maybe SourceSpan per layer. For each file"
        hPutStrLn h "we enumerate all subterms with a Just span, take a deterministic"
        hPutStrLn h "uniform random sample of up to 50, and emit one row per sample with"
        hPutStrLn h "the constructor name, the reported span (1-based, inclusive end), and"
        hPutStrLn h "the text sliced from the original file at that span."
        hPutStrLn h ""
        forM_ files (auditFile h)
    _ -> do
      hPutStrLn stderr "usage: java-pos-audit <outFile> <java-file>..."
      exitFailure

auditFile :: Handle -> FilePath -> IO ()
auditFile h path = do
  hPutStrLn h $ "## " ++ path
  hPutStrLn h ""
  r <- try @SomeException (JParse.parseFile path)
  case r of
    Left e -> do
      hPutStrLn h $ "**EXCEPTION:** " ++ show e
      hPutStrLn h ""
    Right Nothing -> do
      hPutStrLn h "**PARSE FAILED** (tree-sitter rejected the source)"
      hPutStrLn h ""
    Right (Just t) -> do
      src <- TIO.readFile path
      let allSubs = Generic.subterms t
          targets = mapMaybe nodeInfo allSubs
          n = length targets
      hPutStrLn h $ "Total subterms with non-degenerate spans: " ++ show n
      hPutStrLn h ""
      let want = min 50 n
          idxs = sort (take want (uniqueIdxs n (mkStdGen 0xC0FFEE)))
          samples = [ targets !! i | i <- idxs ]
      hPutStrLn h "| # | Constructor | Span | Extracted text |"
      hPutStrLn h "|---|---|---|---|"
      forM_ (zip [(1 :: Int)..] samples) $ \(i, (cn, sp)) -> do
        let txt = sliceSpan src sp
        hPutStrLn h $
          "| " ++ show i ++
          " | " ++ escapeCell (stripTypesPrefix cn) ++
          " | " ++ showSpan sp ++
          " | `" ++ escapeCell (escapeWhitespace (T.unpack txt)) ++ "` |"
      hPutStrLn h ""

--------------------------------------------------------------------------------
-- Node extraction
--------------------------------------------------------------------------------

-- | Pull a (constructor name, span) pair out of an existentially-wrapped
-- subterm, if the subterm has a non-degenerate Just span.
--
-- We sidestep naming the concrete @JavaSig@ /Sum/ type by relying on the
-- @All ConstrNameHF JavaSig@ instance the parser carries; the inner
-- pattern @(v :&: msp)@ matches both the IPS Sum and its annotation in
-- one step.
type JTerm = AnnTerm (Maybe SourceSpan) JavaSig

nodeInfo
  :: E JTerm
  -> Maybe (String, SourceSpan)
nodeInfo (E (Term (v :&: msp))) = case msp of
  Nothing -> Nothing
  Just sp
    | isDegenerate sp -> Nothing
    | otherwise -> Just (caseCxt @ConstrNameHF constrNameHF v, sp)

isDegenerate :: SourceSpan -> Bool
isDegenerate (SourceSpan (SourcePos _ r1 c1) (SourcePos _ r2 c2)) =
  (r2, c2) < (r1, c1)

--------------------------------------------------------------------------------
-- Random sampling
--------------------------------------------------------------------------------

-- | Infinite stream of distinct indices in @[0, n)@ drawn uniformly via
-- repeated rejection. Caller @take@s as many as desired.
uniqueIdxs :: Int -> StdGen -> [Int]
uniqueIdxs n gen0 = go gen0 []
  where
    go g seen
      | length seen >= n = reverse seen
      | otherwise =
          let (i, g') = randomR (0, n - 1) g
          in if i `elem` seen
               then go g' seen
               else i : go g' (i : seen)

--------------------------------------------------------------------------------
-- Text slicing
--------------------------------------------------------------------------------

-- | Slice the original source at a 'SourceSpan'. Positions are 1-based;
-- both endpoints are inclusive (per 'spanOf' in
-- "Cubix.Language.Java.Parse"). Columns are treated as character
-- offsets within the line — matching tree-sitter's "byte column",
-- which is what the parser populates. Tabs count as one column.
sliceSpan :: Text -> SourceSpan -> Text
sliceSpan src (SourceSpan (SourcePos _ r1 c1) (SourcePos _ r2 c2)) =
  let s = rowColToOffset r1 c1
      e = rowColToOffset r2 c2 + 1
  in T.take (e - s) (T.drop s src)
  where
    rowColToOffset :: Int -> Int -> Int
    rowColToOffset r c =
      let ls = T.lines src
          (before, line) = case splitAt (r - 1) ls of
            (xs, y : _) -> (xs, y)
            (xs, [])    -> (xs, T.empty)
          lineStart = sum (map ((+ 1) . T.length) before)
          -- Clamp to line length to handle the (er, 0) sentinel used
          -- when a node ends at end-of-previous-line.
          colOff = max 0 (min (T.length line) (c - 1))
      in lineStart + colOff

--------------------------------------------------------------------------------
-- Pretty helpers
--------------------------------------------------------------------------------

showSpan :: SourceSpan -> String
showSpan (SourceSpan (SourcePos _ r1 c1) (SourcePos _ r2 c2)) =
  show r1 ++ ":" ++ show c1 ++ "-" ++ show r2 ++ ":" ++ show c2

-- | Trim the @Cubix.Language.…Types.@ qualifier from a constructor
-- name to keep table cells narrow. Keeps the suffix verbatim so e.g.
-- @ListF.ConsF@ stays distinguishable from @MaybeF.JustF@ via the
-- preceding fragment-module hint.
stripTypesPrefix :: String -> String
stripTypesPrefix s
  | Just rest <- stripPrefix "Cubix.Language.Java.Parametric.Full.Types." s = "Java." ++ rest
  | Just rest <- stripPrefix "Cubix.Language.Parametric.Syntax.Functor."  s = "Functor." ++ rest
  | Just rest <- stripPrefix "Cubix.Language.Parametric.Syntax."          s = "Sx." ++ rest
  | otherwise = s
  where
    stripPrefix p xs
      | take (length p) xs == p = Just (drop (length p) xs)
      | otherwise = Nothing

-- | Replace pipe, backtick, and angle-bracket so the Markdown table
-- renders correctly even when a slice contains them.
escapeCell :: String -> String
escapeCell = concatMap escChar
  where
    escChar '|'  = "\\|"
    escChar '`'  = "'"
    escChar c    = [c]

-- | Replace tabs/newlines with visible markers so each cell stays
-- on a single Markdown row.
escapeWhitespace :: String -> String
escapeWhitespace = concatMap esc
  where
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c    = [c]
