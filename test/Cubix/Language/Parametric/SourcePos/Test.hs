{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generic property-based test of source-position tracking.
--
-- Each language plugs in a 'SourcePosKit' that specifies how to parse a
-- file (preserving spans), and how to enumerate the subterms that can be
-- standalone-reparsed from a substring of the source. The kit-consumer
-- 'sourcePosSpec' then runs two properties per snippet:
--
--   (1) /Containment/: for every (ancestor, descendant) pair carrying
--       SourceSpans, the ancestor's span encloses the descendant's.
--
--   (2) /Slice + reparse/: for every entry returned by the kit, slicing
--       the source at the span and feeding the slice to the entry's
--       reparser produces a subterm that is structurally equal to the
--       original (after 'stripA').
--
-- Adding a new language is a few dozen lines: write a 'SourcePosKit',
-- hand it to 'sourcePosSpec' from a per-language Hspec module.
module Cubix.Language.Parametric.SourcePos.Test
  ( -- * Kit
    SourcePosKit (..)
  , ReParseTarget (..)

    -- * Driver
  , sourcePosSpec

    -- * Generic helpers exposed for kit authors
  , containmentViolations
  , Violation (..)
  , showViolation
  , showSpan
  , sliceSpan
  , contains
  , isDegenerateSpan
  ) where

import Control.Monad             (forM_)
import Data.Text                 (Text)
import Data.Text qualified       as T
import Data.Text.IO qualified    as TIO
import System.FilePath           (takeFileName)
import System.IO.Temp            (writeSystemTempFile)
import Test.Hspec

import Data.Comp.Multi           (AnnTerm, E (..), HFoldable, stripA)
import Data.Comp.Multi.Equality  (EqHF)
import Data.Comp.Multi.Generic   qualified as Generic
import Data.Comp.Multi.HFoldable (hfoldMap)
import Data.Comp.Multi.HFunctor  (HFunctor)
import Data.Comp.Multi.Ops       (RemA, Sum, (:&:))
import Data.Comp.Multi.Show      (ShowHF)
import Data.Comp.Multi.Term      (Cxt (..))

import Cubix.Language.Info             (SourcePos (..), SourceSpan (..))
import Cubix.Sin.Compdata.Annotation   (getAnn)

------------------------------------------------------------------------
-- The kit
------------------------------------------------------------------------

-- | Everything a language needs to provide so its source-position
-- tracking can be tested generically.
--
-- The signature @fs@ is the IPS sum (e.g. @MLuaSig@, @MPythonSig@).
-- The annotated term type used throughout is
-- @AnnTerm (Maybe SourceSpan) (Sum fs)@, which is equivalent to each
-- language's @M<Lang>TermAnn (Maybe SourceSpan)@.
data SourcePosKit fs root = SourcePosKit
  { -- | Human-readable name (used in test descriptions).
    kitLanguageName :: String

    -- | Inline (name, source) snippets to exercise. Each will be
    -- written to a temp file and parsed. Use these for focused,
    -- self-contained cases.
  , kitSnippets :: [(String, String)]

    -- | Absolute paths to real source files to exercise. Use these
    -- to ground the properties in a real-world corpus (e.g. the
    -- language's own test suite).
  , kitTestFiles :: [FilePath]

    -- | Parse a file preserving source spans.
  , kitParseFile
      :: FilePath -> IO (Maybe (AnnTerm (Maybe SourceSpan) fs root))

    -- | Enumerate subterms that can be sliced and reparsed standalone.
    -- Each target packages: the span of the subterm in the source,
    -- a reparser that takes the slice and returns either an error or
    -- a structurally-comparable subterm, and the original subterm.
  , kitReParseTargets
      :: AnnTerm (Maybe SourceSpan) fs root -> [ReParseTarget fs]
  }

-- | A single subterm we know how to slice and reparse.
data ReParseTarget fs where
  RPT
    :: !SourceSpan
    -> (Text -> Either String (AnnTerm (Maybe SourceSpan) fs l))
    -> !(AnnTerm (Maybe SourceSpan) fs l)
    -> ReParseTarget fs

------------------------------------------------------------------------
-- The driver
------------------------------------------------------------------------

type SourcePosCxt fs =
  ( EqHF (Sum fs)
  , ShowHF (Sum fs)
  , HFunctor (Sum fs)
  , HFoldable (Sum fs)
  , RemA (Sum fs :&: Maybe SourceSpan) (Sum fs)
  )

-- | Run both source-position properties over every snippet and test
-- file in a kit.
sourcePosSpec :: forall fs root. SourcePosCxt fs => SourcePosKit fs root -> Spec
sourcePosSpec kit =
  describe (kitLanguageName kit ++ " source-position tracking") $ do
    forM_ (kitSnippets kit)  $ \(name, src) ->
      describe ("on snippet: " ++ name) $ runProperties (snippetSource kit name src)
    forM_ (kitTestFiles kit) $ \path ->
      describe ("on file: " ++ takeFileName path) $ runProperties (fileSource kit path)
  where
    runProperties :: IO (Text, AnnTerm (Maybe SourceSpan) fs root) -> Spec
    runProperties load = do
      it "every parent span contains its children's spans" $ do
        (_, t) <- load
        case containmentViolations t of
          []  -> pure ()
          vs  -> expectationFailure $
                   "containment violations:\n" ++ unlines (map showViolation vs)

      it "every kit target reparses from its slice" $ do
        (text, t) <- load
        forM_ (kitReParseTargets kit t) $ \target ->
          case checkReparse text target of
            Right () -> pure ()
            Left err -> expectationFailure err

-- | Materialise an inline snippet into a temp file and parse it.
snippetSource
  :: SourcePosKit fs root
  -> String
  -> String
  -> IO (Text, AnnTerm (Maybe SourceSpan) fs root)
snippetSource kit name src = do
  path <- writeSystemTempFile (name ++ ".src") src
  loadParsed kit path

-- | Read a real file in place and parse it.
fileSource
  :: SourcePosKit fs root
  -> FilePath
  -> IO (Text, AnnTerm (Maybe SourceSpan) fs root)
fileSource = loadParsed

loadParsed
  :: SourcePosKit fs root
  -> FilePath
  -> IO (Text, AnnTerm (Maybe SourceSpan) fs root)
loadParsed kit path = do
  mt <- kitParseFile kit path
  case mt of
    Nothing -> error $ kitLanguageName kit ++ " parse failed for " ++ path
    Just t  -> do
      text <- TIO.readFile path
      pure (text, t)

------------------------------------------------------------------------
-- Property 1: containment
------------------------------------------------------------------------

data Violation = Violation
  { violationParent :: !SourceSpan
  , violationChild  :: !SourceSpan
  }

showViolation :: Violation -> String
showViolation (Violation p c) =
  "  parent " ++ showSpan p ++ " does not contain " ++ showSpan c

showSpan :: SourceSpan -> String
showSpan (SourceSpan (SourcePos _ r1 c1) (SourcePos _ r2 c2)) =
  show (r1, c1) ++ "-" ++ show (r2, c2)

-- | Every (parent, direct-child) pair where both have a SourceSpan and
-- the parent's range fails to enclose the child's. Stepwise containment
-- is transitive, so this is equivalent to the cross-product check on
-- (ancestor, descendant) pairs but runs in O(n) instead of O(n²) —
-- which matters when the kit hands us a 1000-line file.
containmentViolations
  :: forall fs l.
     ( HFunctor (Sum fs)
     , HFoldable (Sum fs)
     )
  => AnnTerm (Maybe SourceSpan) fs l
  -> [Violation]
containmentViolations root =
  [ Violation pSp cSp
  | E parent <- Generic.subterms root
  , Just pSp <- [getAnn parent]
  , not (isDegenerateSpan pSp)
  , E child  <- directChildren parent
  , Just cSp <- [getAnn child]
  , not (isDegenerateSpan cSp)
  , not (pSp `contains` cSp)
  ]

-- | The immediate (one-level-deep) children of a term.
directChildren
  :: HFoldable f
  => Cxt h f a l
  -> [E (Cxt h f a)]
directChildren (Term t) = hfoldMap (\c -> [E c]) t
directChildren (Hole _) = []

-- | Whether the first span encloses the second (inclusive on both ends).
contains :: SourceSpan -> SourceSpan -> Bool
contains (SourceSpan ps pe) (SourceSpan cs ce) =
  posLE ps cs && posLE ce pe
  where
    posLE (SourcePos _ r1 c1) (SourcePos _ r2 c2) = (r1, c1) <= (r2, c2)

-- | A span whose end precedes its start. Some upstream parsers
-- (currently @language-lua@) emit these for productions where one
-- side is empty and the parser falls back to a sentinel "fake"
-- 'SourcePos' at @(1,1)@; the result is a span like @(44,41)-(1,1)@
-- that's structurally broken. Kits can use this to skip degenerate
-- targets; the generic 'containmentViolations' already ignores
-- pairs where either side is degenerate.
isDegenerateSpan :: SourceSpan -> Bool
isDegenerateSpan (SourceSpan s e) = posLT e s
  where
    posLT (SourcePos _ r1 c1) (SourcePos _ r2 c2) = (r1, c1) < (r2, c2)

------------------------------------------------------------------------
-- Property 2: slice + reparse
------------------------------------------------------------------------

checkReparse
  :: SourcePosCxt fs
  => Text
  -> ReParseTarget fs
  -> Either String ()
checkReparse source (RPT sp reparse original) = do
  let slice = sliceSpan source sp
  case reparse slice of
    Left err -> Left $
      "reparse failed at " ++ showSpan sp ++
      "\n  slice: " ++ show (T.unpack slice) ++
      "\n  error: " ++ err
    Right reparsed ->
      if stripA original == stripA reparsed
        then Right ()
        else Left $ unlines
          [ "structural mismatch at " ++ showSpan sp
          , "slice was: " ++ show (T.unpack slice)
          , "original: " ++ show (stripA original)
          , "reparsed: " ++ show (stripA reparsed)
          ]

------------------------------------------------------------------------
-- Source-byte slicing
------------------------------------------------------------------------

-- | Slice the substring of @source@ corresponding to @span@. Treats
-- 'SourcePos' row/col as 1-based and the span's end position as
-- /inclusive/ — that's the convention 'language-lua' and friends use
-- when they report @sourceTo@ as the position of the /last/ token
-- consumed by a production. The slice covers the byte range from
-- @start@ through @end@ inclusive.
--
-- Columns are interpreted with tab stops every 8 characters, matching
-- @alex@\'s default lexer behaviour. A bare character-count would
-- diverge from the parser's positions inside any tab-indented file.
sliceSpan :: Text -> SourceSpan -> Text
sliceSpan src (SourceSpan (SourcePos _ r1 c1) (SourcePos _ r2 c2)) =
  let s = rowColToOffset r1 c1
      e = rowColToOffset r2 c2 + 1
  in T.take (e - s) (T.drop s src)
  where
    -- 1-based row, 1-based (tab-aware) col → 0-based byte offset
    rowColToOffset :: Int -> Int -> Int
    rowColToOffset r c =
      let ls = T.lines src
          (before, line) = case splitAt (r - 1) ls of
            (xs, y : _) -> (xs, y)
            (xs, [])    -> (xs, T.empty)
          lineStart = sum (map ((+ 1) . T.length) before)
      in lineStart + colToCharOffset c line

    -- Walk a single line accumulating tab-aware columns until @target@
    -- is reached; return the 0-based char offset within the line.
    colToCharOffset :: Int -> Text -> Int
    colToCharOffset target = go 1 0 . T.unpack
      where
        tabWidth = 8
        go !col !off cs
          | col >= target = off
          | otherwise = case cs of
              []        -> off
              ('\t':xs) -> let !col' = ((col - 1) `div` tabWidth + 1) * tabWidth + 1
                           in go col' (off + 1) xs
              (_   :xs) -> go (col + 1) (off + 1) xs
