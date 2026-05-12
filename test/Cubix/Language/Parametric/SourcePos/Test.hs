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
  ) where

import Control.Monad             (forM_)
import Data.Text                 (Text)
import Data.Text qualified       as T
import Data.Text.IO qualified    as TIO
import System.IO.Temp            (writeSystemTempFile)
import Test.Hspec

import Data.Comp.Multi           (AnnTerm, E (..), HFoldable, stripA)
import Data.Comp.Multi.Equality  (EqHF)
import Data.Comp.Multi.Generic   qualified as Generic
import Data.Comp.Multi.Show      (ShowHF)
import Data.Comp.Multi.HFunctor  (HFunctor)
import Data.Comp.Multi.Ops       (RemA, Sum, (:&:))

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
    -- written to a temp file and parsed.
  , kitSnippets :: [(String, String)]

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

-- | Run both source-position properties over every snippet in a kit.
sourcePosSpec :: forall fs root. SourcePosCxt fs => SourcePosKit fs root -> Spec
sourcePosSpec kit =
  describe (kitLanguageName kit ++ " source-position tracking") $
    forM_ (kitSnippets kit) $ \(name, src) ->
      describe ("on snippet: " ++ name) $ do
        it "every parent span contains its children's spans" $ do
          (_, t) <- parseSnippet kit name src
          case containmentViolations t of
            []  -> pure ()
            vs  -> expectationFailure $
                     "containment violations:\n" ++ unlines (map showViolation vs)

        it "every kit target reparses from its slice" $ do
          (text, t) <- parseSnippet kit name src
          forM_ (kitReParseTargets kit t) $ \target ->
            case checkReparse text target of
              Right () -> pure ()
              Left err -> expectationFailure err

parseSnippet
  :: SourcePosKit fs root
  -> String
  -> String
  -> IO (Text, AnnTerm (Maybe SourceSpan) fs root)
parseSnippet kit name src = do
  path <- writeSystemTempFile (name ++ ".lua") src
  mt   <- kitParseFile kit path
  case mt of
    Nothing -> error $ kitLanguageName kit ++ " parse failed for snippet " ++ name
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

-- | Every (ancestor, descendant) pair where both have a SourceSpan and
-- the ancestor's range fails to enclose the descendant's. Equivalent
-- to strict step-wise containment (since enclosure is transitive) but
-- doesn't require us to single out "direct children" against an
-- existential set.
containmentViolations
  :: forall fs l.
     ( HFunctor (Sum fs)
     , HFoldable (Sum fs)
     )
  => AnnTerm (Maybe SourceSpan) fs l
  -> [Violation]
containmentViolations root =
  [ Violation aSp dSp
  | E ancestor      <- Generic.subterms root
  , Just aSp        <- [getAnn ancestor]
  , E descendant    <- Generic.subterms ancestor
  , Just dSp        <- [getAnn descendant]
  , not (aSp `contains` dSp)
  ]

-- | Whether the first span encloses the second (inclusive on both ends).
contains :: SourceSpan -> SourceSpan -> Bool
contains (SourceSpan ps pe) (SourceSpan cs ce) =
  posLE ps cs && posLE ce pe
  where
    posLE (SourcePos _ r1 c1) (SourcePos _ r2 c2) = (r1, c1) <= (r2, c2)

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
  reparsed <- reparse slice
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
sliceSpan :: Text -> SourceSpan -> Text
sliceSpan src (SourceSpan (SourcePos _ r1 c1) (SourcePos _ r2 c2)) =
  let s = lineColToOffset r1 c1
      e = lineColToOffset r2 c2 + 1
  in T.take (e - s) (T.drop s src)
  where
    lineLengths :: [Int]
    lineLengths = map ((+ 1) . T.length) (T.lines src)
    -- 1-based row, 1-based col → 0-based byte offset
    lineColToOffset r c = sum (take (r - 1) lineLengths) + (c - 1)
