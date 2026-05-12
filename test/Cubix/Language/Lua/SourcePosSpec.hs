-- | Tests that source-span annotations on the Lua IPS faithfully describe
-- where each subterm came from in the original source.
--
-- Two properties:
--
--  (1) /Containment/: every parent's span encloses each child's span.
--
--  (2) /Slice + reparse/: for every subterm at a sort that
--      @language-lua@ can parse standalone (block, statement, expression),
--      the substring of the original source at that subterm's span
--      reparses (and translates through Cubix) to a structurally equal
--      subterm.

module Cubix.Language.Lua.SourcePosSpec (spec) where

import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.IO.Temp (writeSystemTempFile)

import Test.Hspec

import Language.Lua.Annotated.Parser qualified as LP

import Data.Comp.Multi (E (..), stripA)
import Data.Comp.Multi.Generic qualified as Generic
import Data.Comp.Multi.Strategy.Classification (dynProj, isSort)

import Cubix.Language.Info (SourcePos (..), SourceSpan (..), mkSourceSpan)
import Cubix.Language.Lua.Parametric.Common qualified as LCommon
import Cubix.Language.Lua.Parametric.Common.Types (MLuaSig, MLuaTermAnn)
import Cubix.Language.Lua.Parametric.Full qualified as LFull
import Cubix.ParsePretty (parseFileTrackSources)
import Cubix.Sin.Compdata.Annotation (getAnn)

------------------------------------------------------------------------
-- Test corpus
------------------------------------------------------------------------

luaSnippets :: [(String, String)]
luaSnippets =
  [ ("local-and-binop",   "local x = 42\nlocal y = x + 1\n")
  , ("if-elseif-else",    "if x then return 1 elseif y then return 2 else return 3 end\n")
  , ("while-loop",        "while i < 10 do i = i + 1 end\n")
  , ("function-decl",     "function f(a, b) return a + b end\n")
  , ("method-call",       "x:foo(1, 2)\n")
  , ("nested-table",      "local t = { 1, 2, { 3, 4 } }\n")
  , ("string-literal",    "local s = \"hello\"\n")
  ]

------------------------------------------------------------------------
-- spec
------------------------------------------------------------------------

spec :: Spec
spec = describe "Lua source-position tracking" $
  forM_ luaSnippets $ \(name, src) -> describe ("on snippet: " ++ name) $ do
    it "every parent span contains its children's spans" $ do
      (_, t) <- parseSnippet name src
      case containmentViolations t of
        []         -> pure ()
        violations -> expectationFailure $
          "containment violations:\n" ++ unlines (map showViolation violations)

    it "every block/stat/exp subterm reparses from its slice" $ do
      (text, t) <- parseSnippet name src
      forM_ (reParseTargets t) $ \target ->
        case checkReparse text target of
          Right ()  -> pure ()
          Left err  -> expectationFailure err

------------------------------------------------------------------------
-- Driver
------------------------------------------------------------------------

parseSnippet
  :: String
  -> String
  -> IO (Text, MLuaTermAnn (Maybe SourceSpan) LFull.BlockL)
parseSnippet name src = do
  path <- writeSystemTempFile (name ++ ".lua") src
  mt   <- parseFileTrackSources @MLuaSig path
  case mt of
    Nothing -> error $ "parseFileTrackSources failed for snippet " ++ name
    Just t  -> do
      text <- TIO.readFile path
      pure (text, t)

------------------------------------------------------------------------
-- (1) Containment
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

-- | Containment is enforced transitively: for every (ancestor, descendant)
-- pair where both have a SourceSpan, the ancestor's span must enclose the
-- descendant's. This is strictly stronger than checking direct
-- parent\/child pairs and avoids having to single out direct children
-- against an existential set.
containmentViolations
  :: MLuaTermAnn (Maybe SourceSpan) l
  -> [Violation]
containmentViolations root =
  [ Violation ancestorSpan descendantSpan
  | E ancestor    <- Generic.subterms root
  , Just ancestorSpan <- [getAnn ancestor]
  , E descendant  <- Generic.subterms ancestor
  , Just descendantSpan <- [getAnn descendant]
  , not (ancestorSpan `contains` descendantSpan)
  ]

contains :: SourceSpan -> SourceSpan -> Bool
contains (SourceSpan ps pe) (SourceSpan cs ce) =
  posLE ps cs && posLE ce pe
  where
    posLE (SourcePos _ r1 c1) (SourcePos _ r2 c2) = (r1, c1) <= (r2, c2)

------------------------------------------------------------------------
-- (2) Slice + reparse
------------------------------------------------------------------------

-- | A subterm we know how to standalone-reparse, plus the span to slice.
data ReParseTarget where
  RPT
    :: !SourceSpan
    -> !(ParserSort l)
    -> !(MLuaTermAnn (Maybe SourceSpan) l)
    -> ReParseTarget

data ParserSort l where
  PSBlock :: ParserSort LFull.BlockL
  PSStat  :: ParserSort LFull.StatL
  PSExp   :: ParserSort LFull.ExpL

reParseTargets
  :: MLuaTermAnn (Maybe SourceSpan) l
  -> [ReParseTarget]
reParseTargets root = mapMaybe targetOf (Generic.subterms root)
  where
    targetOf
      :: E (MLuaTermAnn (Maybe SourceSpan))
      -> Maybe ReParseTarget
    targetOf (E sub)
      | Just sp <- getAnn sub
      , isSort @LFull.BlockL sub
      , Just sub' <- dynProj @_ @LFull.BlockL sub = Just (RPT sp PSBlock sub')
      | Just sp <- getAnn sub
      , isSort @LFull.StatL sub
      , Just sub' <- dynProj @_ @LFull.StatL sub  = Just (RPT sp PSStat sub')
      | Just sp <- getAnn sub
      , isSort @LFull.ExpL sub
      , Just sub' <- dynProj @_ @LFull.ExpL sub   = Just (RPT sp PSExp sub')
      | otherwise = Nothing

checkReparse
  :: Text
  -> ReParseTarget
  -> Either String ()
checkReparse source (RPT sp ps original) = do
  let slice = sliceSpan source sp
  reparsed <- reparseAt ps slice
  if stripA original == stripA reparsed
    then Right ()
    else Left $ unlines
      [ "structural mismatch at " ++ showSpan sp
      , "slice was: " ++ show (T.unpack slice)
      , "original: " ++ show (stripA original)
      , "reparsed: " ++ show (stripA reparsed)
      ]

reparseAt
  :: ParserSort l
  -> Text
  -> Either String (MLuaTermAnn (Maybe SourceSpan) l)
reparseAt PSBlock slice =
  fmap (LCommon.translate . LFull.translate     . fmap toSourceSpan)
       (parse LP.chunk slice)
reparseAt PSStat slice =
  fmap (LCommon.translate . LFull.translateStat . fmap toSourceSpan)
       (parse LP.stat  slice)
reparseAt PSExp slice =
  fmap (LCommon.translate . LFull.translateExp  . fmap toSourceSpan)
       (parse LP.exp   slice)

parse
  :: LP.Parser a
  -> Text
  -> Either String a
parse p t = case LP.parseText p t of
  Left  (_, msg) -> Left $ "parser error: " ++ msg
  Right v        -> Right v

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Same converter that ParsePretty.parseLuaTrackSources uses.
toSourceSpan :: LP.SourceRange -> Maybe SourceSpan
toSourceSpan x = Just $ mkSourceSpan
  (T.unpack (LP.sourceFile (LP.sourceFrom x)))
  (LP.sourceLine (LP.sourceFrom x), LP.sourceColumn (LP.sourceFrom x))
  (LP.sourceLine (LP.sourceTo   x), LP.sourceColumn (LP.sourceTo   x))

-- | Slice the substring of @source@ at @span@. 1-based row+col. @SourceSpan@'s
-- end position is the position of the LAST character (inclusive), inherited
-- from @language-lua@'s @sourceTo@.
sliceSpan :: Text -> SourceSpan -> Text
sliceSpan src (SourceSpan (SourcePos _ r1 c1) (SourcePos _ r2 c2)) =
  let s = lineColToOffset r1 c1
      e = lineColToOffset r2 c2 + 1
  in T.take (e - s) (T.drop s src)
  where
    lineLengths :: [Int]
    lineLengths = map ((+ 1) . T.length) (T.lines src)
    lineColToOffset r c = sum (take (r - 1) lineLengths) + (c - 1)
