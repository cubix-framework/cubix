{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Sui Move source-position tracking spec.
--
-- Three properties run against every test input (5 snippets + 10 corpus
-- files, ~100 lines each):
--
--   (1) /Containment/: each parent layer's 'SourceSpan' encloses each
--       direct child's (inclusive, 1-based, tab stops 8).
--
--   (2) /Root annotation present/: the root term carries a 'Just' span,
--       so the annotation pipeline can't silently degrade to 'Nothing'
--       everywhere and still vacuously pass containment.
--
--   (3) /Slice + reparse/: for every subterm whose sort matches one of
--       the entries in 'sortWrappers', slice the source bytes at that
--       subterm's span, embed the slice in a minimal Move module that
--       puts the slice in a syntactic position fitting its sort, parse
--       the wrapper as a normal file, dig back to a subterm of the
--       same sort at the slice's known wrapped-source coordinates, and
--       check 'stripA original == stripA reparsed'.
--
-- Property 3 is the moral equivalent of the 'kitReParseTargets' check
-- the Java\/Lua\/Python suites do via 'Cubix.Language.Parametric.SourcePos.Test'.
-- Tree-sitter doesn't accept arbitrary start rules in the C API
-- hs-tree-sitter wraps, so we can't parse a slice as just an
-- @_expression@ directly; we wrap-and-dig instead. The wrapper table
-- covers a dozen sorts across module-, item-, statement-, expression-,
-- type-, and literal-level constructs.
module SourcePosSpec (spec) where

import Control.Monad                          (forM_)
import Data.List                              (sort)
import Data.Maybe                             (listToMaybe, mapMaybe)
import Data.Proxy                             (Proxy (..))
import Data.Text                              (Text)
import Data.Text qualified                    as T
import Data.Text.IO qualified                 as TIO
import Data.Typeable                          (Typeable)
import System.Directory                       (doesDirectoryExist, listDirectory)
import System.FilePath                        ((</>), takeExtension, takeFileName)
import System.IO.Temp                         (writeSystemTempFile)
import Test.Hspec                             (Spec, describe, expectationFailure, it, runIO)

import Data.Comp.Multi                        (AnnTerm, E (..), HFoldable, HFunctor, stripA)
import Data.Comp.Multi.Generic qualified      as Generic
import Data.Comp.Multi.HFoldable              (hfoldMap)
import Data.Comp.Multi.Ops                    (Sum)
import Data.Comp.Multi.Strategy.Classification (DynCase, dynProj)
import Data.Comp.Multi.Term                   (Cxt (..))

import Cubix.Language.Info                    (SourcePos (..), SourceSpan (..))
import Cubix.Language.SuiMove.Modularized
  ( BlockL, BlockItemL, ConstantL
  , HiddenBindL, HiddenExpressionL, HiddenFunctionItemL, HiddenLiteralValueL
  , HiddenStructItemL, HiddenTypeL
  , ModuleBodyL, ModuleDefinitionL
  , SourceFileL
  , UseDeclarationL
  )
import Cubix.Language.SuiMove.ParsePretty     (parseSuiMoveTrackSources)
import Cubix.Language.SuiMove.RawParse        (MoveTermAnn)
import Cubix.Sin.Compdata.Annotation          (getAnn)

spec :: Spec
spec = do
  describe "Sui Move source-position tracking (snippets)" $
    forM_ snippets $ \(name, src) ->
      describe ("on snippet: " ++ name) $
        runProperties (snippetSource name src)

  testFiles <- runIO discoverInputFiles
  describe "Sui Move source-position tracking (corpus files)" $
    forM_ testFiles $ \path ->
      describe ("on file: " ++ takeFileName path) $
        runProperties (fileSource path)

------------------------------------------------------------------------
-- Test driver
------------------------------------------------------------------------

-- | Each input loads to its source bytes plus the parsed annotated tree.
-- Returning both means the slice+reparse check doesn't have to re-read
-- the temp file after parsing — the bytes are already in hand and are
-- exactly what was fed to tree-sitter.
type Loader = IO (Maybe (Text, MoveTermAnn (Maybe SourceSpan) SourceFileL))

runProperties :: Loader -> Spec
runProperties load = do
  it "every parent span contains its children's spans" $
    load >>= \case
      Nothing      -> expectationFailure "parse failed"
      Just (_, t)  -> case containmentViolations t of
        []  -> pure ()
        vs  -> expectationFailure $
                 "containment violations:\n" ++ unlines (map showViolation vs)

  it "root layer carries an attached SourceSpan annotation" $
    load >>= \case
      Nothing      -> expectationFailure "parse failed"
      Just (_, t)  -> case getAnn t of
        Just _  -> pure ()
        Nothing -> expectationFailure
                     "root layer has no SourceSpan — annotation pipeline not wired"

  it "slices reparse to structurally-equal subterms" $
    load >>= \case
      Nothing -> expectationFailure "parse failed"
      Just (src, t) -> do
        let targets = collectTargets t
        -- Guard against vacuous pass: every file in the corpus has at
        -- least a 'ModuleDefinitionL' subterm (the whole module is one),
        -- so zero targets means 'collectTargets' is broken or every
        -- wrapper's 'dynProj' returned 'Nothing'.
        if null targets
          then expectationFailure
                 "no slice+reparse targets discovered — collectTargets returned []"
          else pure ()
        results <- mapM (runReparse src) targets
        let mismatches = [m | ReparseMismatch  m <- results]
            parseFails = [m | ReparseParseFail m <- results]
            passes     = length [() | ReparsePass <- results]
            summary    = show passes ++ " passed / "
                        ++ show (length mismatches) ++ " mismatches / "
                        ++ show (length parseFails) ++ " parse-failures of "
                        ++ show (length targets) ++ " targets"
        case (mismatches, parseFails) of
          ([], [])     -> pure ()
          (m : _, _)   -> expectationFailure $
                            summary ++ "; first mismatch:\n" ++ m
          ([], pf : _) -> expectationFailure $
                            summary ++ "; first parse-failure:\n" ++ pf

snippetSource :: String -> String -> Loader
snippetSource name src = do
  path <- writeSystemTempFile (name ++ ".move") src
  mt <- parseSuiMoveTrackSources path
  pure $ (T.pack src,) <$> mt

fileSource :: FilePath -> Loader
fileSource path = do
  mt <- parseSuiMoveTrackSources path
  case mt of
    Nothing -> pure Nothing
    Just t  -> do
      src <- TIO.readFile path
      pure (Just (src, t))

------------------------------------------------------------------------
-- Snippets and corpus
------------------------------------------------------------------------

snippets :: [(String, String)]
snippets =
  [ ( "module-with-fun"
    , unlines
        [ "module 0x1::M {"
        , "    public fun f(x: u64): u64 {"
        , "        x + 1"
        , "    }"
        , "}"
        ]
    )
  , ( "if-else"
    , unlines
        [ "module 0x1::M {"
        , "    fun f(x: u64): u64 {"
        , "        if (x > 0) { x } else { 0 }"
        , "    }"
        , "}"
        ]
    )
  , ( "while-loop"
    , unlines
        [ "module 0x1::M {"
        , "    fun f(): u64 {"
        , "        let i = 0;"
        , "        while (i < 10) { i = i + 1; };"
        , "        i"
        , "    }"
        , "}"
        ]
    )
  , ( "struct-def"
    , unlines
        [ "module 0x1::M {"
        , "    public struct S has copy, drop {"
        , "        x: u64,"
        , "        y: u64,"
        , "    }"
        , "}"
        ]
    )
  , ( "use-and-call"
    , unlines
        [ "module 0x1::M {"
        , "    use 0x1::N;"
        , "    fun f(): u64 {"
        , "        N::g(1, 2)"
        , "    }"
        , "}"
        ]
    )
  ]

discoverInputFiles :: IO [FilePath]
discoverInputFiles = do
  dir <- resolveInputDir candidates
  entries <- listDirectory dir
  let files = sort [ dir </> e | e <- entries, takeExtension e == ".move" ]
  case files of
    [] -> error $ "Sui Move corpus directory is empty: " ++ dir
    _  -> pure files
  where
    candidates = ["input-files/sui-move", "../input-files/sui-move"]

    resolveInputDir [] = error
      "Sui Move corpus directory not found; checked: input-files/sui-move, ../input-files/sui-move"
    resolveInputDir (p:ps) = do
      exists <- doesDirectoryExist p
      if exists then pure p else resolveInputDir ps

------------------------------------------------------------------------
-- Slice + reparse: per-sort wrappers
------------------------------------------------------------------------

-- | A reparseable sort, with how to embed a slice of that sort in a
-- minimal Move module so tree-sitter can parse the wrapper from
-- @source_file@.
--
-- The wrapper is single-line: @prefix <> slice <> suffix@. After
-- parsing, the slice's outer term has known row/col
-- @(1, T.length prefix + 1)@, so we dig back to a subterm of sort @l@
-- starting at exactly that position. Position+sort together uniquely
-- identify the dig target — multiple sorts can share a column, but the
-- 'dynProj' to sort @l@ disambiguates.
data SortWrapper where
  SortWrapper
    :: forall l.
       ( Typeable l
       , DynCase (MoveTermAnn (Maybe SourceSpan)) l
       )
    => Proxy l
    -> Text  -- ^ prefix (no newlines)
    -> Text  -- ^ suffix (no newlines)
    -> SortWrapper

-- | Twelve sorts spanning module-, item-, block-, statement-,
-- expression-, type-, bind-, and literal-level constructs. Wrappers
-- chosen so the slice ends up in a position the grammar accepts for
-- that sort.
sortWrappers :: [SortWrapper]
sortWrappers =
  [ SortWrapper (Proxy @ModuleDefinitionL)   "" ""
  , SortWrapper (Proxy @ModuleBodyL)         "module 0x1::M " ""
  , SortWrapper (Proxy @HiddenFunctionItemL) "module 0x1::M { " " }"
  , SortWrapper (Proxy @HiddenStructItemL)   "module 0x1::M { " " }"
  , SortWrapper (Proxy @UseDeclarationL)     "module 0x1::M { " " }"
  , SortWrapper (Proxy @ConstantL)           "module 0x1::M { " " }"
  , SortWrapper (Proxy @BlockL)              "module 0x1::M { fun _f() " " }"
  , SortWrapper (Proxy @BlockItemL)          "module 0x1::M { fun _f() { " " } }"
  , SortWrapper (Proxy @HiddenExpressionL)   "module 0x1::M { fun _f() { " " } }"
  , SortWrapper (Proxy @HiddenLiteralValueL) "module 0x1::M { fun _f() { " " } }"
  , SortWrapper (Proxy @HiddenTypeL)         "module 0x1::M { fun _f(_x: " ") { abort 0 } }"
  , SortWrapper (Proxy @HiddenBindL)         "module 0x1::M { fun _f() { let " " = 0; } }"
  ]

-- | A reparse target packages the slice's span (so the runner can pull
-- the bytes out of the original source), an action that wraps + parses
-- + digs back to the same sort, and the original subterm to compare
-- against. The sort @l@ is existential so a single list mixes targets
-- across all the entries in 'sortWrappers'.
data ReparseTarget where
  ReparseTarget
    :: forall l.
       ( Typeable l
       , DynCase (MoveTermAnn (Maybe SourceSpan)) l
       )
    => !SourceSpan
    -> Text  -- ^ wrapper prefix (for the IO action; kept for error msgs too)
    -> Text  -- ^ wrapper suffix
    -> Proxy l
    -> !(MoveTermAnn (Maybe SourceSpan) l)
    -> ReparseTarget

-- | For every (wrapper, original subterm of that sort) pair, emit a
-- target. Degenerate spans are skipped — the generic containment check
-- already ignores them and they can't be meaningfully sliced.
collectTargets
  :: MoveTermAnn (Maybe SourceSpan) SourceFileL
  -> [ReparseTarget]
collectTargets root = concatMap collectFor sortWrappers
  where
    collectFor (SortWrapper proxy prefix suffix) =
      mapMaybe (mkTarget proxy prefix suffix) (Generic.subterms root)

    mkTarget
      :: forall l.
         ( Typeable l
         , DynCase (MoveTermAnn (Maybe SourceSpan)) l
         )
      => Proxy l
      -> Text -> Text
      -> E (MoveTermAnn (Maybe SourceSpan))
      -> Maybe ReparseTarget
    mkTarget proxy prefix suffix (E sub) = do
      sp <- getAnn sub
      if isDegenerateSpan sp then Nothing else pure ()
      orig <- dynProj sub :: Maybe (MoveTermAnn (Maybe SourceSpan) l)
      pure $ ReparseTarget sp prefix suffix proxy orig

------------------------------------------------------------------------
-- The slice+reparse check itself
------------------------------------------------------------------------

data ReparseResult
  = ReparsePass
  | ReparseParseFail !String
  | ReparseMismatch  !String

runReparse :: Text -> ReparseTarget -> IO ReparseResult
runReparse src (ReparseTarget sp prefix suffix (_ :: Proxy l) orig) = do
  let slice    = sliceSpan src sp
      wrapped  = prefix <> slice <> suffix
      sliceCol = T.length prefix + 1
  path <- writeSystemTempFile "reparse.move" (T.unpack wrapped)
  mt <- parseSuiMoveTrackSources path
  pure $ case mt of
    Nothing -> ReparseParseFail $
      "wrapper failed to parse at " ++ showSpan sp
        ++ "\n  wrapped: " ++ show wrapped
    Just root -> case findAtCol sliceCol root :: Maybe (MoveTermAnn (Maybe SourceSpan) l) of
      Nothing -> ReparseParseFail $
        "no subterm of target sort at column " ++ show sliceCol
          ++ " in wrapped: " ++ show wrapped
      Just reparsed
        | stripA orig == stripA reparsed -> ReparsePass
        | otherwise -> ReparseMismatch $
            "structural mismatch at " ++ showSpan sp
              ++ "\n  slice: "    ++ show slice
              ++ "\n  wrapped: "  ++ show wrapped
              ++ "\n  original: " ++ show (stripA orig)
              ++ "\n  reparsed: " ++ show (stripA reparsed)

-- | Find the topmost subterm of sort @l@ on row 1 starting at the given
-- column. 'Generic.subterms' yields a top-down traversal so the
-- ancestors come before descendants at the same start position.
findAtCol
  :: forall l fs.
     ( DynCase (AnnTerm (Maybe SourceSpan) fs) l
     , HFunctor (Sum fs)
     , HFoldable (Sum fs)
     )
  => Int
  -> AnnTerm (Maybe SourceSpan) fs SourceFileL
  -> Maybe (AnnTerm (Maybe SourceSpan) fs l)
findAtCol col root = listToMaybe
  [ s
  | E sub <- Generic.subterms root
  , Just (SourceSpan (SourcePos _ r c) _) <- [getAnn sub]
  , r == 1 && c == col
  , Just s <- [dynProj sub :: Maybe (AnnTerm (Maybe SourceSpan) fs l)]
  ]

------------------------------------------------------------------------
-- Containment property helpers (inlined from
-- Cubix.Language.Parametric.SourcePos.Test in the main cubix package,
-- which lives in that package's test suite and isn't importable here)
------------------------------------------------------------------------

data Violation = Violation
  { _violationParent :: !SourceSpan
  , _violationChild  :: !SourceSpan
  }

showViolation :: Violation -> String
showViolation (Violation p c) =
  "  parent " ++ showSpan p ++ " does not contain " ++ showSpan c

showSpan :: SourceSpan -> String
showSpan (SourceSpan (SourcePos _ r1 c1) (SourcePos _ r2 c2)) =
  show (r1, c1) ++ "-" ++ show (r2, c2)

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
  , E child <- directChildren parent
  , Just cSp <- [getAnn child]
  , not (isDegenerateSpan cSp)
  , not (pSp `contains` cSp)
  ]

directChildren
  :: HFoldable f
  => Cxt h f a l
  -> [E (Cxt h f a)]
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

------------------------------------------------------------------------
-- Byte slicing (also inlined from the same generic framework)
------------------------------------------------------------------------

-- | Slice the substring of @source@ corresponding to @span@. Treats
-- 'SourcePos' row\/col as 1-based with end inclusive, the convention
-- 'Cubix.TreeSitter.nodeSpan' produces. Columns are tab-stop-8 aware to
-- match the parser's column accounting.
sliceSpan :: Text -> SourceSpan -> Text
sliceSpan src sp@(SourceSpan (SourcePos _ r1 c1) (SourcePos _ r2 c2))
  | isDegenerateSpan sp = T.empty
  | otherwise =
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
      in lineStart + colToCharOffset c line

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
