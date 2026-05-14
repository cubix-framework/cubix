-- | Python plug-in for the language-agnostic source-position spec.
--
-- All the property logic lives in
-- "Cubix.Language.Parametric.SourcePos.Test"; this module supplies the
-- Python-specific bits: snippets to exercise, how to parse a file, and
-- how to enumerate parser-able subterms (Module, Statement, Expr)
-- along with the reparser chain
-- @parse{Module,Stmt,Expr} → Full.translate → Common.translate@.

module Cubix.Language.Python.SourcePosSpec (spec) where

import Control.Monad                         (unless, when)
import Data.List                             (sort)
import Data.Maybe                            (mapMaybe)
import Data.Text                             (Text)
import Data.Text qualified                   as T
import System.Directory                      (doesDirectoryExist, listDirectory)
import System.FilePath                       ((</>), takeExtension)
import Test.Hspec

import Language.Python.Common qualified            as Python
import Language.Python.Version3.Parser qualified   as Python

import Data.Comp.Multi                       (E (..))
import Data.Comp.Multi.Generic               qualified as Generic
import Data.Comp.Multi.Strategy.Classification (dynProj, isSort)

import Cubix.Language.Info                   (SourceSpan (..), SourcePos (..), mkSourceSpan)
import Cubix.Language.Python.Parametric.Common (MPythonSig, MPythonTermAnn)
import Cubix.Language.Python.Parametric.Common qualified as PCommon
import Cubix.Language.Python.Parametric.Full qualified as PFull
import Cubix.ParsePretty                     (parseFileTrackSources)
import Cubix.Sin.Compdata.Annotation         (getAnn)

import Cubix.Language.Parametric.SourcePos.Test

spec :: Spec
spec = do
  testFiles <- runIO discoverInputFiles
  sourcePosSpec (pythonKit testFiles)

pythonKit :: [FilePath] -> SourcePosKit MPythonSig PFull.ModuleL
pythonKit testFiles = SourcePosKit
  { kitLanguageName   = "Python"
  , kitSnippets       = pythonSnippets
  , kitTestFiles      = testFiles
  , kitParseFile      = parseFileTrackSources @MPythonSig
  , kitReParseTargets = pythonReParseTargets
    -- Python parsing is context-free (no typedef-style ambiguities), so
    -- reparse mismatches indicate real problems and should fail loudly.
  , kitFileReparseIsBestEffort = False
  }

-- | Discover the @.py@ files in @input-files/python/@ (including the
-- @ipt/@ subdirectory).
discoverInputFiles :: IO [FilePath]
discoverInputFiles = (++) <$> listPyFiles "input-files/python"
                          <*> listPyFiles "input-files/python/ipt"

listPyFiles :: FilePath -> IO [FilePath]
listPyFiles dir = do
  exists <- doesDirectoryExist dir
  unless exists $
    error $ "Python test corpus directory missing — expected to find " ++ dir
  entries <- listDirectory dir
  let files = sort [ dir </> e | e <- entries, takeExtension e == ".py" ]
  when (null files) $
    error $ "Python test corpus directory is empty — no .py files in " ++ dir
  pure files

pythonSnippets :: [(String, String)]
pythonSnippets =
  [ ("assign-and-binop", "x = 42\ny = x + 1\n")
  , ("if-elif-else",     "if x:\n    y = 1\nelif z:\n    y = 2\nelse:\n    y = 3\n")
  , ("while-loop",       "while i < 10:\n    i = i + 1\n")
  , ("function-def",     "def f(a, b):\n    return a + b\n")
  , ("method-call",      "x.foo(1, 2)\n")
  , ("nested-list",      "t = [1, 2, [3, 4]]\n")
  , ("string-literal",   "s = \"hello\"\n")
  ]

-- | Pick out subterms at sorts that @language-python@ can parse
-- standalone (Module, Statement, Expr) and package each with its
-- reparser.
pythonReParseTargets
  :: MPythonTermAnn (Maybe SourceSpan) l
  -> [ReParseTarget MPythonSig]
pythonReParseTargets root = mapMaybe targetOf (Generic.subterms root)
  where
    targetOf
      :: E (MPythonTermAnn (Maybe SourceSpan)) -> Maybe (ReParseTarget MPythonSig)
    targetOf (E sub)
      | Just sp <- getAnn sub, isDegenerateSpan sp = Nothing
      | Just sp <- getAnn sub
      , isSort @PFull.ModuleL sub
      , Just sub' <- dynProj @_ @PFull.ModuleL sub    = Just (RPT sp reparseModule sub')
      | Just sp <- getAnn sub
      , isSort @PFull.StatementL sub
      , Just sub' <- dynProj @_ @PFull.StatementL sub = Just (RPT sp (reparseStatement sp) sub')
      | Just sp <- getAnn sub
      , isSort @PFull.ExprL sub
      , Just sub' <- dynProj @_ @PFull.ExprL sub      = Just (RPT sp reparseExpression sub')
      | otherwise = Nothing

reparseModule :: Text -> Either String (MPythonTermAnn (Maybe SourceSpan) PFull.ModuleL)
reparseModule = fmap (PCommon.translate . PFull.translate . fmap toSourceSpan . fst)
              . runParser Python.parseModule

-- | 'language-python' returns a list of statements (the grammar's
-- @single_input@ accepts a sequence of simple statements on one line).
-- We only enumerate single-statement subterms, so if the parser hands
-- back anything other than a singleton list we treat the reparse as a
-- failure.
--
-- The slice has its first line trimmed to column 1 but keeps the
-- absolute columns of any subsequent lines. So if the statement was
-- originally at column @c1 > 1@ (i.e. itself the body of some outer
-- compound statement), every subsequent line carries an extra
-- @c1 - 1@ spaces of leading whitespace that the parser will see as
-- being too deeply indented relative to the now-column-1 header. We
-- strip exactly that many columns from each subsequent line. We also
-- ensure a trailing newline, which @parseStmt@'s @single_input@
-- grammar requires.
reparseStatement
  :: SourceSpan
  -> Text
  -> Either String (MPythonTermAnn (Maybe SourceSpan) PFull.StatementL)
reparseStatement (SourceSpan (SourcePos _ _ c1) _) t =
  case runParser Python.parseStmt (ensureTrailingNewline (dedentBody (c1 - 1) t)) of
    Left err       -> Left err
    Right ([s], _) -> Right $ PCommon.translate $ PFull.translateStatement $ fmap toSourceSpan s
    Right (ss, _)  -> Left $ "expected exactly one statement, got " ++ show (length ss)

-- | Drop up to @n@ leading-space characters from every line after the
-- first. The first line is left untouched — the slicer already trimmed
-- it to column 1.
dedentBody :: Int -> Text -> Text
dedentBody n t
  | n <= 0    = t
  | otherwise = case T.lines t of
      []         -> t
      (h : rest) ->
        let strip line = T.drop (min n (T.length (T.takeWhile (== ' ') line))) line
            rejoined = T.intercalate (T.singleton '\n') (h : map strip rest)
        in if T.isSuffixOf (T.singleton '\n') t
             then rejoined <> T.singleton '\n'
             else rejoined

-- | Python's @parseStmt@ requires its input to terminate with a
-- newline (@single_input@: @… NEWLINE@). The slicer ends spans on the
-- last consumed character, so a sliced statement won't contain one.
ensureTrailingNewline :: Text -> Text
ensureTrailingNewline t
  | T.isSuffixOf (T.singleton '\n') t = t
  | otherwise                          = t <> T.singleton '\n'

reparseExpression :: Text -> Either String (MPythonTermAnn (Maybe SourceSpan) PFull.ExprL)
reparseExpression = fmap (PCommon.translate . PFull.translateExpression . fmap toSourceSpan . fst)
                  . runParser Python.parseExpr

runParser
  :: (String -> FilePath -> Either Python.ParseError a)
  -> Text
  -> Either String a
runParser p t = case p (T.unpack t) "<slice>" of
  Left  err -> Left $ "parser error: " ++ show err
  Right v   -> Right v

-- | Same converter that 'parsePython' uses.
toSourceSpan :: Python.SrcSpan -> Maybe SourceSpan
toSourceSpan (Python.SpanCoLinear filename row start end) =
  Just $ mkSourceSpan filename (row, start) (row, end)
toSourceSpan (Python.SpanMultiLine filename startRow startCol endRow endCol) =
  Just $ mkSourceSpan filename (startRow, startCol) (endRow, endCol)
toSourceSpan (Python.SpanPoint filename row col) =
  Just $ mkSourceSpan filename (row, col) (row, col)
toSourceSpan Python.SpanEmpty = Nothing
