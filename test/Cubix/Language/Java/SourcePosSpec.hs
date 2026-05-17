{-# LANGUAGE TypeApplications #-}

-- | Java plug-in for the source-position spec
-- ("Cubix.Language.Parametric.SourcePos.Test"). Original parse is the
-- tree-sitter-backed 'JParse.parseFile' (annotated, JAR-faithful);
-- reparse is 'language-java' as a structural probe — has multiple
-- known shape bugs vs. javac, hence 'kitFileReparseIsBestEffort'.
-- (As of 2026-05-17.)
module Cubix.Language.Java.SourcePosSpec (spec) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad     (filterM)
import Data.List          (sort)
import Data.Maybe         (mapMaybe)
import Data.Text          (Text)
import Data.Text qualified as T
import System.Directory   (doesDirectoryExist, listDirectory)
import System.FilePath    ((</>), takeExtension)
import System.IO.Unsafe   (unsafePerformIO)
import Test.Hspec

import Language.Java.Lexer  qualified as JL
import Language.Java.Parser qualified as JP
import Text.Parsec          (Parsec)

import Data.Comp.Multi (AnnTerm, E (..), ann)
import Data.Comp.Multi.Generic qualified as Generic
import Data.Comp.Multi.Strategy.Classification (dynProj, isSort)

import Cubix.Language.Info             (SourceSpan)
import Cubix.Language.Java.Parametric.Full
  ( JavaSig
  , CompilationUnitL, BlockL, BlockStmtL, StmtL, ExpL, NameL
  )
import Cubix.Language.Java.Parametric.Full qualified as JFull
import Cubix.Language.Java.Parse qualified as JParse
import Cubix.Sin.Compdata.Annotation   (getAnn)

import Cubix.Language.Parametric.SourcePos.Test

type JTerm = AnnTerm (Maybe SourceSpan) JavaSig

spec :: Spec
spec = do
  testFiles <- runIO discoverJavaCorpus
  sourcePosSpec (javaKit testFiles)

javaKit :: [FilePath] -> SourcePosKit JavaSig CompilationUnitL
javaKit testFiles = SourcePosKit
  { kitLanguageName            = "Java"
  , kitSnippets                = javaSnippets
  , kitTestFiles               = testFiles
  , kitParseFile               = JParse.parseFile
  , kitReParseTargets          = javaReParseTargets
  , kitFileReparseIsBestEffort = True
  }

-- | K-Java subdirectories chosen to avoid modern-Java constructs our
-- converter rejects (records, lambdas, try-with-resources, multi-catch)
-- as of 2026-05-17.
discoverJavaCorpus :: IO [FilePath]
discoverJavaCorpus = do
  let dirs =
        [ "corpora/java-semantics/tests/01_smoke_tests"
        , "corpora/java-semantics/tests/02_literals"
        , "corpora/java-semantics/tests/11_stmt_other"
        , "corpora/java-semantics/tests/17_stmt_break"
        ]
  -- The K-Java corpus is cloned at runtime (see top-level @corpora/@,
  -- which is gitignored). If it's missing, all the per-file specs
  -- vacuously succeed — make that loud rather than silent so a fresh
  -- checkout / CI without the corpus surfaces immediately. Mirrors the
  -- Lua source-pos spec's behaviour for its 5.3 test suite.
  missing <- filterM (fmap not . doesDirectoryExist) dirs
  case missing of
    [] -> fmap concat (traverse listJavaFiles dirs)
    _  -> error $
      "K-Java corpus is missing — Java source-pos spec needs the following directories:\n  "
        <> unlines (map ("  - " <>) missing)
        <> "Clone the corpus into 'corpora/java-semantics/' before running these tests."

listJavaFiles :: FilePath -> IO [FilePath]
listJavaFiles dir = do
  entries <- listDirectory dir
  pure $ sort [ dir </> e | e <- entries, takeExtension e == ".java" ]

-- | Snippets avoid @language-java@ shape bugs as of 2026-05-17:
-- qualified method calls (@a.b.c(...)@) and update-expression
-- statements (@x++;@).
javaSnippets :: [(String, String)]
javaSnippets =
  [ ("class-decl",  "class T {}\n")
  , ("if-else",
       "class T { void f(int x) { if (x > 0) return; else return; } }\n")
  , ("while-loop", "class T { void f(int i) { while (i < 10) { i = i + 1; } } }\n")
  , ("for-loop",   "class T { void f() { for (int i = 0; i < 10; i = i + 1) {} } }\n")
  , ("expressions",
       "class T { int f(int x, int y) { return (x + y) * 2 - 1; } }\n")
  , ("string-lit", "class T { String s = \"hello\"; }\n")
  , ("array-decl", "class T { int[][] m = new int[3][2]; }\n")
  ]

javaReParseTargets
  :: JTerm l -> [ReParseTarget JavaSig]
javaReParseTargets root = mapMaybe targetOf (Generic.subterms root)
  where
    targetOf
      :: E JTerm -> Maybe (ReParseTarget JavaSig)
    targetOf (E sub)
      | Just sp <- getAnn sub, isDegenerateSpan sp = Nothing
      | Just sp <- getAnn sub
      , isSort @CompilationUnitL sub
      , Just sub' <- dynProj @_ @CompilationUnitL sub =
          Just (RPT sp reparseCompilationUnit sub')
      | Just sp <- getAnn sub
      , isSort @BlockL sub
      , Just sub' <- dynProj @_ @BlockL sub =
          Just (RPT sp reparseBlock sub')
      | Just sp <- getAnn sub
      , isSort @BlockStmtL sub
      , Just sub' <- dynProj @_ @BlockStmtL sub =
          Just (RPT sp reparseBlockStmt sub')
      | Just sp <- getAnn sub
      , isSort @StmtL sub
      , Just sub' <- dynProj @_ @StmtL sub =
          Just (RPT sp reparseStmt sub')
      | Just sp <- getAnn sub
      , isSort @ExpL sub
      , Just sub' <- dynProj @_ @ExpL sub =
          Just (RPT sp reparseExp sub')
      | Just sp <- getAnn sub
      , isSort @NameL sub
      , Just sub' <- dynProj @_ @NameL sub =
          Just (RPT sp reparseName sub')
      | otherwise = Nothing

reparseCompilationUnit :: Text -> Either String (JTerm CompilationUnitL)
reparseCompilationUnit =
  fmap (ann Nothing . JFull.translate) . runJavaParser JP.compilationUnit

reparseBlock :: Text -> Either String (JTerm BlockL)
reparseBlock =
  fmap (ann Nothing . JFull.translateBlock) . runJavaParser JP.block

reparseBlockStmt :: Text -> Either String (JTerm BlockStmtL)
reparseBlockStmt =
  fmap (ann Nothing . JFull.translateBlockStmt) . runJavaParser JP.blockStmt

reparseStmt :: Text -> Either String (JTerm StmtL)
reparseStmt =
  fmap (ann Nothing . JFull.translateStmt) . runJavaParser JP.stmt

reparseExp :: Text -> Either String (JTerm ExpL)
reparseExp =
  fmap (ann Nothing . JFull.translateExp) . runJavaParser JP.exp

reparseName :: Text -> Either String (JTerm NameL)
reparseName =
  fmap (ann Nothing . JFull.translateName) . runJavaParser JP.name

-- | @language-java@'s lexer calls 'error' on some malformed slices
-- (as of 2026-05-17); 'try' converts those crashes to ordinary @Left@.
runJavaParser :: Parsec [JL.L JL.Token] () a -> Text -> Either String a
runJavaParser p t = unsafePerformIO $ do
  r <- try @SomeException (evaluate (JP.parser p (T.unpack t)))
  pure $ case r of
    Left  e         -> Left ("parser/lexer crashed: " ++ show e)
    Right (Left e)  -> Left (show e)
    Right (Right v) -> Right v
