-- | JavaScript plug-in for the language-agnostic source-position spec.
--
-- All the property logic lives in
-- "Cubix.Language.Parametric.SourcePos.Test"; this module supplies the
-- JavaScript-specific bits: snippets to exercise, how to parse a file,
-- and how to enumerate parser-able subterms (JSStatement, JSExpression)
-- along with the reparser chain
-- @parseProgram\/parseExpression → Full.translateStatement\/translateExpression → Full.annotateWithSpans → Common.translate@.
--
-- See 'Cubix.Language.JavaScript.Parametric.Full.Trans.annotateWithSpans'
-- for how 'Maybe SourceSpan' annotations are derived from
-- @language-javascript@'s 'TokenPosn' nodes, and why we collapse
-- @JSAnnot@ subterms before slice comparisons.

module Cubix.Language.JavaScript.SourcePosSpec (spec) where

import Control.Monad                         (unless, when)
import Data.List                             (sort)
import Data.Maybe                            (mapMaybe)
import Data.Text                             (Text)
import Data.Text qualified                   as T
import System.Directory                      (doesDirectoryExist, listDirectory)
import System.FilePath                       ((</>), takeExtension)
import Test.Hspec

import Language.JavaScript.Parser qualified            as JS
import Language.JavaScript.Parser.Grammar5 qualified   as JS
import Language.JavaScript.Parser.Parser qualified     as JS (parseUsing)

import Data.Comp.Multi                       (E (..))
import Data.Comp.Multi.Generic               qualified as Generic
import Data.Comp.Multi.Strategy.Classification (dynProj, isSort)

import Cubix.Language.Info                   (SourceSpan)
import Cubix.Language.JavaScript.Parametric.Common (MJSSig, MJSTermAnn)
import Cubix.Language.JavaScript.Parametric.Common qualified as JSCommon
import Cubix.Language.JavaScript.Parametric.Full qualified as JSFull
import Cubix.ParsePretty                     (parseFileTrackSources)
import Cubix.Sin.Compdata.Annotation         (getAnn)

import Cubix.Language.Parametric.SourcePos.Test

spec :: Spec
spec = do
  testFiles <- runIO discoverInputFiles
  sourcePosSpec (jsKit testFiles)

jsKit :: [FilePath] -> SourcePosKit MJSSig JSFull.JSASTL
jsKit testFiles = SourcePosKit
  { kitLanguageName   = "JavaScript"
  , kitSnippets       = jsSnippets
  , kitTestFiles      = testFiles
  , kitParseFile      = parseFileTrackSources @MJSSig
  , kitReParseTargets = jsReParseTargets
    -- JavaScript parsing is context-free; reparse mismatches indicate
    -- real problems and should fail loudly.
  , kitFileReparseIsBestEffort = False
  }

-- | Discover the @.js@ files in @input-files/javascript/@ (including
-- the @ipt/@ subdirectory).
discoverInputFiles :: IO [FilePath]
discoverInputFiles = (++) <$> listJsFiles "input-files/javascript"
                          <*> listJsFiles "input-files/javascript/ipt"

listJsFiles :: FilePath -> IO [FilePath]
listJsFiles dir = do
  exists <- doesDirectoryExist dir
  unless exists $
    error $ "JavaScript test corpus directory missing — expected to find " ++ dir
  entries <- listDirectory dir
  let files = sort [ dir </> e | e <- entries, takeExtension e == ".js" ]
  when (null files) $
    error $ "JavaScript test corpus directory is empty — no .js files in " ++ dir
  pure files

jsSnippets :: [(String, String)]
jsSnippets =
  [ ("assign-and-binop", "var x = 42;\nvar y = x + 1;\n")
  , ("if-else",          "if (x) { y = 1; } else { y = 2; }\n")
  , ("while-loop",       "while (i < 10) { i = i + 1; }\n")
  , ("function-decl",    "function f(a, b) { return a + b; }\n")
  , ("method-call",      "x.foo(1, 2);\n")
  , ("nested-array",     "var t = [1, 2, [3, 4]];\n")
  , ("postfix-inc",      "i++;\n")
  , ("leading-space",    "\n  var x = 1;\n")
  , ("string-literal",   "var s = \"hello\";\n")
  ]

-- | Pick out subterms at sorts that @language-javascript@ can parse
-- standalone (JSAST, JSStatement, JSExpression).
jsReParseTargets
  :: MJSTermAnn (Maybe SourceSpan) l
  -> [ReParseTarget MJSSig]
jsReParseTargets root = mapMaybe targetOf (Generic.subterms root)
  where
    targetOf
      :: E (MJSTermAnn (Maybe SourceSpan)) -> Maybe (ReParseTarget MJSSig)
    targetOf (E sub)
      | Just sp <- getAnn sub, isDegenerateSpan sp = Nothing
      | Just sp <- getAnn sub
      , isSort @JSFull.JSASTL sub
      , Just sub' <- dynProj @_ @JSFull.JSASTL sub = Just (RPT sp reparseProgram sub')
      | Just sp <- getAnn sub
      , isSort @JSFull.JSStatementL sub
      , Just sub' <- dynProj @_ @JSFull.JSStatementL sub  = Just (RPT sp reparseStatement sub')
      | Just sp <- getAnn sub
      , isSort @JSFull.JSExpressionL sub
      , Just sub' <- dynProj @_ @JSFull.JSExpressionL sub = Just (RPT sp reparseExpression sub')
      | otherwise = Nothing

reparseProgram :: Text -> Either String (MJSTermAnn (Maybe SourceSpan) JSFull.JSASTL)
reparseProgram t = case JS.parse (T.unpack t) "<slice>" of
  Left err  -> Left $ "parser error: " ++ err
  Right ast -> Right $ JSCommon.translate
                     $ JSFull.annotateWithSpans
                     $ JSFull.translate ast

-- @parseStatement@ implements ASI at the end of input, which causes it
-- to reject trailing @;@ tokens that are valid as the statement's own
-- terminator. @parseProgram@ doesn't have that quirk, so we use it
-- here and require the result to be exactly one statement.
reparseStatement :: Text -> Either String (MJSTermAnn (Maybe SourceSpan) JSFull.JSStatementL)
reparseStatement t = case JS.parse (T.unpack t) "<slice>" of
  Left err  -> Left $ "parser error: " ++ err
  Right (JS.JSAstProgram [s] _) ->
    Right $ JSCommon.translate
          $ JSFull.annotateWithSpans
          $ JSFull.translateStatement s
  Right (JS.JSAstProgram ss _) ->
    Left $ "expected exactly one statement, got " ++ show (length ss)
  Right other -> Left $ "expected JSAstProgram, got " ++ show other

reparseExpression :: Text -> Either String (MJSTermAnn (Maybe SourceSpan) JSFull.JSExpressionL)
reparseExpression t = case JS.parseUsing JS.parseExpression (T.unpack t) "<slice>" of
  Left err  -> Left $ "parser error: " ++ err
  Right (JS.JSAstExpression e _) ->
    Right $ JSCommon.translate
          $ JSFull.annotateWithSpans
          $ JSFull.translateExpression e
  Right other -> Left $ "expected JSAstExpression, got " ++ show other
