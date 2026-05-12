-- | Lua plug-in for the language-agnostic source-position spec.
--
-- All the property logic lives in
-- "Cubix.Language.Parametric.SourcePos.Test"; this module supplies the
-- Lua-specific bits: snippets to exercise, how to parse a file, and how
-- to enumerate parser-able subterms (Block, Stat, Exp) along with the
-- reparser chain @parseText → Full.translate → Common.translate@.

module Cubix.Language.Lua.SourcePosSpec (spec) where

import Data.List                             (sort)
import Data.Maybe                            (mapMaybe)
import Data.Text                             (Text)
import Data.Text qualified                   as T
import System.Directory                      (doesDirectoryExist, listDirectory)
import System.FilePath                       ((</>), takeExtension)
import Test.Hspec

import Language.Lua.Annotated.Parser qualified as LP

import Data.Comp.Multi                       (E (..))
import Data.Comp.Multi.Generic               qualified as Generic
import Data.Comp.Multi.Strategy.Classification (dynProj, isSort)

import Cubix.Language.Info                   (SourceSpan, mkSourceSpan)
import Cubix.Language.Lua.Parametric.Common  qualified as LCommon
import Cubix.Language.Lua.Parametric.Common.Types
                                             (MLuaSig, MLuaTermAnn)
import Cubix.Language.Lua.Parametric.Full    qualified as LFull
import Cubix.ParsePretty                     (parseFileTrackSources)
import Cubix.Sin.Compdata.Annotation         (getAnn)

import Cubix.Language.Parametric.SourcePos.Test

spec :: Spec
spec = do
  testFiles <- runIO discoverLua53TestSuite
  sourcePosSpec (luaKit testFiles)

luaKit :: [FilePath] -> SourcePosKit MLuaSig LFull.BlockL
luaKit testFiles = SourcePosKit
  { kitLanguageName   = "Lua"
  , kitSnippets       = luaSnippets
  , kitTestFiles      = testFiles
  , kitParseFile      = parseFileTrackSources @MLuaSig
  , kitReParseTargets = luaReParseTargets
  }

-- | The Lua 5.3 reference test suite ships with the repo. We discover
-- the @.lua@ files at spec-construction time so an empty/missing
-- directory degrades to the inline snippets instead of failing.
discoverLua53TestSuite :: IO [FilePath]
discoverLua53TestSuite = do
  let dir = "test/lua/lua-5.3.3-tests"
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      pure $ sort [ dir </> e | e <- entries, takeExtension e == ".lua" ]

luaSnippets :: [(String, String)]
luaSnippets =
  [ ("local-and-binop", "local x = 42\nlocal y = x + 1\n")
  , ("if-elseif-else",  "if x then return 1 elseif y then return 2 else return 3 end\n")
  , ("while-loop",      "while i < 10 do i = i + 1 end\n")
  , ("function-decl",   "function f(a, b) return a + b end\n")
  , ("method-call",     "x:foo(1, 2)\n")
  , ("nested-table",    "local t = { 1, 2, { 3, 4 } }\n")
  , ("string-literal",  "local s = \"hello\"\n")
  ]

-- | Pick out subterms at sorts that @language-lua@ can parse standalone
-- (Block, Stat, Exp) and package each with its reparser.
luaReParseTargets
  :: MLuaTermAnn (Maybe SourceSpan) l
  -> [ReParseTarget MLuaSig]
luaReParseTargets root = mapMaybe targetOf (Generic.subterms root)
  where
    targetOf
      :: E (MLuaTermAnn (Maybe SourceSpan)) -> Maybe (ReParseTarget MLuaSig)
    targetOf (E sub)
      | Just sp <- getAnn sub, isDegenerateSpan sp = Nothing
      | Just sp <- getAnn sub
      , isSort @LFull.BlockL sub
      , Just sub' <- dynProj @_ @LFull.BlockL sub = Just (RPT sp reparseBlock sub')
      | Just sp <- getAnn sub
      , isSort @LFull.StatL sub
      , Just sub' <- dynProj @_ @LFull.StatL sub  = Just (RPT sp reparseStat  sub')
      | Just sp <- getAnn sub
      , isSort @LFull.ExpL sub
      , Just sub' <- dynProj @_ @LFull.ExpL sub   = Just (RPT sp reparseExp   sub')
      | otherwise = Nothing

reparseBlock :: Text -> Either String (MLuaTermAnn (Maybe SourceSpan) LFull.BlockL)
reparseBlock = fmap (LCommon.translate . LFull.translate     . fmap toSourceSpan)
             . runParser LP.chunk

reparseStat :: Text -> Either String (MLuaTermAnn (Maybe SourceSpan) LFull.StatL)
reparseStat  = fmap (LCommon.translate . LFull.translateStat . fmap toSourceSpan)
             . runParser LP.stat

reparseExp :: Text -> Either String (MLuaTermAnn (Maybe SourceSpan) LFull.ExpL)
reparseExp   = fmap (LCommon.translate . LFull.translateExp  . fmap toSourceSpan)
             . runParser LP.exp

-- | 'language-lua' applies a shebang-stripping pass to /any/ input that
-- starts with @#@, including standalone reparse of an expression like
-- @#x@ (length-of-x), which collapses the whole input to nothing and
-- yields \"unexpected end of file\". Prepending a space sidesteps the
-- check — the parse tree is identical modulo source positions, and we
-- compare with 'stripA' anyway.
--
-- See LexerUtils.hs:dropSpecialComment in language-lua.
runParser :: LP.Parser a -> Text -> Either String a
runParser p t = case LP.parseText p (T.cons ' ' t) of
  Left  (_, msg) -> Left $ "parser error: " ++ msg
  Right v        -> Right v

-- | Same converter that 'parseLuaTrackSources' uses.
toSourceSpan :: LP.SourceRange -> Maybe SourceSpan
toSourceSpan x = Just $ mkSourceSpan
  (T.unpack (LP.sourceFile (LP.sourceFrom x)))
  (LP.sourceLine (LP.sourceFrom x), LP.sourceColumn (LP.sourceFrom x))
  (LP.sourceLine (LP.sourceTo   x), LP.sourceColumn (LP.sourceTo   x))
