-- | C plug-in for the language-agnostic source-position spec.
--
-- All the property logic lives in
-- "Cubix.Language.Parametric.SourcePos.Test"; this module supplies the
-- C-specific bits: snippets to exercise, how to parse a file, and how
-- to enumerate parser-able subterms (translation units, external
-- declarations, statements, expressions) along with the reparser
-- chain @execParser_ → Full.translate → Common.translate@.

module Cubix.Language.C.SourcePosSpec (spec) where

import Data.Maybe                            (mapMaybe)
import Data.Text                             (Text)
import Data.Text qualified                   as T
import Test.Hspec

import Language.C qualified                  as C
import Language.C.Parser qualified           as C ( translUnitP, extDeclP, statementP, expressionP, execParser_, P )

import Data.Comp.Multi                       (E (..))
import Data.Comp.Multi.Generic               qualified as Generic
import Data.Comp.Multi.Strategy.Classification (dynProj, isSort)

import Cubix.Language.Info                   (SourceSpan, mkSourceSpan)
import Cubix.Language.C.Parametric.Common    (MCSig, MCTermAnn)
import Cubix.Language.C.Parametric.Common    qualified as CCommon
import Cubix.Language.C.Parametric.Full      qualified as CFull
import Cubix.ParsePretty                     (parseFileTrackSources)
import Cubix.Sin.Compdata.Annotation         (getAnn)

import Cubix.Language.Parametric.SourcePos.Test

spec :: Spec
spec = sourcePosSpec cKit

cKit :: SourcePosKit MCSig CFull.CTranslationUnitL
cKit = SourcePosKit
  { kitLanguageName   = "C"
  , kitSnippets       = cSnippets
  , kitTestFiles      = []  -- TODO: a real corpus, once preprocessor-free files are in place
  , kitParseFile      = parseFileTrackSources @MCSig
  , kitReParseTargets = cReParseTargets
  }

cSnippets :: [(String, String)]
cSnippets =
  [ ("simple-decl",     "int x;\n")
  , ("two-decls",       "int x;\nint y;\n")
  , ("function",        "int add(int a, int b) { return a + b; }\n")
  , ("if-else",         "int f(int x) { if (x > 0) return 1; else return 0; }\n")
  , ("while-loop",      "int g(int n) { int i = 0; while (i < n) i = i + 1; return i; }\n")
  , ("for-loop",        "int s(int n) { int total = 0; for (int i = 0; i < n; i = i + 1) total = total + i; return total; }\n")
  , ("nested-call",     "int h(int x) { return add(mul(x, 2), 1); }\n")
  ]

-- | Pick out subterms at sorts that @language-c@ can parse standalone
-- and package each with its reparser.
cReParseTargets
  :: MCTermAnn (Maybe SourceSpan) l
  -> [ReParseTarget MCSig]
cReParseTargets root = mapMaybe targetOf (Generic.subterms root)
  where
    targetOf
      :: E (MCTermAnn (Maybe SourceSpan)) -> Maybe (ReParseTarget MCSig)
    targetOf (E sub)
      | Just sp <- getAnn sub, isDegenerateSpan sp = Nothing
      | Just sp <- getAnn sub
      , isSort @CFull.CTranslationUnitL sub
      , Just sub' <- dynProj @_ @CFull.CTranslationUnitL sub = Just (RPT sp reparseTranslUnit sub')
      | Just sp <- getAnn sub
      , isSort @CFull.CExternalDeclarationL sub
      , Just sub' <- dynProj @_ @CFull.CExternalDeclarationL sub = Just (RPT sp reparseExtDecl sub')
      | Just sp <- getAnn sub
      , isSort @CFull.CStatementL sub
      , Just sub' <- dynProj @_ @CFull.CStatementL sub  = Just (RPT sp reparseStat sub')
      | Just sp <- getAnn sub
      , isSort @CFull.CExpressionL sub
      , Just sub' <- dynProj @_ @CFull.CExpressionL sub = Just (RPT sp reparseExp sub')
      | otherwise = Nothing

reparseTranslUnit :: Text -> Either String (MCTermAnn (Maybe SourceSpan) CFull.CTranslationUnitL)
reparseTranslUnit = fmap (CCommon.translate . CFull.translate . fmap nodeInfoToSpan)
                  . runParser C.translUnitP

reparseExtDecl :: Text -> Either String (MCTermAnn (Maybe SourceSpan) CFull.CExternalDeclarationL)
reparseExtDecl = fmap (CCommon.translate . CFull.translateExtDecl . fmap nodeInfoToSpan)
               . runParser C.extDeclP

reparseStat :: Text -> Either String (MCTermAnn (Maybe SourceSpan) CFull.CStatementL)
reparseStat = fmap (CCommon.translate . CFull.translateStatement . fmap nodeInfoToSpan)
            . runParser C.statementP

reparseExp :: Text -> Either String (MCTermAnn (Maybe SourceSpan) CFull.CExpressionL)
reparseExp = fmap (CCommon.translate . CFull.translateExpression . fmap nodeInfoToSpan)
           . runParser C.expressionP

runParser :: C.P a -> Text -> Either String a
runParser p t = case C.execParser_ p (C.inputStreamFromString (T.unpack t)) (C.initPos "<slice>") of
  Left  err -> Left $ "parser error: " ++ show err
  Right v   -> Right v

-- | Slice-side equivalent of 'Cubix.ParsePretty.nodeInfoToSpan' — but
-- the slice text is short and we have no good source to walk for
-- @lengthOfNode@, so we collapse to a start-only span. The reparse
-- comparison uses 'stripA' anyway, so spans on the reparsed tree
-- aren't compared structurally.
nodeInfoToSpan :: C.NodeInfo -> Maybe SourceSpan
nodeInfoToSpan ni =
  let p = C.posOfNode ni
  in if C.isSourcePos p
       then Just $ mkSourceSpan (C.posFile p)
                                (C.posRow p, C.posColumn p)
                                (C.posRow p, C.posColumn p)
       else Nothing
