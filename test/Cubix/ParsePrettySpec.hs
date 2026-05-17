module Cubix.ParsePrettySpec (spec) where

import Data.Comp.Multi                       (E (..), project)
import Data.Comp.Multi.Generic               qualified as Generic
import Data.Comp.Multi.Strategy.Classification (dynProj, isSort)
import Data.Text                             qualified as T
import Data.Text.IO                          qualified as T
import System.IO.Temp (writeSystemTempFile)
import Test.Hspec

import Cubix.Language.Info                   (SourceSpan)
import Cubix.Language.JavaScript.Parametric.Common (MJSSig, MJSTermAnn)
import Cubix.Language.JavaScript.Parametric.Full qualified as JSFull
import Cubix.Language.Parametric.SourcePos.Test (sliceSpan)
import Cubix.ParsePretty (parseFileTrackSources, parseJavaScript)
import Cubix.Sin.Compdata.Annotation         (getAnn)

spec :: Spec
spec =
  describe "parseJavaScript" $ do
    it "normalizes explicit JavaScript semicolons the same as ASI" $ do
      explicitSemi <- parseJavaScript =<< writeSystemTempFile "explicit-semi.js" "i++;\n"
      automaticSemi <- parseJavaScript =<< writeSystemTempFile "automatic-semi.js" "i++\n"
      explicitSemi `shouldBe` automaticSemi

    it "gives JSNoAnnot empty slices without truncating property identifiers" $ do
      path <- writeSystemTempFile "object-literal.js" "({toString: 0});\n"
      Just t <- parseFileTrackSources @MJSSig path
      src <- T.readFile path

      let noAnnSlices = [ sliceSpan src sp
                        | E sub <- Generic.subterms t
                        , Just sp <- [getAnn sub]
                        , isSort @JSFull.JSAnnotL sub
                        , Just sub' <- [dynProj @_ @JSFull.JSAnnotL sub]
                        , Just JSFull.JSNoAnnot <- [project sub']
                        ]

      noAnnSlices `shouldSatisfy` (not . null)
      noAnnSlices `shouldSatisfy` all T.null

      propertySlices t src `shouldContain`
        [("toString", T.pack "toString")]

propertySlices
  :: MJSTermAnn (Maybe SourceSpan) l
  -> T.Text
  -> [(String, T.Text)]
propertySlices root src =
  [ (name, sliceSpan src sp)
  | E sub <- Generic.subterms root
  , Just sp <- [getAnn sub]
  , isSort @JSFull.JSPropertyNameL sub
  , Just sub' <- [dynProj @_ @JSFull.JSPropertyNameL sub]
  , Just prop <- [project sub']
  , name <- case prop of
      JSFull.JSPropertyIdent _ s  -> [s]
      JSFull.JSPropertyString _ s -> [s]
      JSFull.JSPropertyNumber _ s -> [s]
  ]
