module ParseSpec (spec) where

import Data.List (isInfixOf)
import System.IO.Temp (writeSystemTempFile)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldSatisfy)

import Data.Comp.Multi (stripA)
import TreeSitter.SuiMove (tree_sitter_sui_move)

import Cubix.Language.SuiMove.Pretty qualified as Pretty
import Cubix.Language.SuiMove.RawParse qualified as RawParse

spec :: Spec
spec = describe "Sui Move parser" $ do
  it "rejects recovered tree-sitter syntax-error trees" $ do
    path <- writeSystemTempFile "RecoveredError.move" $
      "module 0x1::M { fun f() { let x = ; } }\n"
    RawParse.parse path tree_sitter_sui_move `shouldReturn` Nothing

  it "preserves type arguments on module function accesses" $ do
    path <- writeSystemTempFile "TypedModuleAccess.move" $
      "module 0x1::M { fun g<T>() {} fun f() { M::g<u64>(); } }\n"
    Just parsed <- RawParse.parse path tree_sitter_sui_move
    Pretty.pretty (stripA parsed) `shouldSatisfy` isInfixOf "M::g<u64>"
