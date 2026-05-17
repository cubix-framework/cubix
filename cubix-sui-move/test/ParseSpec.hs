module ParseSpec (spec) where

import System.IO.Temp (writeSystemTempFile)
import Test.Hspec (Spec, describe, it, shouldReturn)

import TreeSitter.SuiMove (tree_sitter_sui_move)

import Cubix.Language.SuiMove.RawParse qualified as RawParse

spec :: Spec
spec = describe "Sui Move parser" $
  it "rejects recovered tree-sitter syntax-error trees" $ do
    path <- writeSystemTempFile "RecoveredError.move" $
      "module 0x1::M { fun f() { let x = ; } }\n"
    RawParse.parse path tree_sitter_sui_move `shouldReturn` Nothing
