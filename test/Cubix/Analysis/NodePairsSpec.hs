module Cubix.Analysis.NodePairsSpec (spec) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.IO.Temp (writeSystemTempFile)

import Test.Hspec

import Cubix.Analysis.NodePairs
import Cubix.ParsePretty (parseLua)

spec :: Spec
spec = describe "countNodePairs" $ do
  it "empty file" $ do
    path <- writeSystemTempFile "test.lua" ""
    mterm <- parseLua path
    case mterm of
      Nothing -> expectationFailure "Failed to parse empty file"
      Just term -> countNodePairs term `shouldBe` emptyFilePairs

  it "return 1" $ do
    path <- writeSystemTempFile "test.lua" "return 1"
    mterm <- parseLua path
    case mterm of
      Nothing -> expectationFailure "Failed to parse"
      Just term -> countNodePairs term `shouldBe` return1Pairs

  it "local x = 1" $ do
    path <- writeSystemTempFile "test.lua" "local x = 1"
    mterm <- parseLua path
    case mterm of
      Nothing -> expectationFailure "Failed to parse"
      Just term -> countNodePairs term `shouldBe` localX1Pairs

-- With pass-through functors, ListF/MaybeF/PairF are transparent.
-- Children are counted as direct children of the grandparent.
-- Parent and child names are constructor names, not fragment names.

-- Module prefixes for readability
luaC, luaF, par :: String
luaC = "Cubix.Language.Lua.Parametric.Common.Types."
luaF = "Cubix.Language.Lua.Parametric.Full.Types."
par  = "Cubix.Language.Parametric.Syntax.VarDecl."

emptyFilePairs :: Map NodePair Int
emptyFilePairs = Map.fromList
  [ (NodePair (luaC ++ "BlockIsBlock")  (par ++ "Block"), 1)
  , (NodePair (par ++ "Block")          (luaC ++ "LuaBlockEnd"), 1)
  ]

return1Pairs :: Map NodePair Int
return1Pairs = Map.fromList
  [ (NodePair (luaC ++ "BlockIsBlock")  (par ++ "Block"), 1)
  , (NodePair (luaC ++ "LuaBlockEnd")   (luaF ++ "Number"), 1)  -- MaybeF is pass-through
  , (NodePair (luaF ++ "Number")        (luaF ++ "IntNum"), 1)
  , (NodePair (par ++ "Block")          (luaC ++ "LuaBlockEnd"), 1)
  ]

localX1Pairs :: Map NodePair Int
localX1Pairs = Map.fromList
  [ (NodePair (luaC ++ "BlockIsBlock")              (par ++ "Block"), 1)
  , (NodePair (luaC ++ "LuaLocalVarInit")           (luaF ++ "Number"), 1)
  , (NodePair (luaC ++ "SingleLocalVarDeclIsStat")  (par ++ "SingleLocalVarDecl"), 1)
  , (NodePair (luaC ++ "StatIsBlockItem")           (luaC ++ "SingleLocalVarDeclIsStat"), 1)
  , (NodePair (luaF ++ "Number")                    (luaF ++ "IntNum"), 1)
  , (NodePair (par ++ "Block")                      (luaC ++ "LuaBlockEnd"), 1)
  , (NodePair (par ++ "Block")                      (luaC ++ "StatIsBlockItem"), 1)  -- ListF is pass-through
  , (NodePair (par ++ "JustLocalVarInit")           (luaC ++ "LuaLocalVarInit"), 1)
  , (NodePair (par ++ "SingleLocalVarDecl")         (par ++ "EmptyLocalVarDeclAttrs"), 1)
  , (NodePair (par ++ "SingleLocalVarDecl")         (par ++ "JustLocalVarInit"), 1)
  , (NodePair (par ++ "SingleLocalVarDecl")         (par ++ "TupleBinder"), 1)
  , (NodePair (par ++ "TupleBinder")                (par ++ "Ident"), 1)  -- ListF is pass-through
  ]
