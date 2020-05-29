{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog

import           Cubix.Language.Lua.Cfg.Test

tests :: IO Bool
tests =
  checkParallel $ Group "cfg-tests" [
      ("unit_lua_cfg", unit_lua_cfg "input-files/lua/Foo.lua")
    ]

main = tests
