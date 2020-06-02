{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog

import           Cubix.Language.Lua.Cfg.Test

tests :: IO Bool
tests = do
  checkParallel $ Group "cfg-tests" [
          ("unit_lua_cfg_foo", unit_lua_cfg "input-files/lua/Foo.lua")
        , ("unit_lua_cfg_bar", unit_lua_cfg "input-files/lua/Bar.lua")
        , ("unit_lua_cfg_zap", unit_lua_cfg "input-files/lua/Zap.lua")
        ]

main = tests
