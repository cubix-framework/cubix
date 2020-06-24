{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog hiding (label)

import           Control.Arrow
import qualified Data.Map as Map
import           Data.String ( IsString (..))
import           System.Directory
import           System.FilePath

import           Cubix.Language.Info
import           Cubix.Language.Lua.Cfg.Test
import           Cubix.Language.Java.Cfg.Test
import           Cubix.Language.JavaScript.Cfg.Test
import           Cubix.Language.C.Cfg.Test
import           Cubix.Language.Python.Cfg.Test

tests :: IO Bool
tests = do
  
  checkParallel $ Group "cfg-unit-tests" $ [
        ("unit_lua_cfg_foo", unit_lua_cfg "input-files/lua/Foo.lua")
      , ("unit_lua_cfg_bar", unit_lua_cfg "input-files/lua/Bar.lua")
      , ("unit_java_cfg_foo", unit_java_cfg "input-files/java/Foo.java")
      , ("unit_java_cfg_bar", unit_java_cfg "input-files/java/Bar.java")  
      , ("unit_c_cfg_foo", unit_c_cfg "input-files/c/Foo.c")
      , ("unit_c_cfg_bar", unit_c_cfg "input-files/c/Bar.c")      
      , ("unit_js_cfg_foo", unit_js_cfg "input-files/javascript/Foo.js")
      , ("unit_js_cfg_bar", unit_js_cfg "input-files/javascript/Bar.js")
      , ("unit_py_cfg_foo", unit_python_cfg "input-files/python/Foo.py")
      , ("unit_py_cfg_bar", unit_python_cfg "input-files/python/Bar.py")      
      ]

  checkParallel $ Group "cfg-integration-tests" $ [
        ("integration_lua_cfg_goto", integration_lua_cfg "integration-test-input-files/lua/goto.lua")
      , ("integration_c_cfg_Bar2", integration_c_cfg "integration-test-input-files/c/Bar2.c")
      , ("integration_c_cfg_Bar", integration_c_cfg "integration-test-input-files/c/Bar.c")
      , ("integration_c_cfg_do_whale", integration_c_cfg "integration-test-input-files/c/do_whale.c")
      , ("integration_c_cfg_go_ahead", integration_c_cfg "integration-test-input-files/c/go_ahead.c")
      , ("integration_c_cfg_go_to", integration_c_cfg "integration-test-input-files/c/go_to.c")
      , ("integration_c_cfg_iffy", integration_c_cfg "integration-test-input-files/c/iffy.c")
      , ("integration_c_cfg_mix_1", integration_c_cfg "integration-test-input-files/c/mix_1.c")
      , ("integration_c_cfg_mix_2", integration_c_cfg "integration-test-input-files/c/mix_2.c")
      , ("integration_c_cfg_switch", integration_c_cfg "integration-test-input-files/c/switch.c")
      , ("integration_c_cfg_whale", integration_c_cfg "integration-test-input-files/c/whale.c")
      ]
    
                                    
main = tests

listRecursive :: String -> FilePath -> IO [FilePath]
listRecursive ext fp = go fp

  where go0 cur v = do
          isDir <- doesDirectoryExist (fp </> cur </> v)
          case isDir of
            True -> go (cur </> v)
            False | takeExtension v == dotExt -> pure [cur </> v]
                  | otherwise                -> pure []

        go cur = do
          vs <- listDirectory (fp </> cur)
          concat <$> mapM (go0 cur) vs

        dotExt = "." <> ext
