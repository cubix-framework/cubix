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
  checkParallel $ Group "cfg-fast-check" $ [
        ("unit_java_cfg_zap", unit_java_cfg "input-files/java/Baz.java")
        ]
{-
  checkParallel $ Group "cfg-unit-tests" $ [
        ("unit_lua_cfg_foo", unit_lua_cfg "input-files/lua/Foo.lua")
      , ("unit_lua_cfg_bar", unit_lua_cfg "input-files/lua/Bar.lua")
      , ("unit_java_cfg_foo", unit_java_cfg "input-files/java/Foo.java")
      , ("unit_java_cfg_bar", unit_java_cfg "input-files/java/Bar.java")  
      , ("unit_c_cfg_foo", unit_c_cfg "input-files/c/Foo.c")
      , ("unit_c_cfg_bar", unit_c_cfg "input-files/c/Bar.c")
      , ("unit_c_cfg_baz", unit_c_cfg "input-files/c/baz.c")
      , ("unit_c_cfg_thud", unit_c_cfg "input-files/c/thud.c")
      , ("unit_c_cfg_variadic", unit_c_cfg "input-files/c/variadic.c")
      , ("unit_js_cfg_foo", unit_js_cfg "input-files/javascript/Foo.js")
      , ("unit_js_cfg_bar", unit_js_cfg "input-files/javascript/Bar.js")
      , ("unit_py_cfg_foo", unit_python_cfg "input-files/python/Foo.py")
      , ("unit_py_cfg_bar", unit_python_cfg "input-files/python/Bar.py")
      , ("unit_py_cfg_baz", unit_python_cfg "input-files/python/Baz.py")
      ]
-}{-  
  checkParallel $ Group "cfg-integration-tests" $ [
        ("integration_lua_cfg_goto", integration_lua_cfg "integration-test-input-files/lua/goto.lua")
      , ("integration_lua_cfg_lExpr", integration_lua_cfg "integration-test-input-files/lua/lExpr.lua")
      , ("integration_lua_cfg_lFunReturn", integration_lua_cfg "integration-test-input-files/lua/lFunReturn.lua")
      , ("integration_lua_cfg_lIf", integration_lua_cfg "integration-test-input-files/lua/lIf.lua")
      , ("integration_lua_cfg_lLamda", integration_lua_cfg "integration-test-input-files/lua/lLamda.lua")
      , ("integration_lua_cfg_lRepUntil", integration_lua_cfg "integration-test-input-files/lua/lRepUntil.lua")
      , ("integration_lua_cfg_lWhile", integration_lua_cfg "integration-test-input-files/lua/lWhile.lua")
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
      , ("integration_c_cfg_duffdevice", integration_c_cfg "integration-test-input-files/c/duffdevice.c")
      , ("integration_c_cfg_nestedConBreak", integration_c_cfg "integration-test-input-files/c/nestedConBreak.c")
      , ("integration_c_cfg_operations", integration_c_cfg "integration-test-input-files/c/operations.c")
      , ("integration_java_cfg_do_while_break", integration_java_cfg "integration-test-input-files/java/do_while_break.java")
      , ("integration_java_cfg_Foo", integration_java_cfg "integration-test-input-files/java/Foo.java")
      , ("integration_java_cfg_for_break", integration_java_cfg "integration-test-input-files/java/for_break.java")
      , ("integration_java_cfg_ifelse", integration_java_cfg "integration-test-input-files/java/ifelse.java")
      , ("integration_java_cfg_jdo_whale", integration_java_cfg "integration-test-input-files/java/jdo_whale.java")
      , ("integration_java_cfg_jfor", integration_java_cfg "integration-test-input-files/java/jfor.java")
      , ("integration_java_cfg_j_lBreak", integration_java_cfg "integration-test-input-files/java/j_lBreak.java")
      , ("integration_java_cfg_jSwitch", integration_java_cfg "integration-test-input-files/java/jSwitch.java")
      , ("integration_java_cfg_jwhale", integration_java_cfg "integration-test-input-files/java/jwhale.java")
      , ("integration_java_cfg_mix1", integration_java_cfg "integration-test-input-files/java/mix1.java")
      , ("integration_java_cfg_while_break", integration_java_cfg "integration-test-input-files/java/while_break.java")
      , ("integration_javascript_cfg_forLabelConBreak", integration_javascript_cfg "integration-test-input-files/javascript/forLabelConBreak.js")
      , ("integration_javascript_cfg_jsDoWhale", integration_javascript_cfg "integration-test-input-files/javascript/jsDoWhale.js")
      , ("integration_javascript_cfg_jsFor", integration_javascript_cfg "integration-test-input-files/javascript/jsFor.js")
      , ("integration_javascript_cfg_jsIF", integration_javascript_cfg "integration-test-input-files/javascript/jsIF.js")
      , ("integration_javascript_cfg_jsLoopConBreak", integration_javascript_cfg "integration-test-input-files/javascript/jsLoopConBreak.js")
      , ("integration_javascript_cfg_jsSwitch", integration_javascript_cfg "integration-test-input-files/javascript/jsSwitch.js")
      , ("integration_javascript_cfg_jsWhale", integration_javascript_cfg "integration-test-input-files/javascript/jsWhale.js")
      , ("integration_javascript_cfg_whileDoLabelConBreak", integration_javascript_cfg "integration-test-input-files/javascript/whileDoLabelConBreak.js")
      , ("integration_javascript_cfg_Foo", integration_javascript_cfg "integration-test-input-files/javascript/Foo.js")
      , ("integration_python_cfg_Foo", integration_python_cfg "integration-test-input-files/python/Foo.py")
      , ("integration_python_cfg_pForConBreak", integration_python_cfg "integration-test-input-files/python/pForConBreak.py")
      , ("integration_python_cfg_pfor", integration_python_cfg "integration-test-input-files/python/pfor.py")
      , ("integration_python_cfg_pif", integration_python_cfg "integration-test-input-files/python/pif.py")
      , ("integration_python_cfg_pInnerFun", integration_python_cfg "integration-test-input-files/python/pInnerFun.py")
      , ("integration_python_cfg_pLambda", integration_python_cfg "integration-test-input-files/python/pLambda.py")
      , ("integration_python_cfg_pRemDup", integration_python_cfg "integration-test-input-files/python/pRemDup.py")
      , ("integration_python_cfg_pwhale", integration_python_cfg "integration-test-input-files/python/pwhale.py")
      , ("integration_python_cfg_pWhileConBreak", integration_python_cfg "integration-test-input-files/python/pWhileConBreak.py")
      ]
-}{-
  let lua_path = "/home/sreenidhi/Work/cubix/cubix/test/lua/lua-5.3.3-tests"
  lua_files <- listDirectory lua_path
  checkParallel $ Group "cfg-unit-lua-tests" $
    map (\fileName -> (fromString fileName, unit_lua_cfg (lua_path </> fileName))) lua_files
-}{-    
  let c_path = "/home/sreenidhi/Work/cubix/gcc/gcc/testsuite/gcc.c-torture/execute/"
  c_files <- listRecursive "c" c_path
  checkParallel $ Group "cfg-unit-c-tests" $
    map (\fileName -> (fromString fileName, unit_c_cfg (c_path </> fileName))) c_files  -}
{-
  let python_path = "/home/sreenidhi/Work/cubix/cpython-made/lib/python3.10"
  python_files <- listRecursive "py" python_path
  checkParallel $ Group "cfg-unit-python-tests" $
    map (\fileName -> (fromString fileName, unit_python_cfg (python_path </> fileName))) (take 10 (drop 50 python_files))
-}
  
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
