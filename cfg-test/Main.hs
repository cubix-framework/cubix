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
      , ("integration_lua_cfg_lExpr", integration_lua_cfg "integration-test-input-files/lua/lExpr.lua")
      , ("integration_lua_cfg_lFunReturn", integration_lua_cfg "integration-test-input-files/lua/lFunReturn.lua")
      , ("integration_lua_cfg_lIf", integration_lua_cfg "integration-test-input-files/lua/lIf.lua")
      , ("integration_lua_cfg_lLamda", integration_lua_cfg "integration-test-input-files/lua/lLamda.lua")
      , ("integration_lua_cfg_lRepUntil", integration_lua_cfg "integration-test-input-files/lua/lRepUntil.lua")
      , ("integration_lua_cfg_lWhile", integration_lua_cfg "integration-test-input-files/lua/lWhile.lua")
      , ("integration_lua_cfg_lForIn", integration_lua_cfg "integration-test-input-files/lua/lForIn.lua")
      , ("integration_lua_cfg_lLoopBreak", integration_lua_cfg "integration-test-input-files/lua/lLoopBreak.lua")
      , ("integration_lua_cfg_operations", integration_lua_cfg "integration-test-input-files/lua/operations.lua")
      , ("integration_c_cfg_Bar2", integration_c_cfg "integration-test-input-files/c/Bar2.c")
      , ("integration_c_cfg_Bar", integration_c_cfg "integration-test-input-files/c/Bar.c")
      , ("integration_c_cfg_do_while", integration_c_cfg "integration-test-input-files/c/do_while.c")
      , ("integration_c_cfg_go_ahead", integration_c_cfg "integration-test-input-files/c/go_ahead.c")
      , ("integration_c_cfg_go_to", integration_c_cfg "integration-test-input-files/c/go_to.c")
      , ("integration_c_cfg_goto_nested", integration_c_cfg "integration-test-input-files/c/goto_nested.c")
      , ("integration_c_cfg_goto_nested_local_label", integration_c_cfg "integration-test-input-files/c/goto_nested_local_label.c")
      , ("integration_c_cfg_iffy", integration_c_cfg "integration-test-input-files/c/iffy.c")
      , ("integration_c_cfg_mix_1", integration_c_cfg "integration-test-input-files/c/mix_1.c")
      , ("integration_c_cfg_mix_2", integration_c_cfg "integration-test-input-files/c/mix_2.c")
      , ("integration_c_cfg_switch", integration_c_cfg "integration-test-input-files/c/switch.c")
      , ("integration_c_cfg_while", integration_c_cfg "integration-test-input-files/c/while.c")
      , ("integration_c_cfg_duffdevice", integration_c_cfg "integration-test-input-files/c/duffdevice.c")
      , ("integration_c_cfg_nestedConBreak", integration_c_cfg "integration-test-input-files/c/nestedConBreak.c")
      , ("integration_c_cfg_operations", integration_c_cfg "integration-test-input-files/c/operations.c")
      , ("integration_java_cfg_do_while_break", integration_java_cfg "integration-test-input-files/java/do_while_break.java")
      , ("integration_java_cfg_Foo", integration_java_cfg "integration-test-input-files/java/Foo.java")
      , ("integration_java_cfg_for_break", integration_java_cfg "integration-test-input-files/java/for_break.java")
      , ("integration_java_cfg_ifelse", integration_java_cfg "integration-test-input-files/java/ifelse.java")
      , ("integration_java_cfg_jdo_while", integration_java_cfg "integration-test-input-files/java/jdo_while.java")
      , ("integration_java_cfg_jfor", integration_java_cfg "integration-test-input-files/java/jfor.java")
      , ("integration_java_cfg_j_lBreak", integration_java_cfg "integration-test-input-files/java/j_lBreak.java")
      , ("integration_java_cfg_jSwitch", integration_java_cfg "integration-test-input-files/java/jSwitch.java")
      , ("integration_java_cfg_jwhile", integration_java_cfg "integration-test-input-files/java/jwhile.java")
      , ("integration_java_cfg_mix1", integration_java_cfg "integration-test-input-files/java/mix1.java")
      , ("integration_java_cfg_while_break", integration_java_cfg "integration-test-input-files/java/while_break.java")
      , ("integration_java_cfg_jNestedConBreak", integration_java_cfg "integration-test-input-files/java/jNestedConBreak.java")
      , ("integration_java_cfg_jLocalClass", integration_java_cfg "integration-test-input-files/java/jLocalClass.java")
      , ("integration_java_cfg_jTryAndCatchMe", integration_java_cfg "integration-test-input-files/java/jTryAndCatchMe.java")
      , ("integration_java_cfg_operations", integration_java_cfg "integration-test-input-files/java/operations.java")
      , ("integration_javascript_cfg_forLabelConBreak", integration_javascript_cfg "integration-test-input-files/javascript/forLabelConBreak.js")
      , ("integration_javascript_cfg_jsDoWhile", integration_javascript_cfg "integration-test-input-files/javascript/jsDoWhile.js")
      , ("integration_javascript_cfg_jsFor", integration_javascript_cfg "integration-test-input-files/javascript/jsFor.js")
      , ("integration_javascript_cfg_jsIF", integration_javascript_cfg "integration-test-input-files/javascript/jsIF.js")
      , ("integration_javascript_cfg_jsLoopConBreak", integration_javascript_cfg "integration-test-input-files/javascript/jsLoopConBreak.js")
      , ("integration_javascript_cfg_jsSwitch", integration_javascript_cfg "integration-test-input-files/javascript/jsSwitch.js")
      , ("integration_javascript_cfg_jsWhile", integration_javascript_cfg "integration-test-input-files/javascript/jsWhile.js")
      , ("integration_javascript_cfg_whileDoLabelConBreak", integration_javascript_cfg "integration-test-input-files/javascript/whileDoLabelConBreak.js")
      , ("integration_javascript_cfg_Foo", integration_javascript_cfg "integration-test-input-files/javascript/Foo.js")
      , ("integration_javascript_cfg_jsCatchMeIfYouCan", integration_javascript_cfg "integration-test-input-files/javascript/jsCatchMeIfYouCan.js")
      , ("integration_javascript_cfg_jsFunExpr", integration_javascript_cfg "integration-test-input-files/javascript/jsFunExpr.js")
      , ("integration_javascript_cfg_jsNestedConBreak", integration_javascript_cfg "integration-test-input-files/javascript/jsNestedConBreak.js")
      , ("integration_javascript_cfg_operations", integration_javascript_cfg "integration-test-input-files/javascript/operations.js")
      , ("integration_python_cfg_Foo", integration_python_cfg "integration-test-input-files/python/Foo.py")
      , ("integration_python_cfg_pForConBreak", integration_python_cfg "integration-test-input-files/python/pForConBreak.py")
      , ("integration_python_cfg_pfor", integration_python_cfg "integration-test-input-files/python/pfor.py")
      , ("integration_python_cfg_pif", integration_python_cfg "integration-test-input-files/python/pif.py")
      , ("integration_python_cfg_pInnerFun", integration_python_cfg "integration-test-input-files/python/pInnerFun.py")
      , ("integration_python_cfg_pLambda", integration_python_cfg "integration-test-input-files/python/pLambda.py")
      , ("integration_python_cfg_pRemDup", integration_python_cfg "integration-test-input-files/python/pRemDup.py")
      , ("integration_python_cfg_pwhile", integration_python_cfg "integration-test-input-files/python/pwhile.py")
      , ("integration_python_cfg_pWhileConBreak", integration_python_cfg "integration-test-input-files/python/pWhileConBreak.py")
      , ("integration_python_cfg_operations", integration_python_cfg "integration-test-input-files/python/operations.py")
      , ("integration_python_cfg_pAttendYourClasses", integration_python_cfg "integration-test-input-files/python/pAttendYourClasses.py")
      , ("integration_python_cfg_pCatchMeIfYouCan", integration_python_cfg "integration-test-input-files/python/pCatchMeIfYouCan.py")
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
