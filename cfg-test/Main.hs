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

tests :: IO Bool
tests = do
  let lua_path = "/home/sreenidhi/Work/cubix/cubix/test/lua/lua-5.3.3-tests" -- "/home/sreenidhi/Work/cubix/lua_files"
      java_path = "/home/sreenidhi/Work/cubix/java-semantics/tests"
  lua_files <- listDirectory lua_path
  java_files <- listRecursive java_path
  checkParallel $ Group "cfg-tests" $ [
        --   ("unit_lua_cfg_foo", unit_lua_cfg "input-files/lua/Foo.lua")
        -- , ("unit_lua_cfg_bar", unit_lua_cfg "input-files/lua/Bar.lua")
        -- , ("unit_lua_cfg_zap", unit_lua_cfg "input-files/lua/Zap.lua")
        
        -- , ("integration_lua_cfg_bar", integration_lua_cfg bar_edges "input-files/lua/Bar.lua")
        ("unit_java_cfg_bar", unit_java_cfg "input-files/java/Baz.java")        
        -- ("unit_js_cfg_baz", unit_js_cfg "input-files/javascript/Zap.js")        
        ] ++ (map (\fileName -> (fromString fileName, unit_lua_cfg (lua_path </> fileName))) lua_files)
          ++ (map (\fileName -> (fromString fileName, unit_java_cfg (java_path </> fileName))) java_files)
        
                                    

main = tests

bar_edges :: Map.Map Int Int
bar_edges =
  Map.fromList
  [
   (0,2),
   (2,7),
   (3,5),
   (4,51),
   (5,6),
   (6,4),
   (6,7),
   (7,9),
   (8,3),
   (9,11),
   (10,25),
   (11,13),
   (12,10),
   (13,14),
   (14,15),
   (15,17),
   (16,12),
   (17,19),
   (18,23),
   (19,21),
   (20,18),
   (21,22),
   (22,20),
   (23,24),
   (24,16),
   (25,27),
   (26,8),
   (27,28),
   (28,29),
   (29,31),
   (30,26),
   (31,33),
   (32,49),
   (33,35),
   (34,32),
   (35,37),
   (36,39),
   (37,38),
   (38,36),
   (39,41),
   (40,34),
   (41,43),
   (42,40),
   (43,45),
   (44,42),
   (45,47),
   (46,44),
   (47,48),
   (48,46),
   (49,50),
   (50,30),
   (51,52),
   (52,1)
  ]

listRecursive :: FilePath -> IO [FilePath]
listRecursive fp = go fp

  where go0 cur v = do
          isDir <- doesDirectoryExist (fp </> cur </> v)
          case isDir of
            True -> go (cur </> v)
            False | takeExtension v == ".java" -> pure [cur </> v]
                  | otherwise                 -> pure []

        go cur = do
          vs <- listDirectory (fp </> cur)
          concat <$> mapM (go0 cur) vs
