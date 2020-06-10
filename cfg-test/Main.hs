{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog hiding (label)

import           Control.Arrow
import qualified Data.Map as Map

import           Cubix.Language.Info
import           Cubix.Language.Lua.Cfg.Test
import           Cubix.Language.Java.Cfg.Test


tests :: IO Bool
tests = do
  checkParallel $ Group "cfg-tests" [
          ("unit_lua_cfg_foo", unit_lua_cfg "input-files/lua/Foo.lua")
        , ("unit_lua_cfg_bar", unit_lua_cfg "input-files/lua/Bar.lua")
        
        -- , ("integration_lua_cfg_bar", integration_lua_cfg bar_edges "input-files/lua/Bar.lua")
        , ("unit_java_cfg_bar", unit_java_cfg "input-files/java/Bar.java")        
        ]

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

