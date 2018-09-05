module Data.Comp_Test where

import Test.Framework 

import qualified Data.Comp.Multi_Test
import qualified Data.Comp.Subsume_Test

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

main = defaultMain [tests]

tests = testGroup "Comp" [
         Data.Comp.Multi_Test.tests,
         Data.Comp.Subsume_Test.tests
        ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

