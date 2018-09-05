module Data.Comp.Multi_Test where

import Test.Framework
import qualified Data.Comp.Examples.Multi
import qualified Data.Comp.Multi.Variables_Test

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

main = defaultMain [tests]

tests = testGroup "Multi" [
         Data.Comp.Multi.Variables_Test.tests
       , Data.Comp.Examples.Multi.tests
       ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------