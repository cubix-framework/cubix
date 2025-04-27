{-# LANGUAGE TypeOperators #-}
module Data.Comp.Examples.Multi where

import Examples.Multi.Common
import Examples.Multi.Eval as Eval
import Examples.Multi.EvalI as EvalI
import Examples.Multi.EvalM as EvalM

import Data.Comp.Multi

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

tests = testGroup "Generalised Compositional Data Types" [
         testCase "eval" evalTest,
         testCase "evalI" evalITest,
         testCase "evalM" evalMTest
        ]


--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

instance (EqHF f, Eq p) => EqHF (f :&: p) where
    eqHF (v1 :&: p1) (v2 :&: p2) = p1 == p2 && v1 `eqHF` v2

evalTest = Eval.evalEx @=? jConst 2
evalITest = evalIEx @=? 2
evalMTest = evalMEx @=? Just (jConst 5)
