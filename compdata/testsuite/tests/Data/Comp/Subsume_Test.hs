{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies #-}

-- | This module exports a dummy test to force type checking of this
-- module. In this module we test the subtyping system.

module Data.Comp.Subsume_Test where

import Data.Comp.Multi.Ops ( Elem )
import Data.Comp.Ops
import Data.Comp.SubsumeCommon


import Test.Framework
import Test.Framework.Providers.QuickCheck2


data S1 a = S1 a
data S2 a = S2 a
data S3 a = S3 a
data S4 a = S4 a

type TA = S1 :+: S2
type TB = S3 :+: S4
type T1 = TA :+: TB
type T2 = TB :+: TA
type T3 = S2 :+: TB

test1 :: ComprEmb (Elem T1 T1) ~ (Found Here) => Int
test1 = 1

test2 :: ComprEmb (Elem T1 T2) ~ (Found (Sum (Ri Here) (Le Here))) => Int
test2 = 1

test3 :: ComprEmb (Elem (T1 :+: S1) T2) ~ Ambiguous => Int
test3 = 1

test4 :: ComprEmb (Elem T1 (T2 :+: S1)) ~ Ambiguous => Int
test4 = 1

test5 :: ComprEmb (Elem T1 T3) ~ NotFound => Int
test5 = 1

test6 :: ComprEmb (Elem TB T1) ~ (Found (Ri Here)) => Int
test6 = 1

test7 :: ComprEmb (Elem T3 T1) ~ (Found (Sum (Le (Ri Here))(Ri Here))) => Int
test7 = 1

main = defaultMain [tests]

tests = testGroup "Subsume" [
         testProperty "prop_typecheck" prop_typecheck
        ]

-- dummy test
prop_typecheck = True
