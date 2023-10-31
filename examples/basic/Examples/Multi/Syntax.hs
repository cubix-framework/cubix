{-# LANGUAGE TemplateHaskell #-}

-- Example modified from the examples directory of compdata

module Examples.Multi.Syntax where

import Data.Comp.Multi

import Cubix.Language.Info
import Cubix.Language.Parametric.Derive

----------------------------------------------------------

data ExpL

-- Signature for values and operators
data Value a i where
  Const ::              Int -> Value a ExpL
  Pair  :: a ExpL -> a ExpL -> Value a ExpL

data Op a i where
  Add, Mult :: a ExpL -> a ExpL -> Op a ExpL
  Fst       ::           a ExpL -> Op a ExpL
  Snd       ::           a ExpL -> Op a ExpL

deriveAll [''Op, ''Value]

-- Signature for the simple expression language
type Sig = '[Op, Value]

type Prog = Term Sig
type ProgLab = TermLab Sig