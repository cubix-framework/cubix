{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs,
  ConstraintKinds, TypeApplications, DataKinds #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Examples.Multi.Eval
-- Copyright   :  (c) 2011 Patrick Bahr, Tom Hvitved
-- License     :  BSD3
-- Maintainer  :  Tom Hvitved <hvitved@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Expression Evaluation
--
-- The example illustrates how to use generalised compositional data types 
-- to implement a small expression language, with a sub language of values, and 
-- an evaluation function mapping expressions to values.
--
--------------------------------------------------------------------------------

module Examples.Multi.Eval where

import Data.Comp.Multi.Ops ( (:-<:) )
import Data.Comp.Multi
import Data.Comp.Multi.Derive
import Examples.Multi.Common
import Data.Comp.Multi.HFunctor ( (:->) )

-- Term evaluation algebra: forall i. f (Term '[Value]) i -> Term '[Value] i  
-- Question: Is this a good fix for this test? We need `Eval` to be a typeclass with a single argument of kind (* -> *) -> * -> *.
class Eval f where
  evalAlg :: Alg f (Term '[Value])

$(derive [liftSum] [''Eval])

-- Lift the evaluation algebra to a catamorphism
eval :: Term Sig :-> Term '[Value]
eval = cata evalAlg

instance Eval Value where
  evalAlg = inject

instance Eval Op where
  evalAlg (Add x y)  = jConst $ projC x + projC y
  evalAlg (Mult x y) = jConst $ projC x * projC y
  evalAlg (Fst x)    = fst $ projP x
  evalAlg (Snd x)    = snd $ projP x

projC :: (Value :-<: fs) => Term fs Int -> Int
projC v = case project v of Just (Const n) -> n

projP :: (Value :-<: fs) => Term fs (s, t) -> (Term fs s, Term fs t)
projP v = case project v of Just (Pair x y) -> (x,y)

-- Example: evalEx = iConst 2
evalEx :: Term '[Value] Int
evalEx = eval (jFst $ jPair (jConst 2) (jConst 1) :: Term Sig Int)
