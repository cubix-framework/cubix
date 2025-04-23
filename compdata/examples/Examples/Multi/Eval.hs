{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs,
  ConstraintKinds, TypeApplications #-}
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

import Data.Comp.Multi
import Data.Comp.Multi.Derive
import Examples.Multi.Common

-- Term evaluation algebra
class Eval f v where
  evalAlg :: Alg f (Term v)
{-  The API has changed and the derivation below no longer works.

$(derive [liftSum] [''Eval])

-- Lift the evaluation algebra to a catamorphism
eval :: (HFunctor f, Eval f v) => Term f :-> Term v
eval = cata evalAlg
instance (f :<: v) => Eval f v where
  evalAlg = inject -- default instance

instance (Value :<: v) => Eval Op v where
  evalAlg (Add x y)  = iConst $ projC x + projC y
  evalAlg (Mult x y) = iConst $ projC x * projC y
  evalAlg (Fst x)    = fst $ projP x
  evalAlg (Snd x)    = snd $ projP x

projC :: (Value :<: v) => Term v Int -> Int
projC v = case project v of Just (Const n) -> n

projP :: (Value :<: v) => Term v (s,t) -> (Term v s, Term v t)
projP v = case project v of Just (Pair x y) -> (x,y)

-- Example: evalEx = iConst 2
evalEx :: Term Value Int
evalEx = eval (iFst $ iPair (iConst 2) (iConst 1) :: Term Sig Int)
-}