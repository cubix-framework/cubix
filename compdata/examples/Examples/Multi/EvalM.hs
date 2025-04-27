{-# LANGUAGE TemplateHaskell, TypeOperators, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs,
  ConstraintKinds, TypeApplications, DataKinds #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Examples.Multi.EvalM
-- Copyright   :  (c) 2011 Patrick Bahr, Tom Hvitved
-- License     :  BSD3
-- Maintainer  :  Tom Hvitved <hvitved@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Monadic Expression Evaluation
--
-- The example illustrates how to use generalised compositional data types to
-- implement a small expression language, with a sub language of values, and a 
-- monadic evaluation function mapping expressions to values.
--
--------------------------------------------------------------------------------

module Examples.Multi.EvalM where

import Data.Comp.Multi.Ops ( (:-<:) )
import Data.Comp.Multi
import Data.Comp.Multi.Derive
import Control.Monad (liftM)
import Examples.Multi.Common

-- Monadic term evaluation algebra
class EvalM f where
  evalAlgM :: AlgM Maybe f (Term '[Value])

$(derive [liftSum] [''EvalM])

evalM :: Term Sig i -> Maybe (Term '[Value] i)
evalM = cataM evalAlgM

instance EvalM Value where
  evalAlgM = return . inject -- default instance

instance EvalM Op where
  evalAlgM (Add x y)  = do n1 <- projC x
                           n2 <- projC y
                           return $ jConst $ n1 + n2
  evalAlgM (Mult x y) = do n1 <- projC x
                           n2 <- projC y
                           return $ jConst $ n1 * n2
  evalAlgM (Fst v)    = liftM fst $ projP v
  evalAlgM (Snd v)    = liftM snd $ projP v

projC :: (Value :-<: fs) => Term fs Int -> Maybe Int
projC v = case project v of
            Just (Const n) -> return n; _ -> Nothing

projP :: (Value :-<: fs) => Term fs (a,b) -> Maybe (Term fs a, Term fs b)
projP v = case project v of
            Just (Pair x y) -> return (x,y); _ -> Nothing

-- Example: evalMEx = Just (iConst 5)
evalMEx :: Maybe (Term '[Value] Int)
evalMEx = evalM (jConst 1 `jAdd` (jConst 2 `jMult` jConst 2) :: Term Sig Int)
