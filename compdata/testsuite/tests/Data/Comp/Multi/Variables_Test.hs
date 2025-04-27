{-# LANGUAGE DataKinds, TemplateHaskell, TypeSynonymInstances, FlexibleInstances,
MultiParamTypeClasses, TypeOperators, FlexibleContexts , RankNTypes,
GADTs, ScopedTypeVariables, EmptyDataDecls, ConstraintKinds #-}

module Data.Comp.Multi.Variables_Test where


import Data.Comp.Multi.Variables
import Data.Comp.Multi.Derive
import Data.Comp.Multi.Sum
import Data.Comp.Multi.Term
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Show ()

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit



--------------------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------------------

data Var = X | Y | Z deriving (Eq,Ord,Show)


data Ex

type Value f = forall i . Term f i
type Expression fs = Term fs Ex

data Val e i where 
    Abs :: Var -> e Ex -> Val e i
    Var :: Var -> Val e i
    Int :: Int -> Val e i


data Op e i where
    App :: e Ex -> e Ex -> Op e Ex
    Plus :: e Ex -> e Ex -> Op e Ex


data Let e i  where 
             Let :: Var -> e Ex -> e Ex -> Let e Ex

data LetRec e i  where 
             LetRec :: Var -> e Ex -> e Ex -> LetRec e Ex

type Sig = '[Op, Val]

type SigLet = '[Let, Op, Val]

type SigRec = '[LetRec, Op, Val]

$(derive [makeHFunctor, makeHTraversable, makeHFoldable,
          makeEqHF, makeShowHF, smartConstructors]
         [''Op, ''Val, ''Let, ''LetRec])

instance HasVars Var Val where
    isVar (Var v) = Just v
    isVar _       = Nothing
    
    bindsVars (Abs v a) = a |-> Set.singleton v
    bindsVars _         = empty

instance HasVars a Op where

instance HasVars Var Let  where
    bindsVars (Let v _ a) = a |-> Set.singleton v

instance HasVars Var LetRec where
    bindsVars (LetRec v a b) = a |-> vs & b |-> vs
        where vs = Set.singleton v

-- let x = x + 1 in (\y. y + x) z
letExp, letExp' :: Expression SigLet
letExp = jLet X (jVar X `jPlus` jInt 1) (jAbs Y (jVar Y `jPlus` jVar X) `jApp` jVar Z)
letExp' = jLet X (jInt 1 `jPlus` jInt 1) (jAbs Y (jVar Y `jPlus` jVar X) `jApp` jInt 3)

-- letrec x = x + 1 in (\y. y + x) z
recExp, recExp' :: Expression SigRec
recExp = jLetRec X (jVar X `jPlus` jInt 1) (jAbs Y (jVar Y `jPlus` jVar X) `jApp` jVar Z)
recExp' = jLetRec X (jVar X `jPlus` jInt 1) (jAbs Y (jVar Y `jPlus` jVar X) `jApp` jInt 3)

subst :: (Val :-<: f) => Subst (Sum f) Var
subst = Map.fromList [(X, A $ jInt 1), (Y, A $ jInt 2), (Z, A $ jInt 3)]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

case_letFree = variables letExp @=? Set.fromList [Z,X]

case_recFree = variables recExp @=? Set.fromList [Z]

case_letSubst = appSubst s letExp @=? letExp'
    where s = subst :: Subst (Sum SigLet) Var

case_recSubst = appSubst s recExp @=? recExp'
    where s = subst :: Subst (Sum SigRec) Var

--------------------------------------------------------------------------------
-- Test Suites
--------------------------------------------------------------------------------

main = defaultMain [tests]

tests = testGroup "Variables" [
         testCase "case_letFree" case_letFree
        ,testCase "case_recFree" case_recFree
        ,testCase "case_letSubst" case_letSubst
        ,testCase "case_recSubst" case_recSubst
        ]
