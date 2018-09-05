{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances,
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
type Expression f = Term f Ex

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

type Sig = Op :+: Val

type SigLet = Let :+: Sig

type SigRec = LetRec :+: Sig

$(derive [makeHFunctor, makeHTraversable, makeHFoldable,
          makeEqHF, makeShowHF, smartConstructors]
         [''Op, ''Val, ''Let, ''LetRec])

instance HasVars Val Var where
    isVar (Var v) = Just v
    isVar _       = Nothing
    
    bindsVars (Abs v a) = a |-> Set.singleton v
    bindsVars _         = empty

instance HasVars Op a where

instance HasVars Let Var where
    bindsVars (Let v _ a) = a |-> Set.singleton v

instance HasVars LetRec Var where
    bindsVars (LetRec v a b) = a |-> vs & b |-> vs
        where vs = Set.singleton v

-- let x = x + 1 in (\y. y + x) z
letExp, letExp' :: Expression SigLet
letExp = iLet X (iVar X `iPlus` iInt 1) (iAbs Y (iVar Y `iPlus` iVar X) `iApp` iVar Z)
letExp' = iLet X (iInt 1 `iPlus` iInt 1) (iAbs Y (iVar Y `iPlus` iVar X) `iApp` iInt 3)

-- letrec x = x + 1 in (\y. y + x) z
recExp, recExp' :: Expression SigRec
recExp = iLetRec X (iVar X `iPlus` iInt 1) (iAbs Y (iVar Y `iPlus` iVar X) `iApp` iVar Z)
recExp' = iLetRec X (iVar X `iPlus` iInt 1) (iAbs Y (iVar Y `iPlus` iVar X) `iApp` iInt 3)

subst :: (Val :<: f) => Subst f Var
subst = Map.fromList [(X, A $ iInt 1), (Y, A $ iInt 2), (Z, A $ iInt 3)]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

case_letFree = variables letExp @=? Set.fromList [Z,X]

case_recFree = variables recExp @=? Set.fromList [Z]

case_letSubst = appSubst s letExp @=? letExp'
    where s = subst :: Subst SigLet Var

case_recSubst = appSubst s recExp @=? recExp'
    where s = subst :: Subst SigRec Var

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

main = defaultMain [tests]

tests = testGroup "Variables" [
         testCase "case_letFree" case_letFree
        ,testCase "case_recFree" case_recFree
        ,testCase "case_letSubst" case_letSubst
        ,testCase "case_recSubst" case_recSubst
        ]
