{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-} -- OVERLAPPING pragma is not in TH
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


-- Example modified from the examples directory of compdata

module Examples.Multi.Syntax where

import Data.Comp.Multi
import Data.Comp.Multi.Show ()
import Data.Comp.Multi.Equality ()
import Data.Comp.Multi.Ordering ()
import Data.Comp.Multi.Derive

import Data.Comp.Multi.Strategy.Derive ( makeDynCase )


data ExpL

-- Signature for values and operators
data Value a i where
  Const ::              Int -> Value a ExpL
  Pair  :: a ExpL -> a ExpL -> Value a ExpL
data Op a i where
  Add, Mult :: a ExpL -> a ExpL -> Op a ExpL
  Fst       ::           a ExpL -> Op a ExpL
  Snd       ::           a ExpL -> Op a ExpL

-- Signature for the simple expression language
type Sig = Op :+: Value
type Label = Integer
type SigLab = (Op :&: Label)  :+: (Value :&: Label)


type Prog = Term Sig
type ProgLab = Term SigLab

-- Derive boilerplate code using Template Haskell (GHC 7 needed)
$(derive [makeHFunctor, makeHFoldable, makeHTraversable, makeShowHF, makeEqHF,
          makeOrdHF, smartConstructors, smartAConstructors, makeDynCase] 
         [''Value, ''Op])