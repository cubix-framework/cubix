{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- This is a separate file due to GHC's phase restriction.

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Java.Parametric.Full.Trans () where
#else
module Cubix.Language.Java.Parametric.Full.Trans (
    translate
  , translateType
  , untranslate
  ) where

import Data.Typeable ( Typeable )

import qualified Language.Java.Syntax as J
import qualified Language.Haskell.TH as TH

import Data.Comp.Multi ( caseH, (:+:) )
import Data.Comp.Trans ( runCompTrans, deriveTrans, deriveUntrans )

import Cubix.Language.Java.Parametric.Full.Names
import Cubix.Language.Java.Parametric.Full.Types
import Cubix.Language.Parametric.Syntax.Functor

runCompTrans $ deriveTrans origASTTypes (TH.ConT ''JavaTerm)

translate :: J.CompilationUnit -> JavaTerm CompilationUnitL
translate = trans

translateType :: J.Type -> JavaTerm TypeL
translateType = trans

instance (Trans c l, Typeable l) => Trans [c] [l] where
  trans [] = riNilF
  trans (x:xs) = (trans x :: JavaTerm l) `iConsF` (trans xs)

instance (Trans c l, Typeable l) => Trans (Maybe c) (Maybe l) where
  trans Nothing = riNothingF
  trans (Just x) = iJustF $ (trans x :: JavaTerm l)

instance (Trans c l, Trans d l', Typeable l, Typeable l') => Trans (c, d) (l, l')  where
  trans (x, y) = riPairF (trans x) (trans y)



runCompTrans $ deriveUntrans origASTTypes (TH.ConT ''JavaTerm)

type instance Targ [l] = [Targ l]
instance Untrans ListF where
  untrans NilF = T []
  untrans (ConsF a b) = T ((t a) : (t b))

type instance Targ (Maybe l) = Maybe (Targ l)
instance Untrans MaybeF where
  untrans NothingF = T Nothing
  untrans (JustF x) = T (Just (t x))

type instance Targ (l, l') = (Targ l, Targ l')
instance Untrans PairF where
  untrans (PairF x y) = T (t x, t y)

instance (Untrans f, Untrans g) => Untrans (f :+: g) where
  untrans = caseH untrans untrans
#endif