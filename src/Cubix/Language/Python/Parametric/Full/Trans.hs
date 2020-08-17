{-# OPTIONS_HADDOCK hide #-}
--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- This is a separate file due to GHC's phase restriction.

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Python.Parametric.Full.Trans () where
#else
module Cubix.Language.Python.Parametric.Full.Trans (
    translate
  , untranslate
  ) where
import Data.Proxy
import Data.Typeable (Typeable )

import Data.Comp.Multi ( caseCxt, Sum, All )


import qualified Language.Python.Common.AST as P
import qualified Language.Haskell.TH as TH

import Data.Comp.Trans ( runCompTrans, withSubstitutions, deriveTrans, deriveUntrans )

import Cubix.Language.Python.Parametric.Full.Names
import Cubix.Language.Python.Parametric.Full.Types
import Cubix.Language.Parametric.Syntax.Base
import Cubix.Language.Parametric.Syntax.Functor

do substs <- makeSubsts
   runCompTrans $ withSubstitutions substs $ deriveTrans origASTTypes (TH.ConT ''PythonTerm)

translate :: P.Module () -> PythonTerm ModuleL
translate = trans

instance (Trans c l, Typeable l) => Trans [c] [l] where
  trans [] = riNilF
  trans (x:xs) = (trans x :: PythonTerm l) `iConsF` (trans xs)

instance (Trans c l, Typeable l) => Trans (Maybe c) (Maybe l) where
  trans Nothing  = riNothingF
  trans (Just x) = iJustF $ (trans x :: PythonTerm l)

instance (Trans c l, Trans d l', Typeable l, Typeable l') => Trans (c, d) (l, l')  where
  trans (x, y) = riPairF (trans x) (trans y)

instance Trans Char CharL where
  trans c = iCharF c

instance Trans () () where
  trans _ = iUnitF


do substs <- makeSubsts
   runCompTrans $ withSubstitutions substs $ deriveUntrans origASTTypes (TH.ConT ''PythonTerm)

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

type instance Targ CharL = Char
instance Untrans CharF where
  untrans (CharF c) = T c

type instance Targ () = ()
instance Untrans UnitF where
  untrans UnitF = T ()

instance (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt (Proxy @Untrans) untrans
#endif
