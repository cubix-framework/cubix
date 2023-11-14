{-# OPTIONS_HADDOCK hide #-}
--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

-- This is a separate file due to GHC's phase restriction.

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Python.Parametric.Full.Trans () where
#else
module Cubix.Language.Python.Parametric.Full.Trans (
    translate
  , untranslate
  ) where

import Data.Typeable (Typeable )

import Data.Comp.Multi ( caseCxt, caseCxt'', Sum, All, (:&:)(..), DistAnn )


import qualified Language.Python.Common.AST as P
import qualified Language.Haskell.TH as TH

import Data.Comp.Trans ( runCompTrans, withSubstitutions, deriveTrans, deriveUntrans, withAnnotationProp, defaultPropAnn, defaultUnpropAnn )

import Cubix.Language.Info
import Cubix.Language.Python.Parametric.Full.Names
import Cubix.Language.Python.Parametric.Full.Types
import Cubix.Language.Parametric.Syntax.Base
import Cubix.Language.Parametric.Syntax.Functor

-----------------------------------------------------------------------------------

------------------------------------------------------------------------------------
-------------- Forward translation: 3rd-party syntax to modularized  ---------------
------------------------------------------------------------------------------------

do substs <- makeSubsts
   runCompTrans $ withAnnotationProp annType isAnn propAnn defaultUnpropAnn
                $ withSubstitutions substs
                $ deriveTrans origASTTypes annotatedTargType

translate :: P.Module (Maybe SourceSpan) -> PythonTermOptAnn SourceSpan ModuleL
translate = trans

instance (Trans c l, Typeable l) => Trans [c] [l] where
  trans [] = riNilF
  trans (x:xs) = trans x `jConsF` trans xs

instance (Trans c l, Typeable l) => Trans (Maybe c) (Maybe l) where
  trans Nothing  = riNothingF
  trans (Just x) = jJustF $ trans x

instance (Trans c l, Trans d l', Typeable l, Typeable l') => Trans (c, d) (l, l')  where
  trans (x, y) = riPairF (trans x) (trans y)

instance Trans Char CharL where
  trans c = jCharF c

instance Trans () () where
  trans _ = jUnitF



------------------------------------------------------------------------------------
------------ Backward translation: Modularized syntax to 3rd-party  ----------------
------------------------------------------------------------------------------------


do substs <- makeSubsts
   runCompTrans $ withAnnotationProp annType isAnn propAnn defaultUnpropAnn
                $ withSubstitutions substs
                $ deriveUntrans origASTTypes annotatedTargType


type instance Targ [l] = [Targ l]
instance Untrans (ListF :&: a) where
  untrans (NilF :&: _) = T []
  untrans (ConsF a b :&: _) = T ((t a) : (t b))

type instance Targ (Maybe l) = Maybe (Targ l)
instance Untrans (MaybeF :&: a) where
  untrans (NothingF :&: _) = T Nothing
  untrans (JustF x :&: _) = T (Just (t x))

type instance Targ (l, l') = (Targ l, Targ l')
instance Untrans (PairF :&: a) where
  untrans (PairF x y :&: _) = T (t x, t y)

type instance Targ CharL = Char
instance Untrans (CharF :&: a) where
  untrans (CharF c :&: _) = T c

type instance Targ () = ()
instance Untrans (UnitF :&: a) where
  untrans (UnitF :&: _) = T ()

instance (All Untrans (DistAnn fs a)) => Untrans (Sum fs :&: a) where
  untrans = caseCxt'' @Untrans untrans
#endif
