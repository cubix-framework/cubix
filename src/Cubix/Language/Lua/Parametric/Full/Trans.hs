{-# OPTIONS_HADDOCK hide #-}

--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

-- This is a separate file due to GHC's phase restriction.

module Cubix.Language.Lua.Parametric.Full.Trans (
    translate
  , untranslate
  , Targ
  ) where

import Prelude hiding ( EQ, GT, LT )

import Data.Maybe ( isJust )
import Data.Typeable ( Typeable )

import Data.Comp.Multi ( inject, inject', caseCxt'', Sum, All, (:&:)(..), DistAnn )

import qualified Language.Lua.Annotated as Lua

import Data.Comp.Trans ( runCompTrans, deriveTrans, deriveUntrans, withSubstitutions, withAnnotationProp, defaultUnpropAnn, withExcludedNames )

import Cubix.Language.Info
import Cubix.Language.Lua.Parametric.Full.Exclusions
import Cubix.Language.Lua.Parametric.Full.Names
import Cubix.Language.Lua.Parametric.Full.Types
import Cubix.Language.Parametric.Syntax.Base
import Cubix.Language.Parametric.Syntax.Functor


------------------------------------------------------------------------------------
-------------- Forward translation: 3rd-party syntax to modularized  ---------------
------------------------------------------------------------------------------------

do substs <- makeSubsts
   runCompTrans $ withAnnotationProp annType isAnn propAnn defaultUnpropAnn
                $ withSubstitutions substs
                $ withExcludedNames excludedNamesSet
                $ deriveTrans origASTTypes annotatedTargType

translate :: Lua.Block (Maybe SourceSpan) -> LuaTermOptAnn SourceSpan BlockL
translate = trans

instance Trans (Lua.FunBody (Maybe SourceSpan)) FunBodyL where
  trans (Lua.FunBody a args varArg blk) = inject' $ (FunBody (trans args) (isJust varArg) (trans blk)) :&: a

instance (Trans c l, Typeable l) => Trans [c] [l] where
  trans [] = inject NilF
  trans (x:xs) = inject $ ConsF (trans x) (trans xs)

instance (Trans c l, Typeable l) => Trans (Maybe c) (Maybe l) where
  trans Nothing  = inject NothingF
  trans (Just x) = inject $ JustF $ trans x

instance (Trans c l, Trans d l', Typeable l, Typeable l') => Trans (c, d) (l, l')  where
  trans (x, y) = inject $ PairF (trans x) (trans y)

instance Trans () () where
  trans _ = inject UnitF

------------------------------------------------------------------------------------
------------ Backward translation: Modularized syntax to 3rd-party  ----------------
------------------------------------------------------------------------------------

do substs <- makeSubsts
   runCompTrans $ withAnnotationProp annType isAnn propAnn defaultUnpropAnn
                $ withExcludedNames excludedNamesSet
                $ withSubstitutions substs
                $ deriveUntrans origASTTypes annotatedTargType

type instance Targ FunBodyL = Lua.FunBody (Maybe SourceSpan)
instance Untrans (FunBody :&: (Maybe SourceSpan)) where
  untrans (FunBody args varArg blk :&: a) = T $ Lua.FunBody a (t args) varArg' (t blk)
    where
      varArg' = if varArg then Just Nothing else Nothing

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

instance (All Untrans (DistAnn fs a)) => Untrans (Sum fs :&: a) where
  untrans = caseCxt'' @Untrans untrans

type instance Targ () = ()
instance Untrans (UnitF :&: a) where
  untrans (UnitF :&: _) = T ()
