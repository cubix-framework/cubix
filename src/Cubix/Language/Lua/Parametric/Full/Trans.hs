--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- This is a separate file due to GHC's phase restriction.

module Cubix.Language.Lua.Parametric.Full.Trans (
    translate
  , untranslate
  , Targ
  ) where

import Prelude hiding ( EQ, GT, LT )

import Data.Maybe ( isJust )
import Data.Typeable ( Typeable )

import Data.Comp.Multi ( inject', caseH', (:+:), (:&:)(..), injectOpt, stripA )

import qualified Language.Haskell.TH as TH
import qualified Language.Lua.Annotated as Lua

import Data.Comp.Trans ( runCompTrans, deriveTrans, deriveUntrans, withSubstitutions, withAnnotationProp, defaultUnpropAnn, withExcludedNames )

import Cubix.Language.Info
import Cubix.Language.Lua.Parametric.Full.Exclusions
import Cubix.Language.Lua.Parametric.Full.Names
import Cubix.Language.Lua.Parametric.Full.Types
import Cubix.Language.Parametric.Syntax.Base
import Cubix.Language.Parametric.Syntax.Functor

do substs <- makeSubsts
   runCompTrans $ withAnnotationProp annType isAnn propAnn defaultUnpropAnn
                $ withSubstitutions substs
                $ withExcludedNames excludedNamesSet
                $ deriveTrans origASTTypes annotatedTargType

translate :: Lua.Block (Maybe SourceSpan) -> LuaTerm {-OptAnn SourceSpan-} BlockL
translate = stripA . trans

instance Trans (Lua.FunBody (Maybe SourceSpan)) FunBodyL where
  trans (Lua.FunBody a args varArg blk) = inject' $ (FunBody (trans args) (isJust varArg) (trans blk)) :&: a

instance (Trans c l, Typeable l) => Trans [c] [l] where
  trans [] = injectOpt NilF
  trans (x:xs) = injectOpt $ ConsF (trans x) (trans xs)

instance (Trans c l, Typeable l) => Trans (Maybe c) (Maybe l) where
  trans Nothing  = injectOpt NothingF
  trans (Just x) = injectOpt $ JustF $ trans x

instance (Trans c l, Trans d l', Typeable l, Typeable l') => Trans (c, d) (l, l')  where
  trans (x, y) = injectOpt $ PairF (trans x) (trans y)

instance Trans () () where
  trans _ = injectOpt UnitF

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


instance (Untrans (f :&: a), Untrans (g :&: a)) => Untrans ((f :+: g) :&: a) where
  untrans = caseH' untrans untrans

type instance Targ () = ()
instance Untrans (UnitF :&: a) where
  untrans (UnitF :&: _) = T ()
