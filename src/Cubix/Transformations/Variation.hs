{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE TypeSynonymInstances    #-}
{-# LANGUAGE UndecidableInstances    #-}

module Cubix.Transformations.Variation (
    StatSort

  , DefaultMultiLocalVarDeclCommonAttrs(..)
  , DefaultLocalVarDeclAttrs(..)

  , VariableInsertionVariation(..)
  , VariableInsertionVariationDict(..)
  , ContainsSingleDec
  , MultiDecInsertion
  , SingleDecInsertion

  , AssignmentInsertion
  , ExplicitDeclsVariationDict(..)
  , ExplicitDeclsVariation(..)

  , StripAnn
  ) where

import Data.Proxy ( Proxy )
import Data.Constraint ( Dict(..) )

import Data.Comp.Multi ( (:-<:), CxtS, (:&:), Sum )

import Cubix.Language.C.Parametric.Common
import Cubix.Language.Java.Parametric.Common
import Cubix.Language.JavaScript.Parametric.Common as JS
import Cubix.Language.Lua.Parametric.Common
import Cubix.Language.Python.Parametric.Common as P

import Cubix.Language.Parametric.InjF

type family StatSort (fs :: [(* -> *) -> * -> *]) :: *

#ifndef ONLY_ONE_LANGUAGE
type instance StatSort MCSig      = BlockItemL
type instance StatSort MJavaSig   = BlockItemL
type instance StatSort MJSSig     = JS.JSStatementL
type instance StatSort MPythonSig = P.StatementL
#endif
type instance StatSort MLuaSig = BlockItemL

class DefaultLocalVarDeclAttrs fs where
  defaultLocalVarDeclAttrs :: CxtS h fs a LocalVarDeclAttrsL

#ifndef ONLY_ONE_LANGUAGE
instance DefaultLocalVarDeclAttrs MJSSig where
  defaultLocalVarDeclAttrs = EmptyLocalVarDeclAttrs'
#endif

instance DefaultLocalVarDeclAttrs MLuaSig where
  defaultLocalVarDeclAttrs = EmptyLocalVarDeclAttrs'

class (DefaultLocalVarDeclAttrs fs) => DefaultMultiLocalVarDeclCommonAttrs fs where
  defaultMultiLocalVarDeclCommonAttrs :: CxtS h fs a MultiLocalVarDeclCommonAttrsL

#ifndef ONLY_ONE_LANGUAGE
instance DefaultMultiLocalVarDeclCommonAttrs MJSSig where
  defaultMultiLocalVarDeclCommonAttrs = EmptyMultiLocalVarDeclCommonAttrs'
#endif

type ContainsSingleDec fs = (SingleLocalVarDecl :-<: fs, OptLocalVarInit :-<: fs)

-- Using this to add to MultiDecInsertion/SingleDecInsertion is an ugly hack
type family StripAnn (f :: (* -> *) -> * -> *) :: (* -> *) -> * -> * where
  StripAnn (f :&: a) = f
  StripAnn (Sum fs)  = Sum (StripAnnList fs)
  StripAnn x         = x

type family StripAnnList (fs :: [(* -> *) -> * -> *]) :: [(* -> *) -> * -> *] where
  StripAnnList (f ': fs) = StripAnn f ': StripAnnList fs
  StripAnnList '[]       = '[]

type MultiDecInsertion  fs g = (AInjF fs MultiLocalVarDeclL, InjF fs MultiLocalVarDeclL BlockItemL, InjF fs MultiLocalVarDeclL (StatSort fs), MultiLocalVarDecl :-<: fs, ContainsSingleDec fs, g fs)
type SingleDecInsertion fs h = (AInjF fs SingleLocalVarDeclL, InjF fs SingleLocalVarDeclL BlockItemL, InjF fs SingleLocalVarDeclL (StatSort fs), ContainsSingleDec fs, h fs)

data VariableInsertionVariationDict f g h =
    MultiDecInsertionVariation  (Dict (MultiDecInsertion f g))
  | SingleDecInsertionVariation (Dict (SingleDecInsertion f h))

class VariableInsertionVariation f g h where
  variableInsertionVariation :: Proxy f -> Proxy g -> Proxy h -> VariableInsertionVariationDict f g h

#ifndef ONLY_ONE_LANGUAGE
instance (g MCSig) => VariableInsertionVariation MCSig g h where
  variableInsertionVariation _ _ _ = MultiDecInsertionVariation Dict

instance (g MJavaSig) => VariableInsertionVariation MJavaSig g h where
  variableInsertionVariation _ _ _ = MultiDecInsertionVariation Dict

instance (g MJSSig) => VariableInsertionVariation MJSSig g h where
  variableInsertionVariation _ _ _ = MultiDecInsertionVariation Dict
#endif

instance (h MLuaSig) => VariableInsertionVariation MLuaSig g h where
  variableInsertionVariation _ _ _ = SingleDecInsertionVariation Dict

type AssignmentInsertion fs i = (InjF fs AssignL BlockItemL, i fs)

data ExplicitDeclsVariationDict f g h i =
    HasExplicitDeclsVariation (VariableInsertionVariationDict f g h)
  | NoExplicitDeclsVariation  (Dict (AssignmentInsertion f i))

class ExplicitDeclsVariation f g h i where
  explicitDeclsVariation :: Proxy f -> Proxy g -> Proxy h -> Proxy i -> ExplicitDeclsVariationDict f g h i

instance {-# OVERLAPPABLE #-} (VariableInsertionVariation f g h) => ExplicitDeclsVariation f g h i where
  explicitDeclsVariation a b c _ = HasExplicitDeclsVariation $ variableInsertionVariation a b c

#ifndef ONLY_ONE_LANGUAGE
instance {-# OVERLAPPING #-} (i MPythonSig) => ExplicitDeclsVariation MPythonSig g h i where
  explicitDeclsVariation _ _ _ _ = NoExplicitDeclsVariation Dict
#endif
