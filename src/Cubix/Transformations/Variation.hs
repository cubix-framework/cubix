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

import Data.Comp.Multi ( Cxt, (:<:), (:&:), Sum )

import Cubix.Language.C.Parametric.Common
import Cubix.Language.Java.Parametric.Common
import Cubix.Language.JavaScript.Parametric.Common as JS
import Cubix.Language.Lua.Parametric.Common
import Cubix.Language.Python.Parametric.Common as P

import Cubix.Language.Parametric.InjF

type family StatSort (f :: (* -> *) -> * -> *) :: *

#ifndef ONLY_ONE_LANGUAGE
type instance StatSort MCSig      = BlockItemL
type instance StatSort MJavaSig   = BlockItemL
type instance StatSort MJSSig     = JS.JSStatementL
type instance StatSort MPythonSig = P.StatementL
#endif
type instance StatSort (Sum MLuaSig) = BlockItemL

class DefaultLocalVarDeclAttrs f where
  defaultLocalVarDeclAttrs :: Cxt h f a LocalVarDeclAttrsL

#ifndef ONLY_ONE_LANGUAGE
instance DefaultLocalVarDeclAttrs MJSSig where
  defaultLocalVarDeclAttrs = EmptyLocalVarDeclAttrs'
#endif

instance DefaultLocalVarDeclAttrs (Sum MLuaSig) where
  defaultLocalVarDeclAttrs = EmptyLocalVarDeclAttrs'

class (DefaultLocalVarDeclAttrs f) => DefaultMultiLocalVarDeclCommonAttrs f where
  defaultMultiLocalVarDeclCommonAttrs :: Cxt h f a MultiLocalVarDeclCommonAttrsL

#ifndef ONLY_ONE_LANGUAGE
instance DefaultMultiLocalVarDeclCommonAttrs MJSSig where
  defaultMultiLocalVarDeclCommonAttrs = EmptyMultiLocalVarDeclCommonAttrs'
#endif

type ContainsSingleDec f = (SingleLocalVarDecl :<: f, OptLocalVarInit :<: f)

-- Using this to add to MultiDecInsertion/SingleDecInsertion is an ugly hack
type family StripAnn (f :: (* -> *) -> * -> *) :: (* -> *) -> * -> * where
  StripAnn (f :&: a) = f
  StripAnn (Sum fs)  = Sum (StripAnnList fs)
  StripAnn x         = x

type family StripAnnList (fs :: [(* -> *) -> * -> *]) :: [(* -> *) -> * -> *] where
  StripAnnList (f ': fs) = StripAnn f ': StripAnnList fs
  StripAnnList '[]       = '[]

type MultiDecInsertion  f g = (AInjF f MultiLocalVarDeclL, InjF f MultiLocalVarDeclL BlockItemL, InjF f MultiLocalVarDeclL (StatSort f), MultiLocalVarDecl :<: f, ContainsSingleDec f, g f)
type SingleDecInsertion f h = (AInjF f SingleLocalVarDeclL, InjF f SingleLocalVarDeclL BlockItemL, InjF f SingleLocalVarDeclL (StatSort f), ContainsSingleDec f, h f)

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

instance (h (Sum MLuaSig)) => VariableInsertionVariation (Sum MLuaSig) g h where
  variableInsertionVariation _ _ _ = SingleDecInsertionVariation Dict

type AssignmentInsertion f i = (InjF f AssignL BlockItemL, i f)

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
