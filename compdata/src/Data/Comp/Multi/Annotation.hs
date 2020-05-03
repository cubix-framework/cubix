{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Annotation
-- Copyright   :  (c) 2011 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module defines annotations on signatures. All definitions are
-- generalised versions of those in "Data.Comp.Annotation".
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Annotation
    (
     AnnTerm,
     (:&:) (..),
     RemA (..),
     liftA,
     ann,
     liftA',
     stripA,
     propAnn,
     project',
     isNode',
     inj',
     inject',
     injectOpt,
     caseH'
    ) where

import Data.Proxy ( Proxy )

import Data.Comp.Multi.Algebra
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Ops
import Data.Comp.Multi.Sum
import Data.Comp.Multi.Term
import Data.Comp.Multi.Alt
import qualified Data.Comp.Ops as O
import Data.Comp.Elem
import Data.Comp.Dict

type AnnTerm a f = Term (f :&: a)

-- | This function transforms a function with a domain constructed
-- from a functor to a function with a domain constructed with the
-- same functor but with an additional annotation.
liftA :: (RemA s s') => (s' a :-> t) -> s a :-> t
liftA f v = f (remA v)

-- | This function annotates each sub term of the given term with the
-- given value (of type a).

ann :: (HFunctor f) => p -> CxtFun f (f :&: p)
ann c = appSigFun (:&: c)

-- | This function transforms a function with a domain constructed
-- from a functor to a function with a domain constructed with the
-- same functor but with an additional annotation.
liftA' :: (HFunctor s)
       => (s a :-> Cxt h s a) -> (s :&: p) a :-> Cxt h (s :&: p) a
liftA' f (v' :&: p) = ann p (f v')

{-| This function strips the annotations from a term over a
functor with annotations. -}

stripA :: (RemA g f, HFunctor g) => CxtFun g f
stripA = appSigFun remA


propAnn :: (HFunctor g) => Hom f g -> Hom (f :&: p) (g :&: p)
propAnn alg (f :&: p) = ann p (alg f)

-- | This function is similar to 'project' but applies to signatures
-- with an annotation which is then ignored.
project' :: (RemA f f', s :<: f') => Cxt h f a i -> Maybe (s (Cxt h f a) i)
project' (Term x) = proj $ remA x
project' _ = Nothing

isNode' :: (HFunctor g, RemA g g', f :<: g') => Proxy f -> Cxt h g a l -> Bool
isNode' p t = isNode p $ stripA t

inj' :: (f :<: g) => (f :&: p) e l -> (g :&: p) e l
inj' (x :&: p) = (inj x) :&: p

inject' :: (f :<: g) => (f :&: p) (Cxt h (g :&: p) a) :-> Cxt h (g :&: p) a
inject' = Term . inj'

injectOpt :: (f :<: g) => f (AnnTerm (Maybe p) g) l -> AnnTerm (Maybe p) g l
injectOpt t = inject' (t :&: Nothing)

caseH' :: forall fs a e l t. Alts (DistAnn fs a) e l t -> (Sum fs :&: a) e l -> t
caseH' alts = caseH alts . distAnn

type family DistAnn (fs :: [(* -> *) -> * -> *]) (a :: *) :: [(* -> *) -> * -> *] where
  DistAnn (f ': fs) a = f :&: a ': DistAnn fs a
  DistAnn '[]       _ = '[]

distAnn :: (Sum fs :&: a) e :-> Sum (DistAnn fs a) e
distAnn (Sum wit v :&: a) =
  unsafeMapSum wit v (:&: a)
