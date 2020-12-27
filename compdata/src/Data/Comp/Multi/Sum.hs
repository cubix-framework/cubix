{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Sum
-- Copyright   :  (c) 2011 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module defines sums on signatures. All definitions are
-- generalised versions of those in "Data.Comp.Sum".
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Sum
    (
     (:<:),
     (:-<:),
     Sum,

     -- * Projections for Signatures and Terms
     proj,
     project,
     deepProject,

     -- * Injections for Signatures and Terms
     inj,
     inject,
     deepInject,

     split,

     -- * Injections and Projections for Constants
     injectConst,
     projectConst,
     injectCxt,
     liftCxt,
     substHoles,
--     substHoles'

     IsSum,
     NotSum,
     isNode
    ) where

import Data.Proxy ( Proxy )

import Data.Comp.Multi.Algebra
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.Kinds
import Data.Comp.Multi.Ops
import Data.Comp.Multi.Term


-- |Project the outermost layer of a term to a sub signature. If the signature
-- @g@ is compound of /n/ atomic signatures, use @project@/n/ instead.
project :: (g :<: f) => NatM Maybe (Cxt h f a) (g (Cxt h f a))
project (Hole _) = Nothing
project (Term t) = proj t


-- | Tries to coerce a term/context to a term/context over a sub-signature. If
-- the signature @g@ is compound of /n/ atomic signatures, use
-- @deepProject@/n/ instead.
deepProject :: (HTraversable g, g :<: f)  => CxtFunM Maybe f g
{-# INLINE deepProject #-}
deepProject = appSigFunM' proj


-- |Inject a term where the outermost layer is a sub signature. If the signature
-- @g@ is compound of /n/ atomic signatures, use @inject@/n/ instead.
inject :: (g :<: f) => g (Cxt h f a) :-> Cxt h f a
inject = Term . inj


-- |Inject a term over a sub signature to a term over larger signature. If the
-- signature @g@ is compound of /n/ atomic signatures, use @deepInject@/n/
-- instead.
deepInject :: (HFunctor g, g :<: f) => CxtFun g f
{-# INLINE deepInject #-}
deepInject = appSigFun inj

split :: (f :=: Sum fs) => (forall e l. Alts fs (HFix f) e (a l)) -> HFix f :-> a
split alts = spl alts . unTerm


-- | This function injects a whole context into another context.
injectCxt :: (HFunctor g, g :<: f) => Cxt h' g (Cxt h f a) :-> Cxt h f a
injectCxt = cata' inject

-- | This function lifts the given functor to a context.
liftCxt :: (HFunctor f, g :<: f) => g a :-> Context f a
liftCxt g = simpCxt $ inj g

-- | This function applies the given context with hole type @a@ to a
-- family @f@ of contexts (possibly terms) indexed by @a@. That is,
-- each hole @h@ is replaced by the context @f h@.

substHoles :: (HFunctor f, HFunctor g, f :<: g)
           => (v :-> Cxt h g a) -> Cxt h' f v :-> Cxt h g a
substHoles f c = injectCxt $ hfmap f c

injectConst :: (HFunctor g, g :<: f) => Const g :-> Cxt h f a
injectConst = inject . hfmap (const undefined)

projectConst :: (HFunctor g, g :<: f) => NatM Maybe (Cxt h f a) (Const g)
projectConst = fmap (hfmap (const (K ()))) . project

-- Used to prevent typechecker from applying the default cases when it shouldn't,
-- i.e.: it can't use the default case when it only has a type variable for the signature
type family IsSum (f :: Node) :: Bool where
  IsSum (Sum fs) = True
  IsSum f        = False

type NotSum f = (IsSum f ~ False)


isNode :: forall f g h a l. (f :<: g) => Proxy f -> Cxt h g a l -> Bool
isNode _ t = case project t :: Maybe (f (Cxt h g a) l) of
               Just _  -> True
               Nothing -> False
