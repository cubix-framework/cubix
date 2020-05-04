{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Term
-- Copyright   :  (c) 2011 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module defines the central notion of mutual recursive (or, higher-order)
-- /terms/ and its generalisation to (higher-order) contexts. All definitions
-- are generalised versions of those in "Data.Comp.Term".
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Term
    (Cxt (..),
     Hole,
     NoHole,
     Context,
     HFix,
     Term,
     Const,
     constTerm,
     unTerm,
     toCxt,
     simpCxt
     ) where

import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.Ops
import Data.Monoid hiding (Sum)

import Control.Applicative hiding (Const)
import Control.Monad

import Unsafe.Coerce

type Const (f :: (* -> *) -> * -> *) = f (K ())

-- | This function converts a constant to a term. This assumes that
-- the argument is indeed a constant, i.e. does not have a value for
-- the argument type of the functor f.

constTerm :: (HFunctor f) => Const f :-> HFix f
constTerm = Term . hfmap (const undefined)

-- | This data type represents contexts over a signature. Contexts are
-- terms containing zero or more holes. The first type parameter is
-- supposed to be one of the phantom types 'Hole' and 'NoHole'. The
-- second parameter is the signature of the context. The third
-- parameter is the type family of the holes. The last parameter is
-- the index/label.

data Cxt h f a i where
    Term ::  f (Cxt h f a) i -> Cxt h f a i
    Hole :: a i -> Cxt Hole f a i

-- | Phantom type that signals that a 'Cxt' might contain holes.
data Hole
-- | Phantom type that signals that a 'Cxt' does not contain holes.
data NoHole

-- | A context might contain holes.
type Context = Cxt Hole

-- | A (higher-order) term is a context with no holes.
type HFix f = Cxt NoHole f (K ())

type Term f = HFix (Sum f)

-- | This function unravels the given term at the topmost layer.
unTerm :: HFix f t -> f (HFix f) t
unTerm (Term t) = t

instance (HFunctor f) => HFunctor (Cxt h f) where
    hfmap f (Hole x) = Hole (f x)
    hfmap f (Term t) = Term (hfmap (hfmap f) t)

instance (HFoldable f) => HFoldable (Cxt h f) where
    hfoldr = hfoldr' where
        hfoldr'  :: forall a b. (a :=> (b -> b)) -> b -> Cxt h f a :=> b
        hfoldr' op c a = run a c where
              run :: (Cxt h f) a :=> (b ->  b)
              run (Hole a) e = a `op` e
              run (Term t) e = hfoldr run e t

    hfoldl = hfoldl' where
        hfoldl' :: forall a b. (b -> a :=> b) -> b -> Cxt h f a :=> b
        hfoldl' op = run where
              run :: b -> (Cxt h f) a :=> b
              run e (Hole a) = e `op` a
              run e (Term t) = hfoldl run e t

    hfold (Hole (K a)) = a
    hfold (Term t) = hfoldMap hfold t

    hfoldMap = hfoldMap' where
        hfoldMap' :: forall m a. Monoid m => (a :=> m) -> Cxt h f a :=> m
        hfoldMap' f = run where
              run :: Cxt h f a :=> m
              run (Hole a) = f a
              run (Term t) = hfoldMap run t

instance (HTraversable f) => HTraversable (Cxt h f) where
   hmapM = hmapM' where
       hmapM' :: forall m a b. (Monad m) => NatM m a b -> NatM m (Cxt h f a) (Cxt h f b)
       hmapM' f = run where
             run :: NatM m (Cxt h f a) (Cxt h f b)
             run (Hole x) = liftM Hole $ f x
             run (Term t) = liftM Term $ hmapM run t
   htraverse f (Hole x) = Hole <$> f x
   htraverse f (Term t) = Term <$> htraverse (htraverse f) t

simpCxt :: (HFunctor f) => f a i -> Context f a i
simpCxt = Term . hfmap Hole

{-| Cast a term over a signature to a context over the same signature. -}
toCxt :: (HFunctor f) => HFix f :-> Context f a
{-# INLINE toCxt #-}
toCxt = unsafeCoerce
-- equivalentto @Term . (hfmap toCxt) . unTerm@
