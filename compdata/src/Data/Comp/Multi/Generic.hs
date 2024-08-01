{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Generic
-- Copyright   :  (c) 2011 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module defines type generic functions and recursive schemes
-- along the lines of the Uniplate library. All definitions are
-- generalised versions of those in "Data.Comp.Generic".
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Generic where

import Control.Monad
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.Sum
import Data.Comp.Multi.Term
import GHC.Exts
import Prelude

import Data.Maybe

-- | This function returns a list of all subterms of the given
-- term. This function is similar to Uniplate's @universe@ function.
subterms :: forall h f a. HFoldable f => Cxt h f a  :=> [E (Cxt h f a)]
subterms t = build (f t)
    where f :: forall i b. Cxt h f a i -> (E (Cxt h f a) -> b -> b) -> b -> b
          f (Term t) cons nil = E (Term t) `cons` hfoldl (\u s -> f s cons u) nil t
          f (Hole h) cons nil = E (Hole h) `cons` nil

-- | This function returns a list of all subterms of the given term
-- that are constructed from a particular functor.
subterms' :: forall f g h a. (HFoldable f, g :<: f) => Cxt h f a :=> [E (g (Cxt h f a))]
subterms' (Term t) = build (f t)
    where f :: forall i b. f (Cxt h f a) i -> (E (g (Cxt h f a)) -> b -> b) -> b -> b
          f t cons nil = let rest = hfoldl (\u s -> case s of
                                                      Term r -> f r cons u
                                                      Hole _ -> nil
                                           )
                                           nil
                                           t
                         in case proj t of
                              Just t' -> E t' `cons` rest
                              Nothing -> rest

allHoles :: (HFoldable f) => Context f a l -> [E a]
allHoles t = [E h | E (Hole h) <- subterms t]

-- |
-- @
-- transform :: (forall i. Term fs i -> Term fs i) -> Term f l -> Term f l
-- @
--
-- If @f :: Term fs i -> Term fs i@ rewrites a single node, then @transform f t@
-- is the result of running @f@ on all nodes within @t@ in a bottom-up fashion.
transform :: forall f . (HFunctor f) => (HFix f :-> HFix f) -> HFix f :-> HFix f
transform f = run
    where run :: HFix f :-> HFix f
          run = f . Term . hfmap run . unTerm


-- | Monadic version of 'transform'.
transformM :: forall f m . (HTraversable f, Monad m) =>
             NatM m (HFix f) (HFix f) -> NatM m (HFix f) (HFix f)
transformM  f = run
    where run :: NatM m (HFix f) (HFix f)
          run t = f =<< liftM Term (hmapM run $ unTerm t)

-- |
-- @
-- query :: (forall i. Term fs i -> r) -> (r -> r -> r) -> Term fs l -> r
-- @
--
-- Example usage: let @getConsts :: (IntConst :-<: fs) => Term fs l -> [Int]@ be
-- a function where @getConsts (iIntConst n) = [n]@, and @getConsts t = []@
-- for everything that is not an @IntConst@. Then
--
-- @
-- query getConsts (++) term
-- @
--
-- returns a list of all integer constants in @term@.
query :: HFoldable f => (Cxt h f a :=>  r) -> (r -> r -> r) -> Cxt h f a :=> r
-- query q c = run
--     where run i@(Term t) = foldl (\s x -> s `c` run x) (q i) t
query q c i@(Term t) = hfoldl (\s x -> s `c` query q c x) (q i) t
query q c i@(Hole _) = q i

subs :: HFoldable f => Cxt h f a  :=> [E (Cxt h f a)]
subs = query (\x-> [E x]) (++)

subs' :: (HFoldable f, g :<: f) => Cxt h f a :=> [E (g (Cxt h f a))]
subs' = mapMaybe pr . subs
        where pr (E v) = fmap E (project v)

-- | This function computes the generic size of the given term,
-- i.e. the its number of subterm occurrences.
size :: HFoldable f => Cxt h f a :=> Int
size (Hole {}) = 0
size (Term t) = hfoldl (\s x -> s + size x) 1 t

-- | This function computes the generic depth of the given term.
depth :: HFoldable f => Cxt h f a :=> Int
depth (Hole {}) = 0
depth (Term t) = 1 + hfoldl (\s x -> s `max` depth x) 0 t
