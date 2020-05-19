{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Variables
-- Copyright   :  (c) 2011 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module defines an abstract notion of (bound) variables in compositional
-- data types, and scoped substitution. Capture-avoidance is /not/ taken into
-- account. All definitions are generalised versions of those in
-- "Data.Comp.Variables".
--
--------------------------------------------------------------------------------
module Data.Comp.Multi.Variables
    (
     HasVars(..),
     GSubst,
     CxtSubst,
     Subst,
     varsToHoles,
     containsVar,
     variables,
     variableList,
     variables',
     appSubst,
     compSubst,
     getBoundVars,
    (&),
    (|->),
    empty
    ) where

import Data.Comp.Multi.Algebra
import Data.Comp.Multi.Derive
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Mapping
import Data.Comp.Multi.Ops

import Data.Comp.Multi.Term
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


type GSubst v a = Map v (A a)

type CxtSubst h a f v =  GSubst v (Cxt h f a)

type Subst f v = CxtSubst NoHole (K ()) f v

type SubstFun v a = NatM Maybe (K v) a



substFun :: Ord v => GSubst v a -> SubstFun v a
substFun s (K v) = fmap unA $ Map.lookup v s

{-| This multiparameter class defines functors with variables. An instance
  @HasVar f v@ denotes that values over @f@ might contain and bind variables of
  type @v@. -}
class HasVars v (f  :: (* -> *) -> * -> *) where
    -- | Indicates whether the @f@ constructor is a variable. The
    -- default implementation returns @Nothing@.
    isVar :: f a :=> Maybe v
    isVar _ = Nothing

    -- | Indicates the set of variables bound by the @f@ constructor
    -- for each argument of the constructor. For example for a
    -- non-recursive let binding:
    -- 
    -- @
    -- data Let i e = Let Var (e i) (e i)
    -- instance HasVars Let Var where
    --   bindsVars (Let v x y) = y |-> Set.singleton v
    -- @
    -- 
    -- If, instead, the let binding is recursive, the methods has to
    -- be implemented like this:
    -- 
    -- @
    --   bindsVars (Let v x y) = x |-> Set.singleton v &
    --                           y |-> Set.singleton v
    -- @
    -- 
    -- This indicates that the scope of the bound variable also
    -- extends to the right-hand side of the variable binding.
    --
    -- The default implementation returns the empty map.
    bindsVars :: Mapping m a => f a :=> m (Set v)
    bindsVars _ = empty

$(derive [liftSum] [''HasVars])

-- | Same as 'isVar' but it returns Nothing@ instead of @Just v@ if
-- @v@ is contained in the given set of variables.

isVar' :: (HasVars v f, Ord v) => Set v -> f a :=> Maybe v
isVar' b t = do v <- isVar t
                if v `Set.member` b
                   then Nothing
                   else return v

-- | This combinator pairs every argument of a given constructor with
-- the set of (newly) bound variables according to the corresponding
-- 'HasVars' type class instance.
getBoundVars :: forall f a v i . (HasVars v f, HTraversable f) => f a i -> f (a :*: K (Set v)) i
getBoundVars t = let n :: f (Numbered a) i
                     n = number t
                     m = bindsVars n
                     trans :: Numbered a :-> (a :*: K (Set v))
                     trans (Numbered i x) = x :*: K (lookupNumMap Set.empty i m)
                 in hfmap trans n

-- | This combinator combines 'getBoundVars' with the 'mfmap' function.
hfmapBoundVars :: forall f a b v i . (HasVars v f, HTraversable f)
                  => (Set v -> a :-> b) -> f a i -> f b i
hfmapBoundVars f t = let n :: f (Numbered a) i
                         n = number t
                         m = bindsVars n
                         trans :: Numbered a :-> b
                         trans (Numbered i x) = f (lookupNumMap Set.empty i m) x
                     in hfmap trans n

-- | This combinator combines 'getBoundVars' with the generic 'hfoldl' function.
hfoldlBoundVars :: forall f a b v i . (HasVars v f, HTraversable f)
                  => (b -> Set v ->  a :=> b) -> b -> f a i -> b
hfoldlBoundVars f e t = let n :: f (Numbered a) i
                            n = number t
                            m = bindsVars n
                            trans :: b -> Numbered a :=> b
                            trans x (Numbered i y) = f x (lookupNumMap Set.empty i m) y
                       in hfoldl trans e n



-- Auxiliary data type, used only to define varsToHoles
newtype C a b i = C{ unC :: a -> b i }

varsToHoles :: forall f v. (HTraversable f, HasVars v f, Ord v) =>
                HFix f :-> Context f (K v)
varsToHoles t = unC (cata alg t) Set.empty
    where alg :: (HTraversable f, HasVars v f, Ord v) => Alg f (C (Set v) (Context f (K v)))
          alg t = C $ \vars -> case isVar t of
            Just v | not (v `Set.member` vars) -> Hole $ K v
            _  -> Term $ hfmapBoundVars run t
              where
                run :: Set v -> C (Set v) (Context f (K v))  :-> Context f (K v)
                run newVars f = f `unC` (newVars `Set.union` vars)

-- | Convert variables to holes, except those that are bound.
containsVarAlg :: forall v f . (Ord v, HasVars v f, HTraversable f) => v -> Alg f (K Bool)
containsVarAlg v t = K $ hfoldlBoundVars run local t
    where local = case isVar t of
                    Just v' -> v == v'
                    Nothing -> False
          run :: Bool -> Set v -> K Bool i -> Bool
          run acc vars (K b) = acc || (not (v `Set.member` vars) && b)

{-| This function checks whether a variable is contained in a context. -}
containsVar :: (Ord v, HasVars v f, HTraversable f, HFunctor f)
            => v -> Cxt h f a :=> Bool
containsVar v = unK . free (containsVarAlg v) (const $ K False)


{-| This function computes the list of variables occurring in a context. -}
variableList :: (HasVars v f, HTraversable f, HFunctor f, Ord v)
             => Cxt h f a :=> [v]
variableList = Set.toList . variables

-- |Algebra for checking whether a variable is contained in a term, except those
-- that are bound.
variablesAlg :: (Ord v, HasVars v f, HTraversable f) => Alg f (K (Set v))
variablesAlg t = K $ hfoldlBoundVars run local t
    where local = case isVar t of
                    Just v -> Set.singleton v
                    Nothing -> Set.empty
          run acc bvars (K vars) = acc `Set.union` (vars `Set.difference` bvars)

{-| This function computes the set of variables occurring in a context. -}
variables :: (Ord v, HasVars v f, HTraversable f, HFunctor f)
            => Cxt h f a :=> Set v
variables = unK . free variablesAlg (const $ K Set.empty)

{-| This function computes the set of variables occurring in a context. -}
variables' :: (Ord v, HasVars v f, HFoldable f, HFunctor f)
            => Const f :=> Set v
variables' c =  case isVar c of
                  Nothing -> Set.empty
                  Just v -> Set.singleton v

{-| This function substitutes variables in a context according to a
partial mapping from variables to contexts.-}
class SubstVars v t a where
    substVars :: SubstFun v t -> a :-> a

appSubst :: (Ord v, SubstVars v t a) => GSubst v t -> a :-> a
appSubst subst = substVars (substFun subst)

instance (Ord v, HasVars v f, HTraversable f) => SubstVars v (Cxt h f a) (Cxt h f a) where
    -- have to use explicit GADT pattern matching!!
    substVars subst = doSubst Set.empty
      where doSubst :: Set v -> Cxt h f a :-> Cxt h f a
            doSubst _ (Hole a) = Hole a
            doSubst b (Term t) = case isVar' b t >>= subst . K of
              Just new -> new
              Nothing  -> Term $ hfmapBoundVars run t
                where run :: Set v -> Cxt h f a :-> Cxt h f a
                      run vars = doSubst (b `Set.union` vars)

instance (SubstVars v t a, HFunctor f) => SubstVars v t (f a) where
    substVars subst = hfmap (substVars subst)

{-| This function composes two substitutions @s1@ and @s2@. That is,
applying the resulting substitution is equivalent to first applying
@s2@ and then @s1@. -}

compSubst :: (Ord v, HasVars v f, HTraversable f)
          => CxtSubst h a f v -> CxtSubst h a f v -> CxtSubst h a f v
compSubst s1 = Map.map (\ (A t) -> A (appSubst s1 t))
