{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Ops
-- Copyright   :  Original (c) 2010-2011 Patrick Bahr, Tom Hvitved; modifications (c) 2020 James Koppel
-- License     :  BSD3
--
-- This module provides sum and product operators on functors.
--
-- The main item of interest is the `Sum` datatype, a constant-memory implementation of type-level sums.
--
--------------------------------------------------------------------------------

module Data.Comp.Ops
        ( module Data.Comp.Ops
        , Alts
        , Alt
        , alt
        , (<|)
        , cons
        , nil
        ) where

import Data.Foldable
import Data.Traversable
import Data.Functor.Identity

import Control.Applicative
import Control.Monad hiding (mapM, sequence)
import Data.Type.Equality
import Data.Comp.Elem
import Data.Comp.Dict
import Data.Comp.Alt

import Prelude hiding (foldl, foldl1, foldr, foldr1, mapM, sequence)

infixr 6 :+:

-- |Data type defining coproducts.
data (f :+: g) (a :: *) = Inl (f a)
                       | Inr (g a)


-- | See documentation for the mult-sorted version, `Data.Comp.Multi.Ops.Sum`
data Sum (fs :: [* -> *]) e where
  Sum :: Elem f fs -> f e -> Sum fs e

at :: Elem f fs -> Sum fs a -> Maybe (f a)
at e (Sum wit a) =
  case elemEq e wit of
    Just Refl -> Just a
    Nothing   -> Nothing

{-| Utility function to case on a functor sum, without exposing the internal
  representation of sums. -}
{-# INLINE caseF #-}
caseF :: Alts fs a b -> Sum fs a -> b
caseF alts (Sum wit v) = extractAt wit alts v

{-# INLINE caseCxt #-}
caseCxt :: forall cxt fs a b. (All cxt fs) => (forall f. (cxt f) => f a -> b) -> Sum fs a -> b
caseCxt f (Sum wit v) = f v \\ dictFor @cxt wit

{-# INLINE caseSumF #-}
caseSumF :: forall cxt f fs a b. (All cxt fs, Functor f) => (forall g. (cxt g) => g a -> f (g b)) -> Sum fs a -> f (Sum fs b)
caseSumF f (Sum wit v) = Sum wit <$> f v \\ dictFor @cxt wit

{-# INLINE caseSum #-}
caseSum :: forall cxt fs a b. (All cxt fs) => (forall g. (cxt g) => g a -> g b) -> Sum fs a -> Sum fs b
caseSum f = runIdentity . caseSumF @cxt (Identity . f)

instance (All Functor fs) => Functor (Sum fs) where
    fmap f = caseSum @Functor (fmap f)

instance ( All Foldable fs
         ) => Foldable (Sum fs) where
    fold      = caseCxt @Foldable fold
    foldMap f = caseCxt @Foldable (foldMap f)
    foldr f b = caseCxt @Foldable (foldr f b)
    foldl f b = caseCxt @Foldable (foldl f b)
    foldr1 f  = caseCxt @Foldable (foldr1 f)
    foldl1 f  = caseCxt @Foldable (foldl1 f)

instance ( All Traversable fs
         , All Functor fs
         , All Foldable fs
         ) => Traversable (Sum fs) where
    traverse f = caseSumF @Traversable (traverse f)
    sequenceA  = caseSumF @Traversable sequenceA
    mapM f     = caseSumF @Traversable (mapM f)
    sequence   = caseSumF @Traversable sequence

infixl 5 :<:
-- infixl 5 :=:

class (f :: * -> *) :<: (g :: * -> *) where
  inj  :: f a -> g a
  prj  :: g a -> Maybe (f a)

instance ( Functor f
         , Mem f fs
         ) => f :<: (Sum fs) where
  inj = Sum witness
  prj = at witness

instance a :<: a where
  inj = id
  prj = Just

-- | A constraint @f :<: g@ expresses that the signature @f@ is
-- subsumed by @g@, i.e. @f@ can be used to construct elements in @g@.

type f :=: g = (f :<: g, g :<: f)

spl :: ( f :=: Sum fs
       ) => Alts fs a b -> f a -> b
spl alts = caseF alts . inj

-- Products

infixr 8 :*:

-- |Formal product of signatures (functors).
data (f :*: g) a = f a :*: g a


ffst :: (f :*: g) a -> f a
ffst (x :*: _) = x

fsnd :: (f :*: g) a -> g a
fsnd (_ :*: x) = x

instance (Functor f, Functor g) => Functor (f :*: g) where
    fmap h (f :*: g) = (fmap h f :*: fmap h g)


instance (Foldable f, Foldable g) => Foldable (f :*: g) where
    foldr f e (x :*: y) = foldr f (foldr f e y) x
    foldl f e (x :*: y) = foldl f (foldl f e x) y


instance (Traversable f, Traversable g) => Traversable (f :*: g) where
    traverse f (x :*: y) = liftA2 (:*:) (traverse f x) (traverse f y)
    sequenceA (x :*: y) = liftA2 (:*:)(sequenceA x) (sequenceA y)
    mapM f (x :*: y) = liftM2 (:*:) (mapM f x) (mapM f y)
    sequence (x :*: y) = liftM2 (:*:) (sequence x) (sequence y)
