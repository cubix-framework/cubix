{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Ops
-- Copyright   :  (c) 2010-2011 Patrick Bahr, Tom Hvitved
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides operators on functors.
--
--------------------------------------------------------------------------------

module Data.Comp.Ops where

import Data.Foldable
import Data.Traversable

import Control.Applicative
import Control.Monad hiding (mapM, sequence)
import Data.Comp.SubsumeCommon


import Prelude hiding (foldl, foldl1, foldr, foldr1, mapM, sequence)


-- Sums

infixr 6 :+:


-- |Formal sum of signatures (functors).
data (f :+: g) e = Inl (f e)
                 | Inr (g e)

fromInl :: (f :+: g) e -> Maybe (f e)
fromInl = caseF Just (const Nothing)

fromInr :: (f :+: g) e -> Maybe (g e)
fromInr = caseF (const Nothing) Just

{-| Utility function to case on a functor sum, without exposing the internal
  representation of sums. -}
caseF :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
{-# INLINE caseF #-}
caseF f g x = case x of
                Inl x -> f x
                Inr x -> g x

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e) = Inl (fmap f e)
    fmap f (Inr e) = Inr (fmap f e)

instance (Foldable f, Foldable g) => Foldable (f :+: g) where
    fold (Inl e) = fold e
    fold (Inr e) = fold e
    foldMap f (Inl e) = foldMap f e
    foldMap f (Inr e) = foldMap f e
    foldr f b (Inl e) = foldr f b e
    foldr f b (Inr e) = foldr f b e
    foldl f b (Inl e) = foldl f b e
    foldl f b (Inr e) = foldl f b e
    foldr1 f (Inl e) = foldr1 f e
    foldr1 f (Inr e) = foldr1 f e
    foldl1 f (Inl e) = foldl1 f e
    foldl1 f (Inr e) = foldl1 f e

instance (Traversable f, Traversable g) => Traversable (f :+: g) where
    traverse f (Inl e) = Inl <$> traverse f e
    traverse f (Inr e) = Inr <$> traverse f e
    sequenceA (Inl e) = Inl <$> sequenceA e
    sequenceA (Inr e) = Inr <$> sequenceA e
    mapM f (Inl e) = Inl `liftM` mapM f e
    mapM f (Inr e) = Inr `liftM` mapM f e
    sequence (Inl e) = Inl `liftM` sequence e
    sequence (Inr e) = Inr `liftM` sequence e

infixl 5 :<:
infixl 5 :=:

type family Elem (f :: * -> *) (g :: * -> *) :: Emb where
    Elem f f = Found Here
    Elem (f1 :+: f2) g =  Sum' (Elem f1 g) (Elem f2 g)
    Elem f (g1 :+: g2) = Choose (Elem f g1) (Elem f g2)
    Elem f g = NotFound

class Subsume (e :: Emb) (f :: * -> *) (g :: * -> *) where
  inj'  :: Proxy e -> f a -> g a
  prj'  :: Proxy e -> g a -> Maybe (f a)

instance Subsume (Found Here) f f where
    inj' _ = id

    prj' _ = Just

instance Subsume (Found p) f g => Subsume (Found (Le p)) f (g :+: g') where
    inj' _ = Inl . inj' (P :: Proxy (Found p))

    prj' _ (Inl x) = prj' (P :: Proxy (Found p)) x
    prj' _ _       = Nothing

instance Subsume (Found p) f g => Subsume (Found (Ri p)) f (g' :+: g) where
    inj' _ = Inr . inj' (P :: Proxy (Found p))

    prj' _ (Inr x) = prj' (P :: Proxy (Found p)) x
    prj' _ _       = Nothing

instance (Subsume (Found p1) f1 g, Subsume (Found p2) f2 g)
    => Subsume (Found (Sum p1 p2)) (f1 :+: f2) g where
    inj' _ (Inl x) = inj' (P :: Proxy (Found p1)) x
    inj' _ (Inr x) = inj' (P :: Proxy (Found p2)) x

    prj' _ x = case prj' (P :: Proxy (Found p1)) x of
                 Just y -> Just (Inl y)
                 _      -> case prj' (P :: Proxy (Found p2)) x of
                             Just y -> Just (Inr y)
                             _      -> Nothing



-- | A constraint @f :<: g@ expresses that the signature @f@ is
-- subsumed by @g@, i.e. @f@ can be used to construct elements in @g@.
type f :<: g = (Subsume (ComprEmb (Elem f g)) f g)

inj :: forall f g a . (f :<: g) => f a -> g a
inj = inj' (P :: Proxy (ComprEmb (Elem f g)))

proj :: forall f g a . (f :<: g) => g a -> Maybe (f a)
proj = prj' (P :: Proxy (ComprEmb (Elem f g)))

type f :=: g = (f :<: g, g :<: f)



spl :: (f :=: f1 :+: f2) => (f1 a -> b) -> (f2 a -> b) -> f a -> b
spl f1 f2 x = case inj x of
            Inl y -> f1 y
            Inr y -> f2 y



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
