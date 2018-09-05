{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Ops
-- Copyright   :  (c) 2011 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides operators on higher-order functors. All definitions are
-- generalised versions of those in "Data.Comp.Ops".
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Ops
    ( (:+:)(..)
    , caseH
    , (:<:)
    , inj
    , proj
    , (:=:)
    , spl
    , (:&:)(..)
    , RemA(..)
    , (O.:*:)(..)
    , O.ffst
    , O.fsnd
    ) where

import Control.Applicative
import Control.Monad
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable
import qualified Data.Comp.Ops as O

import Data.Comp.SubsumeCommon

infixr 6 :+:


-- |Data type defining coproducts.
data (f :+: g) (h :: * -> *) e = Inl (f h e)
                               | Inr (g h e)

{-| Utility function to case on a higher-order functor sum, without exposing the
  internal representation of sums. -}
caseH :: (f a b -> c) -> (g a b -> c) -> (f :+: g) a b -> c
caseH f g x = case x of
                Inl x -> f x
                Inr x -> g x

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g) where
    hfmap f (Inl v) = Inl $ hfmap f v
    hfmap f (Inr v) = Inr $ hfmap f v

instance (HFoldable f, HFoldable g) => HFoldable (f :+: g) where
    hfold (Inl e) = hfold e
    hfold (Inr e) = hfold e
    hfoldMap f (Inl e) = hfoldMap f e
    hfoldMap f (Inr e) = hfoldMap f e
    hfoldr f b (Inl e) = hfoldr f b e
    hfoldr f b (Inr e) = hfoldr f b e
    hfoldl f b (Inl e) = hfoldl f b e
    hfoldl f b (Inr e) = hfoldl f b e

    hfoldr1 f (Inl e) = hfoldr1 f e
    hfoldr1 f (Inr e) = hfoldr1 f e
    hfoldl1 f (Inl e) = hfoldl1 f e
    hfoldl1 f (Inr e) = hfoldl1 f e

instance (HTraversable f, HTraversable g) => HTraversable (f :+: g) where
    htraverse f (Inl e) = Inl <$> htraverse f e
    htraverse f (Inr e) = Inr <$> htraverse f e
    hmapM f (Inl e) = Inl `liftM` hmapM f e
    hmapM f (Inr e) = Inr `liftM` hmapM f e

-- The subsumption relation.

infixl 5 :<:
infixl 5 :=:

type family Elem (f :: (* -> *) -> * -> *)
                 (g :: (* -> *) -> * -> *) :: Emb where
    Elem f f = Found Here
    Elem (f1 :+: f2) g =  Sum' (Elem f1 g) (Elem f2 g)
    Elem f (g1 :+: g2) = Choose (Elem f g1) (Elem f g2)
    Elem f g = NotFound

class Subsume (e :: Emb) (f :: (* -> *) -> * -> *)
                         (g :: (* -> *) -> * -> *) where
  inj'  :: Proxy e -> f a :-> g a
  prj'  :: Proxy e -> NatM Maybe (g a) (f a)

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


inj :: forall f g a . (f :<: g) => f a :-> g a
inj = inj' (P :: Proxy (ComprEmb (Elem f g)))

proj :: forall f g a . (f :<: g) => NatM Maybe (g a) (f a)
proj = prj' (P :: Proxy (ComprEmb (Elem f g)))

type f :=: g = (f :<: g, g :<: f)



spl :: (f :=: f1 :+: f2) => (f1 a :-> b) -> (f2 a :-> b) -> f a :-> b
spl f1 f2 x = case inj x of
            Inl y -> f1 y
            Inr y -> f2 y

-- Constant Products

infixr 7 :&:

-- | This data type adds a constant product to a
-- signature. Alternatively, this could have also been defined as
--
-- @
-- data (f :&: a) (g ::  * -> *) e = f g e :&: a e
-- @
--
-- This is too general, however, for example for 'productHHom'.

data (f :&: a) (g ::  * -> *) e = f g e :&: a


instance (HFunctor f) => HFunctor (f :&: a) where
    hfmap f (v :&: c) = hfmap f v :&: c

instance (HFoldable f) => HFoldable (f :&: a) where
    hfold (v :&: _) = hfold v
    hfoldMap f (v :&: _) = hfoldMap f v
    hfoldr f e (v :&: _) = hfoldr f e v
    hfoldl f e (v :&: _) = hfoldl f e v
    hfoldr1 f (v :&: _) = hfoldr1 f v
    hfoldl1 f (v :&: _) = hfoldl1 f v


instance (HTraversable f) => HTraversable (f :&: a) where
    htraverse f (v :&: c) =  (:&: c) <$> (htraverse f v)
    hmapM f (v :&: c) = liftM (:&: c) (hmapM f v)


class RemA (s :: (* -> *) -> * -> *) s' | s -> s'  where
    remA :: s a :-> s' a


instance (RemA s s') => RemA (f :&: p :+: s) (f :+: s') where
    remA (Inl (v :&: _)) = Inl v
    remA (Inr v) = Inr $ remA v


instance RemA (f :&: p) f where
    remA (v :&: _) = v