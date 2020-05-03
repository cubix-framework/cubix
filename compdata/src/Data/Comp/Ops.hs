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
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE RankNTypes             #-}

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

import Control.Applicative
import Control.Monad hiding (mapM, sequence)
import Data.Type.Equality
import Data.Comp.Elem
import Data.Comp.Dict
import Data.Comp.Alt

import Prelude hiding (foldl, foldl1, foldr, foldr1, mapM, sequence)


-- |Formal sum of signatures (functors).
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

instance (All Functor fs) => Functor (Sum fs) where
    fmap f (Sum wit v) =
      Sum wit $
      fmap f v \\
      dictFor @Functor wit

instance ( All Foldable fs
         , Functor (Sum fs)
         ) => Foldable (Sum fs) where
    fold (Sum wit e) = withDict (dictFor @Foldable wit) $ fold e
    foldMap f (Sum wit e) = foldMap f e \\ dictFor @Foldable wit
    foldr f b (Sum wit e) = foldr f b e \\ dictFor @Foldable wit
    foldl f b (Sum wit e) = foldl f b e \\ dictFor @Foldable wit
    foldr1 f (Sum wit e) = foldr1 f e \\ dictFor @Foldable wit
    foldl1 f (Sum wit e) = foldl1 f e \\ dictFor @Foldable wit

instance ( All Traversable fs
         , Functor (Sum fs)
         , Foldable (Sum fs)
         ) => Traversable (Sum fs) where
    traverse f (Sum wit e) =
      Sum wit <$>
      traverse f e \\
      dictFor @Traversable wit
    sequenceA (Sum wit e) =
      Sum wit <$>
      sequenceA e \\
      dictFor @Traversable wit
    mapM f (Sum wit e) =
      Sum wit `liftM`
      mapM f e \\
      dictFor @Traversable wit
    sequence (Sum wit e) =
      Sum wit `liftM`
      sequence e \\
      dictFor @Traversable wit

infixl 5 :<:
infixl 5 :=:

class (f :: * -> *) :<: (g :: * -> *) where
  inj  :: f a -> g a
  prj  :: g a -> Maybe (f a)

instance ( Functor f
         , f ∈ fs
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
