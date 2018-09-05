{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Mapping
-- Copyright   :  (c) 2014 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides functionality to construct mappings from
-- positions in a functorial value.
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Mapping
    ( Numbered (..)
    , unNumbered
    , number
    , HTraversable ()
    , Mapping (..)
    , lookupNumMap) where

import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable

import Control.Monad.State

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


-- | This type is used for numbering components of a functorial value.
data Numbered a i = Numbered Int (a i)

unNumbered :: Numbered a :-> a
unNumbered (Numbered _ x) = x


-- | This function numbers the components of the given functorial
-- value with consecutive integers starting at 0.
number :: HTraversable f => f a :-> f (Numbered a)
number x = evalState (hmapM run x) 0 where
  run b = do n <- get
             put (n+1)
             return $ Numbered n b



infix 1 |->
infixr 0 &


class Mapping m (k :: * -> *) | m -> k where
    -- | left-biased union of two mappings.
    (&) :: m v -> m v -> m v

    -- | This operator constructs a singleton mapping.
    (|->) :: k i -> v -> m v

    -- | This is the empty mapping.
    empty :: m v

    -- | This function constructs the pointwise product of two maps each
    -- with a default value.
    prodMap :: v1 -> v2 -> m v1 -> m v2 -> m (v1, v2)

    -- | Returns the value at the given key or returns the given
    -- default when the key is not an element of the map.
    findWithDefault :: a -> k i -> m a -> a


newtype NumMap (k :: * -> *) v = NumMap (IntMap v) deriving Functor

lookupNumMap :: a -> Int -> NumMap t a -> a
lookupNumMap d k (NumMap m) = IntMap.findWithDefault d k m

instance Mapping (NumMap k) (Numbered k) where
    NumMap m1 & NumMap m2 = NumMap (IntMap.union m1 m2)
    Numbered k _ |-> v = NumMap $ IntMap.singleton k v
    empty = NumMap IntMap.empty

    findWithDefault d (Numbered i _) m = lookupNumMap d i m

    prodMap p q (NumMap mp) (NumMap mq) = NumMap $ IntMap.mergeWithKey merge 
                                          (IntMap.map (,q)) (IntMap.map (p,)) mp mq
      where merge _ p q = Just (p,q)
