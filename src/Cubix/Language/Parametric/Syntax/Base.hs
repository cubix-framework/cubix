{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}


-- | Allows you to embed Int, (), Bool etc in trees
-- Note that this normally should not be needed because comptrans takes primitives to primitives
-- However, for things like (Maybe Int), because we just have a single hand-written translation of Maybe,
-- we will need to be able to treat the contents of the Maybe uniformly

module Cubix.Language.Parametric.Syntax.Base (
    BoolF(..)
  , BoolL
  , IntF(..)
  , IntL
  , IntegerF(..)
  , IntegerL
  , CharF(..)
  , CharL
  , UnitF(..)

  , pattern BoolF'
  ,        iBoolF
  , pattern IntF'
  ,        iIntF
  , pattern IntegerF'
  ,        iIntegerF
  , pattern CharF'
  ,        iCharF
  , pattern UnitF'
  ,        iUnitF
  ) where

import Data.Comp.Multi ( Cxt, HFunctor, (:<:), project)

import Cubix.Language.Parametric.Derive

data BoolL
data BoolF (e :: * -> *) l where
  BoolF :: Bool -> BoolF e BoolL

data IntL
data IntF (e :: * -> *) l where
  IntF :: Int -> IntF e IntL

data IntegerL
data IntegerF (e :: * -> *) l where
  IntegerF :: Integer -> IntegerF e IntegerL

data CharL
data CharF (e :: * -> *) l where
  CharF :: Char -> CharF e CharL

data UnitF (e :: * -> *) l where
  UnitF :: UnitF e ()


deriveAll [''BoolF, ''IntF, ''IntegerF, ''CharF, ''UnitF]


pattern BoolF' :: (BoolF :<: f, HFunctor f) => Bool -> Cxt h f a BoolL
pattern BoolF' b <- (project -> Just (BoolF b)) where
  BoolF' b = iBoolF b


pattern IntF' :: (IntF :<: f, HFunctor f) => Int -> Cxt h f a IntL
pattern IntF' x <- (project -> Just (IntF x)) where
  IntF' x = iIntF x


pattern IntegerF' :: (IntegerF :<: f, HFunctor f) => Integer -> Cxt h f a IntegerL
pattern IntegerF' x <- (project -> Just (IntegerF x)) where
  IntegerF' x = iIntegerF x


pattern CharF' :: (CharF :<: f, HFunctor f) => Char -> Cxt h f a CharL
pattern CharF' x <- (project -> Just (CharF x)) where
  CharF' x = iCharF x

pattern UnitF' :: (UnitF :<: f, HFunctor f) => Cxt h f a ()
pattern UnitF' <- (project -> Just UnitF) where
  UnitF' = iUnitF