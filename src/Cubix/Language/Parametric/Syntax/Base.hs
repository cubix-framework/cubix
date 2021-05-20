{-# LANGUAGE DataKinds #-}
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

import Data.Comp.Multi ( Node, CxtS, All, HFunctor, (:-<:), project)

import Cubix.Language.Parametric.Derive

data BoolL
data BoolF :: Node where
  BoolF :: Bool -> BoolF e BoolL

data IntL
data IntF :: Node where
  IntF :: Int -> IntF e IntL

data IntegerL
data IntegerF :: Node where
  IntegerF :: Integer -> IntegerF e IntegerL

data CharL
data CharF :: Node where
  CharF :: Char -> CharF e CharL

data UnitF :: Node where
  UnitF :: UnitF e ()


deriveAll [''BoolF, ''IntF, ''IntegerF, ''CharF, ''UnitF]


pattern BoolF' :: (BoolF :-<: fs, All HFunctor fs) => Bool -> CxtS h fs a BoolL
pattern BoolF' b <- (project -> Just (BoolF b)) where
  BoolF' b = iBoolF b


pattern IntF' :: (IntF :-<: fs, All HFunctor fs) => Int -> CxtS h fs a IntL
pattern IntF' x <- (project -> Just (IntF x)) where
  IntF' x = iIntF x


pattern IntegerF' :: (IntegerF :-<: fs, All HFunctor fs) => Integer -> CxtS h fs a IntegerL
pattern IntegerF' x <- (project -> Just (IntegerF x)) where
  IntegerF' x = iIntegerF x


pattern CharF' :: (CharF :-<: fs, All HFunctor fs) => Char -> CxtS h fs a CharL
pattern CharF' x <- (project -> Just (CharF x)) where
  CharF' x = iCharF x

pattern UnitF' :: (UnitF :-<: fs, All HFunctor fs) => CxtS h fs a ()
pattern UnitF' <- (project -> Just UnitF) where
  UnitF' = iUnitF
