{-# LANGUAGE TemplateHaskell #-}

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
  , TextF(..)
  , TextL
  , UnitF(..)

  , pattern BoolF'
  ,        iBoolF
  ,        jBoolF
  , pattern IntF'
  ,        iIntF
  ,        jIntF
  , pattern IntegerF'
  ,        iIntegerF
  ,        jIntegerF
  , pattern CharF'
  ,        iCharF
  ,        jCharF
  , pattern TextF'
  ,        iTextF
  ,        jTextF

  , pattern UnitF'
  ,        iUnitF
  ,        jUnitF
  ) where

import Data.Text ( Text )

import Data.Comp.Multi ( Node )

import Cubix.Language.Parametric.Derive

-------------------------------------------------------------------------


-----------------------------------
------------------ Nodes and sorts for primitives
-----------------------------------

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

data TextL
data TextF :: Node where
  TextF :: Text -> TextF e TextL

data UnitF :: Node where
  UnitF :: UnitF e ()


-----------------------------------
------------------ Instances (via TH)
-----------------------------------

deriveAll [''BoolF, ''IntF, ''IntegerF, ''CharF, ''TextF, ''UnitF]