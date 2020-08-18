{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}



-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Strategy.Classification
-- Copyright   :  James Koppel, 2013
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- This module contains typeclasses and operations allowing dynamic casing on sorts.
-----------------------------------------------------------------------------


module Data.Comp.Multi.Strategy.Classification
  (
    DynCase(..)
  , KDynCase(..)
  , dynProj
  , fromDynProj
  , caseE
  , caseDyn
  , subterms
  , isSort
  , kIsSort
  ) where

import Data.Type.Equality ( (:~:)(..), gcastWith )
import Data.Maybe ( fromJust )
import Data.Proxy

import GHC.Exts ( Constraint )

import Data.Comp.Multi ( HFix, Sum, E, K, runE, caseH, (:&:), remA, Cxt(..), subs, NotSum, All, caseCxt )
import Data.Comp.Multi.HFoldable ( HFoldable )

--------------------------------------------------------------------------------

class EmptyConstraint (e :: * -> *) l
instance EmptyConstraint e l

-- |
-- This operation allows you to rediscover the label giving
-- the sort of a term by inspecting the term. It is mainly used
-- through the 'caseE' and 'dynProj' operators
class DynCase f a where
  -- | Determines whether a node has sort @a@
  dyncase :: f b -> Maybe (b :~: a)

-- | An instance @KDynCase f a@ defines an instance @DynCase (HFix f) a@
class KDynCase (f :: (* -> *) -> * -> *) a where
  kdyncase :: f e b -> Maybe (b :~: a)

-- Stop typeclass resolver from using this when it shouldn't
instance {-# OVERLAPPABLE #-} (NotSum f) => KDynCase f a where
  kdyncase = const Nothing

class (KDynCase f l ) => KDynCaseFlip l f
instance (KDynCase f l) => KDynCaseFlip l f

instance {-# OVERLAPPING #-} (All (KDynCaseFlip l) fs) => KDynCase (Sum fs) l where
  kdyncase = caseCxt (Proxy :: Proxy (KDynCaseFlip l)) kdyncase

instance {-# OVERLAPPING #-} (KDynCase f l) => KDynCase (f :&: a) l where
  kdyncase = kdyncase . remA

instance DynCase (K a) b where
  dyncase _ = Nothing

instance (KDynCase f l, DynCase a l) => DynCase (Cxt h f a) l where
  dyncase (Term x) = kdyncase x
  dyncase (Hole x) = dyncase x

--------------------------------------------------------------------------------

-- | Takes a term @t@ of unknown sort. Returns @Just t@ if @t@ is of sort @l@, and @Nothing@ otherwise.
dynProj :: forall f l l'. (DynCase f l) => f l' -> Maybe (f l)
dynProj x = case (dyncase x :: Maybe (l' :~: l)) of
              Just p -> Just (gcastWith p x)
              Nothing -> Nothing

-- | Equivalent to @`fromJust` . `dynProj`@
fromDynProj :: (DynCase f l) => f l' -> f l
fromDynProj x = fromJust $ dynProj x

-- | Inspect an existentially-quantified sort to determine if it is of sort @l@
caseE :: (DynCase f l) => E f -> Maybe (f l)
caseE = runE dynProj

-- | Runs a sort-specific function on a term of unknown sort, returning a default value
--   if it is of the wrong sort
caseDyn :: (DynCase f l)
        => (f l -> r) -- ^ Function to run on term of sort @l@
        -> f i        -- ^ Term of unknown sort
        -> r          -- ^ Default value
        -> r
caseDyn f t r = maybe r f (dynProj t)

-- | Gives all subterms of any given sort of a term
subterms :: (DynCase (HFix f) l, HFoldable f) => HFix f l' -> [HFix f l]
subterms x = [ y | Just y <- map caseE $ subs x]

-- | @isSort (Proxy :: Proxy l)@ returns a boolean function that tests whether a term has sort @l@
isSort :: forall e l. (DynCase e l) => Proxy l -> forall i. e i -> Bool
isSort _ x = case (dyncase x :: Maybe (_ :~: l)) of
  Just _  -> True
  Nothing -> False

-- | Like `isSort`, but runs on (unwrapped) nodes rather than terms.
kIsSort :: forall f l. (KDynCase f l) => Proxy l -> forall i e. f e i -> Bool
kIsSort _ x = case (kdyncase x :: Maybe (_ :~: l)) of
  Just _  -> True
  Nothing -> False
