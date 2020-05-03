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
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableSuperClasses#-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE MagicHash              #-}

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
    ( Sum (..)
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
    , unsafeMapSum
    , caseCxt
    , Alts
    , Alt
    , alt
    , (<|)
    , cons
    , nil
    ) where

import Control.Monad
import Data.Type.Equality
import Data.Proxy
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.Alt
import qualified Data.Comp.Ops as O
import Data.Comp.Elem
import Data.Comp.Dict

-- |Data type defining a coproduct family.
data Sum (fs :: [(* -> *) -> * -> *]) h e where
  Sum :: Elem f fs -> f h e -> Sum fs h e

at :: Elem f fs -> Sum fs a e -> Maybe (f a e)
at e (Sum wit a) =
  case elemEq e wit of
    Just Refl -> Just a
    Nothing   -> Nothing

{-| Utility function to case on a higher-order functor sum, without exposing the
  internal representation of sums. -}
{-# INLINE caseH #-}
caseH :: Alts fs a e b -> Sum fs a e -> b
caseH alts (Sum wit v) = extractAt wit alts v

{-# INLINE caseCxt #-}
caseCxt :: forall cxt fs a e b. (All cxt fs) => Proxy cxt -> (forall f. (cxt f) => f a e -> b) -> Sum fs a e -> b
caseCxt _ f (Sum wit v) = f v \\ dictFor @cxt wit

instance (All HFunctor fs) => HFunctor (Sum fs) where
    hfmap f (Sum wit v) =
      Sum wit $
      hfmap f v \\
      dictFor @HFunctor wit

instance ( HFunctor (Sum fs)
         , All HFoldable fs
         ) => HFoldable (Sum fs) where
    hfold (Sum wit e) = hfold e \\ dictFor @HFoldable wit
    hfoldMap f (Sum wit e) = hfoldMap f e \\ dictFor @HFoldable wit
    hfoldr f b (Sum wit e) = hfoldr f b e \\ dictFor @HFoldable wit
    hfoldl f b (Sum wit e) = hfoldl f b e \\ dictFor @HFoldable wit
    hfoldr1 f (Sum wit e) = hfoldr1 f e \\ dictFor @HFoldable wit

instance ( HFoldable (Sum fs)
         , All HTraversable fs
         ) => HTraversable (Sum fs) where
    htraverse f (Sum wit e) =
      Sum wit <$>
      htraverse f e \\
      dictFor @HTraversable wit
    hmapM f (Sum wit e) =
      Sum wit `liftM`
      hmapM f e \\
      dictFor @HTraversable wit

-- The subsumption relation.

infixl 5 :<:
infixl 5 :=:

class (f :: (* -> *) -> * -> *) :<: (g :: (* -> *) -> * -> *) where
  inj :: f a :-> g a
  proj :: NatM Maybe (g a) (f a)

instance ( f ∈ fs
         ) => f :<: (Sum fs) where
  inj = Sum witness
  proj = at witness

instance f :<: f where
  inj = id
  proj = Just

type f :=: g = (f :<: g, g :<: f)

spl :: ( f :=: Sum fs
      ) => (forall e l. Alts fs a e (b l)) -> f a :-> b
spl alts = caseH alts . inj

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

-- TODO: remA
-- instance ( -- fs ~ RemAnn gs
--            fs ~ '[f :&: a]
--          , gs ~ '[f]
--          ) => RemA (Sum fs) (Sum gs) where
--     remA (Sum fsWit a) =
--       let gsWit = witness
--       in (Sum gsWit (remA a) :: Sum gs _ _) \\\
--          dictForNat @RemA fsWit (Proxy :: Proxy gs)

instance RemA (f :&: p) f where
    remA (v :&: _) = v

-- NOTE: Invariant => Length fs == Length gs
-- TODO: write gs as a function of fs.    
unsafeMapSum :: Elem f fs -> f a e -> (f a :-> g a) -> Sum gs a e
unsafeMapSum (Elem wit) v f = Sum (Elem wit) (f v)
