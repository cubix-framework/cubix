{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PartialTypeSignatures   #-}
{-# LANGUAGE PatternSynonyms         #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE TypeSynonymInstances    #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Ops
-- Copyright   :  Original (c) 2011 Patrick Bahr; modifications (c) 2020 James Koppel
-- License     :  BSD3
--
-- This module provides operators on higher-order functors. All definitions are
-- generalised versions of those in "Data.Comp.Ops".
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Ops
    ( Sum (..)
    , caseH
    , (:<:)
    , (:-<:)
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
    , unsafeElem
    , caseCxt
    , caseSumF
    , caseSum
    , Alts
    , Alt
    , alt
    , (<|)
    , cons
    , nil
    , Elem
    , pattern Elem
    , Mem
    , at
    , witness
    , extend
    , contract
    ) where

import Control.Monad
import Data.Default
import Data.Functor
import Data.Functor.Identity
import Data.Type.Equality
import Data.Comp.Multi.Alt
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.Kinds
import qualified Data.Comp.Ops as O
import Data.Comp.Elem
import Data.Comp.Dict

-------------------------------------------------------------

-- | Data type defining a sum of signatures.
--
--   It is inspired by modular reifiable matching, as described in
--
--   * Oliveira, Bruno C. D. S., Shin-Cheng Mu, and Shu-Hung You.
--     \"Modular reifiable matching: a list-of-functors approach to two-level types.\"
--     In Haskell Symposium, 2015.
--
--   except that this definition uses value-level integers (in the `Elem` datatype) in place
--   of type-level naturals. It hence uses `unsafeCoerce` under the hood, but is type-safe if used
--   Tthrough the public API. The result is that values of this type take constant memory with respect to the number
--   of summands (unlike vanilla datatypes à la carte), and constant time to dereference
--   (unlike modular reifiable matching). The representation is the bare minimum: an int representing the alternative,
--   and pointer to the value.
data Sum (fs :: Signature) h e where
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
caseCxt :: forall cxt fs a e b. (All cxt fs) => (forall f. (cxt f) => f a e -> b) -> Sum fs a e -> b
caseCxt f (Sum wit v) = f v \\ dictFor @cxt wit

{-# INLINE caseSumF #-}
caseSumF :: forall cxt f fs a e b. (All cxt fs, Functor f) => (forall g. (cxt g) => g a e -> f (g b e)) -> Sum fs a e -> f (Sum fs b e)
caseSumF f (Sum wit v) = Sum wit <$> f v \\ dictFor @cxt wit

{-# INLINE caseSum #-}
caseSum :: forall cxt fs a e b. (All cxt fs) => (forall g. (cxt g) => g a e -> g b e) -> Sum fs a e -> Sum fs b e
caseSum f = runIdentity . caseSumF @cxt (Identity . f) 

instance (All HFunctor fs) => HFunctor (Sum fs) where
    hfmap f = caseSum @HFunctor (hfmap f)
      
instance ( All HFoldable fs
         , All HFunctor fs
         ) => HFoldable (Sum fs) where
    hfold      = caseCxt @HFoldable hfold
    hfoldMap f = caseCxt @HFoldable (hfoldMap f)
    hfoldr f b = caseCxt @HFoldable (hfoldr f b)
    hfoldl f b = caseCxt @HFoldable (hfoldl f b)
    hfoldr1 f  = caseCxt @HFoldable (hfoldr1 f)

instance ( All HTraversable fs
         , All HFoldable fs
         , All HFunctor fs
         ) => HTraversable (Sum fs) where
    htraverse f = caseSumF @HTraversable (htraverse f)
    hmapM f     = caseSumF @HTraversable (hmapM f)

-- The subsumption relation.

infixl 5 :<:
infixl 5 :=:

class (f :: Fragment) :<: (g :: Fragment) where
  inj :: f a :-> g a
  proj :: NatM Maybe (g a) (f a)

instance ( Mem f fs
         ) => f :<: (Sum fs) where
  inj = Sum witness
  proj = at witness

instance {-# OVERLAPPABLE #-}
         ( f :<: (Sum fs)
         , Default a
         ) => f :<: (Sum fs :&: a) where
  inj x = inj x :&: def
  proj (x :&: _) = proj x

instance {-# OVERLAPS #-}
         (f :<: Sum fs
         ) => (f :&: a) :<: (Sum fs :&: a) where
  inj  (x :&: a) = inj x :&: a
  proj (x :&: a) = proj x <&> (:&: a)

instance {-# INCOHERENT #-} f :<: f where
  inj = id
  proj = Just

-- 2023.11.13: Reviewing the GHC docs on instance resolution in manual section 6.8.8.5, this
--             seems to be
--instance {-# INCOHERENT #-} (Sum fs :&: a) :<: (Sum fs :&: a) where
--  inj = id
--  proj = Just

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
-- data ((f :: Node) :&: a) g e = f g e :&: a e
-- @
--
-- This is too general, however, for example for 'productHHom'.

data ((f :: Node) :&: a) g e = f g e :&: a

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

class RemA (s :: Node) s' | s -> s'  where
    remA :: s a :-> s' a

-- TODO: This is linear
--       Is there a way to make this constant time?
instance ( RemA f g
         , RemA (Sum fs) (Sum gs)
         ) => RemA (Sum (f ': fs)) (Sum (g ': gs)) where
  remA (Sum w a) = case contract w of
    Left Refl -> Sum witness (remA a)
    Right w0  -> case go (Sum w0 a) of
      Sum w1 a -> Sum (extend w1) a

    where go :: (RemA (Sum fs) (Sum gs)) => Sum fs a :-> Sum gs a
          go = remA

instance RemA (f :&: p) f where
    remA (v :&: _) = v

-- NOTE: Invariant => Length fs == Length gs
-- TODO: write gs as a function of fs.    
unsafeMapSum :: Elem f fs -> f a e -> (f a :-> g a) -> Sum gs a e
unsafeMapSum wit v f = Sum (unsafeElem wit) (f v)

class (f :<: Sum fs) => f :-<: fs
instance (f :<: Sum fs) => f :-<: fs
