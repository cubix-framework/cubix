{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- 
-- This file provides functorial syntax -- versions of standard functors for
-- use in parametric syntax.

module Cubix.Language.Parametric.Syntax.Functor
  (
    -- * Functorial syntax
    MaybeF(..)
  , ListF(..)
  , PairF(..)
  , TripleF(..)
  , EitherF(..)

  , pattern Just'
  , pattern Nothing'

  , pattern JustA'

  , pattern ConsF'
  , pattern NilF'
  , pattern SingletonF'

  , pattern ConsFA'
  , pattern NilFA'
  , pattern SingletonFA'

  , pattern PairF'
  , pattern TripleF'

  , pattern Left'
  , pattern Right'

    -- * Smart constructors
  , riNothingF
  , iJustF
  , riNilF
  , iConsF
  , riPairF
  , riTripleF
  , riLeftF
  , riRightF

    -- * Converting functorial syntax
  , ExtractF(..)
  , KExtractF(..)
  , KExtractF'(..)
  , ExtractF2(..)
  , KExtractF2(..)
  , KExtractF2'(..)
  , ExtractF3(..)
  , KExtractF3(..)
  , KExtractF3'(..)
  , InsertF(..)

  , insertFHole
  , liftF
  , mapF
  ) where

import Data.Comp.Multi ( HFunctor, (:<:), Sum, (:&:), Cxt(..), Context, K(..), unK, inject, project, project', RemA(..), NotSum, HFix, caseCxt, All, CxtS, (:-<:) )
import Data.Comp.Multi.Derive ( derive, makeHFunctor, makeHTraversable, makeHFoldable, makeEqHF, makeShowHF, makeOrdHF )

import Data.Comp.Multi.Strategy.Classification

import Data.Typeable ( Typeable, eqT )
import Data.Proxy

import Cubix.Language.Parametric.InjF


--------------------------------------------------------------------------------
-- Higher-order versions of standard functors
--------------------------------------------------------------------------------

-- | A higher-order functor version of Maybe, for use with
-- multi-sorted compositional data types
-- 
-- @e@ : a functor mapping labels to terms of the corresponding sort
-- 
-- @l@ : the label. Will have the form @Maybe l'@
data MaybeF e l where
  NothingF :: Typeable l => MaybeF e (Maybe l)
  JustF    :: Typeable l => e l -> MaybeF e (Maybe l)

-- | A higher-order functor version of [], for use with
-- multi-sorted compositional data types
-- 
-- @e@ : a functor mapping labels to terms of the corresponding sort
-- 
-- @l@ : the label. Will have the form [l]
data ListF e l where
  NilF  :: Typeable l => ListF e [l]
  ConsF :: Typeable l => e l -> e [l] -> ListF e [l]

-- | A higher-order functor version of (,), for use with
-- multi-sorted compositional data types
-- 
-- @e@ : a functor mapping labels to terms of the corresponding sort
-- 
-- @l@ : the label . Will have the form (l1,l2)

data PairF e l where
  PairF :: (Typeable i, Typeable j) => e i -> e j -> PairF e (i, j)

data TripleF e l where
  TripleF :: (Typeable i, Typeable j, Typeable k) => e i -> e j -> e k -> TripleF e (i, j, k)


-- | A higher-order functor version of Either, for use with
-- multi-sorted compositional data types
-- 
-- @e@ : a functor mapping labels to terms of the corresponding sort
-- 
-- @l@ : the label . Will have the form (Either l1 l2)
data EitherF e l where
  LeftF  :: (Typeable i, Typeable j) => e i -> EitherF e (Either i j)
  RightF :: (Typeable i, Typeable j) => e j -> EitherF e (Either i j)

--------------------------------------------------------------------------------
-- Instances of Generic
--------------------------------------------------------------------------------

{-
instance G.Generic (MaybeF e (Maybe l)) where
  type Rep (MaybeF e (Maybe l)) = G.U1 G.:+: (G.Rec0 (e l))
  from NothingF      = G.L1 G.U1
  from (JustF x)     = G.R1 $ G.K1 x
  to (G.L1 G.U1)     = NothingF
  to (G.R1 (G.K1 x)) = JustF x

instance G.Generic (ListF e [l]) where
  type Rep (ListF e [l]) = G.U1 G.:+: (G.Rec0 (e l) G.:*: G.Rec0 (e [l]))
  from NilF                        = G.L1 G.U1
  from (ConsF x xs)                = G.R1 $ G.K1 x G.:*: G.K1 xs
  to (G.L1 G.U1)                   = NilF
  to (G.R1 (G.K1 x G.:*: G.K1 xs)) = ConsF x xs


instance G.Generic (PairF e (i,j)) where
  type Rep (PairF e (i,j)) = G.Rec0 (e i) G.:*: G.Rec0 (e j)
  from (PairF x y)         = G.K1 x G.:*: G.K1 y
  to (G.K1 x G.:*: G.K1 y) = PairF x y

instance G.Generic (TripleF e (i,j,k)) where
  type Rep (TripleF e (i,j,k))           = G.Rec0 (e i) G.:*: G.Rec0 (e j) G.:*: G.Rec0 (e k)
  from (TripleF x y z)                   = G.K1 x G.:*: G.K1 y G.:*: G.K1 z
  to (G.K1 x G.:*: G.K1 y G.:*: G.K1 z)  = TripleF x y z

instance G.Generic (EitherF e (Either i j)) where
  type Rep (EitherF e (Either i j)) = G.Rec0 (e i) G.:+: G.Rec0 (e j)
  from (LeftF x)     = G.L1 $ G.K1 x
  from (RightF x)    = G.R1 $ G.K1 x
  to (G.L1 (G.K1 x)) = LeftF x
  to (G.R1 (G.K1 x)) = RightF x

--------------------------------------------------------------------------------
-- Trivial instances of Generic
--------------------------------------------------------------------------------

instance G.Generic (MaybeF e [l]) where
  type Rep (MaybeF e [l]) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (MaybeF e (i,j)) where
  type Rep (MaybeF e (i,j)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (MaybeF e (i,j,k)) where
  type Rep (MaybeF e (i,j,k)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (MaybeF e (Either i j)) where
  type Rep (MaybeF e (Either i j)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (ListF e (Maybe l)) where
  type Rep (ListF e (Maybe l)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (ListF e (i,j)) where
  type Rep (ListF e (i,j)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (ListF e (i,j,k)) where
  type Rep (ListF e (i,j,k)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (ListF e (Either i j)) where
  type Rep (ListF e (Either i j)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (PairF e (Maybe l)) where
  type Rep (PairF e (Maybe l)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (PairF e [l]) where
  type Rep (PairF e [l]) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (PairF e (i,j,k)) where
  type Rep (PairF e (i,j,k)) = G.V1
  from = undefined
  to  = undefined

instance G.Generic (PairF e (Either i j)) where
  type Rep (PairF e (Either i j)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (TripleF e (Maybe l)) where
  type Rep (TripleF e (Maybe l)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (TripleF e [l]) where
  type Rep (TripleF e [l]) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (TripleF e (i,j)) where
  type Rep (TripleF e (i,j)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (TripleF e (Either i j)) where
  type Rep (TripleF e (Either i j)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (EitherF e (Maybe l)) where
  type Rep (EitherF e (Maybe l)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (EitherF e [l]) where
  type Rep (EitherF e [l]) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (EitherF e (i,j)) where
  type Rep (EitherF e (i,j)) = G.V1
  from = undefined
  to   = undefined

instance G.Generic (EitherF e (i,j,k)) where
  type Rep (EitherF e (i,j,k)) = G.V1
  from = undefined
  to   = undefined

-}

--------------------------------------------------------------------------------

-- DynCase instances are usually generated by TH,
-- but the TH currently does not handle higher-kinded labels
--
-- These currently have some unfortunate coupling with KDynCaseC
-- in compstrat. Still better than what we had before (actually able
-- to give us all the instances we want)

instance (Typeable l) => KDynCase MaybeF (Maybe l) where
  kdyncase NothingF  = eqT
  kdyncase (JustF _) = eqT

instance (Typeable l) => KDynCase ListF [l] where
  kdyncase NilF        = eqT
  kdyncase (ConsF _ _) = eqT

instance (Typeable l, Typeable l') => KDynCase PairF (l, l') where
  kdyncase (PairF _ _) = eqT

instance (Typeable l, Typeable l', Typeable l'') => KDynCase TripleF (l, l', l'') where
  kdyncase (TripleF _ _ _) = eqT

instance (Typeable l, Typeable l') => KDynCase EitherF (Either l l') where
  kdyncase (LeftF _)  = eqT
  kdyncase (RightF _) = eqT

-- You cannot write a (KDynCase EitherF (Either l l')) instance. That
-- can be used to write unsafe casts. The best you could do is a (KDynCase EitherF (exists l'. Either l l'))
-- instance; not sure how to write that in Haskell

--------------------------------------------------------------------------------

-- Fully smart constructors for NothingF and NilF do not
-- typecheck because of unconstrained label

-- | Smart constructor for NothingF. Restricted; cannot be lifted through a sort injection
riNothingF :: forall h f a l. (MaybeF :<: f, Typeable l) => Cxt h f a (Maybe l)
riNothingF = inject NothingF

iJustF :: (MaybeF :-<: fs, InjF fs (Maybe l) l', Typeable l) => CxtS h fs a l -> CxtS h fs a l'
iJustF = injF . iJust

iJust :: (MaybeF :<: f, Typeable l) => Cxt h f a l -> Cxt h f a (Maybe l)
iJust = inject . JustF

-- | Smart constructor for NilF. Restricted; cannot be lifted through a sort injection
riNilF :: forall h f a l. (ListF :<: f, Typeable l) => Cxt h f a [l]
riNilF = inject NilF

iConsF :: (ListF :-<: fs, InjF fs [l] l', Typeable l) => CxtS h fs a l -> CxtS h fs a [l] -> CxtS h fs a l'
iConsF x y = injF (iCons x y)

iCons :: (ListF :<: f, Typeable l) => Cxt h f a l -> Cxt h f a [l] -> Cxt h f a [l]
iCons x y = inject (ConsF x y)

-- | Smart constructor for PairF. Restricted; cannot be lifted through a sort injection
riPairF :: (PairF :<: f, Typeable i, Typeable j) => Cxt h f a i -> Cxt h f a j -> Cxt h f a (i, j)
riPairF x y = inject (PairF x y)

riTripleF :: (TripleF :<: f, Typeable i, Typeable j, Typeable k) => Cxt h f a i -> Cxt h f a j -> Cxt h f a k -> Cxt h f a (i, j, k)
riTripleF x y z = inject (TripleF x y z)

riLeftF :: (EitherF :<: f, Typeable i, Typeable j) => Cxt h f a i -> Cxt h f a (Either i j)
riLeftF = inject . LeftF

riRightF :: (EitherF :<: f, Typeable i, Typeable j) => Cxt h f a j -> Cxt h f a (Either i j)
riRightF = inject . RightF

$(derive [makeHFunctor, makeHTraversable, makeHFoldable, makeEqHF, makeShowHF,
                    makeOrdHF ]
       [''MaybeF, ''ListF, ''PairF, ''TripleF, ''EitherF])

--------------------------------------------------------------------------------
-- Dealing with functorial syntax
--------------------------------------------------------------------------------

class ExtractF f e where
  -- | Pulls a functor out of a label.
  -- 
  -- Example:
  -- 
  -- @
  -- 'extractF' :: 'JavaProj' ['SourceFileL'] -> ['JavaProj' 'SourceFileL']
  -- @
  -- 
  -- This function is unsafe, as the type system cannot detect when the label
  -- on a higher-order functorial sum guarantees which class the term lies in.
  -- 
  -- We should be able to make this safe using OrderedOverlappingTypeFamilies.
  -- 
  -- This is an instance of a distributive law, and likely can (and should)
  -- be replaced with such.
  extractF :: e (f l) -> f (e l)

-- | Inductive form of ExtractF.
-- An instance @KExtract f g@ gives rise to an instance @ExtractF f (Term g)@
class KExtractF f g where
  kextractF :: ExtractF f e => g e (f l) -> f (e l)

class KExtractF' f g where
  kextractF' :: g e (f l) -> f (e l)

instance KExtractF' f g => KExtractF f g where
  kextractF = kextractF'

instance (All (KExtractF f) gs) => KExtractF f (Sum gs) where
  kextractF = caseCxt (Proxy @(KExtractF f)) kextractF

instance (All (KExtractF' f) gs) => KExtractF' f (Sum gs) where
  kextractF' = caseCxt (Proxy @(KExtractF' f)) kextractF'

instance (KExtractF f g) => KExtractF f (g :&: a) where
  kextractF = kextractF . remA

instance (KExtractF' f g) => KExtractF' f (g :&: a) where
  kextractF' = kextractF' . remA

instance (Applicative f) => ExtractF f (K a) where
  extractF = pure . K . unK

instance (KExtractF f g, ExtractF f a, Functor f) => ExtractF f (Cxt h g a) where
  extractF (Term x) = kextractF x
  extractF (Hole x) = fmap Hole $ extractF x

instance (NotSum g) => KExtractF' f g where
  kextractF' = error "Undefined use of extractF"

--------------------------------------------------------------------------------

instance KExtractF [] ListF where
  kextractF NilF = []
  kextractF (ConsF x xs) = x : (extractF xs)

instance KExtractF' Maybe MaybeF where
  kextractF' NothingF = Nothing
  kextractF' (JustF x) = Just x

--------------------------------------------------------------------------------

class ExtractF2 f e where
  extractF2 :: e (f l l') -> f (e l) (e l')

class KExtractF2 f g where
  kextractF2 :: ExtractF2 f e => g e (f l l') -> f (e l) (e l')

class KExtractF2' f g where
  kextractF2' :: g e (f l l') -> f (e l) (e l')

instance KExtractF2' f g => KExtractF2 f g where
  kextractF2 = kextractF2'

instance (All (KExtractF2 f) gs) => KExtractF2 f (Sum gs) where
  kextractF2 = caseCxt (Proxy @(KExtractF2 f)) kextractF2

instance (All (KExtractF2' f) gs) => KExtractF2' f (Sum gs) where
  kextractF2' = caseCxt (Proxy @(KExtractF2' f)) kextractF2'

instance (KExtractF2 f g) => KExtractF2 f (g :&: a) where
  kextractF2 = kextractF2 . remA

instance (KExtractF2' f g) => KExtractF2' f (g :&: a) where
  kextractF2' = kextractF2' . remA

instance (KExtractF2 f g) => ExtractF2 f (HFix g) where
  extractF2 (Term x) = kextractF2 x

instance (NotSum g) => KExtractF2' f g where
  kextractF2' = error "Undefined use of extractF2"

--------------------------------------------------------------------------------

instance KExtractF2' (,) PairF where
  kextractF2' (PairF a b) = (a, b)

instance KExtractF2' Either EitherF where
  kextractF2' (LeftF  a) = Left a
  kextractF2' (RightF a) = Right a

--------------------------------------------------------------------------------

class ExtractF3 f e where
  extractF3 :: e (f l l' l'') -> f (e l) (e l') (e l'')

class KExtractF3 f g where
  kextractF3 :: ExtractF3 f e => g e (f l l' l'') -> f (e l) (e l') (e l'')

class KExtractF3' f g where
  kextractF3' :: g e (f l l' l'') -> f (e l) (e l') (e l'')

instance KExtractF3' f g => KExtractF3 f g where
  kextractF3 = kextractF3'

instance (All (KExtractF3 f) gs) => KExtractF3 f (Sum gs) where
  kextractF3 = caseCxt (Proxy @(KExtractF3 f)) kextractF3

instance (All (KExtractF3' f) gs) => KExtractF3' f (Sum gs) where
  kextractF3' = caseCxt (Proxy @(KExtractF3' f)) kextractF3'

instance (KExtractF3 f g) => KExtractF3 f (g :&: a) where
  kextractF3 = kextractF3 . remA

instance (KExtractF3' f g) => KExtractF3' f (g :&: a) where
  kextractF3' = kextractF3' . remA

instance (KExtractF3 f g) => ExtractF3 f (HFix g) where
  extractF3 (Term x) = kextractF3 x

instance (NotSum g) => KExtractF3' f g where
  kextractF3' = error "Undefined use of extractF3"

--------------------------------------------------------------------------------

instance KExtractF3' (,,) TripleF where
  kextractF3' (TripleF a b c) = (a, b, c)

--------------------------------------------------------------------------------

class (Functor f) => InsertF f e where
  -- | Inverse of extractF. Pushes a functor into a label.
  -- 
  -- Example:
  -- 
  -- @
  -- 'insertF' :: ['JavaProj' 'SourceFileL'] -> 'JavaProj' ['SourceFileL']
  -- @
  -- 
  -- Note that this cannot be used on a labeled tree, as the insertion operation will
  -- require generating additional labels.
  -- 
  -- This is an instance of a distributive law, and can probably be replaced with such.
  insertF :: (Typeable l) => f (e l) -> e (f l)


insertFHole :: (InsertF e (Context f (HFix g)), Typeable l) => e (HFix g l) -> Context f (HFix g) (e l)
insertFHole = insertF . fmap Hole

instance (ListF :<: e, HFunctor e) => InsertF [] (Cxt h e a) where
  insertF [] = riNilF
  insertF (x : xs) = x `iCons` (insertF xs)

instance (MaybeF :<: e, HFunctor e) => InsertF Maybe (Cxt h e a) where
  insertF Nothing = riNothingF
  insertF (Just x) = iJust x


liftF :: (InsertF f h, ExtractF f g, Functor f, Typeable b) => (f (g a) -> f (h b)) -> g (f a) -> h (f b)
liftF f = insertF . f . extractF

mapF :: (InsertF f h, ExtractF f g, Functor f, Typeable b) => (g a -> h b) -> g (f a) -> h (f b)
mapF f = liftF (fmap f)

pattern Just' :: (MaybeF :<: f, Typeable l, HFunctor f) => Cxt h f a l -> Cxt h f a (Maybe l)
pattern Just' x <- (project -> (Just (JustF x))) where
  Just' x = inject $ JustF x

pattern Nothing' :: (MaybeF :<: f, Typeable l, HFunctor f) => Cxt h f a (Maybe l)
pattern Nothing' <- (project -> Just NothingF) where
  Nothing' = inject NothingF


pattern NilF' :: (ListF :<: f, Typeable l, HFunctor f) => Cxt h f a [l]
pattern NilF' <- (project -> Just NilF) where
  NilF' = inject NilF

pattern ConsF' :: (ListF :<: f, Typeable l, HFunctor f) => Cxt h f a l -> Cxt h f a [l] -> Cxt h f a [l]
pattern ConsF' x xs <- (project -> (Just (ConsF x xs))) where
  ConsF' x xs = inject $ ConsF x xs

pattern SingletonF' :: (ListF :<: f, Typeable l, HFunctor f) => Cxt h f a l -> Cxt h f a [l]
pattern SingletonF' x = ConsF' x NilF'

pattern PairF' :: (PairF :<: f, Typeable l, Typeable l', HFunctor f) => Cxt h f a l -> Cxt h f a l' -> Cxt h f a (l, l')
pattern PairF' x y <- (project -> (Just (PairF x y))) where
  PairF' x y = inject $ PairF x y

pattern TripleF' :: (TripleF :<: f, Typeable i, Typeable j, Typeable k, HFunctor f) => Cxt h f a i -> Cxt h f a j -> Cxt h f a k -> Cxt h f a (i,j,k)
pattern TripleF' x y z <- (project -> Just (TripleF x y z)) where
  TripleF' x y z = inject $ TripleF x y z

pattern Left'  :: (EitherF :<: f, Typeable l, Typeable l', HFunctor f) => Cxt h f a l  -> Cxt h f a (Either l l')
pattern Left'  x <- (project -> (Just (LeftF x))) where
  Left'  x = inject $ LeftF x

pattern Right' :: (EitherF :<: f, Typeable l, Typeable l', HFunctor f) => Cxt h f a l' -> Cxt h f a (Either l l')
pattern Right' x <- (project -> (Just (RightF x))) where
  Right' x = inject $ RightF x

--------------------------------------------------------------------------------

-- |
-- I really don't like having to copy variants for annotated versions, but must with current design

pattern JustA' :: (MaybeF :<: f, Typeable l, HFunctor f) => Cxt h (f :&: p) a l -> Cxt h (f :&: p) a (Maybe l)
pattern JustA' x <- (project' -> (Just (JustF x)))

pattern NilFA' :: (ListF :<: f, Typeable l, HFunctor f) => Cxt h (f :&: p) a [l]
pattern NilFA' <- (project' -> Just NilF)

pattern ConsFA' :: (ListF :<: f, Typeable l, HFunctor f) => Cxt h (f :&: p) a l -> Cxt h (f :&: p) a [l] -> Cxt h (f :&: p) a [l]
pattern ConsFA' x xs <- (project' -> (Just (ConsF x xs)))

pattern SingletonFA' :: (ListF :<: f, Typeable l, HFunctor f) => Cxt h (f :&: p) a l -> Cxt h (f :&: p) a [l]
pattern SingletonFA' x <- (ConsFA' x NilFA')
