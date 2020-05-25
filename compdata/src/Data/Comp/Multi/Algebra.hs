{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Algebra
-- Copyright   :  (c) 2011 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module defines the notion of algebras and catamorphisms, and their
-- generalizations to e.g. monadic versions and other (co)recursion schemes.
-- All definitions are generalised versions of those in "Data.Comp.Algebra".
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Algebra (
      -- * Algebras & Catamorphisms
      Alg,
      free,
      cata,
      cata',
      appCxt,

      -- * Monadic Algebras & Catamorphisms
      AlgM,
      freeM,
      cataM,
      cataM',
      liftMAlg,

      -- * Term Homomorphisms
      CxtFun,
      SigFun,
      Hom,
      appHom,
      appHom',
      compHom,
      appSigFun,
      appSigFun',
      compSigFun,
      hom,
      compAlg,

      -- * Monadic Term Homomorphisms
      CxtFunM,
      SigFunM,
      HomM,
      sigFunM,
      hom',
      appHomM,
      appHomM',
      homM,
      appSigFunM,
      appSigFunM',
      compHomM,
      compSigFunM,
      compAlgM,
      compAlgM',

      -- * Coalgebras & Anamorphisms
      Coalg,
      ana,
      CoalgM,
      anaM,

      -- * R-Algebras & Paramorphisms
      RAlg,
      para,
      RAlgM,
      paraM,

      -- * R-Coalgebras & Apomorphisms
      RCoalg,
      apo,
      RCoalgM,
      apoM,


      -- * CV-Coalgebras & Futumorphisms
      CVCoalg,
      futu,
      CVCoalgM,
      futuM,
    ) where


import Control.Monad
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.Term
import Data.Comp.Ops

-- | This type represents multisorted @f@-algebras with a family @e@
-- of carriers.
type Alg f e = f e :-> e

-- | Construct a catamorphism for contexts over @f@ with holes of type
-- @b@, from the given algebra.
free :: forall f h a b . (HFunctor f) =>
              Alg f b -> (a :-> b) -> Cxt h f a :-> b
free f g = run
    where run :: Cxt h f a :-> b
          run (Hole v) = g v
          run (Term c) = f $ hfmap run c

-- | Construct a catamorphism from the given algebra.
cata :: forall f a. HFunctor f => Alg f a -> HFix f :-> a
cata f = run
    where run :: HFix f :-> a
          run (Term t) = f (hfmap run t)

-- | A generalisation of 'cata' from terms over @f@ to contexts over
-- @f@, where the holes have the type of the algebra carrier.
cata' :: HFunctor f => Alg f e -> Cxt h f e :-> e
cata' alg = free alg id

-- | This function applies a whole context into another context.
appCxt :: HFunctor f => Context f (Cxt h f a) :-> Cxt h f a
appCxt = cata' Term

-- | This function lifts a many-sorted algebra to a monadic domain.
liftMAlg :: forall m f. (Monad m, HTraversable f) =>
            Alg f I -> Alg f m
liftMAlg alg =  turn . liftM alg . hmapM run
    where run :: m i -> m (I i)
          run m = do x <- m
                     return $ I x
          turn x = do I y <- x
                      return y
-- | This type represents a monadic algebra. It is similar to 'Alg'
-- but the return type is monadic.
type AlgM m f e = NatM m (f e) e

-- | Construct a monadic catamorphism for contexts over @f@ with holes
-- of type @b@, from the given monadic algebra.
freeM :: forall f m h a b. (HTraversable f, Monad m) =>
               AlgM m f b -> NatM m a b -> NatM m (Cxt h f a)  b
freeM algm var = run
    where run :: NatM m (Cxt h f a) b
          run (Hole x) = var x
          run (Term x) = hmapM run x >>= algm

-- | This is a monadic version of 'cata'.
cataM :: forall f m a. (HTraversable f, Monad m) =>
         AlgM m f a -> NatM m (HFix f) a
cataM alg = run
    where run :: NatM m (HFix f) a
          run (Term x) = alg =<< hmapM run x
-- cataM alg h (Term t) = alg =<< hmapM (cataM alg h) t


cataM' :: forall m h a f. (Monad m, HTraversable f) => AlgM m f a -> NatM m (Cxt h f a) a
cataM' f = run
    where run :: NatM m (Cxt h f a) a
          run (Hole x) = return x
          run (Term x) = hmapM run x >>= f
-- cataM' alg = freeM alg return


-- | This type represents uniform signature function specification.
type SigFun f g = forall (a :: * -> *). f a :-> g a

-- | This type represents context function.
type CxtFun f g = forall h . SigFun (Cxt h f) (Cxt h g)

-- | This type represents term homomorphisms.
type Hom f g = SigFun f (Context g)

-- | This function applies the given term homomorphism to a
-- term/context.
appHom :: forall f g . (HFunctor f, HFunctor g) => Hom f g -> CxtFun f g
-- Note: The rank 2 type polymorphism is not necessary. Alternatively, also the type
-- (Functor f, Functor g) => (f (Cxt h g b) -> Context g (Cxt h g b)) -> Cxt h f b -> Cxt h g b
-- would achieve the same. The given type is chosen for clarity.
appHom f = run where
    run :: CxtFun f g
    run (Hole b) = Hole b
    run (Term t) = appCxt . f . hfmap run $ t


-- | This function applies the given term homomorphism to a
-- term/context. This is the top-down variant of 'appHom'.
appHom' :: forall f g . (HFunctor g) => Hom f g -> CxtFun f g
appHom' f = run where
    run :: CxtFun f g
    run (Hole b) = Hole b
    run (Term t) = appCxt . hfmap run . f $ t

-- | This function composes two term algebras.
compHom :: (HFunctor g, HFunctor h) => Hom g h -> Hom f g -> Hom f h
-- Note: The rank 2 type polymorphism is not necessary. Alternatively, also the type
-- (Functor f, Functor g) => (f (Cxt h g b) -> Context g (Cxt h g b))
-- -> (a -> Cxt h f b) -> a -> Cxt h g b
-- would achieve the same. The given type is chosen for clarity.
compHom f g = appHom f . g

-- | This function composes a term algebra with an algebra.
compAlg :: (HFunctor g) => Alg g a -> Hom f g -> Alg f a
compAlg alg talg = cata' alg . talg

-- | This function applies a signature function to the given
-- context. This is the top-down variant of 'appSigFun'.
appSigFun' :: forall f g. (HFunctor g) => SigFun f g -> CxtFun f g
appSigFun' f = run
    where run :: CxtFun f g
          run (Hole b) = Hole b
          run (Term t) = Term . hfmap run . f $ t

-- | This function applies a signature function to the given context.
appSigFun :: forall f g. (HFunctor f) => SigFun f g -> CxtFun f g
appSigFun f = run
    where run :: CxtFun f g
          run (Hole b) = Hole b
          run (Term t) = Term . f . hfmap run $ t


-- | This function composes two signature functions.
compSigFun :: SigFun g h -> SigFun f g -> SigFun f h
compSigFun f g = f . g

-- | Lifts the given signature function to the canonical term homomorphism.
hom :: (HFunctor g) => SigFun f g -> Hom f g
hom f = simpCxt . f

-- | This type represents monadic signature functions.
type SigFunM m f g = forall (a :: * -> *) . NatM m (f a) (g a)


-- | This type represents monadic context function.
type CxtFunM m f g = forall h. SigFunM m (Cxt h f) (Cxt h g)


-- | This type represents monadic term algebras.
type HomM m f g = SigFunM m f (Context g)

-- | This function lifts the given signature function to a monadic
-- signature function. Note that term algebras are instances of
-- signature functions. Hence this function also applies to term
-- algebras.
sigFunM :: (Monad m) => SigFun f g -> SigFunM m f g
sigFunM f = return . f

-- | This function lifts the give monadic signature function to a
-- monadic term algebra.
hom' :: (HFunctor f, HFunctor g, Monad m) =>
            SigFunM m f g -> HomM m f g
hom' f = liftM  (Term . hfmap Hole) . f

-- | This function lifts the given signature function to a monadic
-- term algebra.

homM :: (HFunctor g, Monad m) => SigFun f g -> HomM m f g
homM f = sigFunM $ hom f

-- | This function applies the given monadic term homomorphism to the
-- given term/context.

appHomM :: forall f g m . (HTraversable f, HFunctor g, Monad m)
         => HomM m f g -> CxtFunM m f g
appHomM f = run
    where run :: CxtFunM m f g
          run (Hole b) = return $ Hole b
          run (Term t) = liftM appCxt . (>>= f) . hmapM run $ t

-- | This function applies the given monadic term homomorphism to the
-- given term/context. This is a top-down variant of 'appHomM'.

appHomM' :: forall f g m . (HTraversable g, Monad m)
         => HomM m f g -> CxtFunM m f g
appHomM' f = run
    where run :: CxtFunM m f g
          run (Hole b) = return $ Hole b
          run (Term t) = liftM appCxt . hmapM run =<< f t

-- | This function applies the given monadic signature function to the
-- given context.

appSigFunM :: forall f g m. (HTraversable f, Monad m) =>
                SigFunM m f g -> CxtFunM m f g
appSigFunM f = run
    where run :: CxtFunM m f g
          run (Hole b) = return $ Hole b
          run (Term t) = liftM Term . f =<< hmapM run t

-- | This function applies the given monadic signature function to the
-- given context. This is a top-down variant of 'appSigFunM'.
appSigFunM' :: forall f g m. (HTraversable g, Monad m) =>
                SigFunM m f g -> CxtFunM m f g
appSigFunM' f = run
    where run :: CxtFunM m f g
          run (Hole b) = return $ Hole b
          run (Term t) = liftM Term . hmapM run =<< f t

-- | This function composes two monadic term algebras.

compHomM :: (HTraversable g, HFunctor h, Monad m)
             => HomM m g h -> HomM m f g -> HomM m f h
compHomM f g a = g a >>= appHomM f

{-| This function composes a monadic term algebra with a monadic algebra -}

compAlgM :: (HTraversable g, Monad m) => AlgM m g a -> HomM m f g -> AlgM m f a
compAlgM alg talg c = cataM' alg =<< talg c

-- | This function composes a monadic term algebra with a monadic
-- algebra.

compAlgM' :: (HTraversable g, Monad m) => AlgM m g a -> Hom f g -> AlgM m f a
compAlgM' alg talg = cataM' alg . talg


{-| This function composes two monadic signature functions.  -}

compSigFunM :: (Monad m) => SigFunM m g h -> SigFunM m f g -> SigFunM m f h
compSigFunM f g a = g a >>= f


----------------
-- Coalgebras --
----------------

type Coalg f a = a :-> f a

{-| This function unfolds the given value to a term using the given
unravelling function. This is the unique homomorphism @a -> Term f@
from the given coalgebra of type @a -> f a@ to the final coalgebra
@Term f@. -}

ana :: forall f a. HFunctor f => Coalg f a -> a :-> HFix f
ana f = run
    where run :: a :-> HFix f
          run t = Term $ hfmap run (f t)

type CoalgM m f a = NatM m a (f a)

-- | This function unfolds the given value to a term using the given
-- monadic unravelling function. This is the unique homomorphism @a ->
-- Term f@ from the given coalgebra of type @a -> f a@ to the final
-- coalgebra @Term f@.

anaM :: forall a m f. (HTraversable f, Monad m)
          => CoalgM m f a -> NatM m a (HFix f)
anaM f = run
    where run :: NatM m a (HFix f)
          run t = liftM Term $ f t >>= hmapM run

--------------------------------
-- R-Algebras & Paramorphisms --
--------------------------------

-- | This type represents r-algebras over functor @f@ and with domain
-- @a@.

type RAlg f a = f (HFix f :*: a) :-> a

-- | This function constructs a paramorphism from the given r-algebra
para :: forall f a. (HFunctor f) => RAlg f a -> HFix f :-> a
para f = fsnd . cata run
    where run :: Alg f  (HFix f :*: a)
          run t = Term (hfmap ffst t) :*: f t

-- | This type represents monadic r-algebras over monad @m@ and
-- functor @f@ and with domain @a@.
type RAlgM m f a = NatM m (f (HFix f :*: a)) a

-- | This function constructs a monadic paramorphism from the given
-- monadic r-algebra
paraM :: forall f m a. (HTraversable f, Monad m) =>
         RAlgM m f a -> NatM m(HFix f)  a
paraM f = liftM fsnd . cataM run
    where run :: AlgM m f (HFix f :*: a)
          run t = do
            a <- f t
            return (Term (hfmap ffst t) :*: a)

--------------------------------
-- R-Coalgebras & Apomorphisms --
--------------------------------
-- | This type represents r-coalgebras over functor @f@ and with
-- domain @a@.
type RCoalg f a = a :-> f (HFix f :+: a)

-- | This function constructs an apomorphism from the given
-- r-coalgebra.
apo :: forall f a . (HFunctor f) => RCoalg f a -> a :-> HFix f
apo f = run
    where run :: a :-> HFix f
          run = Term . hfmap run' . f
          run' :: HFix f :+: a :-> HFix f
          run' (Inl t) = t
          run' (Inr a) = run a

-- | This type represents monadic r-coalgebras over monad @m@ and
-- functor @f@ with domain @a@.

type RCoalgM m f a = NatM m a (f (HFix f :+: a))

-- | This function constructs a monadic apomorphism from the given
-- monadic r-coalgebra.
apoM :: forall f m a . (HTraversable f, Monad m) =>
        RCoalgM m f a -> NatM m a (HFix f)
apoM f = run
    where run :: NatM m a (HFix f)
          run a = do
            t <- f a
            t' <- hmapM run' t
            return $ Term t'
          run' :: NatM m (HFix f :+: a)  (HFix f)
          run' (Inl t) = return t
          run' (Inr a) = run a

-----------------------------------
-- CV-Coalgebras & Futumorphisms --
-----------------------------------


-- | This type represents cv-coalgebras over functor @f@ and with domain
-- @a@.

type CVCoalg f a = a :-> f (Context f a)


-- | This function constructs the unique futumorphism from the given
-- cv-coalgebra to the term algebra.

futu :: forall f a . HFunctor f => CVCoalg f a -> a :-> HFix f
futu coa = ana run . Hole
    where run :: Coalg f (Context f a)
          run (Hole a) = coa a
          run (Term v) = v


-- | This type represents monadic cv-coalgebras over monad @m@ and
-- functor @f@, and with domain @a@.

type CVCoalgM m f a = NatM m a (f (Context f a))

-- | This function constructs the unique monadic futumorphism from the
-- given monadic cv-coalgebra to the term algebra.
futuM :: forall f a m . (HTraversable f, Monad m) =>
         CVCoalgM m f a -> NatM m a (HFix f)
futuM coa = anaM run . Hole
    where run :: CoalgM m f (Context f a)
          run (Hole a) = coa a
          run (Term v) = return v
