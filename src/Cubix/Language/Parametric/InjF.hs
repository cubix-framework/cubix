{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- 
-- This module enables the creation of "sort injections," stating that
-- one sort can be considered a coercive subsort of another
module Cubix.Language.Parametric.InjF
  (
    InjF(..)
  , injectF
  , fromProjF
  , labeledInjF
  , injFAnnDef
  , injectFAnnDef

  , InjectableSorts
  , AInjF(..)
  , promoteInjRF
  ) where

import Control.Monad ( MonadPlus(..), liftM )

import Data.Default ( Default )
import Data.Proxy ( Proxy(..) )
import Data.Type.Equality ( (:~:), gcastWith )

import Data.Comp.Multi ( Cxt(..), (:<:),  (:&:), Cxt, inject, ann, stripA, HFunctor(..), HTraversable, AnnTerm, AnnHFix )
import Data.Comp.Multi.Strategic ( RewriteM, GRewriteM )
import Data.Comp.Multi.Strategy.Classification ( DynCase(..), KDynCase(..) )

import Cubix.Language.Info

import Cubix.Sin.Compdata.Annotation ( MonadAnnotater, AnnotateDefault, runAnnotateDefault )

--------------------------------------------------------------------------------

-- |
-- InjF allows us to create "sort injections," stating that one sort can be considered
-- a coercive subsort of another..
-- 
-- For example, if we wanted to parameterize whether a given syntax
-- allows arbitrary expressions to be used as function arguments,
-- we could have the function terms have arguments of sort "FunArg"
-- and create an "ExpressionIsFunArg" . Defining an instance
-- 
-- > instance (ExpressionIsFunArg :<: f) => InjF (Term f) ExpL FunArgL
-- 
-- would then allow us to use expression as function arguments freely.
class (HFunctor f) => InjF f l l' where
  injF :: Cxt h f a l -> Cxt h f a l'

  -- |
  -- Dynamically casing on subsorts
  projF' :: Cxt h (f :&: p) a l' -> Maybe (Cxt h (f :&: p) a l)

  projF :: Cxt h f a l' -> Maybe (Cxt h f a l)
  projF = liftM stripA . projF' . ann ()

instance (HFunctor f) => InjF f l l where
  injF = id
  projF' = Just

-- | 'injF' but for terms. Or 'inject', but allowing sort injections
-- We would like this to replace the 'inject' function outright
injectF :: (g :<: f, InjF f l l') => g (Cxt h f a) l -> Cxt h f a l'
injectF = injF . inject

fromProjF :: (InjF f l l') => Cxt h f a l' -> Cxt h f a l
fromProjF x = case projF x of
  Just y  -> y
  Nothing -> error "InjF.fromProjF"

labeledInjF :: (MonadAnnotater Label m, InjF f l l', HTraversable f) => HFixLab f l -> m (HFixLab f l')
labeledInjF t = annotateLabelOuter $ injF $ Hole t

-- This MonadAnnotater instance leaks because it's technically possible to define a MonadLabeller
-- instance for AnnotateDefault. Gah!
-- FIXME: Anything that can be done about this?
injFAnnDef :: (InjF f l l', HTraversable f, MonadAnnotater a (AnnotateDefault a)) => AnnHFix a f l -> AnnHFix a f l'
injFAnnDef t = runAnnotateDefault $ annotateOuter $ injF $ Hole t

injectFAnnDef :: (InjF f l l', HTraversable f, MonadAnnotater a (AnnotateDefault a)) => (f :&: a) (AnnHFix a f) l -> AnnHFix a f l'
injectFAnnDef =  injFAnnDef . inject

--------------------------------------------------------------------------------


type family InjectableSorts (sig :: (* -> *) -> * -> *) (sort :: *) :: [*]

-- NOTE: There should be some way to express this in terms of the Lens library
class AInjF f l where
  ainjF :: (MonadAnnotater Label m) => HFixLab f l' -> Maybe (HFixLab f l, HFixLab f l -> m (HFixLab f l'))

class AInjF' f l (is :: [*]) where
  ainjF' :: (MonadAnnotater Label m) => Proxy is -> HFixLab f l' -> Maybe (HFixLab f l, HFixLab f l -> m (HFixLab f l'))

instance AInjF' f l '[] where
  ainjF' _ _ = Nothing

instance (HTraversable f, AInjF' f l is, InjF f l i, KDynCase f i) => AInjF' f l (i ': is) where
  -- NOTE: Moved application of dyncase into the where clause (dcase)
  --       because the constraint KDynCase given in context was getting
  --       ignored.
  ainjF' _ x = case dcase x of
      Just p  -> gcastWith p spec x
      Nothing -> ainjF' (Proxy :: Proxy is) x
    where
      dcase :: forall li. (KDynCase f i) => HFixLab f li -> Maybe (li :~: i)
      dcase a = dyncase a :: Maybe (li :~: i)
      spec :: (MonadAnnotater Label m) => HFixLab f i -> Maybe (HFixLab f l, HFixLab f l -> m (HFixLab f i))
      spec t = case projF' t of
        Just t' -> Just (t', labeledInjF)
        Nothing -> Nothing

instance {-# OVERLAPPABLE #-} (AInjF' f l (InjectableSorts f l)) => AInjF f l where
  ainjF = ainjF' (Proxy :: Proxy (InjectableSorts f l))


promoteInjRF :: (AInjF f l, MonadPlus m, MonadAnnotater Label m) => RewriteM m (HFixLab f) l -> GRewriteM m (HFixLab f)
promoteInjRF f t = case ainjF t of
  Nothing        -> mzero
  Just (t', ins) -> ins =<< f t'

