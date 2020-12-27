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

import Data.Proxy ( Proxy(..) )
import Data.Type.Equality ( (:~:), gcastWith )

import Data.Comp.Multi ( Signature, Sort, Cxt(..), (:-<:),  (:&:), Cxt, inject, ann, stripA, HFunctor(..), HTraversable, AnnTerm, Sum, All, CxtS, HFoldable )
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
-- > instance (ExpressionIsFunArg :-<: f) => InjF fs ExpL FunArgL
-- 
-- would then allow us to use expression as function arguments freely.
class (All HFunctor fs) => InjF fs l l' where
  injF :: CxtS h fs a l -> CxtS h fs a l'

  -- |
  -- Dynamically casing on subsorts
  projF' :: Cxt h (Sum fs :&: p) a l' -> Maybe (Cxt h (Sum fs :&: p) a l)

  projF :: CxtS h fs a l' -> Maybe (CxtS h fs a l)
  projF = liftM stripA . projF' . ann ()

instance (All HFunctor fs) => InjF fs l l where
  injF = id
  projF' = Just

-- | 'injF' but for terms. Or 'inject', but allowing sort injections
-- We would like this to replace the 'inject' function outright
injectF :: (g :-<: fs, InjF fs l l') => g (CxtS h fs a) l -> CxtS h fs a l'
injectF = injF . inject

fromProjF :: (InjF fs l l') => CxtS h fs a l' -> CxtS h fs a l
fromProjF x = case projF x of
  Just y  -> y
  Nothing -> error "InjF.fromProjF"

labeledInjF :: ( MonadAnnotater Label m
              , InjF fs l l'
              , All HTraversable fs
              , All HFoldable fs
              ) => TermLab fs l -> m (TermLab fs l')
labeledInjF t = annotateLabelOuter $ injF $ Hole t

-- This MonadAnnotater instance leaks because it's technically possible to define a MonadLabeller
-- instance for AnnotateDefault. Gah!
-- FIXME: Anything that can be done about this?
injFAnnDef :: ( InjF fs l l'
             , All HTraversable fs
             , MonadAnnotater a (AnnotateDefault a)
             , All HFoldable fs
             ) => AnnTerm a fs l -> AnnTerm a fs l'
injFAnnDef t = runAnnotateDefault $ annotateOuter $ injF $ Hole t

injectFAnnDef :: ( InjF fs l l'
                , All HTraversable fs
                , MonadAnnotater a (AnnotateDefault a)
                , All HFoldable fs
                ) => (Sum fs :&: a) (AnnTerm a fs) l -> AnnTerm a fs l'
injectFAnnDef =  injFAnnDef . inject

--------------------------------------------------------------------------------


type family InjectableSorts (fs :: Signature) (l :: Sort) :: [Sort]

-- NOTE: There should be some way to express this in terms of the Lens library
class AInjF fs l where
  ainjF :: (MonadAnnotater Label m) => TermLab fs l' -> Maybe (TermLab fs l, TermLab fs l -> m (TermLab fs l'))

class AInjF' fs l (is :: [Sort]) where
  ainjF' :: (MonadAnnotater Label m) => Proxy is -> TermLab fs l' -> Maybe (TermLab fs l, TermLab fs l -> m (TermLab fs l'))

instance AInjF' fs l '[] where
  ainjF' _ _ = Nothing

instance ( All HTraversable fs
         , All HFoldable fs
         , AInjF' fs l is
         , InjF fs l i
         , KDynCase (Sum fs) i
         ) => AInjF' fs l (i ': is) where
  -- NOTE: Moved application of dyncase into the where clause (dcase)
  --       because the constraint KDynCase given in context was getting
  --       ignored.
  ainjF' _ x = case dcase x of
      Just p  -> gcastWith p spec x
      Nothing -> ainjF' (Proxy :: Proxy is) x
    where
      dcase :: forall li. (KDynCase (Sum fs) i) => TermLab fs li -> Maybe (li :~: i)
      dcase a = dyncase a :: Maybe (li :~: i)
      spec :: (MonadAnnotater Label m) => TermLab fs i -> Maybe (TermLab fs l, TermLab fs l -> m (TermLab fs i))
      spec t = case projF' t of
        Just t' -> Just (t', labeledInjF)
        Nothing -> Nothing

instance {-# OVERLAPPABLE #-} (AInjF' fs l (InjectableSorts fs l)) => AInjF fs l where
  ainjF = ainjF' (Proxy :: Proxy (InjectableSorts fs l))


promoteInjRF :: (AInjF fs l, MonadPlus m, MonadAnnotater Label m) => RewriteM m (TermLab fs) l -> GRewriteM m (TermLab fs)
promoteInjRF f t = case ainjF t of
  Nothing        -> mzero
  Just (t', ins) -> ins =<< f t'
