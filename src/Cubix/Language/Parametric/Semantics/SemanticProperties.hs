{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Language.Parametric.Semantics.SemanticProperties (
    Place(..)
  , Strictness(..)
  , GetStrictness'(..)
  , GetStrictness(..)
  , defaultGetStrictness

  , NodeEvaluationPoint(..)
  , InsertAt'(..)
  , InsertAt(..)
  , insertBefore
  , canInsertBefore
  ) where

import Control.DeepSeq ( NFData )
import Control.Monad ( liftM )

import Data.Proxy ( Proxy(..) )
import Data.Type.Equality ( (:~:)(..), gcastWith )
import Data.Typeable ( Typeable )

import GHC.Generics ( Generic )

import Data.Comp.Multi ( unTerm, (:<:), (:&:)(..), caseCxt, HFunctor, HFoldable, htoList, inject', All, Sum, HFix, AnnHFix, caseCxt' )
import Data.Comp.Multi.Strategy.Classification ( DynCase(..), KDynCase(..), kIsSort )

import Cubix.Language.Parametric.Syntax.Functor

import Cubix.Sin.Compdata.Annotation ( MonadAnnotater(..) )

--------------------------------------------O------------------------------------------

-- Need something for locations

data Place = Place Int | NegPlace Int
data Strictness = Strict | GuardedBy Place | NoEval

-- Need better place for this
data Kleene = KTrue | KUnknown | KFalse
  deriving (Eq, Ord, Show)


class GetStrictness' g f where
  getStrictness' :: f (HFix g) l -> [Strictness]

defaultGetStrictness :: (HFoldable f) => f e l -> [Strictness]
defaultGetStrictness t = take (length (htoList t)) (repeat Strict)

instance {-# OVERLAPPABLE #-} (HFoldable f) => GetStrictness' g f where
  getStrictness' = defaultGetStrictness

instance {-# OVERLAPPING #-} (All (GetStrictness' g) fs) => GetStrictness' g (Sum fs) where
  getStrictness' = caseCxt (Proxy @(GetStrictness' g)) getStrictness'

class GetStrictness f where
  getStrictness :: HFix f l -> [Strictness]

instance (GetStrictness' f f) => GetStrictness f where
  getStrictness = getStrictness' . unTerm

--------------------------------------------------------------------------------------

class HasSideEffects' g f where
  hasSideEffects' :: f (HFix g) l -> Kleene

instance {-# OVERLAPPABLE #-} HasSideEffects' g f where
  hasSideEffects' = const KUnknown

instance {-# OVERLAPPING #-} (All (HasSideEffects' g) fs) => HasSideEffects' g (Sum fs) where
  hasSideEffects' = caseCxt (Proxy @(HasSideEffects' g)) hasSideEffects'

class HasSideEffects f where
  hasSideEffects :: HFix f l -> Kleene

instance (HasSideEffects' f f) => HasSideEffects f where
  hasSideEffects = hasSideEffects' . unTerm


--------------------------------------------------------------------------------------


-- Ordered from first evaluation to last
data NodeEvaluationPoint = EnterEvalPoint
                         | LoopEntryPoint -- Meant as a target for continue statements
                         | BeforeIntermediateEvalPoint Int -- Meant for nodes that have a list of computations in them, like if/elif
                                                           -- If you want to use this for not a list index, don't
                         | ExitEvalPoint
  deriving ( Eq, Ord, Show, Generic, NFData )

-- We have to expand the type-synonym AnnTerm in a couple places because no partial application
class (DynCase (HFix g) l) => InsertAt' g l f where
  insertAt'    :: (MonadAnnotater a m) => NodeEvaluationPoint -> AnnHFix a g l -> (f :&: a) (AnnHFix a g) i -> m (AnnHFix a g i)
  canInsertAt' :: NodeEvaluationPoint -> Proxy l -> (f :&: a) (AnnHFix a g) i -> Bool

instance {-# OVERLAPPABLE #-} (f :<: g, DynCase (HFix g) l) => InsertAt' g l f where
  insertAt'    _ _ t = return $ inject' t
  canInsertAt' _ _ _ = False

instance {-# OVERLAPPING #-} (ListF :<: g, HFunctor g, KDynCase g l, KDynCase g [l], Typeable l) => InsertAt' g l ListF where
  insertAt' EnterEvalPoint e t = case kdyncase t :: Maybe (_ :~: [l]) of
                                   Nothing -> return $ inject' t
                                   Just p  -> gcastWith p $ liftM inject' $ annM $ ConsF e (inject' t)
  insertAt' _              _ t = return $ inject' t

  canInsertAt' EnterEvalPoint _ = kIsSort (Proxy :: Proxy [l])
  canInsertAt' _              _ = const False

instance {-# OVERLAPPING #-} (All (InsertAt' g l) fs, DynCase (HFix g) l) => InsertAt' g l (Sum fs) where
  insertAt' p e = caseCxt' (Proxy @(InsertAt' g l)) (insertAt' p e)
  canInsertAt' p e = caseCxt' (Proxy @(InsertAt' g l)) (canInsertAt' p e)

class InsertAt g l where
  insertAt    :: (MonadAnnotater a m) => NodeEvaluationPoint -> AnnHFix a g l -> AnnHFix a g i -> m (AnnHFix a g i)
  canInsertAt :: NodeEvaluationPoint -> Proxy l -> AnnHFix a g i -> Bool


instance (InsertAt' g l g) => InsertAt g l where
 insertAt p e t       = insertAt' p e (unTerm t)
 canInsertAt p prox t = canInsertAt' p prox (unTerm t)


insertBefore :: (InsertAt g l, MonadAnnotater a m) => AnnHFix a g l -> AnnHFix a g i -> m (AnnHFix a g i)
insertBefore = insertAt EnterEvalPoint

canInsertBefore :: (InsertAt g l) => Proxy l -> AnnHFix a g i -> Bool
canInsertBefore = canInsertAt EnterEvalPoint
