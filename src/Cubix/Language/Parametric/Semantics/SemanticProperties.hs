{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE UndecidableInstances   #-}

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

import Data.Comp.Multi ( unTerm, (:&:)(..), caseCxt, HFunctor, HFoldable, htoList, inject', All, Sum, caseCxt', AnnTerm, Term, (:-<:) )
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


class GetStrictness' gs f where
  getStrictness' :: f (Term gs) l -> [Strictness]

defaultGetStrictness :: (HFoldable f) => f e l -> [Strictness]
defaultGetStrictness t = take (length (htoList t)) (repeat Strict)

instance {-# OVERLAPPABLE #-} (HFoldable f) => GetStrictness' gs f where
  getStrictness' = defaultGetStrictness

instance {-# OVERLAPPING #-} (All (GetStrictness' gs) fs) => GetStrictness' gs (Sum fs) where
  getStrictness' = caseCxt (Proxy @(GetStrictness' gs)) getStrictness'

class GetStrictness fs where
  getStrictness :: Term fs l -> [Strictness]

instance (GetStrictness' fs (Sum fs)) => GetStrictness fs where
  getStrictness = getStrictness' . unTerm

--------------------------------------------------------------------------------------

class HasSideEffects' gs f where
  hasSideEffects' :: f (Term gs) l -> Kleene

instance {-# OVERLAPPABLE #-} HasSideEffects' gs f where
  hasSideEffects' = const KUnknown

instance {-# OVERLAPPING #-} (All (HasSideEffects' gs) fs) => HasSideEffects' gs (Sum fs) where
  hasSideEffects' = caseCxt (Proxy @(HasSideEffects' gs)) hasSideEffects'

class HasSideEffects fs where
  hasSideEffects :: Term fs l -> Kleene

instance (HasSideEffects' fs (Sum fs)) => HasSideEffects fs where
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
class (DynCase (Term gs) l) => InsertAt' gs l f where
  insertAt'    :: (MonadAnnotater a m) => NodeEvaluationPoint -> AnnTerm a gs l -> (f :&: a) (AnnTerm a gs) i -> m (AnnTerm a gs i)
  canInsertAt' :: NodeEvaluationPoint -> Proxy l -> (f :&: a) (AnnTerm a gs) i -> Bool

instance {-# OVERLAPPABLE #-} (f :-<: gs, DynCase (Term gs) l) => InsertAt' gs l f where
  insertAt'    _ _ t = return $ inject' t
  canInsertAt' _ _ _ = False

instance {-# OVERLAPPING #-} (ListF :-<: gs, All HFunctor gs, KDynCase (Sum gs) l, KDynCase (Sum gs) [l], Typeable l) => InsertAt' gs l ListF where
  insertAt' EnterEvalPoint e t = case kdyncase t :: Maybe (_ :~: [l]) of
                                   Nothing -> return $ inject' t
                                   Just p  -> gcastWith p $ liftM inject' $ annM $ ConsF e (inject' t)
  insertAt' _              _ t = return $ inject' t

  canInsertAt' EnterEvalPoint _ = kIsSort (Proxy :: Proxy [l])
  canInsertAt' _              _ = const False

instance {-# OVERLAPPING #-} (All (InsertAt' gs l) fs, DynCase (Term gs) l) => InsertAt' gs l (Sum fs) where
  insertAt' p e = caseCxt' (Proxy @(InsertAt' gs l)) (insertAt' p e)
  canInsertAt' p e = caseCxt' (Proxy @(InsertAt' gs l)) (canInsertAt' p e)

class InsertAt gs l where
  insertAt    :: (MonadAnnotater a m) => NodeEvaluationPoint -> AnnTerm a gs l -> AnnTerm a gs i -> m (AnnTerm a gs i)
  canInsertAt :: NodeEvaluationPoint -> Proxy l -> AnnTerm a gs i -> Bool

-- TODO: When this constraint is replace with All, it asks for a bunch of other constraints.
instance (InsertAt' gs l (Sum gs)) => InsertAt gs l where
 insertAt p e t       = insertAt' p e (unTerm t)
 canInsertAt p prox t = canInsertAt' p prox (unTerm t)


insertBefore :: (InsertAt gs l, MonadAnnotater a m) => AnnTerm a gs l -> AnnTerm a gs i -> m (AnnTerm a gs i)
insertBefore = insertAt EnterEvalPoint

canInsertBefore :: (InsertAt gs l) => Proxy l -> AnnTerm a gs i -> Bool
canInsertBefore = canInsertAt EnterEvalPoint
