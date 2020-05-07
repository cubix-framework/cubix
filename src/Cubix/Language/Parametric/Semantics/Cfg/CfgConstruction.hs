{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cubix.Language.Parametric.Semantics.Cfg.CfgConstruction (
    collapseFProd
  , collapseFProd'
  , fprodFst'
  , EnterExitPair(..)
  , extractEEPList
  , extractEEPPair
  , extractEEPMaybe
  , identEnterExit
  , combineEnterExit
  , collapseEnterExit

  , ComputationSorts
  , SuspendedComputationSorts
  , ContainerFunctors
  , PreRAlg
  , ConstructCfg(..)
  , HState(..)
  , runSubCfgs
  , CfgComponent
  , constructCfgGeneric
  , constructCfgDefault

  , CfgState
  , CfgInitState(..)
  , CfgBuilder
  , makeCfg
 ) where


import Control.Monad.State ( MonadState, State, execState )
import Data.Proxy ( Proxy(..) )
import Data.Typeable ( Typeable )

import Control.Lens ( (^.), (%=)  )

import Data.Comp.Multi ( Cxt(..), (:->), K(..), inj, proj, project, para, stripA, inj, Sum, (:<:), HFunctor(..), HFoldable(..), HTraversable(..), (:*:)(..), (:&:)(..), ffst, fsnd, ShowHF(..), inj', caseCxt', All, HFix, AnnHFix )
import Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )

import Cubix.Language.Info
import Cubix.Language.Parametric.Semantics.Cfg.Graph
import Cubix.Language.Parametric.Syntax.Functor

--------------------------------------------------------------------------------------

type family ComputationSorts (f :: (* -> *) -> * -> *) :: [*]

class IsComputationSort' f (l :: [*]) where
  isComputationSort' :: Proxy l -> HFix f i -> Bool

class IsComputationSort f where
  isComputationSort :: HFix f i -> Bool

instance (IsComputationSort' f (ComputationSorts f)) => IsComputationSort f where
  isComputationSort = isComputationSort' (Proxy :: Proxy (ComputationSorts f))

instance IsComputationSort' f '[] where
  isComputationSort' _ = const False

instance (IsComputationSort' f ls, DynCase (HFix f) l) => IsComputationSort' f (l ': ls) where
  isComputationSort' _ t = (isSort (Proxy :: Proxy l) t) || (isComputationSort' (Proxy :: Proxy ls) t)

labeledIsComputationSort :: forall f f' l. (IsComputationSort f, HFunctor f) => HFixLab f l -> Bool
labeledIsComputationSort = isComputationSort . stripA

type family SuspendedComputationSorts (f :: (* -> *) -> * -> *) :: [*]

class IsSuspendedComputationSort' f (l :: [*]) where
  isSuspendedComputationSort' :: Proxy l -> HFix f i -> Bool

class IsSuspendedComputationSort f where
  isSuspendedComputationSort :: HFix f i -> Bool

instance (IsSuspendedComputationSort' f (SuspendedComputationSorts f)) => IsSuspendedComputationSort f where
  isSuspendedComputationSort = isSuspendedComputationSort' (Proxy :: Proxy (SuspendedComputationSorts f))

instance IsSuspendedComputationSort' f '[] where
  isSuspendedComputationSort' _ = const False

instance (IsSuspendedComputationSort' f ls, DynCase (HFix f) l) => IsSuspendedComputationSort' f (l ': ls) where
  isSuspendedComputationSort' _ t = (isSort (Proxy :: Proxy l) t) || (isSuspendedComputationSort' (Proxy :: Proxy ls) t)

labeledIsSuspendedComputationSort :: (IsSuspendedComputationSort f, HFunctor f) => AnnHFix a f l -> Bool
labeledIsSuspendedComputationSort = isSuspendedComputationSort . stripA

type family ContainerFunctors (f :: (* -> *) -> * -> *) :: [(* -> *) -> * -> *]

class IsContainer' f (l :: [(* -> *) -> * -> *])where
  isContainer' :: Proxy l -> HFix f i -> Bool

class IsContainer f where
  isContainer :: HFix f i -> Bool

instance (IsContainer' f (ContainerFunctors f)) => IsContainer f where
  isContainer = isContainer' (Proxy :: Proxy (ContainerFunctors f))

instance IsContainer' f '[] where
  isContainer' _ = const False

instance (IsContainer' f ls, l :<: f) => IsContainer' f (l ': ls) where
  isContainer' _ t = case (project t :: Maybe (l _ _)) of
                       Just _  -> True
                       Nothing -> isContainer' (Proxy :: Proxy ls) t

labeledIsContainer :: (IsContainer f, HFunctor f) => AnnHFix a f l -> Bool
labeledIsContainer = isContainer . stripA


data EnterExitPair f l = EnterExitPair { enter :: CfgNode f
                                       , exit  :: CfgNode f
                                       }
                       | SubPairs (f (EnterExitPair f) l) -- naming is hard
                       | EmptyEnterExit

instance (ShowHF f, HFunctor f) => Show (EnterExitPair f l) where
  show (EnterExitPair ent ex) = "(EnterExitPair " ++ show ent ++ " " ++ show ex ++ ")"
  show (SubPairs t) = "(SubPairs " ++ showHF' (hfmap (K . show) t) ++ ")"
  show EmptyEnterExit = "EmptyEnterExit"

mapEnterExitPair :: (HFunctor f) => (forall e i. f e i -> g e i) -> (EnterExitPair f l -> EnterExitPair g l)
mapEnterExitPair f EmptyEnterExit = EmptyEnterExit
mapEnterExitPair f (EnterExitPair n x) = EnterExitPair (mapCfgNode f n) (mapCfgNode f x)
mapEnterExitPair f (SubPairs t) = SubPairs $ f $ hfmap (mapEnterExitPair f) t

iSubPairs ::  (f' :<: f) => f' (EnterExitPair f) l -> EnterExitPair f l
iSubPairs x = SubPairs (inj x)

-- Had such confusing type errors with this
extractEEPList :: forall f l. (ListF :<: f, Typeable l) => EnterExitPair f [l] -> [EnterExitPair f l]
extractEEPList (SubPairs ((proj :: f _ _ -> Maybe (ListF _ _)) -> Just (ConsF x xs))) = x : extractEEPList xs
extractEEPList (SubPairs ((proj :: f _ _ -> Maybe (ListF _ _)) -> Just NilF))      = []

extractEEPPair :: forall f a b. (PairF :<: f) => EnterExitPair f (a,b) -> (EnterExitPair f a, EnterExitPair f b)
extractEEPPair (SubPairs ((proj :: f _ _ -> Maybe (PairF _ _)) -> Just (PairF x y))) = (x, y)

-- Yes, it's ugly that this has a different signature from the above. No, I don't have
-- time to care ATM
extractEEPMaybe :: (KExtractF' Maybe f, Monad m) => m (EnterExitPair f (Maybe l)) -> m (Maybe (EnterExitPair f l))
extractEEPMaybe m = do
  p' <- m
  let SubPairs p = p'
  return $ kextractF' p


identEnterExit :: CfgNode f -> EnterExitPair f l
identEnterExit n = EnterExitPair n n

collapseEnterExit :: (HasCurCfg s f, HTraversable f, MonadState s m) => EnterExitPair f i -> m (EnterExitPair f j)
collapseEnterExit p@(EnterExitPair n x) = return $ EnterExitPair n x
collapseEnterExit EmptyEnterExit = return EmptyEnterExit
collapseEnterExit (SubPairs subCfgs) =
    hfoldr (\k t -> t >>= combineEnterExit k) (return EmptyEnterExit) subCfgs

combineEnterExit :: (HasCurCfg s f, HTraversable f, MonadState s m) => EnterExitPair f i -> EnterExitPair f j -> m (EnterExitPair f k)
combineEnterExit p1 p2 = do
  p1' <- collapseEnterExit p1
  p2' <- collapseEnterExit p2
  case p1' of
    EmptyEnterExit                    -> return p2'
    EnterExitPair {enter=n1, exit=x1} -> case p2' of
      EmptyEnterExit                    -> return p1'
      EnterExitPair {enter=n2, exit=x2} -> do
          cur_cfg %= addEdge x1 n2
          return $ EnterExitPair n1 x2

collapseFProdG :: (HFunctor f, f :<: g) => f (HFix g :*: t) :-> HFix g :*: f t
collapseFProdG t = (Term $ inj $ hfmap ffst t) :*: (hfmap fsnd t)

collapseFProd :: (HFunctor f, f :<: g) => f (HFix g :*: t) :-> HFix g :*: f t
collapseFProd = collapseFProdG

collapseFProd' :: (HFunctor f, f :<: g) => (f :&: a) (AnnHFix a g :*: t) :-> AnnHFix a g :*: f t
collapseFProd' t@(x :&: _) = (Term $ inj' $ hfmap ffst t) :*: (hfmap fsnd x)

fprodFst' :: (HFunctor f, f :<: g) => (f :&: a) (AnnHFix a g :*: t) :-> AnnHFix a g
fprodFst' (collapseFProd' -> t :*: _) = t

-- | PreRAlg's are things that can be modularly composed
-- to form R-algebras. R-algebras are in turn the building
-- block used to define paramorphisms, a recursion scheme
-- where each step can depend on both an entire term and the
-- previous results. Because the map of variables in scope
-- within a term depends both on the entire term and the
-- variables in scope in each subterm, this is a correct
-- recursion scheme to use.
--
-- @
-- PreRAlg f f a == RAlg f a
-- @
--
-- In @PreRAlg f g a@, @f@ is the signature functor of
-- the outer layer, while @g@ is the recursive signature
-- functor, and @a@ is the carrier of the R-algebra.
type PreRAlg f g a = f (HFix g :*: a) :-> a

data HState s f l = HState { unHState :: State s (f l) }

class ConstructCfg g s f where
  constructCfg :: PreRAlg (f :&: Label) (g :&: Label) (HState s (EnterExitPair g))

type CfgComponent g s = (HasLabelGen s, HasCurCfg s g, HTraversable g)
type SortChecks g = (IsComputationSort g, IsSuspendedComputationSort g, IsContainer g)

runSubCfgs :: (f :<: g, HTraversable f, CfgComponent g s) => f (HState s (EnterExitPair g)) i -> State s (EnterExitPair g j)
runSubCfgs subCfgs = do
  x <- hmapM unHState subCfgs
  collapseEnterExit $ SubPairs (inj x)

constructCfgGeneric :: forall f g s. (f :<: g, HTraversable f, CfgComponent g s) => PreRAlg (f :&: Label) (g :&: Label) (HState s (EnterExitPair g))
constructCfgGeneric (collapseFProd' -> (t :*: subCfgs)) = HState $ do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode

  body <- runSubCfgs subCfgs

  tmpNode <- combineEnterExit (identEnterExit enterNode) body
  combineEnterExit tmpNode (identEnterExit exitNode)

constructCfgDefault :: forall f g s. (f :<: g, HTraversable f, CfgComponent g s, SortChecks g) => PreRAlg (f :&: Label) (g :&: Label) (HState s (EnterExitPair g))
constructCfgDefault p@(collapseFProd' -> (t :*: subCfgs)) =
  if labeledIsComputationSort t then
    constructCfgGeneric p
  else if labeledIsSuspendedComputationSort t then
    HState $ do
      -- Connect children to each other, but don't connect them to surrounding context
      runSubCfgs subCfgs
      return EmptyEnterExit
  else if labeledIsContainer t then
    HState $ iSubPairs <$> hmapM unHState subCfgs -- by default, don't make node for containers
  else
    HState $ runSubCfgs subCfgs

instance {-# OVERLAPPABLE #-} (f :<: g, HTraversable f, CfgComponent g s, SortChecks g) => ConstructCfg g s f where
  constructCfg = constructCfgDefault

instance {-# OVERLAPPING #-} (All (ConstructCfg g s) fs) => ConstructCfg g s (Sum fs) where
  constructCfg = caseCxt' (Proxy @(ConstructCfg g s)) constructCfg


------------------------------------------------------------------------------------------------------------------------

type family CfgState (f :: (* -> *) -> * -> *) :: *

class CfgInitState f where
   cfgInitState :: Proxy f -> CfgState f

class    (CfgComponent f (CfgState f), ConstructCfg f (CfgState f) f, CfgInitState f, HFunctor f) => CfgBuilder f
instance (CfgComponent f (CfgState f), ConstructCfg f (CfgState f) f, CfgInitState f, HFunctor f) => CfgBuilder f

makeCfg :: forall f l. (CfgBuilder f) => HFixLab f l -> Cfg f
makeCfg t = (execState (unHState $ para constructCfg t) initState) ^. cur_cfg
  where
    initState = cfgInitState (Proxy :: Proxy f)
