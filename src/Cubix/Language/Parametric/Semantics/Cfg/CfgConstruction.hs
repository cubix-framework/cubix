{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

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

  , labeledIsComputationSort
  , labeledIsSuspendedComputationSort
  , labeledIsContainer

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

import Data.Comp.Multi ( Node, Signature, Cxt(..), (:->), K(..), inj, proj, project, para, stripA, inj, Sum, HFunctor(..), HFoldable(..), HTraversable(..), (:*:)(..), (:&:)(..), ffst, fsnd, ShowHF(..), inj', caseCxt', All, HFix, AnnTerm, Term, (:-<:) )
import Data.Comp.Multi.Strategy.Classification ( DynCase, isSort, hasAnySort )

import Cubix.Language.Info
import Cubix.Language.Parametric.Semantics.Cfg.Graph
import Cubix.Language.Parametric.Syntax.Functor

--------------------------------------------------------------------------------------


------------------------------------------------------------------
--------------------------- Sort checking ------------------------
------------------------------------------------------------------

---------
---- Computation Sorts
---------

type family ComputationSorts (fs :: Signature) :: [*]

type IsComputationSort fs = All (DynCase (Term fs)) (ComputationSorts fs)

isComputationSort :: forall fs l. (IsComputationSort fs) => Term fs l -> Bool
isComputationSort = hasAnySort @(ComputationSorts fs)

labeledIsComputationSort :: forall fs l. (IsComputationSort fs, All HFunctor fs) => TermLab fs l -> Bool
labeledIsComputationSort = isComputationSort . stripA

---------
---- Suspended Computation Sorts
---------

type family SuspendedComputationSorts (fs :: Signature) :: [*]

type IsSuspendedComputationSort fs = All (DynCase (Term fs)) (SuspendedComputationSorts fs)

isSuspendedComputationSort :: forall fs l. (IsSuspendedComputationSort fs) => Term fs l -> Bool
isSuspendedComputationSort = hasAnySort @(SuspendedComputationSorts fs)

labeledIsSuspendedComputationSort :: (IsSuspendedComputationSort fs, All HFunctor fs) => AnnTerm a fs l -> Bool
labeledIsSuspendedComputationSort = isSuspendedComputationSort . stripA

---------
---- Container Functors
---------

type family ContainerFunctors (fs :: Signature) :: [Node]

class IsContainer' fs (l :: Signature)where
  isContainer' :: Proxy l -> Term fs i -> Bool

class IsContainer fs where
  isContainer :: Term fs i -> Bool

instance (IsContainer' fs (ContainerFunctors fs)) => IsContainer fs where
  isContainer = isContainer' (Proxy @(ContainerFunctors fs))

instance IsContainer' fs '[] where
  isContainer' _ = const False

instance (IsContainer' fs ls, l :-<: fs) => IsContainer' fs (l ': ls) where
  isContainer' _ t = case go t of
                       Just _  -> True
                       Nothing -> isContainer' (Proxy @ls) t

    where go :: forall lab. Term fs lab -> Maybe (l (Term fs) lab)
          go = project

labeledIsContainer :: (IsContainer fs, All HFunctor fs) => AnnTerm a fs l -> Bool
labeledIsContainer = isContainer . stripA

------------------------------------------------------------------
--------------------------- Uncategorized ------------------------
------------------------------------------------------------------


data EnterExitPair fs l = EnterExitPair { enter :: CfgNode fs
                                        , exit  :: CfgNode fs
                                        }
                       | SubPairs (Sum fs (EnterExitPair fs) l) -- naming is hard
                       | EmptyEnterExit

instance (All ShowHF fs, All HFunctor fs) => Show (EnterExitPair fs l) where
  show (EnterExitPair ent ex) = "(EnterExitPair " ++ show ent ++ " " ++ show ex ++ ")"
  show (SubPairs t) = "(SubPairs " ++ showHF' (hfmap (K . show) t) ++ ")"
  show EmptyEnterExit = "EmptyEnterExit"

mapEnterExitPair :: (All HFunctor fs) => (forall e i. Sum fs e i -> Sum gs e i) -> (EnterExitPair fs l -> EnterExitPair gs l)
mapEnterExitPair f EmptyEnterExit = EmptyEnterExit
mapEnterExitPair f (EnterExitPair n x) = EnterExitPair (mapCfgNode f n) (mapCfgNode f x)
mapEnterExitPair f (SubPairs t) = SubPairs $ f $ hfmap (mapEnterExitPair f) t

iSubPairs ::  (f' :-<: fs) => f' (EnterExitPair fs) l -> EnterExitPair fs l
iSubPairs x = SubPairs (inj x)

-- Had such confusing type errors with this
extractEEPList :: forall fs l. (ListF :-<: fs, Typeable l) => EnterExitPair fs [l] -> [EnterExitPair fs l]
extractEEPList (SubPairs ps) =
  case go ps of
    Just (ConsF x xs) -> x : extractEEPList xs
    Just NilF         -> []

  where go :: Sum fs e [l] -> Maybe (ListF e [l])
        go = proj

extractEEPPair :: forall fs a b. (PairF :-<: fs) => EnterExitPair fs (a,b) -> (EnterExitPair fs a, EnterExitPair fs b)
extractEEPPair (SubPairs ps) =
  case go ps of
    Just (PairF x y) -> (x, y)

  where go :: Sum fs e (a, b) -> Maybe (PairF e (a, b))
        go = proj

-- Yes, it's ugly that this has a different signature from the above. No, I don't have
-- time to care ATM
extractEEPMaybe :: (All (KExtractF' Maybe) fs, Monad m) => m (EnterExitPair fs (Maybe l)) -> m (Maybe (EnterExitPair fs l))
extractEEPMaybe m = do
  p' <- m
  let SubPairs p = p'
  return $ kextractF' p


identEnterExit :: CfgNode fs -> EnterExitPair fs l
identEnterExit n = EnterExitPair n n

collapseEnterExit ::
  ( HasCurCfg s fs
  , All HTraversable fs
  , All HFoldable fs
  , All HFunctor fs
  , MonadState s m
  ) => EnterExitPair fs i -> m (EnterExitPair fs j)
collapseEnterExit p@(EnterExitPair n x) = return $ EnterExitPair n x
collapseEnterExit EmptyEnterExit = return EmptyEnterExit
collapseEnterExit (SubPairs subCfgs) =
    hfoldr (\k t -> t >>= combineEnterExit k) (return EmptyEnterExit) subCfgs

combineEnterExit ::
  ( HasCurCfg s fs
  , All HTraversable fs
  , All HFoldable fs
  , All HFunctor fs  
  , MonadState s m
  ) => EnterExitPair fs i -> EnterExitPair fs j -> m (EnterExitPair fs k)
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

collapseFProdG :: (HFunctor f, f :-<: gs) => f (Term gs :*: t) :-> Term gs :*: f t
collapseFProdG t = (Term $ inj $ hfmap ffst t) :*: (hfmap fsnd t)

collapseFProd :: (HFunctor f, f :-<: gs) => f (Term gs :*: t) :-> Term gs :*: f t
collapseFProd = collapseFProdG

collapseFProd' :: (HFunctor f, f :-<: gs) => (f :&: a) (AnnTerm a gs :*: t) :-> AnnTerm a gs :*: f t
collapseFProd' t@(x :&: _) = (Term $ inj' $ hfmap ffst t) :*: (hfmap fsnd x)

fprodFst' :: (HFunctor f, f :-<: gs) => (f :&: a) (AnnTerm a gs :*: t) :-> AnnTerm a gs
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

class ConstructCfg gs s f where
  constructCfg :: PreRAlg (f :&: Label) (Sum gs :&: Label) (HState s (EnterExitPair gs))

type CfgComponent gs s = (HasLabelGen s, HasCurCfg s gs, All HTraversable gs, All HFoldable gs, All HFunctor gs)
type SortChecks gs = (IsComputationSort gs, IsSuspendedComputationSort gs, IsContainer gs)

runSubCfgs ::
  ( f :-<: gs
  , HTraversable f
  , CfgComponent gs s
  ) => f (HState s (EnterExitPair gs)) i -> State s (EnterExitPair gs j)
runSubCfgs subCfgs = do
  x <- hmapM unHState subCfgs
  collapseEnterExit $ SubPairs (inj x)

constructCfgGeneric ::
  forall f gs s.
  ( f :-<: gs
  , HTraversable f
  , CfgComponent gs s
  ) => PreRAlg (f :&: Label) (Sum gs :&: Label) (HState s (EnterExitPair gs))
constructCfgGeneric (collapseFProd' -> (t :*: subCfgs)) = HState $ do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode

  body <- runSubCfgs subCfgs

  tmpNode <- combineEnterExit (identEnterExit enterNode) body
  combineEnterExit tmpNode (identEnterExit exitNode)

constructCfgDefault ::
  forall f gs s.
  ( f :-<: gs
  , HTraversable f
  , CfgComponent gs s
  , SortChecks gs
  ) => PreRAlg (f :&: Label) (Sum gs :&: Label) (HState s (EnterExitPair gs))
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

instance {-# OVERLAPPABLE #-}
  ( f :-<: gs
  , HTraversable f
  , CfgComponent gs s
  , SortChecks gs
  ) => ConstructCfg gs s f where
  constructCfg = constructCfgDefault

instance {-# OVERLAPPING #-} (All (ConstructCfg g s) fs) => ConstructCfg g s (Sum fs) where
  constructCfg = caseCxt' @(ConstructCfg g s) constructCfg


------------------------------------------------------------------------------------------------------------------------

type family CfgState (fs :: Signature) :: *

class CfgInitState fs where
   cfgInitState :: Proxy fs -> CfgState fs

class    (CfgComponent fs (CfgState fs), ConstructCfg fs (CfgState fs) (Sum fs), CfgInitState fs) => CfgBuilder fs
instance (CfgComponent fs (CfgState fs), ConstructCfg fs (CfgState fs) (Sum fs), CfgInitState fs) => CfgBuilder fs

-- | Constructs a CFG for the given labelled term
makeCfg :: forall fs l. (CfgBuilder fs) => TermLab fs l -> Cfg fs
makeCfg t = (execState (unHState $ para constructCfg t) initState) ^. cur_cfg
  where
    initState = cfgInitState (Proxy @fs)
