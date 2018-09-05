{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Language.Parametric.Semantics.CfgInserter (
    EmptyInsertOkay(..)
  , MonadCfgInsertion(..)
  , CfgInserterT
  , performCfgInsertions
  ) where

import Control.Monad ( liftM )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.State ( MonadState, StateT, execStateT )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Maybe ( MaybeT )
import Control.Monad.Writer ( MonadWriter(..), WriterT(..) )

import Data.Foldable ( for_ )
import Data.Function ( on )
import Data.List ( sortOn, minimumBy )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Proxy ( Proxy(..) )

import Control.Lens ( makeLenses, use, (%=), (%%~), (^.) )

import Data.Comp.Multi ( E(..), runE, HFunctor, HTraversable )
import Data.Comp.Multi.Strategic ( GRewriteM, RewriteM, allbuR )

import Cubix.Language.Info
import Cubix.Language.Parametric.Path
import Cubix.Language.Parametric.ProgInfo
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Semantics.SemanticProperties

import Cubix.Sin.Compdata.Annotation ( MonadAnnotater, getAnn )

-- |
-- I initially wrote this module to use MonadState, but it conflicted with the already present MonadState instances
--
-- With little time left for coding, instead of figuring this out, I just went with a log construction and
-- switched to MonadWriter

-- Nothing to control what order these are applied in. Ideally, want operational transform maybe?
data InsertionOp f l = InsertionOp { _insert_op_eval_point :: NodeEvaluationPoint
                                   , _insert_op_node       :: TermLab f l
                                   }
makeLenses ''InsertionOp

data CfgInsertState f l = CfgInsertState {
                            _pendingInsertions :: Map Label [InsertionOp f l]
                          , _cis_proginf       :: ProgInfo f
                          }

makeLenses ''CfgInsertState

instance HasProgInfo (CfgInsertState f l) f where progInfo = cis_proginf
instance HasCurCfg (CfgInsertState f l) f where cur_cfg = cis_proginf.proginf_cfg

type CfgInserterT f l m = WriterT [Action f l] m

data EmptyInsertOkay = EmptyInsertOkay | EmptyInsertNotOkay

data Action f l = DominatingPrependFirst (E (TermLab f)) (TermLab f l) EmptyInsertOkay
                | DominatingPrependLast  (E (TermLab f)) (TermLab f l) EmptyInsertOkay
                | DominatingAppendFirst  (E (TermLab f)) (TermLab f l) EmptyInsertOkay
                | DominatingAppendLast   (E (TermLab f)) (TermLab f l) EmptyInsertOkay
                | FirstPredPrependLast   (E (TermLab f)) (TermLab f l) EmptyInsertOkay
                | RestPredPrependLast    (E (TermLab f)) (TermLab f l) EmptyInsertOkay


collapseMaybeList :: Maybe [a] -> [a]
collapseMaybeList = concat

dominatingInsert :: forall f l m i. (Monad m, HFunctor f, InsertAt f l) => Bool
                                                                        -> (forall a. a -> [a] -> [a])
                                                                        -> ([CfgNode f] -> StateT (CfgInsertState f l) m [CfgNode f])
                                                                        -> E (TermLab f)
                                                                        -> TermLab f l
                                                                        -> EmptyInsertOkay
                                                                        -> StateT (CfgInsertState f l) m ()
dominatingInsert isPrepend op filt t toInsert empOk = do
    cfg <- use cur_cfg
    let (Just tNode) = runE (cfgNodeForTerm cfg EnterNode) t
    let boundaryFunc = if isPrepend then satisfyingPredBoundary else satisfyingSuccBoundary
    case boundaryFunc canInsert cfg tNode of
      Nothing    -> case empOk of
                      EmptyInsertOkay   -> return ()
                      EmptyInsertNotOkay -> error $ "Cannot insert at node " ++ show (runE getAnn t) ++ " when asked to insert before " ++ (show (tNode ^. cfg_node_lab))
      Just prevs -> do
        prevsFilt <- filt prevs
        for_ prevsFilt $ \p -> do
          let lab = runE getAnn (p ^. cfg_node_term)
          insertions <- liftM (Map.lookup lab) (use pendingInsertions)
          let insertions' = op (InsertionOp (nodeTypeToEvalPoint (p ^. cfg_node_type)) toInsert) (collapseMaybeList insertions)
          pendingInsertions %= Map.insert lab insertions'
  where
    canInsert :: CfgNode f -> Bool
    canInsert n =  (runE (canInsertAt (nodeTypeToEvalPoint (n ^. cfg_node_type)) (Proxy :: Proxy l)) (n ^. cfg_node_term))


append :: a -> [a] -> [a]
append x l = l ++ [x]

trivFilt :: (Monad m) => [a] -> m [a]
trivFilt = return

firstNode :: (MonadState (CfgInsertState f l) m) => [CfgNode f] -> m [CfgNode f]
firstNode nodes = do
    progInfo <- use progInfo
    return $ [minimumBy (compare `on` (getPath progInfo)) nodes]

butFirstNode :: (MonadState (CfgInsertState f l) m) => [CfgNode f] -> m [CfgNode f]
butFirstNode nodes = do
    progInfo <- use progInfo
    return $ tail $ sortOn (getPath progInfo) nodes

getPath :: ProgInfo f -> CfgNode f -> Path
getPath inf node = case cfgNodePath inf node of
  Just p  -> p
  Nothing -> emptyPath

runAction :: (Monad m, InsertAt f l, HFunctor f) => Action f l -> StateT (CfgInsertState f l) m ()
runAction (DominatingPrependFirst l t emp) = dominatingInsert True  (:)    trivFilt     l t emp
runAction (DominatingPrependLast  l t emp) = dominatingInsert True  append trivFilt     l t emp
runAction (DominatingAppendFirst  l t emp) = dominatingInsert False (:)    trivFilt     l t emp
runAction (DominatingAppendLast   l t emp) = dominatingInsert False append trivFilt     l t emp
runAction (FirstPredPrependLast   l t emp) = dominatingInsert True  append firstNode    l t emp
runAction (RestPredPrependLast    l t emp) = dominatingInsert True  append butFirstNode l t emp


-- NOTE: I think this should be refactored so that there's just one kind of
-- append/prepend action, with many options
class (Monad m) => MonadCfgInsertion m f l where
  dominatingPrependFirstOpts :: TermLab f i -> TermLab f l -> EmptyInsertOkay -> m ()
  dominatingPrependLastOpts  :: TermLab f i -> TermLab f l -> EmptyInsertOkay -> m ()
  dominatingAppendFirstOpts  :: TermLab f i -> TermLab f l -> EmptyInsertOkay -> m ()
  dominatingAppendLastOpts   :: TermLab f i -> TermLab f l -> EmptyInsertOkay -> m ()
  firstPredPrependLastOpts   :: TermLab f i -> TermLab f l -> EmptyInsertOkay -> m ()
  restPredPrependLastOpts    :: TermLab f i -> TermLab f l -> EmptyInsertOkay -> m ()

  dominatingPrependFirst :: TermLab f i -> TermLab f l -> m ()
  dominatingPrependFirst t x = dominatingPrependFirstOpts t x EmptyInsertNotOkay

  dominatingPrependLast :: TermLab f i -> TermLab f l -> m ()
  dominatingPrependLast t x = dominatingPrependLastOpts t x EmptyInsertNotOkay

  dominatingAppendFirst :: TermLab f i -> TermLab f l -> m ()
  dominatingAppendFirst t x = dominatingAppendFirstOpts t x EmptyInsertNotOkay

  dominatingAppendLast :: TermLab f i -> TermLab f l -> m ()
  dominatingAppendLast t x = dominatingAppendLastOpts t x EmptyInsertNotOkay

  firstPredPrependLast :: TermLab f i -> TermLab f l -> m ()
  firstPredPrependLast t x = firstPredPrependLastOpts t x EmptyInsertNotOkay

  restPredPrependLast :: TermLab f i -> TermLab f l -> m ()
  restPredPrependLast t x = restPredPrependLastOpts t x EmptyInsertNotOkay


instance (Monad m) => MonadCfgInsertion (CfgInserterT f l m) f l where
  dominatingPrependFirstOpts t x emp = tell [DominatingPrependFirst (E t) x emp]
  dominatingPrependLastOpts  t x emp = tell [DominatingPrependLast  (E t) x emp]
  dominatingAppendFirstOpts  t x emp = tell [DominatingAppendFirst  (E t) x emp]
  dominatingAppendLastOpts   t x emp = tell [DominatingAppendLast   (E t) x emp]
  firstPredPrependLastOpts   t x emp = tell [FirstPredPrependLast   (E t) x emp]
  restPredPrependLastOpts    t x emp = tell [RestPredPrependLast    (E t) x emp]

instance (MonadCfgInsertion m f l) => MonadCfgInsertion (MaybeT m) f l where
  dominatingPrependFirstOpts t x emp = lift $ dominatingPrependFirstOpts t x emp
  dominatingPrependLastOpts  t x emp = lift $ dominatingPrependLastOpts  t x emp
  dominatingAppendFirstOpts  t x emp = lift $ dominatingAppendFirstOpts  t x emp
  dominatingAppendLastOpts   t x emp = lift $ dominatingAppendLastOpts   t x emp
  firstPredPrependLastOpts   t x emp = lift $ firstPredPrependLastOpts   t x emp
  restPredPrependLastOpts    t x emp = lift $ restPredPrependLastOpts    t x emp

instance (MonadCfgInsertion m f l) => MonadCfgInsertion (ReaderT s m) f l where
  dominatingPrependFirstOpts t x emp = lift $ dominatingPrependFirstOpts t x emp
  dominatingPrependLastOpts  t x emp = lift $ dominatingPrependLastOpts  t x emp
  dominatingAppendFirstOpts  t x emp = lift $ dominatingAppendFirstOpts  t x emp
  dominatingAppendLastOpts   t x emp = lift $ dominatingAppendLastOpts   t x emp
  firstPredPrependLastOpts   t x emp = lift $ firstPredPrependLastOpts   t x emp
  restPredPrependLastOpts    t x emp = lift $ restPredPrependLastOpts    t x emp

finalizeInsertions :: forall f m l. (InsertAt f l, MonadAnnotater Label m, HTraversable f) => Map Label [InsertionOp f l] -> GRewriteM m (TermLab f)
finalizeInsertions insertMap t = foldr (\(InsertionOp p x) r -> r >>= insertAt p x) (return t) =<< insertions
  where
    insertions :: m [InsertionOp f l]
    insertions = do
      let raw_list = collapseMaybeList $ Map.lookup (getAnn t) insertMap

      -- Some things to be inserted will have their own insertions
      normalized <- mapM (insert_op_node %%~ (allbuR (finalizeInsertions insertMap))) raw_list

      -- For each eval point, do insertions in the order given. But, do the later eval points first.
      --
      -- We assume sortOn is a stable sort. The documentation is kinda weird on this point: it promises that sort
      -- is stable, and that sortOn is closely related to sort, but doesn't explicitly say that sortBy is stable,
      -- though it is today.
      -- I'll rely on the fact that there are many, many users of sortBy to constrain them to keep it stable.
      --
      -- This code makes a small sin: it assumes that repeated InsertAt's work better when applied back-to-front,
      -- which is not in the interface. But, maybe it should be: both current uses of inserting into the middle of
      -- a statement have this property for the same reason: inserting into point p preserves everything that happens
      -- before p.
      let sorted = sortOn (^. insert_op_eval_point) normalized
      return sorted


performCfgInsertions :: (MonadAnnotater Label m, InsertAt f l, HTraversable f) => Proxy l -> ProgInfo f -> RewriteM (CfgInserterT f l m) (TermLab f) i -> RewriteM m (TermLab f) i
performCfgInsertions _ proginf f t = do
   (t', actions) <- runWriterT (f t)
   s <- execStateT (mapM_ runAction actions) (CfgInsertState Map.empty proginf)
   allbuR (finalizeInsertions (s ^. pendingInsertions)) t'
