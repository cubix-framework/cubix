{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

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

import Data.Comp.Multi ( E(..), runE, HFunctor, HTraversable, HFoldable, All )
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
data InsertionOp fs l = InsertionOp { _insert_op_eval_point :: NodeEvaluationPoint
                                    , _insert_op_node       :: TermLab fs l
                                    }
makeLenses ''InsertionOp

data CfgInsertState fs l = CfgInsertState {
                            _pendingInsertions :: Map Label [InsertionOp fs l]
                          , _cis_proginf       :: ProgInfo fs
                          }

makeLenses ''CfgInsertState

instance HasProgInfo (CfgInsertState fs l) fs where progInfo = cis_proginf
instance HasCurCfg (CfgInsertState fs l) fs where cur_cfg = cis_proginf.proginf_cfg

type CfgInserterT fs l m = WriterT [Action fs l] m

data EmptyInsertOkay = EmptyInsertOkay | EmptyInsertNotOkay

data Action fs l = DominatingPrependFirst (E (TermLab fs)) (TermLab fs l) EmptyInsertOkay
                 | DominatingPrependLast  (E (TermLab fs)) (TermLab fs l) EmptyInsertOkay
                 | DominatingAppendFirst  (E (TermLab fs)) (TermLab fs l) EmptyInsertOkay
                 | DominatingAppendLast   (E (TermLab fs)) (TermLab fs l) EmptyInsertOkay
                 | FirstPredPrependLast   (E (TermLab fs)) (TermLab fs l) EmptyInsertOkay
                 | RestPredPrependLast    (E (TermLab fs)) (TermLab fs l) EmptyInsertOkay


collapseMaybeList :: Maybe [a] -> [a]
collapseMaybeList = concat

dominatingInsert ::
  forall fs l m.
  ( Monad m
  , All HFunctor fs
  , InsertAt fs l
  ) => Bool
  -> (forall a. a -> [a] -> [a])
  -> ([CfgNode fs] -> StateT (CfgInsertState fs l) m [CfgNode fs])
  -> E (TermLab fs)
  -> TermLab fs l
  -> EmptyInsertOkay
  -> StateT (CfgInsertState fs l) m ()
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
    canInsert :: CfgNode fs -> Bool
    canInsert n =  (runE (canInsertAt (nodeTypeToEvalPoint (n ^. cfg_node_type)) (Proxy :: Proxy l)) (n ^. cfg_node_term))


append :: a -> [a] -> [a]
append x l = l ++ [x]

trivFilt :: (Monad m) => [a] -> m [a]
trivFilt = return

firstNode :: (MonadState (CfgInsertState fs l) m) => [CfgNode fs] -> m [CfgNode fs]
firstNode nodes = do
    progInfo <- use progInfo
    return $ [minimumBy (compare `on` (getPath progInfo)) nodes]

butFirstNode :: (MonadState (CfgInsertState fs l) m) => [CfgNode fs] -> m [CfgNode fs]
butFirstNode nodes = do
    progInfo <- use progInfo
    return $ tail $ sortOn (getPath progInfo) nodes

getPath :: ProgInfo fs -> CfgNode fs -> Path
getPath inf node = case cfgNodePath inf node of
  Just p  -> p
  Nothing -> emptyPath

runAction :: (Monad m, InsertAt fs l, All HFunctor fs) => Action fs l -> StateT (CfgInsertState fs l) m ()
runAction (DominatingPrependFirst l t emp) = dominatingInsert True  (:)    trivFilt     l t emp
runAction (DominatingPrependLast  l t emp) = dominatingInsert True  append trivFilt     l t emp
runAction (DominatingAppendFirst  l t emp) = dominatingInsert False (:)    trivFilt     l t emp
runAction (DominatingAppendLast   l t emp) = dominatingInsert False append trivFilt     l t emp
runAction (FirstPredPrependLast   l t emp) = dominatingInsert True  append firstNode    l t emp
runAction (RestPredPrependLast    l t emp) = dominatingInsert True  append butFirstNode l t emp


-- NOTE: I think this should be refactored so that there's just one kind of
-- append/prepend action, with many options
class (Monad m) => MonadCfgInsertion m fs l where
  dominatingPrependFirstOpts :: TermLab fs i -> TermLab fs l -> EmptyInsertOkay -> m ()
  dominatingPrependLastOpts  :: TermLab fs i -> TermLab fs l -> EmptyInsertOkay -> m ()
  dominatingAppendFirstOpts  :: TermLab fs i -> TermLab fs l -> EmptyInsertOkay -> m ()
  dominatingAppendLastOpts   :: TermLab fs i -> TermLab fs l -> EmptyInsertOkay -> m ()
  firstPredPrependLastOpts   :: TermLab fs i -> TermLab fs l -> EmptyInsertOkay -> m ()
  restPredPrependLastOpts    :: TermLab fs i -> TermLab fs l -> EmptyInsertOkay -> m ()


  -- | Insert a node in places that dominate all control paths to some other node
  dominatingPrependFirst :: TermLab fs i -- location to perform insertion
                         -> TermLab fs l -- node to insert
                         -> m ()
  dominatingPrependFirst t x = dominatingPrependFirstOpts t x EmptyInsertNotOkay

  dominatingPrependLast :: TermLab fs i -> TermLab fs l -> m ()
  dominatingPrependLast t x = dominatingPrependLastOpts t x EmptyInsertNotOkay

  dominatingAppendFirst :: TermLab fs i -> TermLab fs l -> m ()
  dominatingAppendFirst t x = dominatingAppendFirstOpts t x EmptyInsertNotOkay

  dominatingAppendLast :: TermLab fs i -> TermLab fs l -> m ()
  dominatingAppendLast t x = dominatingAppendLastOpts t x EmptyInsertNotOkay

  firstPredPrependLast :: TermLab fs i -> TermLab fs l -> m ()
  firstPredPrependLast t x = firstPredPrependLastOpts t x EmptyInsertNotOkay

  restPredPrependLast :: TermLab fs i -> TermLab fs l -> m ()
  restPredPrependLast t x = restPredPrependLastOpts t x EmptyInsertNotOkay


instance (Monad m) => MonadCfgInsertion (CfgInserterT fs l m) fs l where
  dominatingPrependFirstOpts t x emp = tell [DominatingPrependFirst (E t) x emp]
  dominatingPrependLastOpts  t x emp = tell [DominatingPrependLast  (E t) x emp]
  dominatingAppendFirstOpts  t x emp = tell [DominatingAppendFirst  (E t) x emp]
  dominatingAppendLastOpts   t x emp = tell [DominatingAppendLast   (E t) x emp]
  firstPredPrependLastOpts   t x emp = tell [FirstPredPrependLast   (E t) x emp]
  restPredPrependLastOpts    t x emp = tell [RestPredPrependLast    (E t) x emp]

instance (MonadCfgInsertion m fs l) => MonadCfgInsertion (MaybeT m) fs l where
  dominatingPrependFirstOpts t x emp = lift $ dominatingPrependFirstOpts t x emp
  dominatingPrependLastOpts  t x emp = lift $ dominatingPrependLastOpts  t x emp
  dominatingAppendFirstOpts  t x emp = lift $ dominatingAppendFirstOpts  t x emp
  dominatingAppendLastOpts   t x emp = lift $ dominatingAppendLastOpts   t x emp
  firstPredPrependLastOpts   t x emp = lift $ firstPredPrependLastOpts   t x emp
  restPredPrependLastOpts    t x emp = lift $ restPredPrependLastOpts    t x emp

instance (MonadCfgInsertion m fs l) => MonadCfgInsertion (ReaderT s m) fs l where
  dominatingPrependFirstOpts t x emp = lift $ dominatingPrependFirstOpts t x emp
  dominatingPrependLastOpts  t x emp = lift $ dominatingPrependLastOpts  t x emp
  dominatingAppendFirstOpts  t x emp = lift $ dominatingAppendFirstOpts  t x emp
  dominatingAppendLastOpts   t x emp = lift $ dominatingAppendLastOpts   t x emp
  firstPredPrependLastOpts   t x emp = lift $ firstPredPrependLastOpts   t x emp
  restPredPrependLastOpts    t x emp = lift $ restPredPrependLastOpts    t x emp

finalizeInsertions ::
  forall fs m l.
  ( InsertAt fs l
  , MonadAnnotater Label m
  , All HTraversable fs
  , All HFoldable fs
  , All HFunctor fs
  ) => Map Label [InsertionOp fs l] -> GRewriteM m (TermLab fs)
finalizeInsertions insertMap t = foldr (\(InsertionOp p x) r -> r >>= insertAt p x) (return t) =<< insertions
  where
    insertions :: m [InsertionOp fs l]
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


performCfgInsertions ::
  forall l fs m i.
  ( MonadAnnotater Label m
  , InsertAt fs l
  , All HTraversable fs
  , All HFunctor fs
  , All HFoldable fs
  ) => ProgInfo fs -> RewriteM (CfgInserterT fs l m) (TermLab fs) i -> RewriteM m (TermLab fs) i
performCfgInsertions proginf f t = do
   (t', actions) <- runWriterT (f t)
   s <- execStateT (mapM_ runAction actions) (CfgInsertState Map.empty proginf)
   allbuR (finalizeInsertions (s ^. pendingInsertions)) t'
