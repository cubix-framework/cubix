{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cubix.Language.Lua.Parametric.Common.Cfg () where

import Control.Monad
import Control.Monad.State ( State, MonadState )
import qualified Data.List as List
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Proxy
import Data.Typeable ( Typeable )

import Control.Lens ( (%=), makeLenses, Lens', to, use, (^..), (^.), (&), (%~), (.=), over )

import Data.Comp.Multi ( project, project', stripA, remA, (:-<:) )
import Data.Comp.Multi.Ops ( Sum, (:*:)(..), ffst )

import Cubix.Language.Info

import Cubix.Language.Lua.Parametric.Common.Types as C
import Cubix.Language.Lua.Parametric.Full.Types as F
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Syntax as P

-----------------------------------------------------------------------------------
---------------           Labelling mechanism              ------------------------
-----------------------------------------------------------------------------------


-- For Lua - quoting ref:
-- A label is visible in the entire block where it is defined, except inside nested blocks where a label with the same name is defined and inside nested functions. A goto may jump to any visible label as long as it does not enter into the scope of a local variable.

-- So we push a new stack of label map or Fun marker whenever we enter a new block or a function respectively


data LuaLabelMap = Fun
                 | BlockLabelMap { _block_label_map :: LabelMap }
                 deriving ( Eq, Ord, Show)

data LuaLabelMapStack = LuaLabelMapStack { _label_map_stack :: [LuaLabelMap]
                                         } deriving ( Eq, Ord, Show)

makeLenses ''LuaLabelMapStack


data LuaCfgState = LuaCfgState {
                   _lcs_cfg       :: Cfg MLuaSig
                 , _lcs_labeler   :: LabelGen
                 , _lcs_stack     :: LoopStack
                 , _lcs_goto_labs :: LuaLabelMapStack
                 }

makeLenses ''LuaCfgState

withBlockLabelMap :: (MonadState LuaCfgState m) => m a -> m a
withBlockLabelMap mee = do
  pushBlockLabelMapStack emptyLabelMap
  ee <- mee
  lm <- use labelMap
  popLabelMapStack
  mergeLabelMap lm
  pure ee

pushFunLabelMapStack :: (MonadState LuaCfgState m) => m ()
pushFunLabelMapStack =
  lcs_goto_labs.label_map_stack %= (Fun :)

pushBlockLabelMapStack :: (MonadState LuaCfgState m) => LabelMap -> m ()
pushBlockLabelMapStack m =
  lcs_goto_labs.label_map_stack %= (:) (BlockLabelMap m)

popLabelMapStack :: (MonadState LuaCfgState m) => m ()
popLabelMapStack =
  lcs_goto_labs.label_map_stack %= tail

emptyLabelMapStack :: LuaLabelMapStack
emptyLabelMapStack = LuaLabelMapStack []

-- NOTE: Merges the top of the stack with the subsequent
--       element in the stack, adding in the unresolved labels in the top
mergeLabelMap :: (MonadState LuaCfgState m) => LabelMap -> m ()
mergeLabelMap x = do
  lmstack <- use (lcs_goto_labs.label_map_stack)
  case lmstack of
    (y : _) -> do
      case y of
        BlockLabelMap y0 -> do
          y1 <- go (x ^. label_map) y0
          labelMap.label_map .= y1
        Fun -> pure ()
    [] -> pure ()

  where go x0 y0 =
          Map.foldrWithKey go0 (pure (y0 ^. label_map)) x0

        go0 _ (_, []) b = b
        go0 k (a, vs) b = do
          b0 <- b
          case Map.lookup k b0 of
            Nothing -> pure (Map.insert k (a, vs) b0)
            Just (a0, vs0) -> case vs0 of
              [] -> do
                forM_ vs $ \p -> cur_cfg %= addEdgeLab (Proxy :: Proxy MLuaSig) p a0
                pure b0
              _  -> do
                pure (Map.insertWith go2 k (a, vs) b0)

        go2 (_, vs0) (a1, vs1) = (a1, vs0 ++ vs1)

-- Unsafe constructs
-- Taken from Data-List-Lens
_head :: Lens' [a] a
_head _ [] = error "_head: empty list"
_head f (a:as) = (:as) <$> f a
{-# INLINE _head #-}

block_label_map :: Lens' LuaLabelMap LabelMap
block_label_map _ Fun = error "block_label_map: unexpected Fun"
block_label_map f (BlockLabelMap a) = BlockLabelMap <$> f a
{-# INLINE block_label_map #-}


-----------------------------------------------------------------------------------
---------------           CfgConstruction Instances        ------------------------
-----------------------------------------------------------------------------------


instance HasCurCfg LuaCfgState MLuaSig where cur_cfg = lcs_cfg
instance HasLabelGen LuaCfgState where labelGen = lcs_labeler
instance HasLoopStack LuaCfgState where loopStack = lcs_stack
instance HasLabelMap LuaCfgState where labelMap = lcs_goto_labs.label_map_stack._head.block_label_map

type instance ComputationSorts MLuaSig = '[StatL, ExpL, PrefixExpL, VarL, TableFieldL, FunCallL, [BlockItemL], AssignL]
type instance SuspendedComputationSorts MLuaSig = '[P.FunctionDefL]
type instance ContainerFunctors MLuaSig = '[PairF, ListF, MaybeF]
type instance CfgState MLuaSig = LuaCfgState

nameString :: MLuaTermLab NameL -> String
nameString (stripA -> project -> Just (IdentIsName (Ident' s))) = s

extractClauses ::
  forall fs s a b.
  ( ListF :-<: fs
  , KExtractF2' (,) (Sum fs)
  , Typeable a
  , Typeable b
  ) => HState s (EnterExitPair fs) [(a, b)]
  -> State s [(EnterExitPair fs a, EnterExitPair fs b)]
extractClauses hs = do
    (extractEEPList -> cs) <- unHState hs
    return $ map extractClause cs
  where
    extractClause :: EnterExitPair fs (a, b) -> (EnterExitPair fs a, EnterExitPair fs b)
    extractClause (SubPairs p) = kextractF2' p

-- Lua's for loop is weird
constructCfgLuaForRange :: MLuaTermLab h -> State LuaCfgState (EnterExitPair MLuaSig i)
                                         -> State LuaCfgState (EnterExitPair MLuaSig j)
                                         -> State LuaCfgState (EnterExitPair MLuaSig k)
                                         -> State LuaCfgState (EnterExitPair MLuaSig l)
                                         -> State LuaCfgState (EnterExitPair MLuaSig m)
constructCfgLuaForRange t mInit mFinal mOptStep mBody = do
  enterNode <- addCfgNode t EnterNode
  exitNode  <- addCfgNode t ExitNode

  init <- mInit
  final <- mFinal
  step <- mOptStep

  pushLoopNode enterNode exitNode
  body <- mBody
  popLoopNode

  p  <- combineEnterExit init final
  p' <- combineEnterExit p step

  let setupExit = exit p' -- either the exit of final or step (b/c step is optional)


  cur_cfg %= addEdge enterNode (enter init)
  cur_cfg %= addEdge setupExit (enter body)
  cur_cfg %= addEdge setupExit exitNode

  cur_cfg %= addEdge (exit body) (enter body) -- there isn't really a separate node for the comparison

  return $ EnterExitPair enterNode exitNode

instance ConstructCfg MLuaSig LuaCfgState Stat where
  constructCfg (collapseFProd' -> (t :*: Break))        = HState $ constructCfgBreak t
  constructCfg (collapseFProd' -> (t :*: (While e b)))  = HState $ constructCfgWhile t (unHState e) (unHState b)
  constructCfg (collapseFProd' -> (t :*: (Repeat b e))) = HState $ constructCfgDoWhile t (unHState e) (unHState b)

  -- We can get away with using the While cfg-generator for for-each's.
  constructCfg (collapseFProd' -> (t :*: (ForIn _ e b))) = HState $ constructCfgWhile t (unHState e) (unHState b)
  constructCfg            t@(remA -> Goto (nam :*: _))   = HState $ constructCfgGoto  (ffst $ collapseFProd' t) (nameString nam)
  constructCfg           t@(remA -> Label (nam :*: _))   = HState $ constructCfgLabel (ffst $ collapseFProd' t) (nameString nam)

  constructCfg (collapseFProd' -> (t :*: (ForRange _ init final optStep body))) = HState $ constructCfgLuaForRange  t (unHState init) (unHState final) (unHState optStep) (unHState body)
  constructCfg (collapseFProd' -> (t :*: (If clauses optElse)))                 = HState $ constructCfgIfElseIfElse t (extractClauses clauses) (extractEEPMaybe $ unHState optElse)
  constructCfg t = constructCfgDefault t

instance ConstructCfg MLuaSig LuaCfgState P.FunctionDef where
  constructCfg (collapseFProd' -> (_ :*: subCfgs)) = HState $ do
    pushFunLabelMapStack
    runSubCfgs subCfgs
    popLabelMapStack
    return EmptyEnterExit

instance ConstructCfg MLuaSig LuaCfgState P.Block where
  constructCfg p@(collapseFProd' -> (t :*: _)) = case project' t of
    Just (P.Block xs r) -> case (extractF xs, project' r) of
      ([], Just (LuaBlockEnd e)) -> HState $ do
        withBlockLabelMap (unHState $ constructCfgGeneric p) -- FIXME: Doesn't properly handle returns, but I think the TACer won't notice
      _  -> HState $ do
        withBlockLabelMap (unHState $ constructCfgDefault p)

instance ConstructCfg MLuaSig LuaCfgState Exp where
  constructCfg t'@(remA -> (Binop (op :*: _) _ _)) = do
    let (t :*: (Binop _ el er)) = collapseFProd' t'
    case extractOp op of
      And -> HState $ constructCfgShortCircuitingBinOp t (unHState el) (unHState er)
      Or  -> HState $ constructCfgShortCircuitingBinOp t (unHState el) (unHState er)
      _   -> constructCfgDefault t'

    where extractOp :: MLuaTermLab BinopL -> Binop MLuaTerm BinopL
          extractOp (stripA -> project -> Just bp) = bp

  constructCfg t = constructCfgDefault t

instance CfgInitState MLuaSig where
  cfgInitState _ = LuaCfgState emptyCfg (unsafeMkCSLabelGen ()) emptyLoopStack emptyLabelMapStack
