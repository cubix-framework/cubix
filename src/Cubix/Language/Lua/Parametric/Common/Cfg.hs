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

import Control.Lens ( (%=), makeLenses, Lens', to, use, (^..), (^.) )

import Data.Comp.Multi ( project, project', stripA, remA, (:-<:) )
import Data.Comp.Multi.Ops ( Sum, (:*:)(..), ffst )

import Cubix.Language.Info

import Cubix.Language.Lua.Parametric.Common.Types as C
import Cubix.Language.Lua.Parametric.Full.Types as F
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Syntax as P

-- For Lua - quoting ref:
-- A label is visible in the entire block where it is defined, except inside nested blocks where a label with the same name is defined and inside nested functions. A goto may jump to any visible label as long as it does not enter into the scope of a local variable.

-- So we push a new stack of label map or Fun marker whenever we enter a new block or a function respectively

type LuaLabelMapStack = [LuaLabelMap]

data LuaLabelMap = Fun
                 | BlockLabelMap { _block_label_map :: LabelMap }
                 deriving ( Eq, Ord, Show)

data LuaCfgState = LuaCfgState {
                   _lcs_cfg       :: Cfg MLuaSig
                 , _lcs_labeler   :: LabelGen
                 , _lcs_stack     :: LoopStack
                 , _lcs_goto_labs :: LuaLabelMapStack
                 }

makeLenses ''LuaCfgState

instance HasCurCfg LuaCfgState MLuaSig where cur_cfg = lcs_cfg
instance HasLabelGen LuaCfgState where labelGen = lcs_labeler
instance HasLoopStack LuaCfgState where loopStack = lcs_stack
instance HasLabelMap LuaCfgState where labelMap = lcs_goto_labs._head.block_label_map

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
    ee <- runSubCfgs subCfgs
    popLabelMapStack
    pure ee

instance ConstructCfg MLuaSig LuaCfgState P.Block where
  constructCfg p@(collapseFProd' -> (t :*: _)) = case project' t of
    Just (P.Block xs r) -> case (extractF xs, project' r) of
      ([], Just (LuaBlockEnd e)) -> HState $ do
        pushBlockLabelMapStack emptyLabelMap
        ee <- unHState $ constructCfgGeneric p -- FIXME: Doesn't properly handle returns, but I think the TACer won't notice
        resolveVisibleLabels
        popLabelMapStack
        pure ee
      _  -> HState $ do
        pushBlockLabelMapStack emptyLabelMap
        ee <- unHState $ constructCfgDefault p
        resolveVisibleLabels
        popLabelMapStack
        pure ee

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

pushFunLabelMapStack :: (MonadState LuaCfgState m) => m ()
pushFunLabelMapStack =
  lcs_goto_labs %= (Fun :)

pushBlockLabelMapStack :: (MonadState LuaCfgState m) => LabelMap -> m ()
pushBlockLabelMapStack m =
  lcs_goto_labs %= (:) (BlockLabelMap m)

popLabelMapStack :: (MonadState LuaCfgState m) => m ()
popLabelMapStack =
  lcs_goto_labs %= init

emptyLabelMapStack :: LuaLabelMapStack
emptyLabelMapStack = []

resolveVisibleLabels :: (MonadState LuaCfgState m) => m ()
resolveVisibleLabels = do
  vs <- use lcs_goto_labs
  lm <- use labelMap
  let visLabs = List.takeWhile isBlockLabelMap vs ^.. traverse.block_label_map.label_map
  forM_ (Map.toList (lm ^. label_map)) $ \(fst -> lab) -> do
    let Just (l, prevs) =
          List.foldl' (\acc a -> case acc of
                        Nothing -> Map.lookup lab a
                        Just a0  -> Just a0) Nothing visLabs
    forM_ prevs $ \p -> cur_cfg %= addEdgeLab (Proxy :: Proxy MLuaSig) p l

      where isBlockLabelMap (BlockLabelMap {}) = True
            isBlockLabelMap _                  = False

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
