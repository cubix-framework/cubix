{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Cubix.Transformations.TAC.State (
    TACState
  , tac_gen
  , tac_gensym_state
  , is_accumulating_prepends
  , to_prepend

  , makeTACState

  , LocalTACState
  , ltac_mod_set
  , makeLocalTACState

  , withSubPrepends
  , doPrepend

  , PrependPair(..)
  , doPrependSplit
  , doOptionalAppend

  , MonadTAC
  ) where

import Control.Monad ( MonadPlus )
import Control.Monad.Reader ( MonadReader )
import Control.Monad.State ( MonadState )

import Control.Lens ( (%=), (.=), use, makeLenses )
import Control.Monad.Random ( MonadRandom )

import Cubix.Language.Info
import Cubix.Language.Parametric.Semantics.CfgInserter
import Cubix.Language.Parametric.Syntax

import Cubix.Transformations.TAC.Custom
import Cubix.Transformations.TAC.Gensym

--------------------------------------------------------------------------------------

data TACState fs = TACState { _tac_gen          :: LabelGen
                            , _tac_gensym_state :: GensymState
                            , _is_accumulating_prepends :: Bool
                            , _to_prepend       :: [TermLab fs BlockItemL]
                            }

makeLenses ''TACState

instance HasLabelGen (TACState fs) where labelGen = tac_gen
instance HasGensymState (TACState fs) where gensymState = tac_gensym_state

makeTACState :: LabelGen -> GensymState -> TACState fs
makeTACState gen gs = TACState gen gs False []

data LocalTACState fs = LocalTACState { _ltac_mod_set :: ModifiedSet fs}

makeLenses ''LocalTACState

makeLocalTACState :: ModifiedSet fs -> LocalTACState fs
makeLocalTACState = LocalTACState

-- Simulating a sub runWriterT; we needed to switch from Writer to State
-- to allow using the CfgInsertert
withSubPrepends :: (MonadState (TACState fs) m) => m x -> m (x, [TermLab fs BlockItemL])
withSubPrepends sub = do
  old_is_acc <- use is_accumulating_prepends
  old <- use to_prepend
  to_prepend .= []
  is_accumulating_prepends .= True
  x <- sub
  p <- use to_prepend
  to_prepend .= old
  is_accumulating_prepends .= old_is_acc
  return (x, p)

doPrepend :: (MonadState (TACState fs) m, MonadCfgInsertion m fs BlockItemL) => TermLab fs l -> TermLab fs BlockItemL -> m ()
doPrepend targ t = do
  is_acc <- use is_accumulating_prepends
  if is_acc then
    to_prepend %= (++[t])
  else
    dominatingPrependLast targ t


data PrependPair fs l = PrependPair { prepend_first :: TermLab fs l
                                    , prepend_rest  :: Maybe (TermLab fs l)
                                    }

doPrependSplit ::
  ( MonadState (TACState fs) m
  , MonadCfgInsertion m fs BlockItemL
  ) => TermLab fs l -> PrependPair fs BlockItemL -> m ()
doPrependSplit targ p = do
  is_acc <- use is_accumulating_prepends
  if is_acc then
    to_prepend %= (++[prepend_first p])
  else do
    firstPredPrependLast targ (prepend_first p)
    maybe (return ()) (restPredPrependLast targ) (prepend_rest p)

doOptionalAppend ::
  ( MonadState (TACState fs) m
  , MonadCfgInsertion m fs BlockItemL
  ) => TermLab fs l -> TermLab fs BlockItemL -> m ()
doOptionalAppend targ e = do
  is_acc <- use is_accumulating_prepends
  if is_acc then
    return ()
  else
    dominatingAppendFirstOpts targ e EmptyInsertOkay


type MonadTAC fs m = (MonadState (TACState fs) m, MonadReader (LocalTACState fs) m, MonadCfgInsertion m fs BlockItemL, MonadRandom m, MonadPlus m, MonadFail m)
