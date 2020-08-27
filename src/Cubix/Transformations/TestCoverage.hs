{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cubix.Transformations.TestCoverage (
    instrumentTestCoverage
  ) where

import Control.Monad ( when )
import Control.Monad.State ( MonadState, evalState, execState )

import Control.Lens ( makeClassy, use, (-=), (^.))

import Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set

import Data.Text ( pack )

import Data.Comp.Multi ( project', ShowHF, (:-<:), All, HFoldable )
import Data.Comp.Multi.Strategic ( GRewriteM, revAllbuR )
import Data.Comp.Multi.Strategy.Classification ( caseDyn )

import Cubix.Language.Info
import Cubix.Language.C.Parametric.Common as C
import Cubix.Language.Java.Parametric.Common as J
import Cubix.Language.JavaScript.Parametric.Common as JS
import Cubix.Language.Lua.Parametric.Common as L
import Cubix.Language.Python.Parametric.Common as Py

import Cubix.Language.Parametric.ProgInfo
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Semantics.CfgInserter
import Cubix.Language.Parametric.Semantics.SemanticProperties
import Cubix.Language.Parametric.Syntax as P

import Cubix.Sin.Compdata.Annotation

import Cubix.Transformations.Variation

--------------------------------------------------------------------------------------

data TestCovState = TestCovState {
                                   _bb_counter  :: Int
                                 , _bb_labelgen :: LabelGen
                                 }

makeClassy ''TestCovState

instance HasLabelGen TestCovState where labelGen = bb_labelgen


-- Easiest way to get reasonable labels: Visit in reverse order of desired,
-- count backwards. Do prepass to count number of labels needed
nextBlockId :: (MonadState s m, HasTestCovState s) => m Int
nextBlockId = do
  id <- use bb_counter
  bb_counter -= 1
  return id

class BlockCounterStart (fs :: [(* -> *) -> * -> *]) where
  blockCounterStart :: Proxy fs -> Int

-- Every language but Lua
instance {-# OVERLAPPABLE #-} BlockCounterStart fs where
  blockCounterStart _ = 0

instance {-# OVERLAPPING #-} BlockCounterStart MLuaSig where
  blockCounterStart _ = 1

class MarkBlockCovered fs where
  markBlockCovered :: (MonadAnnotater Label m) => Int -> m (TermLab fs (StatSort fs))

#ifndef ONLY_ONE_LANGUAGE
cInt :: Int -> MCTerm CExpressionL
cInt n = iCConst $ iCIntConst (CInteger' (toInteger n)) iUnitF

instance MarkBlockCovered MCSig where
  markBlockCovered n = annotateLabel term
    where
      term :: MCTerm BlockItemL
      term = P.iAssign (iCIndex covArr (cInt n) iUnitF) AssignOpEquals' rhs

      covArr :: MCTerm CExpressionL
      covArr = iCMember (iCVar (iIdent "TestCoverage") iUnitF) (iIdent "coverage") False iUnitF

      rhs :: MCTerm RhsL
      rhs = iCConst $ iCIntConst (iCInteger 1 iDecRepr (C.iFlags 0)) iUnitF


instance MarkBlockCovered MJavaSig where
  markBlockCovered n = annotateLabel term
    where
      term :: MJavaTerm BlockItemL
      term = P.iAssign (J.iArrayLhs (J.iArrayIndex covArr (insertF [J.iLit $ J.iInt $ toInteger n]))) AssignOpEquals' (J.iLit (J.iBoolean True))

      covArr :: MJavaTerm J.ExpL
      covArr = J.iFieldAccess $ J.iPrimaryFieldAccess (J.iExpName $ J.iName $ insertF [iIdent "TestCoverage"]) (iIdent "coverage")

instance MarkBlockCovered MJSSig where
  markBlockCovered n = annotateLabel term
    where
      term :: MJSTerm JSStatementL
      term = iJSExpressionStatement (P.iAssign (iJSMemberSquare covArr noAnn (iJSDecimal noAnn (show n)) noAnn) AssignOpEquals' (iJSLiteral noAnn "true")) semi

      covArr :: MJSTerm JSExpressionL
      covArr = iJSMemberDot (iJSIdentifier noAnn "TestCoverage") noAnn (iJSIdentifier noAnn "coverage")

      noAnn :: MJSTerm JSAnnotL
      noAnn = iJSNoAnnot

      semi :: MJSTerm JSSemiL
      semi = iJSSemi noAnn


instance MarkBlockCovered MPythonSig where
  markBlockCovered n = annotateLabel term
    where
      term :: MPythonTerm Py.StatementL
      term = P.iAssign (iPyLhs $ insertF [Py.iSubscriptLValue covArr (Py.iInt (toInteger n) (show n) iUnitF)]) AssignOpEquals' (Py.iBool True iUnitF)

      covArr :: MPythonTerm Py.ExprL
      covArr = Py.iDot (Py.iVar (iIdent "TestCoverage") iUnitF) (iIdent "coverage") iUnitF
#endif

luaNumber :: Int -> MLuaTerm L.ExpL
luaNumber n = L.iNumber L.iIntNum $ pack $ show n

instance MarkBlockCovered MLuaSig where
  markBlockCovered n = annotateLabel term
    where
      term :: MLuaTerm BlockItemL
      term = P.iAssign (iLuaLhs $ insertF $ [L.iSelect covArr (luaNumber n)])  AssignOpEquals' (iLuaRhs $ insertF [L.iBool True])

      covArr :: MLuaTerm L.PrefixExpL
      covArr = iPEVar $ L.iSelectName (iPEVar $ iVarName $ iIdent "TestCoverage") (iIdent "coverage")

class ExcludeBasicBlock fs where
  excludeBasicBlock :: TermLab fs l -> Bool

-- |
-- There's no good reason why you shouldn't consider each loop to be its own basic block
-- (because the condition gets executed some number of times independently), but it looks weird
-- to try to instrument it, and humans wouldn't do that

instance {-#OVERLAPPABLE #-} ExcludeBasicBlock fs where
  excludeBasicBlock = const False

#ifndef ONLY_ONE_LANGUAGE
instance {-# OVERLAPPING #-} ExcludeBasicBlock MCSig where
  excludeBasicBlock t = caseDyn excludeExp t $
                         caseDyn excludeWhile t $
                         False
    where
      excludeExp :: MCTermLab CExpressionL -> Bool
      excludeExp _ = True

      excludeWhile :: MCTermLab CStatementL -> Bool
      excludeWhile (project' -> Just (CWhile _ _ _ _)) = True
      excludeWhile _                                   = False

instance {-# OVERLAPPING #-} ExcludeBasicBlock MJavaSig where
  excludeBasicBlock t = caseDyn excludeExp t $
                         caseDyn excludeWhile t $
                         False
    where
      excludeExp :: MJavaTermLab J.ExpL -> Bool
      excludeExp _ = True

      excludeWhile :: MJavaTermLab J.StmtL -> Bool
      excludeWhile (project' -> Just (J.While _ _)) = True
      excludeWhile _                                = False
#endif

class TrustReachability (fs :: [(* -> *) -> * -> *]) where
  trustReachability :: Proxy fs -> Bool

instance {-# OVERLAPPABLE #-} TrustReachability fs where
  trustReachability _ = False

#ifndef ONLY_ONE_LANGUAGE
instance {-# OVERLAPPING #-} TrustReachability MJavaSig where
  trustReachability _ = True
#endif

type CanInstrument fs = ( ListF :-<: fs
                        , MarkBlockCovered fs
                        , BlockCounterStart fs
                        , ExcludeBasicBlock fs
                        , TrustReachability fs
                        , CfgBuilder fs
                        , InsertAt fs (StatSort fs)
                        , All ShowHF fs
                        , All HFoldable fs
                        )

type MonadTestCov fs m = (MonadState TestCovState m, MonadAnnotater Label m, MonadCfgInsertion m fs (StatSort fs))

-- This prevents marking the space after return's, but does have the effect of not marking empty catch blocks because of the ways our CFGs currently work.
-- Pick your poison.
isUnreachableAndEmpty :: (ListF :-<: fs) => Cfg fs -> TermLab fs l -> Bool
isUnreachableAndEmpty cfg t = isEmpty t && isUnreachable t
  where
    isUnreachable t = case cfgNodeForTerm cfg EnterNode t of
      Nothing -> True
      Just cfgNode -> isStartNode cfg cfgNode

    isEmpty (project' -> Just NilF) = True
    isEmpty _                       = False

-- Quick-and-dirty for rebuttal. Need to check for unreachable nodes, so we trace back the graph and look for an entry point.
-- We don't currently have a notion of entry point, but we know that exit nodes are not entry points
unreachableTest :: forall fs l. (CanInstrument fs) => Cfg fs -> TermLab fs l -> Bool
unreachableTest cfg t = if not (trustReachability (Proxy :: Proxy fs)) then
                          False
                        else
                          case cfgNodeForTerm cfg EnterNode t of
                            Nothing -> True
                            Just cfgNode -> case satisfyingPredBoundary (\n -> Set.null (n ^. cfg_node_prevs)) cfg cfgNode of
                                              Nothing -> False
                                              Just xs -> all (\n -> (n ^. cfg_node_type) == ExitNode) xs



addCoverageStatement :: (CanInstrument fs, MonadTestCov fs m) => Cfg fs -> GRewriteM m (TermLab fs)
addCoverageStatement cfg t = case cfgNodeForTerm cfg EnterNode t of
                               Nothing       -> return t
                               Just cfgNode  -> do
                                 when (startsBasicBlock cfg cfgNode && not ((excludeBasicBlock t) || (isUnreachableAndEmpty cfg t) || (unreachableTest cfg t))) $ do
                                   n <- nextBlockId
                                   covStat <- markBlockCovered n
                                   dominatingAppendFirst t covStat

                                 return t

instrumentTestCoverage :: forall fs l. (CanInstrument fs) => TermLab fs l -> IO (TermLab fs l)
instrumentTestCoverage t = do
    gen <- mkConcurrentSupplyLabelGen
    let progInfo = makeProgInfo t

    let labelsNeeded = (^. bb_counter) $ execState (trans progInfo t) (TestCovState 0 gen)
    let counterStart = (-labelsNeeded) - 1 + (blockCounterStart (Proxy :: Proxy fs))
    return $ evalState (trans progInfo t) (TestCovState counterStart gen)
  where
    trans progInfo = performCfgInsertions (Proxy :: Proxy (StatSort fs)) progInfo $ (revAllbuR $ addCoverageStatement (progInfo ^. proginf_cfg))
