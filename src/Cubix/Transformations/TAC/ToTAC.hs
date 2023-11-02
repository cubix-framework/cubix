{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module Cubix.Transformations.TAC.ToTAC (
    toTAC
  ) where

import Control.Monad ( when, liftM, (>=>), MonadPlus(..))
import Control.Monad.Reader ( ReaderT(..) )
import Control.Monad.State ( State, evalState )

import qualified Data.Map as Map
import Data.Proxy ( Proxy(..) )
import Data.Set ( Set )
import qualified Data.Set as Set

import System.Random ( StdGen, getStdGen )

import Control.Lens ( (&), (%~), (^.), (.~), makeLenses, view)
import Control.Monad.Random ( MonadRandom, RandT, evalRandT )
import Data.Constraint ( Dict(..) )

import Data.Comp.Multi ( Signature, Cxt(..), project', EqHF, E(..), stripA, (:&:)(..), HTraversable, ShowHF, inject', (:-<:), All, HFoldable )
import Data.Comp.Multi.Strategic ( GRewriteM, RewriteM, allbuR, allStateR, dynamicR,
           tryR, (<+), idR, guardedT, failR )
import Data.Comp.Multi.Strategy.Classification ( DynCase, subterms, caseE )

import Cubix.Language.Info

import Cubix.Language.JavaScript.Parametric.Common as JSCommon
import Cubix.Language.Python.Parametric.Common     as PyCommon

import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.ProgInfo
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Semantics.CfgInserter
import Cubix.Language.Parametric.Semantics.SemanticProperties
import Cubix.Language.Parametric.Syntax

import Cubix.Sin.Compdata.Annotation ( getAnn )

import Cubix.Transformations.TAC.Custom
import Cubix.Transformations.TAC.Gensym
import Cubix.Transformations.TAC.Sorts
import Cubix.Transformations.TAC.State
import Cubix.Transformations.Variation

--------------------------------------------------------------------------------------

data HoistExpState fs = HoistExpState { _hes_sns   :: [Strictness]
                                     , _hes_hcs   :: [ShouldHoist]
                                     , _hes_prevs :: [E (TermLab fs)]
                                     }

makeLenses ''HoistExpState

--------------------------------------------------------------------------------------

-- | I could make a general mechanism for declaring evaluation order, but I'm not going to.
--   I've overdue on time spent polishing
class TACSpecial fs where
  tacSpecial :: (MonadTAC fs m) => GRewriteM m (TermLab fs)

instance {-# OVERLAPPABLE #-} TACSpecial fs where
  tacSpecial = failR

#ifndef ONLY_ONE_LANGUAGE
instance {-# OVERLAPPING #-} TACSpecial MJSSig where
  tacSpecial t@(project' -> Just (JSFor init cond step s)) = do
    init' <- subexpToTmp Strict ShouldHoist []                 init
    step' <- subexpToTmp Strict ShouldHoist [E init']          step
    cond' <- subexpToTmp Strict ShouldHoist [E init', E step'] cond
    return $ inject' $ (JSFor init' cond' step' s) :&: getAnn t

  tacSpecial t@(project' -> Just (JSForVar init cond step s)) = do
    init' <- subexpToTmp Strict ShouldHoist []                 init
    mapM (doPrepend t) =<< mapM labeledInjF (extractF init') -- lift out var decls
    step' <- subexpToTmp Strict ShouldHoist [E init']          step
    cond' <- subexpToTmp Strict ShouldHoist [E init', E step'] cond

    emptyInit <- annotateLabel NilF'
    return $ inject' $ (JSFor emptyInit cond' step' s) :&: getAnn t

  tacSpecial _ = mzero
#endif


-- | Problem: Creating temps creates extra references to variables,
-- which causes extra retention in the GC, particularly for JS/Python, which
-- don't believe in scoping. Many tests (~15) break because of this behavior.
--
-- Approach: Delete temporaries after use
--
-- Option 1: Think very hard about how we do prepends and what is the nature
--           of a non-strict expression; design an approach that integrates
--           cleanly into our handling of non-strict expressions, which changes
--           how prepends work. Design a good interface, expressing cleanly
--           what this requires
-- Option 2: Say screw it, throw together a hack that totally does not work
--           for non-strict expressions, and couple the code tightly
--           to the TAC transformation and its monad.
--
--
-- I pick option 2. These del's should be optional anyway; it's only in rare flukes
-- that this matters. It's a sin to do otherwise
-- and hence delay research. Fuck you Python for not having scopes. Fuck you CPython
-- developers for writing tests that depend on this. And fuck you reviewers for making
-- me bring this transformation to 100%.

class SpecialTmpHoistAction fs where
  specialTmpHoistAction :: (MonadTAC fs m) => TermLab fs IdentL -> TermLab fs l -> m ()

instance {-# OVERLAPPABLE #-} SpecialTmpHoistAction fs where
  specialTmpHoistAction _ _ = return ()

#ifndef ONLY_ONE_LANGUAGE
instance {-# OVERLAPPING #-} SpecialTmpHoistAction MPythonSig where
  specialTmpHoistAction n targ = do
    let cxt = PyCommon.iDelete (SingletonF' $ injF (Hole n)) iUnitF
    (e :: TermLab MPythonSig BlockItemL) <- annotateLabelOuter cxt
    doOptionalAppend targ e

#endif

--------------------------------------------------------------------------------------

type DecFragment fs = ( All HTraversable fs
                      , All HFoldable fs
                      , All EqHF fs
                      , Ident :-<: fs, Assign :-<: fs, AssignOpEquals :-<: fs
                      , ListF :-<: fs, MaybeF :-<: fs
                      )

class (InjF fs AssignL BlockItemL) => ExtraAssignConstraints fs
instance (InjF fs AssignL BlockItemL) => ExtraAssignConstraints fs

class    (DefaultMultiLocalVarDeclCommonAttrs fs, InjF fs (ExpressionSort fs) LocalVarInitL, InjF fs IdentL VarDeclBinderL) => ExtraMultiVarDeclConstraints fs
instance (DefaultMultiLocalVarDeclCommonAttrs fs, InjF fs (ExpressionSort fs) LocalVarInitL, InjF fs IdentL VarDeclBinderL) =>  ExtraMultiVarDeclConstraints fs

class    (DefaultLocalVarDeclAttrs fs, InjF fs (ExpressionSort fs) LocalVarInitL, InjF fs IdentL VarDeclBinderL) => ExtraSingleVarDeclConstraints fs
instance (DefaultLocalVarDeclAttrs fs, InjF fs (ExpressionSort fs) LocalVarInitL, InjF fs IdentL VarDeclBinderL) => ExtraSingleVarDeclConstraints fs

type CanTransTAC (fs :: Signature) =
      ( DynCase (TermLab fs) (ExpressionSort fs)
      , DynCase (TermLab fs) (StatSort fs)
      , DynCase (TermLab fs) AssignL
      , DynCase (TermLab fs) IdentL
      , InjF fs IdentL (ExpressionSort fs)
      , InjF fs IdentL LhsL
      , InjF fs (ExpressionSort fs) RhsL
      , InjF fs AssignL BlockItemL
      , AInjF fs AssignL

      , BarrierCheck fs
      , IsValue fs
      , RenderGuard fs
      , GetStrictness fs
      , ShouldHoistChild fs
      , TACSpecial fs
      , SpecialTmpHoistAction fs

      , DecFragment fs
      , All EqHF fs
      , ExtractF [] (TermLab fs)
      , ExplicitDeclsVariation fs ExtraMultiVarDeclConstraints ExtraSingleVarDeclConstraints ExtraAssignConstraints

      , CfgBuilder fs
      , InsertAt fs BlockItemL

      , All ShowHF fs
      )



tacDeclsVariation :: forall fs. (CanTransTAC fs) => Proxy fs
                                               -> ExplicitDeclsVariationDict
                                                           fs
                                                           ExtraMultiVarDeclConstraints
                                                           ExtraSingleVarDeclConstraints
                                                           ExtraAssignConstraints
tacDeclsVariation p = explicitDeclsVariation p
                                             (Proxy @ExtraMultiVarDeclConstraints)
                                             (Proxy @ExtraSingleVarDeclConstraints)
                                             (Proxy @ExtraAssignConstraints)

-- | This is here because Lua functions can only have 200 local variables,
--   and this transformation generates more
randomlySkipHoist :: (MonadRandom m) => m Bool
randomlySkipHoist = return False
--randomlySkipHoist = (< 0.6) <$> getRandomR (0.0, (1.0 :: Double))

-- FIXME: I'd like to refactor out the args as a HoistExpState, but it doesn't quite match
-- I think I'd need a bigger hoist state refactor to do this properly
extractNonValue :: forall fs m. (CanTransTAC fs, MonadTAC fs m) =>
                    Strictness -> ShouldHoist -> [E (TermLab fs)] -> RewriteM m (TermLab fs) (ExpressionSort fs)
extractNonValue sn hc prevs e = view ltac_mod_set >>= \modSet  ->
    if isValue modSet e then
      return e
    else if hc == StopHoisting then
      return e
    else
      case sn of
        Strict -> do
          e' <- doHoistChildren

          skipHoist <- randomlySkipHoist

          case (skipHoist, hc) of
            (True, _) ->
              return e'
            (False, ShouldntHoist) ->
              return e'
            (False, ShouldHoist) -> do
              tmp <- gensym
              mapM (doPrependSplit e) =<< makeVarDecPair tmp (Just e')
              specialTmpHoistAction tmp e
              labeledInjF tmp
        GuardedBy p -> do
          let (sign, i) = case p of
                           Place j    -> (True, j)
                           NegPlace j -> (False, j)

          when (i >= length prevs) $ fail "Expression guarded by later-evaluated expression"
          let Just prev = caseE (prevs !! i)

          (e', ps) <- withSubPrepends doHoistChildren
          tmp <- gensym

          (finalAssn :: TermLab fs BlockItemL) <- annotateLabelOuter $ injF $ Assign' (injF $ Hole tmp) AssignOpEquals' (injF $ Hole e')

          let inside = ps ++ [finalAssn]

          mapM (doPrependSplit e) =<< makeVarDecPair tmp Nothing
          guard <- renderGuard sign prev inside
          doPrepend e guard

          labeledInjF tmp

        NoEval -> return e
  where
    subsns = getStrictness $ stripA e
    subhcs = shouldHoistChild hc $ stripA e
    doHoistChildren = allStateR hoistChild (HoistExpState subsns subhcs []) e

hoistChild :: (CanTransTAC fs, MonadTAC fs m) => HoistExpState fs -> TermLab fs l -> m (TermLab fs l, HoistExpState fs)
hoistChild hes t = do t' <- (guardedT barrierCheck idR (subexpToTmp (head sns) (head hcs) ps)) t
                      return $ (t', hes & hes_sns   .~ tail sns
                                        & hes_hcs   .~ tail hcs
                                        & hes_prevs %~ (++[E t']))
  where
    sns = hes ^. hes_sns
    hcs = hes ^. hes_hcs
    ps  = hes ^. hes_prevs

subexpToTmp :: (CanTransTAC fs, MonadTAC fs m) => Strictness -> ShouldHoist -> [E (TermLab fs)] -> GRewriteM m (TermLab fs)
subexpToTmp sn hc prevs t = if hc == StopHoisting then
                              return t
                            else
                              (   dynamicR (extractNonValue sn hc prevs)
                                  <+ tacSpecial
                                  <+ allStateR hoistChild (HoistExpState subsns subhcs []))
                                t
  where
    subsns = getStrictness $ stripA t
    subhcs = shouldHoistChild hc $ stripA t


makeVarDecSingle ::
  ( CanTransTAC fs
  , MonadTAC fs m
  , ContainsSingleDec fs
  , ExtraSingleVarDeclConstraints fs
  ) => TermLab fs IdentL
  -> Maybe (TermLab fs (ExpressionSort fs))
  -> m (TermLab fs SingleLocalVarDeclL)
makeVarDecSingle ident mExp = annotateLabelOuter $ SingleLocalVarDecl' defaultLocalVarDeclAttrs (injF (Hole ident)) init
  where 
    init = case mExp of
      Nothing  -> NoLocalVarInit'
      Just exp -> JustLocalVarInit' (injF (Hole exp))

makeVarDecMulti ::
  ( CanTransTAC fs
  , MonadTAC fs m
  , MultiLocalVarDecl :-<: fs
  , ContainsSingleDec fs
  , ExtraMultiVarDeclConstraints fs
  ) => TermLab fs IdentL
  -> Maybe (TermLab fs (ExpressionSort fs))
  -> m (TermLab fs MultiLocalVarDeclL)
makeVarDecMulti ident exp = do
  innerDec <- makeVarDecSingle ident exp
  annotateLabelOuter $ MultiLocalVarDecl' defaultMultiLocalVarDeclCommonAttrs
                                          (insertFHole [innerDec])

makeAssignment ::
  ( CanTransTAC fs
  , AssignmentInsertion fs ExtraAssignConstraints
  , MonadTAC fs m
  ) => TermLab fs IdentL
  -> Maybe (TermLab fs (ExpressionSort fs))
  -> m (Maybe (TermLab fs AssignL))
makeAssignment ident exp = case exp of
  Nothing -> return Nothing
  Just e  -> liftM Just $ annotateLabelOuter $ Assign' (injF $ Hole ident) AssignOpEquals' (injF $ Hole e)

makeVarDec :: forall fs m.
  ( CanTransTAC fs
  , MonadTAC fs m
  ) => TermLab fs IdentL -> Maybe (TermLab fs (ExpressionSort fs)) -> m (Maybe (TermLab fs BlockItemL))
makeVarDec ident x = case tacDeclsVariation (Proxy @fs) of
  HasExplicitDeclsVariation (MultiDecInsertionVariation  Dict) -> Just <$> (labeledInjF =<< makeVarDecMulti  ident x)
  HasExplicitDeclsVariation (SingleDecInsertionVariation Dict) -> Just <$> (labeledInjF =<< makeVarDecSingle ident x)
  NoExplicitDeclsVariation  Dict                               -> mapM labeledInjF =<< makeAssignment ident x

makeVarDecPair ::
  ( CanTransTAC fs
  , MonadTAC fs m
  ) => TermLab fs IdentL -> Maybe (TermLab fs (ExpressionSort fs)) -> m (Maybe (PrependPair fs BlockItemL))
makeVarDecPair ident x = do
  mDec <- makeVarDec ident x
  case mDec of
    Nothing  -> return Nothing
    Just dec -> do
       assn <- makeAssignment ident x
       lassn <- mapM labeledInjF assn
       return $ Just $ PrependPair dec lassn

trackModSet ::
  ( CanTransTAC fs
  , Monad m
  ) => GRewriteM (ReaderT (LocalTACState fs) m) (TermLab fs)
  -> GRewriteM m (TermLab fs)
trackModSet f t = runReaderT (f t) (makeLocalTACState (modifiedVariables t))

toTAC :: forall fs m l. (CanTransTAC fs) => TermLab fs l -> IO (TermLab fs l)
toTAC t = do
   gen <- mkConcurrentSupplyLabelGen
   let progInf = makeProgInfo t
   randGen <- getStdGen

   let inserted = performCfgInsertions @BlockItemL progInf trans t
   let evalled  = evalState (evalRandT inserted randGen) (makeTACState gen (GensymState 0 allIds Map.empty))
   return $ evalState (finalizeGensymNames evalled) (GensymState 0 allIds Map.empty)
  where
    allIds :: Set String
    allIds = Set.fromList $ [s | Ident' s <- (map stripA $ (subterms t :: [TermLab fs IdentL]))]

    trans :: GRewriteM (CfgInserterT fs BlockItemL (RandT StdGen (State (TACState fs)))) (TermLab fs)
    trans = allbuR (tryR (barrierCheck >=> (trackModSet $ subexpToTmp Strict ShouldHoist [])))
