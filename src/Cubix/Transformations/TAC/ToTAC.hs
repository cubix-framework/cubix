{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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

import Data.Comp.Multi ( Cxt(..), (:<:), project', EqHF, E(..), stripA, (:&:)(..), HTraversable, ShowHF, inject' )
import Data.Comp.Multi.Strategic ( GRewriteM, RewriteM, allbuR, allStateR, dynamicR,
           tryR, (+>), idR, guardedT, failR )
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

data HoistExpState f = HoistExpState { _hes_sns   :: [Strictness]
                                     , _hes_hcs   :: [ShouldHoist]
                                     , _hes_prevs :: [E (HFixLab f)]
                                     }

makeLenses ''HoistExpState

--------------------------------------------------------------------------------------

-- | I could make a general mechanism for declaring evaluation order, but I'm not going to.
--   I've overdue on time spent polishing
class TACSpecial f where
  tacSpecial :: (MonadTAC f m) => GRewriteM m (HFixLab f)

instance {-# OVERLAPPABLE #-} TACSpecial f where
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

class SpecialTmpHoistAction f where
  specialTmpHoistAction :: (MonadTAC f m) => HFixLab f IdentL -> HFixLab f l -> m ()

instance {-# OVERLAPPABLE #-} SpecialTmpHoistAction f where
  specialTmpHoistAction _ _ = return ()

#ifndef ONLY_ONE_LANGUAGE
instance {-# OVERLAPPING #-} SpecialTmpHoistAction MPythonSig where
  specialTmpHoistAction n targ = do
    let cxt = PyCommon.iDelete (SingletonF' $ injF (Hole n)) iUnitF
    (e :: HFixLab MPythonSig BlockItemL) <- annotateLabelOuter cxt
    doOptionalAppend targ e

#endif

--------------------------------------------------------------------------------------

type DecFragment f = ( HTraversable f
                     , EqHF f
                     , Ident :<: f, Assign :<: f, AssignOpEquals :<: f
                     , ListF :<: f, MaybeF :<: f
                     )

class (InjF f AssignL BlockItemL) => ExtraAssignConstraints f
instance (InjF f AssignL BlockItemL) => ExtraAssignConstraints f

class    (DefaultMultiLocalVarDeclCommonAttrs f, InjF f (ExpressionSort f) LocalVarInitL, InjF f IdentL VarDeclBinderL) => ExtraMultiVarDeclConstraints f
instance (DefaultMultiLocalVarDeclCommonAttrs f, InjF f (ExpressionSort f) LocalVarInitL, InjF f IdentL VarDeclBinderL) =>  ExtraMultiVarDeclConstraints f

class    (DefaultLocalVarDeclAttrs f, InjF f (ExpressionSort f) LocalVarInitL, InjF f IdentL VarDeclBinderL) => ExtraSingleVarDeclConstraints f
instance (DefaultLocalVarDeclAttrs f, InjF f (ExpressionSort f) LocalVarInitL, InjF f IdentL VarDeclBinderL) => ExtraSingleVarDeclConstraints f

type CanTransTAC (f :: (* -> *) -> * -> *) =
      ( DynCase (HFixLab f) (ExpressionSort f)
      , DynCase (HFixLab f) (StatSort f)
      , DynCase (HFixLab f) AssignL
      , DynCase (HFixLab f) IdentL
      , InjF f IdentL (ExpressionSort f)
      , InjF f IdentL LhsL
      , InjF f (ExpressionSort f) RhsL
      , InjF f AssignL BlockItemL
      , AInjF f AssignL

      , BarrierCheck f
      , IsValue f
      , RenderGuard f
      , GetStrictness f
      , ShouldHoistChild f
      , TACSpecial f
      , SpecialTmpHoistAction f

      , DecFragment f
      , HTraversable f
      , EqHF f
      , ExtractF [] (HFixLab f)
      , ExplicitDeclsVariation f ExtraMultiVarDeclConstraints ExtraSingleVarDeclConstraints ExtraAssignConstraints

      , CfgBuilder f
      , InsertAt f BlockItemL

      , ShowHF f
      )


tacDeclsVariation :: forall f. (CanTransTAC f) => Proxy f
                                               -> ExplicitDeclsVariationDict
                                                           f
                                                           ExtraMultiVarDeclConstraints
                                                           ExtraSingleVarDeclConstraints
                                                           ExtraAssignConstraints
tacDeclsVariation p = explicitDeclsVariation p
                                             (Proxy :: Proxy ExtraMultiVarDeclConstraints)
                                             (Proxy :: Proxy ExtraSingleVarDeclConstraints)
                                             (Proxy :: Proxy ExtraAssignConstraints)

-- | This is here because Lua functions can only have 200 local variables,
--   and this transformation generates more
randomlySkipHoist :: (MonadRandom m) => m Bool
randomlySkipHoist = return False
--randomlySkipHoist = (< 0.6) <$> getRandomR (0.0, (1.0 :: Double))

-- FIXME: I'd like to refactor out the args as a HoistExpState, but it doesn't quite match
-- I think I'd need a bigger hoist state refactor to do this properly
extractNonValue :: forall f m. (CanTransTAC f, MonadTAC f m) =>
                    Strictness -> ShouldHoist -> [E (HFixLab f)] -> RewriteM m (HFixLab f) (ExpressionSort f)
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

          (finalAssn :: HFixLab f BlockItemL) <- annotateLabelOuter $ injF $ Assign' (injF $ Hole tmp) AssignOpEquals' (injF $ Hole e')

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

hoistChild :: (CanTransTAC f, MonadTAC f m) => HoistExpState f -> HFixLab f l -> m (HFixLab f l, HoistExpState f)
hoistChild hes t = do t' <- (guardedT barrierCheck idR (subexpToTmp (head sns) (head hcs) ps)) t
                      return $ (t', hes & hes_sns   .~ tail sns
                                        & hes_hcs   .~ tail hcs
                                        & hes_prevs %~ (++[E t']))
  where
    sns = hes ^. hes_sns
    hcs = hes ^. hes_hcs
    ps  = hes ^. hes_prevs

subexpToTmp :: (CanTransTAC f, MonadTAC f m) => Strictness -> ShouldHoist -> [E (HFixLab f)] -> GRewriteM m (HFixLab f)
subexpToTmp sn hc prevs t = if hc == StopHoisting then
                              return t
                            else
                              (   dynamicR (extractNonValue sn hc prevs)
                                  +> tacSpecial
                                  +> allStateR hoistChild (HoistExpState subsns subhcs []))
                                t
  where
    subsns = getStrictness $ stripA t
    subhcs = shouldHoistChild hc $ stripA t


makeVarDecSingle :: (CanTransTAC f, MonadTAC f m, ContainsSingleDec f, ExtraSingleVarDeclConstraints f) =>
                      HFixLab f IdentL -> Maybe (HFixLab f (ExpressionSort f)) -> m (HFixLab f SingleLocalVarDeclL)
makeVarDecSingle ident mExp = annotateLabelOuter $ SingleLocalVarDecl' defaultLocalVarDeclAttrs (injF (Hole ident)) init
  where 
    init = case mExp of
      Nothing  -> NoLocalVarInit'
      Just exp -> JustLocalVarInit' (injF (Hole exp))

makeVarDecMulti :: (CanTransTAC f, MonadTAC f m, MultiLocalVarDecl :<: f, ContainsSingleDec f, ExtraMultiVarDeclConstraints f) =>
                      HFixLab f IdentL -> Maybe (HFixLab f (ExpressionSort f)) -> m (HFixLab f MultiLocalVarDeclL)
makeVarDecMulti ident exp = do
  innerDec <- makeVarDecSingle ident exp
  annotateLabelOuter $ MultiLocalVarDecl' defaultMultiLocalVarDeclCommonAttrs
                                          (insertFHole [innerDec])

makeAssignment :: (CanTransTAC f, AssignmentInsertion f ExtraAssignConstraints, MonadTAC f m) =>
                      HFixLab f IdentL -> Maybe (HFixLab f (ExpressionSort f)) -> m (Maybe (HFixLab f AssignL))
makeAssignment ident exp = case exp of
  Nothing -> return Nothing
  Just e  -> liftM Just $ annotateLabelOuter $ Assign' (injF $ Hole ident) AssignOpEquals' (injF $ Hole e)

makeVarDec :: forall f m. (CanTransTAC f, MonadTAC f m) => HFixLab f IdentL -> Maybe (HFixLab f (ExpressionSort f)) -> m (Maybe (HFixLab f BlockItemL))
makeVarDec ident x = case tacDeclsVariation (Proxy :: Proxy f) of
  HasExplicitDeclsVariation (MultiDecInsertionVariation  Dict) -> Just <$> (labeledInjF =<< makeVarDecMulti  ident x)
  HasExplicitDeclsVariation (SingleDecInsertionVariation Dict) -> Just <$> (labeledInjF =<< makeVarDecSingle ident x)
  NoExplicitDeclsVariation  Dict                               -> mapM labeledInjF =<< makeAssignment ident x

makeVarDecPair :: (CanTransTAC f, MonadTAC f m) => HFixLab f IdentL -> Maybe (HFixLab f (ExpressionSort f)) -> m (Maybe (PrependPair f BlockItemL))
makeVarDecPair ident x = do
  mDec <- makeVarDec ident x
  case mDec of
    Nothing  -> return Nothing
    Just dec -> do
       assn <- makeAssignment ident x
       lassn <- mapM labeledInjF assn
       return $ Just $ PrependPair dec lassn

trackModSet :: (CanTransTAC f, Monad m) => GRewriteM (ReaderT (LocalTACState f) m) (HFixLab f)
                                       -> GRewriteM m (HFixLab f)
trackModSet f t = runReaderT (f t) (makeLocalTACState (modifiedVariables t))

toTAC :: forall f m l. (CanTransTAC f) => HFixLab f l -> IO (HFixLab f l)
toTAC t = do
   gen <- mkCSLabelGen
   let progInf = makeProgInfo t
   randGen <- getStdGen

   let inserted = performCfgInsertions (Proxy :: Proxy BlockItemL) progInf trans t
   let evalled  = evalState (evalRandT inserted randGen) (makeTACState gen (GensymState 0 allIds Map.empty))
   return $ evalState (finalizeGensymNames evalled) (GensymState 0 allIds Map.empty)
  where
    allIds :: Set String
    allIds = Set.fromList $ [s | Ident' s <- (map stripA $ (subterms t :: [HFixLab f IdentL]))]

    trans :: GRewriteM (CfgInserterT f BlockItemL (RandT StdGen (State (TACState f)))) (HFixLab f)
    trans = allbuR (tryR (barrierCheck >=> (trackModSet $ subexpToTmp Strict ShouldHoist [])))
