{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Cubix.Transformations.Hoist.Hoisting (
    hoistDeclarations
  ) where


import Control.Monad ( (>=>), liftM, mzero )
import Control.Monad ( MonadPlus )
import Control.Monad.State ( State, MonadState, get, put, evalState )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Maybe ( MaybeT, runMaybeT )
import Control.Monad.Writer.Lazy ( WriterT, MonadWriter, tell, runWriterT )
import Data.Maybe ( isJust )
import Data.Proxy ( Proxy(..) )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Traversable ( for )

import Control.Lens ( (&), (.~), (%=), (.=), (^.), use, Lens' )

import Data.Constraint ( Dict(..) )
import Data.Comp.Multi ( (:<:), HFix, HTraversable, HFunctor )

import Data.Comp.Multi.Strategic ( Rewrite, RewriteM, GRewrite, GRewriteM, TranslateM,
                                   promoteR, addFail, tryR, idR, allR, alltdR,
                                   guardedT, guardBoolT, isSortT,
                                   promoteRF, (>+>) )
import Data.Comp.Multi.Strategy.Classification ( DynCase, subterms )
import Data.Typeable ( Typeable )

import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax as P
import Cubix.Transformations.Hoist.Custom
import Cubix.Transformations.Variation

class NoConstraint (f :: (* -> *) -> * -> *) where
instance NoConstraint f where

type VarDeclFragment f = ( SingleLocalVarDecl :<: f
                         , OptLocalVarInit :<: f, Ident :<: f )
type AssignFragment f = ( Assign :<: f, AssignOpEquals :<: f )
type HasFunctors f = (ListF :<: f, ExtractF [] (HFix f))

-- Assume one-to-one mapping between (StatSort f) and BlockItemL
type CanHoist f = ( VariableInsertionVariation f NoConstraint NoConstraint
                  , VarDeclFragment f, AssignFragment f, Block :<: f
                  , VarInitToRhs (HFix f), VarDeclBinderToLhs (HFix f)
                  , HoistStateConstraints f
                  , SpecialHoist f
                  , BuiltinSpecialIdents f
                  , BlockHoisting f
                  , InjF f AssignL (StatSort f)
                  , InjF f (StatSort f) BlockItemL
                  , HTraversable f, HasFunctors f
                  , DynCase (HFix f) BlockL
                  , DynCase (HFix f) IdentL
                  , DynCase (HFix f) SingleLocalVarDeclL
                  , DynCase (HFix f) [(StatSort f)]
                  , DynCase (HFix f) [BlockItemL]
                  , Typeable (StatSort f)
                  )

type HoistMonad f = WriterT [HFix f BlockItemL] (State (HoistState f))
type MonadHoist f m = (MonadWriter [HFix f BlockItemL] m, MonadState (HoistState f) m)

-- FIXME FIXME FIXME: This breaks if a hoisted declaration shadows a declaration in an
-- outer scope, and the shadowed variable is referred to earlier in the block
--
-- 4/15/2018: Not sure if the above is still true; deserves some thinking
declToAssign :: (CanHoist f) => HFix f MultiLocalVarDeclCommonAttrsL -> HFix f SingleLocalVarDeclL -> [HFix f (StatSort f)]
declToAssign mattrs (SingleLocalVarDecl' lattrs b optInit) = case optInit of
  NoLocalVarInit'        -> []
  JustLocalVarInit' init -> [P.iAssign (varDeclBinderToLhs b)
                                        AssignOpEquals'
                                       (varInitToRhs mattrs b lattrs init)]

removeInit :: (CanHoist f) => HFix f SingleLocalVarDeclL -> HFix f SingleLocalVarDeclL
removeInit (SingleLocalVarDecl' a n _) = SingleLocalVarDecl' a n NoLocalVarInit'

refCheck :: (CanHoist f) => Set String -> HFix f SingleLocalVarDeclL -> Bool
refCheck prevRef (SingleLocalVarDecl' _ binder init) = Set.null $ Set.intersection boundIdents refs
  where
    refs = prevRef `Set.union` (referencedIdents init)
    boundIdents = referencedIdents binder

refCheckMulti :: (CanHoist f) => Set String -> [HFix f SingleLocalVarDeclL] -> Bool
refCheckMulti prevRefs decls = all (uncurry refCheck) $ zip cumPrevDecl decls
  where
    cumPrevDecl = map (Set.union prevRefs) $ cumulativeReferencedIdents decls

referencedIdents :: (CanHoist f) => HFix f l -> Set String
referencedIdents t = Set.fromList $ map (\(Ident' s) -> s) $ subterms t

-- This is stupid and conservative. For example, in the case of Java, if you
-- refer to a package "x.y.z", it will list "x", "y", and "z" as referred symbols
cumulativeReferencedIdents :: forall f l. (CanHoist f) => [HFix f l] -> [Set String]
cumulativeReferencedIdents = init . scanl addIdents Set.empty
  where
    addIdents :: Set String -> HFix f l -> Set String
    addIdents start t = Set.union start $ referencedIdents t

getThenModify :: MonadState s m => Lens' s a -> (a -> a) -> m a
getThenModify l f = use l >>= \x -> (l .= f x) >>= const (return x)
  
transformSingle :: (CanHoist f, SingleDecInsertion f NoConstraint, MonadHoist f m) => HFix f (StatSort f) -> MaybeT m [HFix f (StatSort f)]
transformSingle (projF -> Just t@(SingleLocalVarDecl' attr binder init)) = do
  seenIdents %= (Set.union $ referencedIdents init)
  hoistState <- get
  seenIdents %= (Set.union $ referencedIdents binder)

  if refCheck (hoistState ^. seenIdents) t && not (blockHoistSingle hoistState t)
    then do
      tell [injF $ removeInit t]
      return $ declToAssign undefined t
    else do
      mzero
transformSingle _ = mzero

-- If blocking one part of a multi-hoist, we could still hoist the other parts,
-- but that sounds like work to code.
transformMulti :: (CanHoist f, MultiDecInsertion f NoConstraint, MonadHoist f m) => HFix f (StatSort f) -> MaybeT m [HFix f (StatSort f)]
transformMulti (projF -> Just t@(MultiLocalVarDecl' attrs sdecls)) = do
  hoistState <- get
  seenIdents %= Set.union (referencedIdents sdecls)
  if refCheckMulti (hoistState ^. seenIdents) (extractF sdecls) && not (blockHoistMulti hoistState t)
    then
      -- This is hacky as fuck. It will break in something of the form "struct S {} x = {}, y = {};"
      -- (will cause the "struct S" to be duplicated.)
      if any (\(sdecl@(SingleLocalVarDecl' lattrs b init)) -> isJust $ specialHoistMulti hoistState attrs lattrs b init) (extractF sdecls) then
        liftM concat $ for (extractF sdecls) $ \(sdecl@(SingleLocalVarDecl' lattrs b init)) ->
                       case specialHoistMulti hoistState attrs lattrs b init of
                         Just (hoistedDecls, assns) -> do
                             tell hoistedDecls
                             return assns
                         Nothing -> do
                             tell [iMultiLocalVarDecl attrs (SingletonF' $ removeInit sdecl)]
                             return $ declToAssign attrs sdecl
      else do
        tell [iMultiLocalVarDecl attrs (mapF removeInit sdecls)]
        return $ concatMap (declToAssign attrs) (extractF sdecls)

    else
      mzero
transformMulti _ = mzero

transformListHead :: (MonadPlus m, Typeable l, HFunctor f, ListF :<: f) => (HFix f l -> m [HFix f l]) -> RewriteM m (HFix f) [l]
transformListHead f (ConsF' t ts) = f t >>= return . (foldr (ConsF' . injF) ts)
transformListHead _ NilF' = mzero

transformStatSorts :: forall f m. (CanHoist f, MonadHoist f m) => RewriteM (MaybeT m) (HFix f) [(StatSort f)]
transformStatSorts = case variableInsertionVariation (Proxy :: Proxy f) (Proxy :: Proxy NoConstraint) (Proxy :: Proxy NoConstraint) of
  MultiDecInsertionVariation  Dict -> transformListHead transformMulti
  SingleDecInsertionVariation Dict -> transformListHead transformSingle

transformBlockItems :: forall f m. (CanHoist f, MonadHoist f m) => RewriteM (MaybeT m) (HFix f) [BlockItemL]
transformBlockItems t = mapF injF <$> (transformStatSorts $ mapF fromProjF t)

addIdents :: (CanHoist f, MonadHoist f m) => RewriteM (MaybeT m) (HFix f) IdentL
addIdents (Ident' s) = (seenIdents %= Set.insert s) *> mzero

updateState :: (CanHoist f, MonadHoist f m) => GRewriteM (MaybeT m) (HFix f)
updateState = promoteRF addIdents >+> (\t -> updateSpecialState t *> mzero)

tillFailure :: (Monad m, HTraversable f) => GRewriteM (MaybeT m) (HFix f) -> GRewriteM m (HFix f)
tillFailure f t = liftM (maybe t id) $ runMaybeT $ f t >>= lift . (tillFailure f)

-- Run non-transformation f top-down on block subtrees, and apply transformation g top-down on non-block nodes
transformOuterScope :: (MonadHoist f m, CanHoist f) => GRewriteM m (HFix f) -> GRewriteM (MaybeT m) (HFix f) -> GRewriteM m (HFix f)
transformOuterScope f g = tryR $ guardedT (guardBoolT (isSortT (Proxy :: Proxy BlockL)))
                                          (addFail $ alltdR f) (addFail $ (tillFailure g) >=> allR (transformOuterScope f g))

hoistBlock :: forall f. (CanHoist f) => Rewrite (HFix f) BlockL
hoistBlock (Block' items end) = return $ Block' (insertF $ decls ++ extractF items') end
  where
    hoist :: HoistMonad f (HFix f [BlockItemL])
    hoist = transformOuterScope (promoteR addIdents) (updateState >+> (promoteRF transformBlockItems) >+> (promoteRF transformStatSorts)) items
    startState = emptyHoistState & seenIdents .~ (builtinReferencedIdents (Proxy :: Proxy f))
    (items', decls) = evalState (runWriterT hoist) startState

hoistDeclarations :: (CanHoist f) => GRewrite (HFix f)
hoistDeclarations = alltdR $ promoteR $ addFail hoistBlock
