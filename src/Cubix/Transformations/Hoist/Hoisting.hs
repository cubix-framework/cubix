{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

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
import Data.Comp.Multi ( HTraversable, HFunctor, Term, (:-<:), All, HFoldable )

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

class NoConstraint (fs :: [(* -> *) -> * -> *]) where
instance NoConstraint fs where

type VarDeclFragment fs = ( SingleLocalVarDecl :-<: fs
                         , OptLocalVarInit :-<: fs, Ident :-<: fs )
type AssignFragment fs = ( Assign :-<: fs, AssignOpEquals :-<: fs )
type HasFunctors fs = (ListF :-<: fs, ExtractF [] (Term fs))

-- Assume one-to-one mapping between (StatSort f) and BlockItemL
type CanHoist fs = ( VariableInsertionVariation fs NoConstraint NoConstraint
                   , VarDeclFragment fs, AssignFragment fs, Block :-<: fs
                   , VarInitToRhs (Term fs), VarDeclBinderToLhs (Term fs)
                   , HoistStateConstraints fs
                   , SpecialHoist fs
                   , BuiltinSpecialIdents fs
                   , BlockHoisting fs
                   , InjF fs AssignL (StatSort fs)
                   , InjF fs (StatSort fs) BlockItemL
                   , All HTraversable fs, HasFunctors fs
                   , All HFoldable fs
                   , DynCase (Term fs) BlockL
                   , DynCase (Term fs) IdentL
                   , DynCase (Term fs) SingleLocalVarDeclL
                   , DynCase (Term fs) [(StatSort fs)]
                   , DynCase (Term fs) [BlockItemL]
                   , Typeable (StatSort fs)
                  )

type HoistMonad fs = WriterT [Term fs BlockItemL] (State (HoistState fs))
type MonadHoist fs m = (MonadWriter [Term fs BlockItemL] m, MonadState (HoistState fs) m)

-- FIXME FIXME FIXME: This breaks if a hoisted declaration shadows a declaration in an
-- outer scope, and the shadowed variable is referred to earlier in the block
--
-- 4/15/2018: Not sure if the above is still true; deserves some thinking
declToAssign :: (CanHoist fs) => Term fs MultiLocalVarDeclCommonAttrsL -> Term fs SingleLocalVarDeclL -> [Term fs (StatSort fs)]
declToAssign mattrs (SingleLocalVarDecl' lattrs b optInit) = case optInit of
  NoLocalVarInit'        -> []
  JustLocalVarInit' init -> [P.iAssign (varDeclBinderToLhs b)
                                        AssignOpEquals'
                                       (varInitToRhs mattrs b lattrs init)]

removeInit :: (CanHoist fs) => Term fs SingleLocalVarDeclL -> Term fs SingleLocalVarDeclL
removeInit (SingleLocalVarDecl' a n _) = SingleLocalVarDecl' a n NoLocalVarInit'

refCheck :: (CanHoist fs) => Set String -> Term fs SingleLocalVarDeclL -> Bool
refCheck prevRef (SingleLocalVarDecl' _ binder init) = Set.null $ Set.intersection boundIdents refs
  where
    refs = prevRef `Set.union` (referencedIdents init)
    boundIdents = referencedIdents binder

refCheckMulti :: (CanHoist fs) => Set String -> [Term fs SingleLocalVarDeclL] -> Bool
refCheckMulti prevRefs decls = all (uncurry refCheck) $ zip cumPrevDecl decls
  where
    cumPrevDecl = map (Set.union prevRefs) $ cumulativeReferencedIdents decls

referencedIdents :: (CanHoist fs) => Term fs l -> Set String
referencedIdents t = Set.fromList $ map (\(Ident' s) -> s) $ subterms t

-- This is stupid and conservative. For example, in the case of Java, if you
-- refer to a package "x.y.z", it will list "x", "y", and "z" as referred symbols
cumulativeReferencedIdents :: forall fs l. (CanHoist fs) => [Term fs l] -> [Set String]
cumulativeReferencedIdents = init . scanl addIdents Set.empty
  where
    addIdents :: Set String -> Term fs l -> Set String
    addIdents start t = Set.union start $ referencedIdents t

getThenModify :: MonadState s m => Lens' s a -> (a -> a) -> m a
getThenModify l f = use l >>= \x -> (l .= f x) >>= const (return x)
  
transformSingle ::
  ( CanHoist fs
  , SingleDecInsertion fs NoConstraint
  , MonadHoist fs m
  ) => Term fs (StatSort fs) -> MaybeT m [Term fs (StatSort fs)]
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
transformMulti ::
  ( CanHoist fs
  , MultiDecInsertion fs NoConstraint
  , MonadHoist fs m
  ) => Term fs (StatSort fs) -> MaybeT m [Term fs (StatSort fs)]
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

transformListHead :: (MonadPlus m, Typeable l, All HFunctor fs, ListF :-<: fs) => (Term fs l -> m [Term fs l]) -> RewriteM m (Term fs) [l]
transformListHead f (ConsF' t ts) = f t >>= return . (foldr (ConsF' . injF) ts)
transformListHead _ NilF' = mzero

transformStatSorts :: forall fs m. (CanHoist fs, MonadHoist fs m) => RewriteM (MaybeT m) (Term fs) [(StatSort fs)]
transformStatSorts = case variableInsertionVariation (Proxy :: Proxy fs) (Proxy :: Proxy NoConstraint) (Proxy :: Proxy NoConstraint) of
  MultiDecInsertionVariation  Dict -> transformListHead transformMulti
  SingleDecInsertionVariation Dict -> transformListHead transformSingle

transformBlockItems :: forall fs m. (CanHoist fs, MonadHoist fs m) => RewriteM (MaybeT m) (Term fs) [BlockItemL]
transformBlockItems t = mapF injF <$> (transformStatSorts $ mapF fromProjF t)

addIdents :: (CanHoist fs, MonadHoist fs m) => RewriteM (MaybeT m) (Term fs) IdentL
addIdents (Ident' s) = (seenIdents %= Set.insert s) *> mzero

updateState :: (CanHoist fs, MonadHoist fs m) => GRewriteM (MaybeT m) (Term fs)
updateState = promoteRF addIdents >+> (\t -> updateSpecialState t *> mzero)

tillFailure :: (Monad m, All HTraversable fs) => GRewriteM (MaybeT m) (Term fs) -> GRewriteM m (Term fs)
tillFailure f t = liftM (maybe t id) $ runMaybeT $ f t >>= lift . (tillFailure f)

-- Run non-transformation f top-down on block subtrees, and apply transformation g top-down on non-block nodes
transformOuterScope ::
  ( MonadHoist fs m
  , CanHoist fs
  ) => GRewriteM m (Term fs) -> GRewriteM (MaybeT m) (Term fs) -> GRewriteM m (Term fs)
transformOuterScope f g = tryR $ guardedT (guardBoolT (isSortT (Proxy :: Proxy BlockL)))
                                          (addFail $ alltdR f) (addFail $ (tillFailure g) >=> allR (transformOuterScope f g))

hoistBlock :: forall fs. (CanHoist fs) => Rewrite (Term fs) BlockL
hoistBlock (Block' items end) = return $ Block' (insertF $ decls ++ extractF items') end
  where
    hoist :: HoistMonad fs (Term fs [BlockItemL])
    hoist = transformOuterScope (promoteR addIdents) (updateState >+> (promoteRF transformBlockItems) >+> (promoteRF transformStatSorts)) items
    startState = emptyHoistState & seenIdents .~ (builtinReferencedIdents (Proxy :: Proxy fs))
    (items', decls) = evalState (runWriterT hoist) startState

hoistDeclarations :: (CanHoist fs) => GRewrite (Term fs)
hoistDeclarations = alltdR $ promoteR $ addFail hoistBlock
