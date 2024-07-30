{-# LANGUAGE AllowAmbiguousTypes #-} -- For isSortR and isSortT
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Strategic
-- Copyright   :  James Koppel, 2013
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Strategy combinators for cubix-compdata. Older versions of this package were built for vanilla compdata.
--
-- Strategy combinators are functions that build more complicated traversals
-- out of smaller (perhaps single-node) traversals. The name comes from the idea of a
-- rewriting strategy, an algorithm for choosing how to apply a set of rewrite rules.
-- For a tutorial introduction to strategy combinators, see:
--
-- * \"The Essence of Strategic Programming\", Ralf Lämmel et al, online
--    at https://eelcovisser.org/publications/2002/LaemmelVV02.pdf
--
-- Functions in this module have a particular naming schema:
--
--
-- * Suffix @R@ (e.g.: `anybuR`): Short for \"rewrite\". It means the function is type-preserving
--   (rewrites a term to another term of the same sort).
--
-- * Suffix @T@ (e.g.: `onetdT`): Short for \"translate\". It means the function rewrites a tree to a fixed type.
--
-- * Suffix @F@ (e.g.: `promoteTF`): Short for \"failable\". This denotes combinators whose result is a rewrite in the
--   `Maybe` monad.
--
-- * @td@ (e.g.: `crushtdT`): Short for \"top-down traversal\"
--
-- * @bu@ (e.g.: `anybuR`): Short for \"bottom-up traversal\"
--
-- The design (and naming system) of `compstrat` is heavily inspired by an older library of strategy combinators,
-- KURE, https://hackage.haskell.org/package/kure
--
-----------------------------------------------------------------------------

module Data.Comp.Multi.Strategic
  (
    -- * Rewrites
    -- ** Core types
    RewriteM
  , Rewrite
  , GRewriteM
  , GRewrite

  -- ** Rewrite combinators for failure
  , addFail
  , tryR
  , failR

  -- ** Rewrite combinators for sorts
  , dynamicR
  , promoteR
  , promoteRF

  -- ** Sequential combination of rewrites
  , (>+>)
  , (<+)

  -- ** One-level traversal combinators
  , allR
  , revAllR
  , anyR
  , oneR
  , allStateR
  , allIdxR

  -- ** Whole-term traversals
  , alltdR
  , allbuR
  , anytdR
  , anybuR
  , revAllbuR
  , prunetdRF
  , prunetdR
  , onetdR
  , onebuR

  , idR
  , traceR
  , isSortR

    -- * Translations
    -- ** Core types
  , Translate
  , TranslateM
  , GTranslateM

    -- ** Conditions
  , guardBoolT
  , guardedT

    -- ** Traversals
  , foldtdT
  , crushtdT
  , onetdT

  , (+>>)
  , isSortT
  , failT
  , notT
  , promoteTF
  , mtryM
  , foldT
  ) where

import Control.Applicative ( Applicative, (<*), liftA, liftA2, Alternative(..) )
import Control.Applicative.Backwards ( Backwards(..) )

import Control.Monad ( MonadPlus(..), liftM, liftM2, (>=>), guard )
import Control.Monad.Identity ( Identity, runIdentity )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Maybe ( MaybeT, runMaybeT )
import Control.Monad.State ( StateT, runStateT, evalStateT, get, put )
import Control.Monad.Writer ( WriterT, runWriterT, tell )

import Control.Parallel.Strategies ( withStrategy, rparWith, rpar, Eval, runEval )

import Debug.Trace ( traceM )

import Data.Comp.Multi ( Cxt(..), HFix, unTerm, (:->), (:=>) )
import Data.Comp.Multi.Generic ( query )
import Data.Comp.Multi.HFoldable ( HFoldable(..) )
import Data.Comp.Multi.HTraversable ( HTraversable(..) )
import Data.Comp.Multi.Show ( KShow, ShowHF )
import Data.Monoid ( Monoid, mappend, mempty, Any(..) )
import Data.Type.Equality ( (:~:)(..), sym )

import Data.Comp.Multi.Strategy.Classification

--------------------------------------------------------------------------------

-- Porting from the old type-equality library to the new base Data.Type.Equality
-- Haven't yet looked into rewriting with gcastWith instead

subst :: (a :~: b) -> f a -> f b
subst Refl x = x

subst2 :: (a :~: b) -> f (g a) -> f (g b)
subst2 Refl x = x

--------------------------------------------------------------------------------

evalPar :: (HTraversable f) => f l :-> f l
--evalPar = withStrategy (htraverse id)
evalPar = id

--------------------------------------------------------------------------------

-- | The basic type of rewrites. A @RewriteM m f l@ rewrites a term
--   of signature @f@, sort @l@, to another such term, with effects in monad @m@
--
-- @Rewrite m f l@ is equivalent to @`TranslateM` m f l (f l)@.
type RewriteM m f l = f l -> m (f l)

-- | A rewrite with no effects
type Rewrite f l = RewriteM Identity f l

-- | An effectful rewrite that runs on terms of any sort
type GRewriteM m f = forall l. RewriteM m f l

-- | A rewrite that runs on terms of any sort and has no effects
type GRewrite f = GRewriteM Identity f

--------------------------------------------------------------------------------
-- Rewrites
--------------------------------------------------------------------------------

type AnyR m = WriterT Any m

wrapAnyR :: (MonadPlus m) => RewriteM m f l -> RewriteM (AnyR m) f l
wrapAnyR f t = (lift (f t) <* tell (Any True)) `mplus` return t

unwrapAnyR :: MonadPlus m => RewriteM (AnyR m) f l -> RewriteM m f l
unwrapAnyR f t = do (t', Any b) <- runWriterT (f t)
                    if b then
                      return t'
                     else
                      mzero

--------------------------------------------------------------------------------

type OneR m = StateT Bool m

wrapOneR :: (MonadPlus m) => RewriteM m f l -> RewriteM (OneR m) f l
wrapOneR f t = do b <- get
                  if b then
                    return t
                   else
                    (lift (f t) <* put True) `mplus` return t

unwrapOneR :: MonadPlus m => RewriteM (OneR m) f l -> RewriteM m f l
unwrapOneR f t = do (t', b) <- runStateT (f t) False
                    if b then
                      return t'
                     else
                      mzero

--------------------------------------------------------------------------------

-- | Turns a rewrite that runs on a single sort to one that runs on any sort,
--   failing for all other sorts.
dynamicR :: (DynCase f l, MonadPlus m) => RewriteM m f l -> GRewriteM m f
dynamicR f t = case dyncase t of
                 Just p -> subst2 (sym p) $ f (subst p t)
                 Nothing -> mzero

-- | Turns a rewrite that may fail into one that unconditionally succeeds, replacing
--   failures with identity.
tryR :: (Monad m) => RewriteM (MaybeT m) f l -> RewriteM m f l
tryR f t = liftM (maybe t id) $ runMaybeT (f t)

-- | The rewrite that always fails
failR :: (MonadPlus m) => RewriteM m f l
failR = const mzero

-- | Turns a failable rewrite on one sort @l@ into a rewrite that always succeeds, and runs on any sort,
--   performing the identity rewrite on terms of sort other than @l@. Defined @`tryR` . `dynamicR`@
promoteR :: (DynCase f l, Monad m) => RewriteM (MaybeT m) f l -> GRewriteM m f
promoteR = tryR . dynamicR

-- | Turns a rewrite that runs on a single sort to one that runs on any sort,
--   failing for all other sorts. Equivalent to `dynamicR`
promoteRF :: (DynCase f l, Monad m) => RewriteM (MaybeT m) f l -> GRewriteM (MaybeT m) f
promoteRF = dynamicR

-- | Applies a rewrite to all immediate subterms of the current term.
--   Ignores holes.
allR :: (Applicative m, HTraversable f) => GRewriteM m (Cxt h f a) -> RewriteM m (Cxt h f a) l
allR f h@(Hole _) = pure h
allR f   (Term t) = liftA Term $ htraverse f $ t
--allR f t = liftA Term $ evalPar $ htraverse f $ unTerm t

-- Ignores holes
-- | Like `allR`, but runs on the children in reverse order
revAllR :: (Applicative m, HTraversable f) => GRewriteM m (Cxt h f a) -> RewriteM m (Cxt h f a) l
revAllR f h@(Hole _) = pure h
revAllR f   (Term t) = liftA Term $ forwards $ htraverse (Backwards . f) $ t

-- | Runs a stateful computation on all immediate children of the current term.
allStateR :: forall m f s h a l.
               (Monad m, HTraversable f)
            => (forall i. s -> Cxt h f a i -> m (Cxt h f a i, s)) -- ^ A stateful computation
            -> s                                                  -- ^ The start state
            -> RewriteM m (Cxt h f a) l
allStateR f s h@(Hole _) = pure h
allStateR f s   (Term t) = liftA Term $ evalStateT (htraverse f' t) s
  where
    f' :: GRewriteM (StateT s m) (Cxt h f a)
    f' x = do st <- get
              (x', st') <- lift $ f st x
              put st'
              return x'


-- | Let @f@ be a rewrite with an extra `Int` parameter, intended to be called @f i t@, where @t@
--   is a term and @i@ is the index of @t@ among its parent's children. Then @allIdxR f x@ runs
--   @f@ on all children of @x@.
allIdxR :: (Monad m, HTraversable f) => (Int -> GRewriteM m (Cxt h f a)) -> RewriteM m (Cxt h f a) l
allIdxR f = allStateR (\n x -> (,) <$> f n x <*> pure (n + 1)) 0


-- | Applies two rewrites in suceesion, succeeding if one or both succeed
(>+>) :: (MonadPlus m) => GRewriteM m f -> GRewriteM m f -> GRewriteM m f
f >+> g = unwrapAnyR (wrapAnyR f >=> wrapAnyR g)

-- | Left-biased choice -- (f <+ g) runs f, and, if it fails, then runs g
(<+) :: (Alternative m) => TranslateM m f l t -> TranslateM m f l t -> TranslateM m f l t
(<+) f g x = f x <|> g x

-- | Applies a rewrite to all immediate subterms of the current term, succeeding if any succeed
--   Ignores holes.
anyR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> RewriteM m (Cxt h f a) l
anyR f = unwrapAnyR $ allR $ wrapAnyR f -- not point-free because of type inference

-- | Applies a rewrite to the first immediate subterm of the current term where it can succeed
--   Ignores holes.
oneR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> RewriteM m (Cxt h f a) l
oneR f = unwrapOneR $ allR $ wrapOneR f -- not point-free because of type inference

-- | Runs a rewrite in a bottom-up traversal. Defined:
--   @
--       allbuR f = `allR` (allbuR f) >=> f
--   @
--   Ignores holes.
allbuR :: (Monad m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
allbuR f = allR (allbuR f) >=> f


-- | Like `allbuR`, but runs in right-to-left order
revAllbuR :: (Monad m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
revAllbuR f = revAllR (revAllbuR f) >=> f

-- | Runs a rewrite in a top-down traversal Defined:
--   @
--       alltdR f = f >=> allR (alltdR f)
--   @
--
--   Ignores holes.
alltdR :: (Monad m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
alltdR f = f >=> allR (alltdR f)

-- | Runs a rewrite in a bottom-up traversal, succeeding if any succeed. Defined:
--    @
--      anybuR f = anyR (anybuR f) >+> f
--    @
--
--   Ignores holes.
anybuR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
anybuR f = anyR (anybuR f) >+> f

-- | Runs a rewrite in a top-down traversal, succeeding if any succeed. Defined:
--    @
--      anytdR f = f >+> anyR (anytdR f)
--    @
anytdR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
anytdR f = f >+> anyR (anytdR f)

-- | Runs a rewrite in a top-down traversal, succeeding if any succeed, and pruning below successes. Defined:
--    @
--       prunetdRF f = f <+ anyR (prunetdRF f)
--    @
--
--   Ignores holes.
prunetdRF :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
prunetdRF f = f <+ anyR (prunetdRF f)

-- | Like `prunetdRF`, but the outer level always succeeds. Defined @`tryR` . `prunetdRF`@
--   Ignores holes.
prunetdR :: (Monad m, HTraversable f) => GRewriteM (MaybeT m) (Cxt h f a) -> GRewriteM m (Cxt h f a)
prunetdR f = tryR (prunetdRF f)

-- | Applies a rewrite to the first node where it can succeed in a bottom-up traversal. Defined:
--    @
--      onebuR f = oneR (onebuR f) <+ f
--    @
--
--   Ignores holes.
onebuR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
onebuR f = oneR (onebuR f) <+ f

-- | Applies a rewrite to the first node where it can succeed in a top-down traversal. Defined:
--    @
--      onetdR f = f <+ oneR (onetdR f)
--    @
-- 
--   Ignores holes.
onetdR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
onetdR f = f <+ oneR (onetdR f)

-- | The identity rewrite
idR :: (Applicative m) => RewriteM m f l
idR = pure

-- | Wraps a rewrite with one that performs a debug-print of each visited node
traceR :: (ShowHF f, KShow a, HTraversable f, Monad m) => RewriteM m (Cxt h f a) l
traceR x = do
  traceM $ show x
  return x

-- | @isSortR \@l@ performs the identity rewrite at terms of sort @l@,
--   and fails for all other terms.
isSortR :: forall l l' f m. (MonadPlus m, DynCase f l) => RewriteM m f l'
isSortR = guardedT (guardBoolT (isSortT @l)) idR failR

--------------------------------------------------------------------------------
-- Translations
--------------------------------------------------------------------------------

-- | A single-sorted translation in the Identity monad
type Translate f l t = TranslateM Identity f l t

-- | A monadic translation for a single sort
type TranslateM m f l t = f l -> m t

-- | A monadic translation for all sorts
type GTranslateM m f t = forall l. TranslateM m f l t

-- | @f <+> g@ runs @f@ and @g@ in sequence, ignoring the output of @f@, and returning the output of @g@.
(+>>) :: (Monad m) => TranslateM m f l t -> TranslateM m f l u -> TranslateM m f l u
(+>>) f g t = f t *> g t

-- | @isSortT \@l@ is the translate that succeeds for terms of sort @l@, and fails elsewhere
isSortT :: forall l l' f m. (DynCase f l, Applicative m) => TranslateM m f l' Bool
isSortT = pure . isSort @l

-- | Takes a boolean function of a term, and converts `False` values to failure in the monad
guardBoolT :: (MonadPlus m) => TranslateM m f l Bool -> TranslateM m f l ()
guardBoolT t x = t x >>= guard

-- | Guarded choice: @guardedT g t e@ runs @t@ (\"then branch\") on its input if @g@ succeeds, and otherwise runs @e@
--   (\"else branch\")
guardedT :: (Alternative m) => TranslateM m f l t -> TranslateM m f l u -> TranslateM m f l u -> TranslateM m f l u
guardedT g t e x = (g x *> t x) <|> (e x)

-- | The translation that always fails
failT :: (Alternative m) => TranslateM m f l t
failT = const empty

-- FIXME: This is apparently broken
-- | Takes a translation, and replaces success with failure, and replaces failure with the identity rewrite
notT :: (Alternative m) => TranslateM m f l t -> RewriteM m f l
notT t = guardedT t failT idR

-- | Allows a one-sorted translation to be applied to any sort, failing at sorts
--   different form the original
promoteTF :: (DynCase f l, Alternative m) => TranslateM m f l t -> GTranslateM m f t
promoteTF f t = case dyncase t of
                  Just p -> f (subst p t)
                  Nothing -> empty

-- | Lifts a translation into the Maybe monad, allowing it to fail
addFail :: Monad m => TranslateM m f l t -> TranslateM (MaybeT m) f l t
addFail = (lift . )

-- | Runs a failable computation, replacing failure with `mempty`
mtryM :: (Applicative m, Monoid a) => MaybeT m a -> m a
mtryM = fmap (maybe mempty id) . runMaybeT

-- | Runs a translation in a top-down manner, combining its effects. Succeeds if any succeeds.
--   When run using MaybeT, returns its result for the last node where it succeded
onetdT :: (MonadPlus m, HFoldable f) => GTranslateM m (HFix f) t -> GTranslateM m (HFix f) t
onetdT t = query t mplus

parQuery :: forall r f. HFoldable f => (HFix f :=> r) -> (r -> r -> r) -> HFix f :=> r
parQuery q c tree = runEval $ rec 8 tree
  where
    rec :: Int -> HFix f :=> Eval r
    rec 0 i = rpar $ query q c i
    rec depth i@(Term t) = hfoldl (\s x -> liftM2 c s (rec (depth-1) x)) (rpar $ q i) t

-- | Runs a translation on each node which returns a value in some monoid, and combines the results.
foldT :: (HFoldable f, Monoid t, Applicative m) => GTranslateM m (HFix f) t -> TranslateM m (HFix f) l t
foldT t (Term tree) = hfoldl (\s x -> liftA2 mappend s (t x)) (pure mempty) tree

-- | Fold a tree in a top-down manner. Includes some rudimentary parallelism.
foldtdT :: (HFoldable f, Monoid t, Monad m) => GTranslateM m (HFix f) t -> GTranslateM m (HFix f) t
foldtdT t = parQuery t (liftM2 mappend)

-- | An always successful top-down fold, replacing failures with `mempty`.
crushtdT :: (HFoldable f, Monoid t, Monad m) => GTranslateM (MaybeT m) (HFix f) t -> GTranslateM m (HFix f) t
crushtdT f = foldtdT $ mtryM . f

-- | Similar to crushtdT, except it never recurses below successes
pruningCrushtdTF :: (HFoldable f, Monoid t, Alternative m) => GTranslateM m (HFix f) t -> GTranslateM m (HFix f) t
pruningCrushtdTF f = f <+ (\(Term t) -> hfoldl (\s x -> liftA2 (<>) s $ pruningCrushtdTF f x) (pure mempty) t)

-- NOTE 2024.07.30: Monad instead of Applicative, because the Applicative instance for MaybeT requires Monad (will submit issue)
-- | pruningCrushtdTF, replacing top-level failure with mempty
pruningCrushtdT :: (HFoldable f, Monoid t, Monad m) => GTranslateM (MaybeT m) (HFix f) t -> GTranslateM m (HFix f) t
pruningCrushtdT f = mtryM . pruningCrushtdTF f

-- | Gives all subterms of any given sort of a term which are not contained in another term of that sort
maximalSubterms :: forall f l l'. (DynCase (HFix f) l, HFoldable f) => HFix f l' -> [HFix f l]
maximalSubterms = runIdentity . pruningCrushtdT (promoteTF idOnTargetSort)
  where
    idOnTargetSort :: TranslateM (MaybeT Identity) (HFix f) l [HFix f l]
    idOnTargetSort x = return [x]