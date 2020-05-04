{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Comp.Multi.Strategic
  (
    -- * Rewrites
    RewriteM
  , Rewrite
  , GRewriteM
  , GRewrite
  , addFail
  , tryR
  , failR
  , dynamicR
  , promoteR
  , promoteRF
  , allR
  , revAllR
  , allStateR
  , allIdxR
  , (>+>)
  , (+>)
  , anyR
  , oneR
  , alltdR
  , allbuR
  , revAllbuR
  , anytdR
  , anybuR
  , prunetdRF
  , prunetdR
  , onetdR
  , onebuR
  , idR
  , traceR
  , isSortR

    -- * Translations
  , Translate
  , TranslateM
  , GTranslateM
  , (+>>)
  , isSortT
  , guardBoolT
  , guardedT
  , failT
  , notT
  , promoteTF
  , mtryM
  , onetdT
  , foldT
  , foldtdT
  , crushtdT
  ) where

import Control.Applicative ( Applicative, (<*), liftA, liftA2, Alternative(..) )
import Control.Applicative.Backwards ( Backwards(..) )

import Control.Monad ( MonadPlus(..), liftM, liftM2, (>=>), guard )
import Control.Monad.Identity ( Identity )
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
import Data.Proxy ( Proxy(..) )
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

type RewriteM m f l = f l -> m (f l)
type Rewrite f l = RewriteM Identity f l
type GRewriteM m f = forall l. RewriteM m f l
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

dynamicR :: (DynCase f l, MonadPlus m) => RewriteM m f l -> GRewriteM m f
dynamicR f t = case dyncase t of
                 Just p -> subst2 (sym p) $ f (subst p t)
                 Nothing -> mzero

tryR :: (Monad m) => RewriteM (MaybeT m) f l -> RewriteM m f l
tryR f t = liftM (maybe t id) $ runMaybeT (f t)

failR :: (MonadPlus m) => RewriteM m f l
failR = const mzero

promoteR :: (DynCase f l, Monad m) => RewriteM (MaybeT m) f l -> GRewriteM m f
promoteR = tryR . dynamicR

promoteRF :: (DynCase f l, Monad m) => RewriteM (MaybeT m) f l -> GRewriteM (MaybeT m) f
promoteRF = dynamicR

-- | Applies a rewrite to all immediate subterms of the current term
--   Ignores holes.
allR :: (Applicative m, HTraversable f) => GRewriteM m (Cxt h f a) -> RewriteM m (Cxt h f a) l
allR f h@(Hole _) = pure h
allR f   (Term t) = liftA Term $ htraverse f $ t
--allR f t = liftA Term $ evalPar $ htraverse f $ unTerm t

-- Ignores holes
revAllR :: (Applicative m, HTraversable f) => GRewriteM m (Cxt h f a) -> RewriteM m (Cxt h f a) l
revAllR f h@(Hole _) = pure h
revAllR f   (Term t) = liftA Term $ forwards $ htraverse (Backwards . f) $ t

allStateR :: forall m f s h a l. (Monad m, HTraversable f) => (forall i. s -> Cxt h f a i -> m (Cxt h f a i, s)) -> s -> RewriteM m (Cxt h f a) l
allStateR f s h@(Hole _) = pure h
allStateR f s   (Term t) = liftA Term $ evalStateT (htraverse f' t) s
  where
    f' :: GRewriteM (StateT s m) (Cxt h f a)
    f' x = do st <- get
              (x', st') <- lift $ f st x
              put st'
              return x'


allIdxR :: (Monad m, HTraversable f) => (Int -> GRewriteM m (Cxt h f a)) -> RewriteM m (Cxt h f a) l
allIdxR f = allStateR (\n x -> (,) <$> f n x <*> pure (n + 1)) 0


-- | Applies two rewrites in suceesion, succeeding if one or both succeed
(>+>) :: (MonadPlus m) => GRewriteM m f -> GRewriteM m f -> GRewriteM m f
f >+> g = unwrapAnyR (wrapAnyR f >=> wrapAnyR g)

-- | Left-biased choice -- (f +> g) runs f, and, if it fails, then runs g
-- This naming is questionable. I believe KURE used +> for this, but Stratego uses <+
(+>) :: (Alternative m) => RewriteM m f l -> RewriteM m f l -> RewriteM m f l
(+>) f g x = f x <|> g x

-- | Applies a rewrite to all immediate subterms of the current term, succeeding if any succeed
anyR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> RewriteM m (Cxt h f a) l
anyR f = unwrapAnyR $ allR $ wrapAnyR f -- not point-free because of type inference

-- | Applies a rewrite to the first immediate subterm of the current term where it can succeed
oneR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> RewriteM m (Cxt h f a) l
oneR f = unwrapOneR $ allR $ wrapOneR f -- not point-free because of type inference

-- | Runs a rewrite in a bottom-up traversal
allbuR :: (Monad m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
allbuR f = allR (allbuR f) >=> f

revAllbuR :: (Monad m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
revAllbuR f = revAllR (revAllbuR f) >=> f

-- | Runs a rewrite in a top-down traversal
alltdR :: (Monad m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
alltdR f = f >=> allR (alltdR f)

-- | Runs a rewrite in a bottom-up traversal, succeeding if any succeed
anybuR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
anybuR f = anyR (anybuR f) >+> f

-- | Runs a rewrite in a top-down traversal, succeeding if any succeed
anytdR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
anytdR f = f >+> anyR (anytdR f)

-- | Runs a rewrite in a top-down traversal, succeeding if any succeed, and pruning below successes
prunetdRF :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
prunetdRF f = f +> anyR (prunetdRF f)

-- | Like prunetdRF, but the outer level always succeeds
prunetdR :: (Monad m, HTraversable f) => GRewriteM (MaybeT m) (Cxt h f a) -> GRewriteM m (Cxt h f a)
prunetdR f = tryR (prunetdRF f)

-- | Applies a rewrite to the first node where it can succeed in a bottom-up traversal
onebuR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
onebuR f = oneR (onebuR f) +> f

-- | Applies a rewrite to the first node where it can succeed in a top-down traversal
onetdR :: (MonadPlus m, HTraversable f) => GRewriteM m (Cxt h f a) -> GRewriteM m (Cxt h f a)
onetdR f = f +> oneR (onetdR f)

idR :: (Applicative m) => RewriteM m f l
idR = pure

traceR :: (ShowHF f, KShow a, HTraversable f, Monad m) => RewriteM m (Cxt h f a) l
traceR x = do
  traceM $ show x
  return x

isSortR :: (MonadPlus m, DynCase f l) => Proxy l -> RewriteM m f l'
isSortR p = guardedT (guardBoolT (isSortT p)) idR failR

--------------------------------------------------------------------------------
-- Translations
--------------------------------------------------------------------------------

-- | A single-sorted translation in the Identity monad
type Translate f l t = TranslateM Identity f l t

-- | A monadic translation for a single sort
type TranslateM m f l t = f l -> m t

-- | A monadic translation for all sorts
type GTranslateM m f t = forall l. TranslateM m f l t

(+>>) :: (Monad m) => TranslateM m f l t -> TranslateM m f l u -> TranslateM m f l u
(+>>) f g t = f t *> g t

isSortT :: (DynCase f l, Applicative m) => Proxy l -> TranslateM m f l' Bool
isSortT p = pure . isSort p

guardBoolT :: (MonadPlus m) => TranslateM m f l Bool -> TranslateM m f l ()
guardBoolT t x = t x >>= guard

-- | Guarded choice:
guardedT :: (Alternative m) => TranslateM m f l t -> TranslateM m f l u -> TranslateM m f l u -> TranslateM m f l u
guardedT g t e x = (g x *> t x) <|> (e x)

failT :: (Alternative m) => TranslateM m f l t
failT = const empty

-- FIXME: This is apparently broken
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

-- | Runs a failable computation, replacing failure with mempty
mtryM :: (Monad m, Monoid a) => MaybeT m a -> m a
mtryM = liftM (maybe mempty id) . runMaybeT

-- | Runs a translation in a top-down manner, combining its
--   When run using MaybeT, returns its result for the last node where it succeded
onetdT :: (MonadPlus m, HFoldable f) => GTranslateM m (HFix f) t -> GTranslateM m (HFix f) t
onetdT t = query t mplus

parQuery :: forall r f. HFoldable f => (HFix f :=>  r) -> (r -> r -> r) -> HFix f :=> r
parQuery q c tree = runEval $ rec 8 tree
  where
    rec :: Int -> HFix f :=> Eval r
    rec 0 i = rpar $ query q c i
    rec depth i@(Term t) = hfoldl (\s x -> liftM2 c s (rec (depth-1) x)) (rpar $ q i) t

foldT :: (HFoldable f, Monoid t, Applicative m) => GTranslateM m (HFix f) t -> TranslateM m (HFix f) l t
foldT t (Term tree) = hfoldl (\s x -> liftA2 mappend s (t x)) (pure mempty) tree

-- | Fold a tree in a top-down manner
foldtdT :: (HFoldable f, Monoid t, Monad m) => GTranslateM m (HFix f) t -> GTranslateM m (HFix f) t
foldtdT t = parQuery t (liftM2 mappend)

-- | An always successful top-down fold, replacing failures with mempty.
crushtdT :: (HFoldable f, Monoid t, Monad m) => GTranslateM (MaybeT m) (HFix f) t -> GTranslateM m (HFix f) t
crushtdT f = foldtdT $ mtryM . f
