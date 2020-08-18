{-# LANGUAGE GADTs               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}



--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Mutable
-- Copyright   :  (c) 2020 James Koppel
-- License     :  BSD3
--
-- Experimental module for mutable terms.
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Mutable (
    Label
  , Mut
  , HMut
  , Cell(..)

  , MutCxt
  , MutTerm
  ) where


import Control.Monad
import Control.Monad.IO.Class

import Data.IORef

import Data.Comp.Multi.Term
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable

newtype Mut a = Mut { runMut :: IO a }
  deriving ( Functor, Applicative, Monad )

instance MonadIO Mut where
  liftIO = Mut

type HMut = HMonad Mut

newtype HIORef g l = HIORef { unHIORef :: IORef (g l)}

type Label = Int
data Cell f g i = Cell { parent :: IORef (Maybe (E g))
                       , elt    :: f (HIORef g) i
                       --, label  :: Label
                       }

type MutCxt h f = Cxt h (Cell f)
type MutTerm f = MutCxt NoHole f (K ())

ref :: (MonadIO m) => a -> m (IORef a)
ref = liftIO . newIORef

deref :: (MonadIO m) => IORef a -> m a
deref = liftIO . readIORef

hderef :: (MonadIO m) => HIORef g i -> m (g i)
hderef = deref . unHIORef

refify :: (MonadIO m, HTraversable f) => f e i -> m (f (HIORef e) i)
refify = htraverse (liftM HIORef . ref)

(=:) :: (MonadIO m) => IORef a -> a -> m ()
(=:) r x = liftIO $ writeIORef r x

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

htraverseMut :: (MonadIO m, HTraversable f) => NatM m a b -> f (HIORef a) i -> m (f b i)
htraverseMut f = htraverse (f <=< hderef)

setChildren :: (HTraversable f) => MutTerm f i -> Mut ()
setChildren t@(Term (Cell {elt=elt})) = do
  htraverseMut (\(Term (Cell {parent=parPtr})) -> do curPar <- deref parPtr
                                                   --  assert (isNothing curPar) (return ())
                                                     parPtr =: Just (E t)
                                                     return (K ()))
    elt
  return ()


mkCell' :: (HTraversable f) => f (HIORef (MutTerm f)) i -> Mut (MutTerm f i)
mkCell' t = do
  parPtr <- ref Nothing
  let cell = Term $ Cell { elt=t, parent=parPtr }
  setChildren cell
  return cell

mkCell :: (HTraversable f) => f (MutTerm f) i -> Mut (MutTerm f i)
mkCell = mkCell' <=< refify
