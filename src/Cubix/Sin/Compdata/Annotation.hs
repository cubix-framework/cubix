{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Sin.Compdata.Annotation (
    Annotated(..)
  , getAnn
  , MonadAnnotater(..)
  , AnnotateDefault
  , pattern AnnotateDefault
  , runAnnotateDefault
  , annotateM
  , propAnnSigFun
  ) where

import Control.Monad.Identity ( Identity(..) )
import Data.Default ( Default(..) )
import Data.Comp.Multi ( AnnTerm, Cxt(..), Term, (:=>), CxtFunM, SigFun, appSigFunM )
import Data.Comp.Multi.HTraversable ( HTraversable )
import Data.Comp.Multi.Ops ((:&:)(..), (:+:)(..), caseH )

import Cubix.Sin.Compdata.Instances ()

---- This exists so you can constrain a functor to be annotated without also
---- naming the unannotated functor. This makes it more convenient for inclusion
---- in constraint synonyms
class Annotated (f :: (* -> *) -> * -> *) a | f -> a where
  getAnn' :: f e l -> a

instance Annotated (f :&: a) a where
  getAnn' (_ :&: x) = x

instance (Annotated f a, Annotated g a) => Annotated (f :+: g) a where
  getAnn' = caseH getAnn' getAnn'

getAnn :: (Annotated f a) => Term f :=> a
getAnn (Term x) = getAnn' x

class (Monad m) => MonadAnnotater a m where
  annM :: forall f (e :: * -> *) l. f e l -> m ((f :&: a) e l)

newtype AnnotateDefault a x = AnnotateDefault' { runAnnotateDefault' :: Identity x}
  deriving ( Functor, Applicative, Monad )

pattern AnnotateDefault :: x -> AnnotateDefault a x
pattern AnnotateDefault x = AnnotateDefault' (Identity x)

runAnnotateDefault :: AnnotateDefault a (AnnTerm a f l) -> AnnTerm a f l
runAnnotateDefault = runIdentity . runAnnotateDefault'

-- | Specializing annotation to Maybe a to aid instance selection
instance MonadAnnotater (Maybe a) (AnnotateDefault a) where
  annM x = return (x :&: def)

annotateM :: (HTraversable f, MonadAnnotater a m) => CxtFunM m f (f :&: a)
annotateM = appSigFunM annM

propAnnSigFun :: SigFun f g -> SigFun (f :&: a) (g :&: a)
propAnnSigFun f (t :&: a) = (f t) :&: a