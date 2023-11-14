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
  , annotateOuter
  ) where

import Control.Monad.Identity ( Identity(..) )
import Control.Monad.Trans ( MonadTrans(..) )
import Data.Default ( Default(..) )
import Data.Comp.Multi ( Node, Cxt(..), Context, (:=>), CxtFunM, SigFun, appSigFunM, appCxt, HFix, AnnHFix )
import Data.Comp.Multi.HTraversable ( HTraversable )
import Data.Comp.Multi.Ops ((:&:)(..), Sum (..), contract)

import Cubix.Sin.Compdata.Instances ()
import Data.Type.Equality

---- This exists so you can constrain a functor to be annotated without also
---- naming the unannotated functor. This makes it more convenient for inclusion
---- in constraint synonyms
class Annotated (f :: Node) a | f -> a where
  getAnn' :: f e l -> a

instance Annotated (f :&: a) a where
  getAnn' (_ :&: x) = x

instance ( Annotated f a
         , Annotated (Sum fs) a
         ) => Annotated (Sum (f ': fs)) a where
  getAnn' (Sum w a) = case contract w of
    Left Refl -> getAnn' a
    Right w0  -> go (Sum w0 a)

    where go :: (Annotated (Sum fs) a) => Sum fs e l -> a
          go = getAnn'


getAnn :: (Annotated f a) => HFix f :=> a
getAnn (Term x) = getAnn' x

class (Monad m) => MonadAnnotater a m where
  annM :: forall (f :: Node) e l. f e l -> m ((f :&: a) e l)

newtype AnnotateDefault x = AnnotateDefault' { runAnnotateDefault' :: Identity x}
  deriving ( Functor, Applicative, Monad )

pattern AnnotateDefault :: x -> AnnotateDefault x
pattern AnnotateDefault x = AnnotateDefault' (Identity x)

runAnnotateDefault :: AnnotateDefault (AnnHFix a f l) -> AnnHFix a f l
runAnnotateDefault = runIdentity . runAnnotateDefault'

-- | Specializing annotation to Maybe a to aid instance selection
instance (Default a) => MonadAnnotater a AnnotateDefault where
  annM x = return (x :&: def)

instance (MonadAnnotater a m, MonadTrans t, Monad (t m)) => MonadAnnotater a (t m) where
  annM = lift . annM

annotateM :: (HTraversable f, MonadAnnotater a m) => CxtFunM m f (f :&: a)
annotateM = appSigFunM annM

propAnnSigFun :: SigFun f g -> SigFun (f :&: a) (g :&: a)
propAnnSigFun f (t :&: a) = (f t) :&: a

annotateOuter :: (HTraversable f, MonadAnnotater a m) => Context f (AnnHFix a f) l -> m (AnnHFix a f l)
annotateOuter = fmap appCxt . annotateM