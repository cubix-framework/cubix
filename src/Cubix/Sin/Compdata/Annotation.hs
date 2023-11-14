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
import Data.Comp.Multi ( All, caseCxt, Node, Cxt(..), Context, (:=>), CxtFunM, SigFun, appSigFunM, appCxt, HFix, AnnHFix )
import Data.Comp.Multi.HTraversable ( HTraversable )
import Data.Comp.Multi.Ops ((:&:)(..), Sum (..))

import Cubix.Sin.Compdata.Instances ()

--------------------------------------------------------------------------------------


-------------------------------------------------------------------
------------------------- Getting annotations ---------------------
-------------------------------------------------------------------

---- This exists so you can constrain a functor to be annotated without also
---- naming the unannotated functor. This makes it more convenient for inclusion
---- in constraint synonyms
class Annotated a (f :: Node) | f -> a where
  getAnn' :: f e l -> a

instance Annotated a (f :&: a) where
  getAnn' (_ :&: x) = x

-- The funny constraint is to get the functional dependency working
instance ( Annotated a f
         , All (Annotated a) fs
         ) => Annotated a (Sum (f ': fs)) where
  getAnn' = caseCxt @(Annotated a) getAnn'


getAnn :: (Annotated a f) => HFix f :=> a
getAnn (Term x) = getAnn' x

-------------------------------------------------------------------
------------------------- Adding annotations ---------------------
-------------------------------------------------------------------

-----------------------------------
------------------ MonadAnnotater
------------------------------------

-------------
--- Class
-------------

class (Monad m) => MonadAnnotater a m where
  annM :: forall (f :: Node) e l. f e l -> m ((f :&: a) e l)

instance (MonadAnnotater a m, MonadTrans t, Monad (t m)) => MonadAnnotater a (t m) where
  annM = lift . annM

-------------
--- Operations
-------------

annotateM :: (HTraversable f, MonadAnnotater a m) => CxtFunM m f (f :&: a)
annotateM = appSigFunM annM

----------------------------------
------------------- AnnotateDefault
----------------------------------

newtype AnnotateDefault x = AnnotateDefault' { runAnnotateDefault' :: Identity x}
  deriving ( Functor, Applicative, Monad )

pattern AnnotateDefault :: x -> AnnotateDefault x
pattern AnnotateDefault x = AnnotateDefault' (Identity x)

runAnnotateDefault :: AnnotateDefault (AnnHFix a f l) -> AnnHFix a f l
runAnnotateDefault = runIdentity . runAnnotateDefault'

-- | Specializing annotation to Maybe a to aid instance selection
instance (Default a) => MonadAnnotater a AnnotateDefault where
  annM x = return (x :&: def)



-----------------------------------
------------------ Lifting other functions to annotated versions
------------------------------------

propAnnSigFun :: SigFun f g -> SigFun (f :&: a) (g :&: a)
propAnnSigFun f (t :&: a) = (f t) :&: a

annotateOuter :: (HTraversable f, MonadAnnotater a m) => Context f (AnnHFix a f) l -> m (AnnHFix a f l)
annotateOuter = fmap appCxt . annotateM