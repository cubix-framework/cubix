{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- This modules contains facilites for providing meta-information for
-- program terms, especially node labeling
module Cubix.Language.Info
  (
    SourcePos
  , sourceFile
  , sourceRow
  , sourceCol

  , SourceSpan
  , sourceStart
  , sourceEnd
  , mkSourceSpan

  , Attrs
  , attrLabel
  , attrSpan

  , Label -- opaque!
  , HasLabel(..)
  , TermLab
  , MonadLabeler(..)
  , ppLabel
  , LabelGen -- opaque!
  , HasLabelGen(..)
  , mkCSLabelGen
  , unsafeMkCSLabelGen
  , debugMakeLabel
  , nextLabel

  , annotateLabel
  , annotateOuter
  , annotateLabelOuter
  , labelProg
  , annotateTop
  , annotateTop'

  , Project
  , parseProject
  , rewriteProjectM
  , rewriteProjectWithFilM
  , putProject

  , HFixLab
  ) where

import Control.Concurrent.Supply ( Supply, newSupply, freshId, splitSupply )
import Control.DeepSeq ( NFData(..) )
import Control.DeepSeq.Generics ( genericRnf )
import Control.Lens ( Lens', (&), (.~), (^.), use, (.=) )
import Control.Lens.TH ( makeClassy, makeLenses )
import Control.Monad ( liftM, forM_ )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State ( MonadState, StateT(..), state, evalState, runState )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Maybe ( MaybeT(..) )

import Data.Data ( Data )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import Data.Traversable ( traverse )
import Data.Typeable ( Typeable )

import GHC.Generics ( Generic )

import System.IO.Unsafe ( unsafePerformIO )

import Data.Comp.Multi ( AnnTerm, AnnHFix, All, Cxt(..), Context, appCxt, Term, (:&:)(..), (:<:), CxtFunM, inj, HTraversable, E(..), rewriteEM, HFix , HFunctor, HFoldable)

import Cubix.Sin.Compdata.Annotation ( MonadAnnotater(..), annotateM )

--------------------------------------------------------------------------------
-- Labeling
--------------------------------------------------------------------------------
                          
-- | Provides unique labels for AST nodes
newtype Label = Label Int
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance NFData Label where rnf = genericRnf

makeClassy ''Label

data SourcePos = SourcePos { _sourceFile :: !String
                           , _sourceRow  :: !Int
                           , _sourceCol  :: !Int
                           }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance NFData SourcePos where rnf = genericRnf

makeClassy ''SourcePos

data SourceSpan = SourceSpan { _sourceStart :: !SourcePos, _sourceEnd :: !SourcePos }

makeClassy ''SourceSpan

mkSourceSpan :: String -> (Int, Int ) -> (Int, Int) -> SourceSpan
mkSourceSpan fileName (sRow, sCol) (eRow, eCol) = SourceSpan (SourcePos fileName sRow sCol)
                                                             (SourcePos fileName eRow eCol)

data Attrs = Attrs { _attrLabel :: !Label
                   , _attrSpan  :: !(Maybe SourceSpan)
                   }

makeClassy ''Attrs


type TermLab f   = AnnTerm Label f
type TermAttrs f = AnnTerm Attrs f

type HFixLab f   = AnnHFix Label f
type HFixAttrs f = AnnHFix Attrs f

ppLabel :: Label -> String
ppLabel (Label n) = show n

data LabelGen = forall a. LabelGenInterface a => LabelGen a

class LabelGenInterface g where
  genLabel :: g -> (Label, LabelGen)
  split    :: g -> (LabelGen, LabelGen)

class HasLabelGen s where
  labelGen :: Lens' s LabelGen

instance HasLabelGen LabelGen where
  labelGen = id

class (MonadState s m, HasLabelGen s) => MonadLabeler s m | m -> s
instance (MonadState s m, HasLabelGen s) => MonadLabeler s m

instance (Monad m, MonadLabeler s m) => MonadAnnotater Label m where
  annM t = (:&:) t <$> nextLabel

-- | Allows embedding a smaller state inside a larger one
-- This has major advantages over 'Control.Lens.Zoom.zoom' in
-- that it doesn't require an explicit monad stack.
--
-- However, it runs the risk of behavior changing due to noncommutative
-- monad transformers (accessed via e.g.: @lift . put@), and is incompatible
-- with RWST.
zoom :: (MonadState s m) => Lens' s t -> StateT t m a -> m a
zoom f m = do s <- use f
              (a, s') <- runStateT m s
              f .= s'
              return a

nextLabel :: (MonadLabeler s m) => m Label
nextLabel = zoom labelGen $ state (\(LabelGen g) -> genLabel g)

--------------------------------------------------------------------------------

data ConcurrentSupplyLabelGen = ConcurrentSupplyLabelGen
    { _supply :: Supply
    }
  deriving ( Eq, Ord, Show )

makeLenses ''ConcurrentSupplyLabelGen

mkCSLabelGen :: MonadIO m => m LabelGen
mkCSLabelGen = do s <- liftIO newSupply
                  return $ LabelGen $ ConcurrentSupplyLabelGen { _supply = s }

unsafeMkCSLabelGen :: () -> LabelGen
unsafeMkCSLabelGen () = unsafePerformIO mkCSLabelGen
{-# NOINLINE unsafeMkCSLabelGen #-}

debugMakeLabel :: Int -> Label
debugMakeLabel = Label

instance LabelGenInterface ConcurrentSupplyLabelGen where
  genLabel g = ( Label l
               , LabelGen (g & supply .~ s)
               )
    where
      (l, s) = freshId (g ^. supply)

  split g = ( LabelGen (g & supply .~ s)
            , LabelGen (g & supply .~ s')
            )
    where
      (s,s') = splitSupply (g ^. supply)

--------------------------------------------------------------------------------

labelToAttrs :: (f :&: Label) e l -> (f :&: Attrs) e l
labelToAttrs (x :&: l) = x :&: Attrs l Nothing


-- | Fully annotates a term with fresh labels
annotateLabel :: (HTraversable f, MonadAnnotater Label m) => CxtFunM m f (f :&: Label)
annotateLabel = annotateM

annotateOuter :: (HTraversable f, MonadAnnotater a m) => Context f (AnnHFix a f) l -> m (AnnHFix a f l)
annotateOuter = liftM appCxt . annotateM

annotateLabelOuter :: (HTraversable f, MonadAnnotater Label m) => Context f (HFixLab f) l -> m (HFixLab f l)
annotateLabelOuter = annotateOuter

labelProg' :: (HTraversable f) => LabelGen -> HFix f l -> (HFixLab f l, LabelGen)
labelProg' gen t = runState (annotateLabel t) gen

labelProg :: (HTraversable f) => LabelGen -> HFix f l -> HFixLab f l
labelProg gen t = fst $ labelProg' gen t

annotateTop :: (MonadAnnotater Label m) => f (HFixLab f) l -> m (HFixLab f l)
annotateTop = liftM Term . annM

annotateTop' :: (f :<: g, MonadAnnotater Label m) => f (HFixLab g) l -> m (HFixLab g l)
annotateTop' = annotateTop . inj

annotateTopAttrs :: (MonadAnnotater Label m) => f (HFixAttrs f) l -> m (HFixAttrs f l)
annotateTopAttrs = liftM Term . liftM labelToAttrs . annM

annotateTopAttrs' :: (f :<: g, MonadAnnotater Label m) => f (HFixAttrs g) l -> m (HFixAttrs g l)
annotateTopAttrs' = annotateTopAttrs . inj

--------------------------------------------------------------------------------

type Project f = Map FilePath (E (HFixLab f))

parseProject :: forall f l.
               ( HTraversable f
               ) => LabelGen -> (FilePath -> IO (Maybe (HFix f l))) -> [FilePath] -> IO (Maybe (Project f))
parseProject gen parse fils = runMaybeT (go gen fils)
  where
    go :: LabelGen -> [FilePath] -> MaybeT IO (Project f)
    go gen []         = return Map.empty
    go gen (fil:fils) = do t <- MaybeT $ parse fil
                           let (tLab, gen') = labelProg' gen t
                           prj <- go gen' fils
                           return $ Map.insert fil (E tLab) prj

rewriteProjectM :: (Applicative m) => (forall l. HFixLab f l -> m (HFixLab f l)) -> Project f -> m (Project f)
rewriteProjectM f = traverse (rewriteEM f)

rewriteProjectWithFilM :: (Applicative m) => (forall l. FilePath -> HFixLab f l -> m (HFixLab f l)) -> Project f -> m (Project f)
rewriteProjectWithFilM f = Map.traverseWithKey (\k t -> rewriteEM (f k) t)

putProject :: (forall l. HFixLab f l -> String) -> Project f -> IO ()
putProject pp prj = forM_ (Map.toList prj) (\(fil, E t) -> writeFile fil (pp t))
