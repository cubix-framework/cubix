{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- This modules contains facilites for providing meta-information for
-- program terms, especially node labeling
module Cubix.Language.Info
  (
    -- ** SourcePos
    SourcePos
  , sourceFile
  , sourceRow
  , sourceCol

    -- ** SourceSpan
  , SourceSpan
  , sourceStart
  , sourceEnd
  , mkSourceSpan

    -- ** Attrs
  , Attrs
  , attrLabel
  , attrSpan

    -- **  Label
  , Label -- opaque!
  , HasLabel(..)
  , TermLab
  , ppLabel
  , LabelGen -- opaque!
  , HasLabelGen(..)
  , mkConcurrentSupplyLabelGen
  , runConcurrentSupplyLabeler
  , unsafeMkConcurrentSupplyLabelGen
  , debugMakeLabel
  , nextLabel

  , annotateLabel
  , annotateLabelOuter
  , labelProg
  , annotateTop
  , annotateTop'
  , annotateTopAttrs
  , annotateTopAttrs'

  , HFixLab

    -- ** Project
  , Project
  , parseProject
  , rewriteProjectM
  , rewriteProjectWithFilM
  , putProject

  ) where

import Control.Concurrent.Supply ( Supply, newSupply, freshId, splitSupply )
import Control.DeepSeq ( NFData(..) )
import Control.DeepSeq.Generics ( genericRnf )
import Control.Lens ( Lens', (&), (.~), (^.), use, (.=) )
import Control.Lens.TH ( makeClassy, makeLenses )
import Control.Monad ( liftM, forM_ )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State ( MonadState, StateT(..), state, runState, evalStateT )
import Control.Monad.Trans.Maybe ( MaybeT(..) )

import Data.Data ( Data )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Typeable ( Typeable )

import GHC.Generics ( Generic )

import System.IO.Unsafe ( unsafePerformIO )

import Data.Comp.Multi ( AnnTerm, AnnHFix, All, Cxt(..), Context, appCxt, Term, (:&:)(..), (:<:), CxtFunM, inj, HTraversable, E(..), rewriteEM, HFix , HFunctor, HFoldable)

import Cubix.Sin.Compdata.Annotation ( MonadAnnotater(..), annotateM, annotateOuter )

--------------------------------------------------------------------------------
---------------------------------- Labels --------------------------------------
--------------------------------------------------------------------------------
                          
-- | Provides unique labels for AST nodes
newtype Label = Label Int
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance NFData Label where rnf = genericRnf

makeClassy ''Label


--------------------------------------------------------------------------------
---------------------------------- SourcePos -----------------------------------
--------------------------------------------------------------------------------

data SourcePos = SourcePos { _sourceFile :: !String
                           , _sourceRow  :: !Int
                           , _sourceCol  :: !Int
                           }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance NFData SourcePos where rnf = genericRnf

makeClassy ''SourcePos


--------------------------------------------------------------------------------
--------------------------------- SourceSpan -----------------------------------
--------------------------------------------------------------------------------

data SourceSpan = SourceSpan { _sourceStart :: !SourcePos, _sourceEnd :: !SourcePos } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

makeClassy ''SourceSpan

mkSourceSpan :: String -> (Int, Int ) -> (Int, Int) -> SourceSpan
mkSourceSpan fileName (sRow, sCol) (eRow, eCol) = SourceSpan (SourcePos fileName sRow sCol)
                                                             (SourcePos fileName eRow eCol)


--------------------------------------------------------------------------------
------------------------------------ Attrs -------------------------------------
--------------------------------------------------------------------------------

data Attrs = Attrs { _attrLabel :: !Label
                   , _attrSpan  :: !(Maybe SourceSpan)
                   }

makeClassy ''Attrs



--------------------------------------------------------------------------------
-------------------------------- Uncategorized ---------------------------------
--------------------------------------------------------------------------------

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

instance {-# OVERLAPS #-} (Monad m, HasLabelGen s) => MonadAnnotater Label (StateT s m) where
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

nextLabel :: (MonadState s m, HasLabelGen s) => m Label
nextLabel = zoom labelGen $ state (\(LabelGen g) -> genLabel g)

--------------------------------------------------------------------------------

data ConcurrentSupplyLabelGen = ConcurrentSupplyLabelGen
    { _supply :: Supply
    }
  deriving ( Eq, Ord, Show )

makeLenses ''ConcurrentSupplyLabelGen

mkConcurrentSupplyLabelGen :: MonadIO m => m LabelGen
mkConcurrentSupplyLabelGen = do s <- liftIO newSupply
                                return $ LabelGen $ ConcurrentSupplyLabelGen { _supply = s }

runConcurrentSupplyLabeler :: (MonadIO m) => StateT LabelGen m a -> m a
runConcurrentSupplyLabeler m = do s <- mkConcurrentSupplyLabelGen
                                  evalStateT m s

unsafeMkConcurrentSupplyLabelGen :: () -> LabelGen
unsafeMkConcurrentSupplyLabelGen () = unsafePerformIO mkConcurrentSupplyLabelGen
{-# NOINLINE unsafeMkConcurrentSupplyLabelGen #-}

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

type Project fs = Map FilePath (E (TermLab fs))

parseProject ::
  forall fs l.
  ( All HFoldable fs
  , All HFunctor fs
  , All HTraversable fs
  ) => LabelGen -> (FilePath -> IO (Maybe (Term fs l))) -> [FilePath] -> IO (Maybe (Project fs))
parseProject gen parse fils = runMaybeT (go gen fils)
  where
    go :: (All HFoldable fs, All HFunctor fs, All HTraversable fs) => LabelGen -> [FilePath] -> MaybeT IO (Project fs)
    go gen []         = return Map.empty
    go gen (fil:fils) = do t <- MaybeT $ parse fil
                           let (tLab, gen') = labelProg' gen t
                           prj <- go gen' fils
                           return $ Map.insert fil (E tLab) prj

rewriteProjectM :: (Applicative m) => (forall l. TermLab fs l -> m (TermLab fs l)) -> Project fs -> m (Project fs)
rewriteProjectM f = traverse (rewriteEM f)

rewriteProjectWithFilM :: (Applicative m) => (forall l. FilePath -> TermLab fs l -> m (TermLab fs l)) -> Project fs -> m (Project fs)
rewriteProjectWithFilM f = Map.traverseWithKey (\k t -> rewriteEM (f k) t)

putProject :: (forall l. TermLab fs l -> String) -> Project fs -> IO ()
putProject pp prj = forM_ (Map.toList prj) (\(fil, E t) -> writeFile fil (pp t))
