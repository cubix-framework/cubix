module Examples.Multi.Strat where

import Control.Monad ( MonadPlus(..) )
import Control.Monad.Identity ( Identity(..) )

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Monoid ( Sum(..) )

import Data.Comp.Multi hiding ( Sum )
import Data.Comp.Multi.Strategic
import Data.Comp.Multi.Strategy.Classification ( DynCase )

import Cubix.Language.Info ( TermLab )
import Cubix.Sin.Compdata.Annotation ( Annotated, getAnn )

import Examples.Multi.Syntax

-----------------------------------------------------------------------

extractLabels' :: (Ord a, Annotated a f) => Translate (HFix f) i (Map a (HFix f i))
extractLabels' s = return $ Map.singleton (getAnn s) s

extractLabels :: (Ord a, Annotated a f, DynCase (HFix f) i, HFoldable f) => HFix f :=> (Map a (HFix f i))
extractLabels = runIdentity . (crushtdT $ promoteTF $ addFail extractLabels')

consts' :: (Value :-<: fs, MonadPlus m) => TranslateM m (TermLab fs) ExpL [Int]
consts' (project' -> Just (Const x)) = return [x]
consts' _                            = mzero

consts :: ProgLab :=> [Int]
consts = runIdentity . (crushtdT $ promoteTF consts')

countConsts' :: (Value :-<: fs, MonadPlus m) => TranslateM m (TermLab fs) ExpL (Sum Int)
countConsts' (project' -> Just (Const x)) = return $ Sum 1
countConsts' _                            = mzero

countConsts :: ProgLab :=> Int
countConsts = getSum . runIdentity . (crushtdT $ promoteTF countConsts')
