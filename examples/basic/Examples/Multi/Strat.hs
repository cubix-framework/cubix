{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Examples.Multi.Strat where

import Control.Monad ( MonadPlus(..) )
import Control.Monad.Identity ( Identity(..) )

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Monoid ( Sum(..) )

import qualified Data.Comp.Ops as O
import Data.Comp.Multi
import Data.Comp.Multi.Strategic
import Data.Comp.Multi.Strategy.Classification ( DynCase )

import Examples.Multi.Syntax



-- | Extract the annotation from the top of an annotated term
getAnn :: (DistAnn s p s') => Term s' :=> p
getAnn = annSnd . projectA . unTerm
  where
    annSnd (_ O.:&: x) = x

extractLabels' :: (DistAnn f Label f') => Translate (Term f') i (Map Label (Term f' i))
extractLabels' s = return $ Map.singleton (getAnn s) s

extractLabels :: (DistAnn f Label f', DynCase (Term f') i, HFoldable f') => Term f' :=> (Map Label (Term f' i))
extractLabels = runIdentity . (crushtdT $ promoteTF $ addFail extractLabels')

consts' :: (Value :<: f', MonadPlus m, RemA f f') => TranslateM m (Term f) ExpL [Int]
consts' (project' -> Just (Const x)) = return [x]
consts' _                            = mzero

consts :: ProgLab :=> [Int]
consts = runIdentity . (crushtdT $ promoteTF consts')

countConsts' :: (Value :<: f', MonadPlus m, RemA f f') => TranslateM m (Term f) ExpL (Sum Int)
countConsts' (project' -> Just (Const x)) = return $ Sum 1
countConsts' _                            = mzero

countConsts :: ProgLab :=> Int
countConsts = getSum . runIdentity . (crushtdT $ promoteTF countConsts')
