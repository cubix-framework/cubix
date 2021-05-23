{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

module Cubix.Analysis.Call.Trivial (
    NodeIdx(..)
  , TrivialCallAnalysisConstraints
  , CallAnalysis(..)
  , TrivialFunctionAnalysisConstraints
  , FunctionAnalysis(..)
  ) where

import Control.Monad.Identity ( Identity(..) )

import           Data.Map ( Map )
import qualified Data.Map as Map

import Data.Foldable ( fold )

import Data.Comp.Multi ( E(..), TreeLike, stripA, (:-<:), All, HFoldable, HFunctor )
import Data.Comp.Multi.Strategic ( TranslateM, GTranslateM, crushtdT, promoteTF, addFail )
import Data.Comp.Multi.Strategy.Classification ( DynCase )

import Cubix.Language.Info
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax

import Cubix.Sin.Compdata.Annotation ( getAnn )

data NodeIdx = NodeIdx FilePath Label
  deriving ( Eq, Ord, Show )
type FunctionId = String


newtype AccumMap k v = AccumMap { runAccumMap :: Map k v }

mergeKey :: (Ord k, Semigroup v) => k -> v -> Map k v -> Map k v
mergeKey = Map.insertWith (<>)

instance (Ord k, Semigroup v) => Semigroup (AccumMap k v) where
  (AccumMap m1) <> (AccumMap m2) = AccumMap $ Map.foldrWithKey (\k v m -> mergeKey k v m) m2 m1

instance (Ord k, Monoid v) => Monoid (AccumMap k v) where
  mempty = AccumMap $ Map.empty

----------------------------------------------------------------------------------------------------------------

class CallAnalysis fs where
  callAnalysis :: Project fs -> Map FunctionId [NodeIdx]


type TrivialCallAnalysisConstraints fs = ( FunctionCall :-<: fs, Ident :-<: fs
                                        , InjF fs IdentL FunctionExpL, TreeLike fs
                                        , DynCase (TermLab fs) FunctionCallL)
type TCAC fs = TrivialCallAnalysisConstraints fs


getCalls' :: (TCAC fs, Monad m) => TranslateM m (TermLab fs) FunctionCallL (AccumMap FunctionId [Label])
getCalls' t@(stripA -> FunctionCall' _ f _) = return $ case projF f of
                                                         Just (Ident' n) -> AccumMap $ Map.singleton n [getAnn t]
                                                         Nothing         -> mempty

getCalls :: (TCAC fs, All HFoldable fs) => GTranslateM Identity (TermLab fs) (AccumMap FunctionId [Label])
getCalls = crushtdT (promoteTF $ addFail getCalls')

instance (TCAC fs, All HFoldable fs) => CallAnalysis fs where
  callAnalysis prj = runAccumMap $ fold $ map getCallsAnnSource $ Map.toList prj
    where
      -- After a lot of painful debugging, I've determined that let-statements are evil
      -- (within a let statement, the typeclass-resolver will run wild and try to infer stuff
      --   while ignoring the things provided by the context)
      getCallsAnnSource :: forall fs. (TCAC fs, All HFoldable fs) => (FilePath, E (TermLab fs)) -> AccumMap FunctionId [NodeIdx]
      getCallsAnnSource (fil, E t) = case runIdentity $ getCalls t of
                                       AccumMap m -> AccumMap $ Map.map (map (NodeIdx fil)) m

----------------------------------------------------------------------------------------------------------------



type TrivialFunctionAnalysisConstraints fs = ( FunctionDef :-<: fs, Ident :-<: fs
                                            , TreeLike fs
                                            , DynCase (TermLab fs) FunctionDefL)

type TFAC fs = TrivialFunctionAnalysisConstraints fs

class FunctionAnalysis fs where
  functionAnalysis :: Project fs -> Map FunctionId [NodeIdx]


getFuncs' :: (TFAC fs, Monad m, All HFunctor fs) => TranslateM m (TermLab fs) FunctionDefL (AccumMap FunctionId [Label])
getFuncs' t@(stripA -> FunctionDef' _ (Ident' n) _ _) = return $ AccumMap $ Map.singleton n [getAnn t]

getFuncs :: (TFAC fs, All HFunctor fs, All HFoldable fs) => GTranslateM Identity (TermLab fs) (AccumMap FunctionId [Label])
getFuncs = crushtdT (promoteTF $ addFail getFuncs')

instance (TFAC fs, All HFunctor fs, All HFoldable fs) => FunctionAnalysis fs where
  functionAnalysis prj = runAccumMap $ fold $ map getFuncsAnnSource $ Map.toList prj
    where
      getFuncsAnnSource :: forall fs. (TFAC fs, All HFunctor fs, All HFoldable fs) => (FilePath, E (TermLab fs)) -> AccumMap FunctionId [NodeIdx]
      getFuncsAnnSource (fil, E t) = case runIdentity $ getFuncs t of
                                       AccumMap m -> AccumMap $ Map.map (map (NodeIdx fil)) m
