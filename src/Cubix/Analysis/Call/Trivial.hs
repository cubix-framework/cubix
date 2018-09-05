{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}

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

import Data.Comp.Multi ( E(..), HTraversable, stripA, (:<:) )
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

mergeKey :: (Ord k, Monoid v) => k -> v -> Map k v -> Map k v
mergeKey = Map.insertWith mappend

instance (Ord k, Monoid v) => Monoid (AccumMap k v) where
  mempty = AccumMap $ Map.empty
  mappend (AccumMap m1) (AccumMap m2) = AccumMap $ Map.foldrWithKey (\k v m -> mergeKey k v m) m2 m1

----------------------------------------------------------------------------------------------------------------

class CallAnalysis f where
  callAnalysis :: Project f -> Map FunctionId [NodeIdx]


type TrivialCallAnalysisConstraints f = ( FunctionCall :<: f, Ident :<: f
                                        , InjF f IdentL FunctionExpL, HTraversable f
                                        , DynCase (TermLab f) FunctionCallL)
type TCAC f = TrivialCallAnalysisConstraints f


getCalls' :: (TCAC f, Monad m) => TranslateM m (TermLab f) FunctionCallL (AccumMap FunctionId [Label])
getCalls' t@(stripA -> FunctionCall' _ f _) = return $ case projF f of
                                                         Just (Ident' n) -> AccumMap $ Map.singleton n [getAnn t]
                                                         Nothing         -> mempty

getCalls :: (TCAC f) => GTranslateM Identity (TermLab f) (AccumMap FunctionId [Label])
getCalls = crushtdT (promoteTF $ addFail getCalls')

instance (TCAC f) => CallAnalysis f where
  callAnalysis prj = runAccumMap $ fold $ map getCallsAnnSource $ Map.toList prj
    where
      -- After a lot of painful debugging, I've determined that let-statements are evil
      -- (within a let statement, the typeclass-resolver will run wild and try to infer stuff
      --   while ignoring the things provided by the context)
      getCallsAnnSource :: forall f. (TCAC f) => (FilePath, E (TermLab f)) -> AccumMap FunctionId [NodeIdx]
      getCallsAnnSource (fil, E t) = case runIdentity $ getCalls t of
                                       AccumMap m -> AccumMap $ Map.map (map (NodeIdx fil)) m

----------------------------------------------------------------------------------------------------------------



type TrivialFunctionAnalysisConstraints f = ( FunctionDef :<: f, Ident :<: f
                                            , HTraversable f
                                            , DynCase (TermLab f) FunctionDefL)

type TFAC f = TrivialFunctionAnalysisConstraints f

class FunctionAnalysis f where
  functionAnalysis :: Project f -> Map FunctionId [NodeIdx]


getFuncs' :: (TFAC f, Monad m) => TranslateM m (TermLab f) FunctionDefL (AccumMap FunctionId [Label])
getFuncs' t@(stripA -> FunctionDef' _ (Ident' n) _ _) = return $ AccumMap $ Map.singleton n [getAnn t]

getFuncs :: (TFAC f) => GTranslateM Identity (TermLab f) (AccumMap FunctionId [Label])
getFuncs = crushtdT (promoteTF $ addFail getFuncs')

instance (TFAC f) => FunctionAnalysis f where
  functionAnalysis prj = runAccumMap $ fold $ map getFuncsAnnSource $ Map.toList prj
    where
      getFuncsAnnSource :: forall f. (TFAC f) => (FilePath, E (TermLab f)) -> AccumMap FunctionId [NodeIdx]
      getFuncsAnnSource (fil, E t) = case runIdentity $ getFuncs t of
                                       AccumMap m -> AccumMap $ Map.map (map (NodeIdx fil)) m
