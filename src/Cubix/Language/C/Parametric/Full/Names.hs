{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- This is in a separate file due to GHC's phase restriction

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.C.Parametric.Full.Names () where
#else
module Cubix.Language.C.Parametric.Full.Names (
    origASTTypes
  , newASTTypes
  , cSigNames
  , makeSubsts
  ) where

import           Data.Map ( Map )
import qualified Data.Map as Map

import           Language.Haskell.TH hiding ( Name )
import qualified Language.Haskell.TH as TH
import           Language.C

import           Data.Comp.Trans ( runCompTrans, generateNameLists, getTypeParamVars )

import           Cubix.Language.Parametric.Syntax.Base
import           Cubix.Language.Parametric.Syntax.Functor

runCompTrans $ generateNameLists ''CTranslationUnit

cSigNames :: [TH.Name]
cSigNames = newASTTypes ++ [''PairF, ''TripleF, ''ListF, ''MaybeF, ''EitherF, ''BoolF, ''IntF, ''IntegerF, ''UnitF]

makeSubsts :: Q (Map TH.Name Type)
makeSubsts = do
  vars <- runCompTrans $ getTypeParamVars origASTTypes
  let substs = Map.fromList (zip vars (repeat $ TupleT 0))
  inf <- reify ''Flags
  TyConI (NewtypeD _ _ [KindedTV f () StarT] _ _ _) <- reify ''Flags
  return $ Map.insert f (ConT ''CIntFlag) substs
#endif