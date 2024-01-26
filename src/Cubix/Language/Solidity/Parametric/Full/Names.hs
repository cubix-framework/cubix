{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Solidity.Parametric.Full.Names () where
#else
module Cubix.Language.Solidity.Parametric.Full.Names (
    origASTTypes
  , newASTTypes
  , soliditySigNames
  ) where


import           Language.Haskell.TH hiding ( Name )
import qualified Language.Haskell.TH as TH

import           Solidity

import           Data.Comp.Trans ( runCompTrans, generateNameLists )

import           Cubix.Language.Parametric.Syntax.Base
import           Cubix.Language.Parametric.Syntax.Functor

----------------------------------------------------------------

runCompTrans $ generateNameLists ''Solidity

soliditySigNames :: [TH.Name]
soliditySigNames = newASTTypes ++ [''PairF, ''TripleF, ''ListF, ''MaybeF, ''IntF, ''TextF, ''UnitF]

#endif