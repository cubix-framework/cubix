{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TemplateHaskell #-}

module Cubix.Language.Solidity.Parametric.Full.Names (
    origASTTypes
  , newASTTypes
  , soliditySigNames
  ) where


import Language.Haskell.TH qualified as TH

import Solidity

import Data.Comp.Trans ( runCompTrans, generateNameLists )

import Cubix.Language.Parametric.Syntax

----------------------------------------------------------------

runCompTrans $ generateNameLists ''Solidity

soliditySigNames :: [TH.Name]
soliditySigNames = newASTTypes ++ [''PairF, ''TripleF, ''ListF, ''MaybeF, ''IntF, ''TextF, ''UnitF]
