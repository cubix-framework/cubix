{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

-- This is a separate file due to GHC's phase restriction

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Solidity.Parametric.Full.Types () where
#else
module Cubix.Language.Solidity.Parametric.Full.Types where

import Data.Comp.Multi ( Term )

import qualified Solidity            as Solidity

import Data.Comp.Trans ( runCompTrans, deriveMultiComp, makeSumType )

import Cubix.Language.Info
import Cubix.Language.Solidity.Parametric.Full.Names
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax.Base

-----------------------------------------------------------

runCompTrans $ deriveMultiComp ''Solidity.Solidity

deriveAll newASTTypes

runCompTrans $ makeSumType "SoliditySig" soliditySigNames

type SolidityTerm      = Term SoliditySig
type SolidityTermLab l = TermLab SoliditySig l
#endif