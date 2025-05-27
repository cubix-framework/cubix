{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

-- This is a separate file due to GHC's phase restriction

module Cubix.Language.Solidity.Parametric.Full.Types where

import Data.Comp.Multi ( Term )

import Solidity qualified as Solidity

import Data.Comp.Trans ( runCompTrans, deriveMultiComp, makeSumType )

import Cubix.Language.Info
import Cubix.Language.Solidity.Parametric.Full.Names
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax

-----------------------------------------------------------

runCompTrans $ deriveMultiComp ''Solidity.Solidity

deriveAll newASTTypes

runCompTrans $ makeSumType "SoliditySig" soliditySigNames

type SolidityTerm      = Term SoliditySig
type SolidityTermLab l = TermLab SoliditySig l

