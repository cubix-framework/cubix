{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 

module Cubix.Language.Solidity.Parametric.Common.Types where

#ifndef ONLY_ONE_LANGUAGE
import Data.List ( (\\) )

import Language.Haskell.TH ( mkName )

import Data.Comp.Multi ( Node, HFunctor, Term, project', project, (:-<:), CxtS, All, AnnCxtS )
import Data.Comp.Trans ( runCompTrans, makeSumType )

import Cubix.Language.Solidity.Parametric.Full.Names
import Cubix.Language.Solidity.Parametric.Full.Types as Solidity
import Cubix.Language.Info
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax as P

--------------------------------------------------------------------------------


-----------------------------------------------------------------------------------
---------------------     Identifiers and assignments      ------------------------
-----------------------------------------------------------------------------------



createSortInclusionTypes [  ''IdentL
                         ] [''IdentifierL
                         ]
deriveAll [''IdentIsIdentifier ]
createSortInclusionInfers [  ''IdentL
                          ] [''IdentifierL
                          ]

-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------

do let soliditySortInjections = [ ''IdentIsIdentifier ]
   let solidityNewNodes       = [ ]
   let names = (soliditySigNames \\ [mkName "Identifier"])
                          ++ soliditySortInjections
                          ++ solidityNewNodes
                          ++ [ ''P.Ident --, ''Assign, ''AssignOpEquals
                             ]
   sumDec <- runCompTrans $ makeSumType "MSoliditySig" names
   return sumDec


-- TODO 2023.11.02: I don't remember what this is for
--type instance InjectableSorts MCSig MultiLocalVarDeclL = '[CCompoundBlockItemL]

type MSolidityTerm = Term MSoliditySig
type MSolidityTermLab = TermLab MSoliditySig

type MSolidityCxt h a = CxtS h MSoliditySig a
type MSolidityCxtA h a p = AnnCxtS p h MSoliditySig a


-----------------------------------------------------------------------------------
----------------------         Sort injections             ------------------------
-----------------------------------------------------------------------------------

#endif
