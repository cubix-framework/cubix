{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 

module Cubix.Language.Solidity.Parametric.Common.Types where

import Data.List ( (\\) )

import Language.Haskell.TH ( mkName )

import Data.Comp.Multi ( Term,  project', CxtS, AnnCxtS )
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

createSortInclusionTypes
  [''P.IdentL,    ''P.AssignL,   ''ExpressionL, ''ExpressionL ]
  [''IdentifierL, ''ExpressionL, ''P.RhsL,      ''P.LhsL      ]
deriveAll
  [''IdentIsIdentifier, ''AssignIsExpression, ''ExpressionIsRhs, ''ExpressionIsLhs]
createSortInclusionInfers
  [''P.IdentL,    ''P.AssignL,   ''ExpressionL, ''ExpressionL]
  [''IdentifierL, ''ExpressionL, ''P.RhsL,      ''P.LhsL     ]


-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------

do let soliditySortInjections = [ ''IdentIsIdentifier, ''AssignIsExpression, ''ExpressionIsRhs, ''ExpressionIsLhs ]
   let solidityNewNodes       = [ ]
   let names = (soliditySigNames \\ [mkName "Identifier"])
                          ++ soliditySortInjections
                          ++ solidityNewNodes
                          ++ [ ''P.Ident, ''P.Assign, ''AssignOpEquals
                             ]
   runCompTrans $ makeSumType "MSoliditySig" names


-- TODO 2023.11.02: I don't remember what this is for
--type instance InjectableSorts MCSig MultiLocalVarDeclL = '[CCompoundBlockItemL]

type MSolidityTerm = Term MSoliditySig
type MSolidityTermLab = TermLab MSoliditySig

type MSolidityCxt h a = CxtS h MSoliditySig a
type MSolidityCxtA h a p = AnnCxtS p h MSoliditySig a


-----------------------------------------------------------------------------------
----------------------         Sort injections             ------------------------
-----------------------------------------------------------------------------------

instance InjF MSoliditySig IdentL ExpressionL where
    injF = iIdentifierExpression . injF

    projF' e
     | Just (IdentifierExpression i) <- project' e
     , Just (IdentIsIdentifier j) <- project' i
     = Just j
    projF' _ = Nothing
