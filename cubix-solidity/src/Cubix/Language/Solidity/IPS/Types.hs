{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 

module Cubix.Language.Solidity.IPS.Types where

import Data.List ( (\\) )

import Language.Haskell.TH ( mkName )

import Data.Comp.Multi ( Term,  project', CxtS, AnnCxtS )
import Data.Comp.Trans ( runCompTrans, makeSumType )

import Cubix.Language.Solidity.Modularized.Names
import Cubix.Language.Solidity.Modularized.Types as Solidity
import Cubix.Language.Info
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax qualified as P

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
---------------               Expressions                  ------------------------
-----------------------------------------------------------------------------------

createSortInclusionType' ''P.ExpressionL ''ExpressionL (mkName "ExpressionIsSolExp")
createSortInclusionType' ''ExpressionL ''P.ExpressionL (mkName "SolExpIsExpression")
deriveAll [''ExpressionIsSolExp, ''SolExpIsExpression]
createSortInclusionInfer' ''P.ExpressionL ''ExpressionL (mkName "ExpressionIsSolExp")
createSortInclusionInfer' ''ExpressionL ''P.ExpressionL (mkName "SolExpIsExpression")

-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------

do let soliditySortInjections =
         [ ''IdentIsIdentifier, ''AssignIsExpression, ''ExpressionIsRhs, ''ExpressionIsLhs
         , ''ExpressionIsSolExp, ''SolExpIsExpression
         ]
       solidityNewNodes = [ ]
       names =
         (soliditySigNames \\ [mkName "Identifier"]) ++
         soliditySortInjections ++
         solidityNewNodes ++
         [ ''P.Ident, ''P.Assign, ''P.AssignOpEquals
         , ''P.UnaryMinusOp, ''P.ComplementOp, ''P.LogicalNegationOp
         , ''P.ArithBinOp, ''P.DivOp, ''P.ModOp, ''P.ExpOp
         , ''P.BitwiseBinOp, ''P.LogicalBinOp, ''P.ShlOp, ''P.ArithShrOp
         , ''P.RelationalBinOp
         , ''P.CondTernaryOp
         , ''P.Operator
         , ''P.SeqOp
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

instance InjF MSoliditySig P.IdentL ExpressionL where
    injF = iIdentifierExpression . injF

    projF' e
     | Just (IdentifierExpression i) <- project' e
     , Just (IdentIsIdentifier j) <- project' i
     = Just j
    projF' _ = Nothing
