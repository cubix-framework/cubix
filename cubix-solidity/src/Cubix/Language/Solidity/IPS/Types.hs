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

-----------------------------------------------------------------------------------
---------------       Variable declarations and blocks     ------------------------
-----------------------------------------------------------------------------------

data LValueL

data LValue e l where
  IndexExpr        :: e P.ExpressionL -> e P.ExpressionL              -> LValue e LValueL
  IdentifierExpr   :: e P.IdentL                                      -> LValue e LValueL
  MemberAccessExpr :: e P.ExpressionL -> e Solidity.MemberAccessTypeL -> LValue e LValueL
  TupleExpr        :: e [Maybe P.ExpressionL]                         -> LValue e LValueL

deriveAll [''LValue]

-----------------------------------------------------------------------------------
---------------------     Identifiers and assignments      ------------------------
-----------------------------------------------------------------------------------

createSortInclusionTypes
  [''P.IdentL,    ''P.AssignL,     ''P.ExpressionL, ''LValueL, ''LValueL       ]
  [''IdentifierL, ''P.ExpressionL, ''P.RhsL,        ''P.LhsL,  ''P.ExpressionL ]
deriveAllButSortInjection
  [ ''IdentIsIdentifier, ''AssignIsExpression, ''ExpressionIsRhs
  , ''LValueIsLhs, ''LValueIsExpression
  ]
createSortInclusionInfers
  [''P.IdentL,    ''P.AssignL,     ''P.ExpressionL, ''LValueL, ''LValueL       ]
  [''IdentifierL, ''P.ExpressionL, ''P.RhsL,        ''P.LhsL,  ''P.ExpressionL ]

-----------------------------------------------------------------------------------
---------------               Expressions                  ------------------------
-----------------------------------------------------------------------------------

createSortInclusionType' ''P.ExpressionL ''ExpressionL (mkName "ExpressionIsSolExp")
createSortInclusionType' ''ExpressionL ''P.ExpressionL (mkName "SolExpIsExpression")
deriveAllButSortInjection [''ExpressionIsSolExp]
deriveAllButSortInjection [''SolExpIsExpression]
createSortInclusionInfer' ''P.ExpressionL ''ExpressionL (mkName "ExpressionIsSolExp")
createSortInclusionInfer' ''ExpressionL ''P.ExpressionL (mkName "SolExpIsExpression")

-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------

do let soliditySortInjections =
         [ ''IdentIsIdentifier
         , ''LValueIsLhs, ''LValueIsExpression
         , ''AssignIsExpression
         , ''ExpressionIsRhs
         , ''ExpressionIsSolExp, ''SolExpIsExpression
         ]
       solidityNewNodes = [ ''LValue ]
       names =
         (soliditySigNames \\ [mkName "Identifier"]) ++
         soliditySortInjections ++
         solidityNewNodes ++
         [ ''P.Ident, ''P.Assign, ''P.AssignOpEquals
         , ''P.UnaryMinusOp, ''P.ComplementOp, ''P.LogicalNegationOp
         , ''P.ArithBinOp, ''P.DivOp, ''P.ModOp, ''P.ExpOp
         , ''P.BitwiseBinOp, ''P.LogicalBinOp, ''P.ShlOp, ''P.LogicalShrOp, ''P.ArithShrOp
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

instance {-# OVERLAPPING #-} InjF MSoliditySig P.IdentL LValueL where
  injF = iIdentifierExpr

  projF' e
    | Just (IdentifierExpr ident) <- project' e
    = Just ident
  projF' _ = Nothing

instance {-# OVERLAPPING #-} InjF MSoliditySig P.IdentL P.LhsL where
    injF = LValueIsLhs' . injF

    projF' e
      | Just (LValueIsLhs lval) <- project' e
      = projF' lval
    projF' _ = Nothing

instance {-# OVERLAPPING #-} InjF MSoliditySig P.IdentL P.ExpressionL where
    injF = LValueIsExpression' . injF

    projF' e
      | Just (LValueIsExpression lval) <- project' e
      = projF' lval
    projF' _ = Nothing

instance {-# OVERLAPPING #-} InjF MSoliditySig P.IdentL ExpressionL where
    injF = iIdentifierExpression . injF

    projF' e
     | Just (IdentifierExpression i) <- project' e
     , Just (IdentIsIdentifier j) <- project' i
     = Just j
    projF' _ = Nothing

instance InjF MSoliditySig P.AssignL ExpressionL where
  injF = ExpressionIsSolExp' . injF

  projF' e
    | Just (ExpressionIsSolExp exp) <- project' e
    = projF' exp
  projF' _ = Nothing

instance InjF MSoliditySig ExpressionL P.RhsL where
  injF = injF . SolExpIsExpression'

  projF' e
    | Just (ExpressionIsRhs rhs) <- project' e
    = projF' rhs
  projF' _ = Nothing

instance InjF MSoliditySig LValueL ExpressionL where
  injF = ExpressionIsSolExp' . injF

  projF' e
    | Just (ExpressionIsSolExp exp) <- project' e
    = projF' exp
  projF' _ = Nothing
