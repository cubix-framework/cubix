{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Language.SuiMove.IPS.Types where

import Data.List ( (\\) )

import Language.Haskell.TH ( Name, mkName, runQ )
import Language.Haskell.TH qualified as TH

import Data.Comp.Multi ( Term, project', CxtS, AnnCxtS, inject, project )
import Data.Comp.Trans ( runCompTrans, makeSumType )

import Cubix.Language.SuiMove.Modularized as Modularized
import Cubix.Language.Info
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax qualified as Parametric

-----------------------------------------------------------------------------------
---------------------     Identifiers                      ------------------------
-----------------------------------------------------------------------------------

createSortInclusionTypes
  [''Parametric.IdentL]
  [''IdentifierL]
deriveAllButSortInjection
  [ ''IdentIsIdentifier ]
createSortInclusionInfers
  [''Parametric.IdentL]
  [''IdentifierL]

-----------------------------------------------------------------------------------
----------------------         Block                       ------------------------
-----------------------------------------------------------------------------------

data SuiMoveBlockEnd e l where
  SuiMoveBlockEnd :: e (Maybe HiddenExpressionL) -> SuiMoveBlockEnd e Parametric.BlockEndL

deriveAll [''SuiMoveBlockEnd]

createSortInclusionTypes
  [''UseDeclarationL, ''BlockItemL, ''Parametric.BlockL]
  [''Parametric.BlockItemL, ''Parametric.BlockItemL, ''BlockL]
deriveAllButSortInjection
  [ ''UseDeclarationIsBlockItem, ''BlockItemIsBlockItem, ''BlockIsBlock ]
createSortInclusionInfers
  [''UseDeclarationL, ''BlockItemL, ''Parametric.BlockL]
  [''Parametric.BlockItemL, ''Parametric.BlockItemL, ''BlockL]

-----------------------------------------------------------------------------------
----------------------           Expressions               ------------------------
-----------------------------------------------------------------------------------

createSortInclusionTypes
  [''(), ''HiddenUnaryExpressionInternal0L, ''HiddenExpressionL, ''Parametric.AssignL, ''Parametric.ExpressionL, ''HiddenExpressionL]
  [''UnitExpressionL, ''Parametric.LhsL, ''Parametric.RhsL, ''HiddenExpressionL, ''HiddenExpressionL, ''Parametric.ExpressionL]
deriveAllButSortInjection
  [''UnitIsUnitExpression, ''HiddenUnaryExpressionInternal0IsLhs, ''HiddenExpressionIsRhs, ''AssignIsHiddenExpression, ''ExpressionIsHiddenExpression, ''HiddenExpressionIsExpression]
createSortInclusionInfers
  [''(), ''HiddenUnaryExpressionInternal0L, ''HiddenExpressionL, ''Parametric.AssignL, ''Parametric.ExpressionL, ''HiddenExpressionL]
  [''UnitExpressionL, ''Parametric.LhsL, ''Parametric.RhsL, ''HiddenExpressionL, ''HiddenExpressionL, ''Parametric.ExpressionL]

-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------

do let suiSortInjections =
         [ ''IdentIsIdentifier
         , ''ExpressionIsHiddenExpression
         , ''HiddenExpressionIsExpression
         , ''UseDeclarationIsBlockItem
         , ''BlockItemIsBlockItem
         , ''BlockIsBlock
         , ''UnitIsUnitExpression
         , ''HiddenUnaryExpressionInternal0IsLhs
         , ''HiddenExpressionIsRhs
         , ''AssignIsHiddenExpression
         ]
       suiNewNodes =
         [ ''SuiMoveBlockEnd
         ]
       names =
         (moveSigNames \\
          [ mkName "BinaryExpression", mkName "UnaryExpression", mkName "UnitExpression", mkName "AssignExpression"
          , mkName "Identifier", mkName "Block"
          ]) ++
         suiSortInjections ++
         suiNewNodes ++
         [ ''Parametric.Ident
         , ''Parametric.Operator
         , ''Parametric.ArithBinOp
         , ''Parametric.DivOp
         , ''Parametric.ModOp
         , ''Parametric.BitwiseBinOp
         , ''Parametric.LogicalBinOp
         , ''Parametric.ShlOp
         , ''Parametric.ArithShrOp
         , ''Parametric.RelationalBinOp
         , ''Parametric.LogicalNegationOp
         , ''Parametric.Block
         , ''Parametric.EmptyBlockEnd
         , ''Parametric.UnitF
         , ''Parametric.AssignOpEquals
         , ''Parametric.Assign
         ]
   runCompTrans $ makeSumType "MSuiMoveSig" names

type MSuiMoveTerm = Term MSuiMoveSig
type MSuiMoveTermLab = TermLab MSuiMoveSig

type MSuiMoveCxt h a = CxtS h MSuiMoveSig a
type MSuiMoveCxtA h a p = AnnCxtS p h MSuiMoveSig a

-----------------------------------------------------------------------------------
----------------------         Sort injections             ------------------------
-----------------------------------------------------------------------------------

instance {-# OVERLAPPING #-} InjF MSuiMoveSig Modularized.BinaryExpressionL Parametric.ExpressionL where
  injF = iHiddenExpressionBinaryExpression

  projF' e
    | Just (HiddenExpressionIsExpression e') <- project' e
    , Just (HiddenExpressionBinaryExpression exp) <- project' e'
    = projF' exp
  projF' _ = Nothing

instance {-# OVERLAPPING #-} InjF MSuiMoveSig Modularized.UnaryExpressionL Parametric.ExpressionL where
  injF = iHiddenExpressionUnaryExpression . iHiddenUnaryExpressionInternal0UnaryExpression

  projF' e
    | Just (HiddenExpressionIsExpression e') <- project' e
    , Just (HiddenExpressionUnaryExpression ue) <- project' e'
    , Just (HiddenUnaryExpressionInternal0UnaryExpression uexp) <- project' ue
    = projF' uexp
  projF' _ = Nothing
