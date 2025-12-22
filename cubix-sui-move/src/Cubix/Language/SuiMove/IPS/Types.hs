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

import Cubix.Language.SuiMove.Modularized as SuiMove
import Cubix.Language.Info
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax qualified as P

-----------------------------------------------------------------------------------
---------------------     Identifiers                      ------------------------
-----------------------------------------------------------------------------------

createSortInclusionTypes
  [''P.IdentL]
  [''IdentifierL]
deriveAllButSortInjection
  [ ''IdentIsIdentifier ]
createSortInclusionInfers
  [''P.IdentL]
  [''IdentifierL]

-----------------------------------------------------------------------------------
----------------------         Binary Operations           ------------------------
-----------------------------------------------------------------------------------

createSortInclusionTypes
  [''P.ExpressionL]
  [''HiddenExpressionL]
deriveAllButSortInjection
  [ ''ExpressionIsHiddenExpression ]
createSortInclusionInfers
  [''P.ExpressionL]
  [''HiddenExpressionL]

createSortInclusionTypes
  [''HiddenExpressionL]
  [''P.ExpressionL]
deriveAllButSortInjection
  [ ''HiddenExpressionIsExpression ]
createSortInclusionInfers
  [''HiddenExpressionL]
  [''P.ExpressionL]

-----------------------------------------------------------------------------------
----------------------         Block                       ------------------------
-----------------------------------------------------------------------------------

-- SuiMoveBlockEnd wraps the optional final expression in blocks
data SuiMoveBlockEnd e l where
  SuiMoveBlockEnd :: e (Maybe HiddenExpressionL) -> SuiMoveBlockEnd e P.BlockEndL

deriveAll [''SuiMoveBlockEnd]

-- Sort injection: UseDeclaration can appear as BlockItem
createSortInclusionTypes
  [''UseDeclarationL]
  [''P.BlockItemL]
deriveAllButSortInjection
  [ ''UseDeclarationIsBlockItem ]
createSortInclusionInfers
  [''UseDeclarationL]
  [''P.BlockItemL]

-- Sort injection: BlockItem to parametric BlockItem
createSortInclusionTypes
  [''BlockItemL]
  [''P.BlockItemL]
deriveAllButSortInjection
  [ ''BlockItemIsBlockItem ]
createSortInclusionInfers
  [''BlockItemL]
  [''P.BlockItemL]

-- Sort injection: Block to parametric Block
createSortInclusionTypes
  [''P.BlockL]
  [''BlockL]
deriveAllButSortInjection
  [ ''BlockIsBlock ]
createSortInclusionInfers
  [''P.BlockL]
  [''BlockL]

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
         ]
       suiNewNodes =
         [ ''SuiMoveBlockEnd
         ]
       names =
         (moveSigNames \\ [mkName "Identifier", mkName "BinaryExpression", mkName "Block"]) ++
         suiSortInjections ++
         suiNewNodes ++
         [ ''P.Ident
         , ''P.Operator
         , ''P.ArithBinOp
         , ''P.DivOp
         , ''P.ModOp
         , ''P.BitwiseBinOp
         , ''P.LogicalBinOp
         , ''P.ShlOp
         , ''P.ArithShrOp
         , ''P.RelationalBinOp
         , ''P.Block
         , ''P.EmptyBlockEnd
         ]
   runCompTrans $ makeSumType "MSuiMoveSig" names

type MSuiMoveTerm = Term MSuiMoveSig
type MSuiMoveTermLab = TermLab MSuiMoveSig

type MSuiMoveCxt h a = CxtS h MSuiMoveSig a
type MSuiMoveCxtA h a p = AnnCxtS p h MSuiMoveSig a

-----------------------------------------------------------------------------------
----------------------         Sort injections             ------------------------
-----------------------------------------------------------------------------------
