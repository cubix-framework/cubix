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
----------------------         Binary Operations           ------------------------
-----------------------------------------------------------------------------------

createSortInclusionTypes
  [''Parametric.ExpressionL]
  [''HiddenExpressionL]
deriveAllButSortInjection
  [ ''ExpressionIsHiddenExpression ]
createSortInclusionInfers
  [''Parametric.ExpressionL]
  [''HiddenExpressionL]

createSortInclusionTypes
  [''HiddenExpressionL]
  [''Parametric.ExpressionL]
deriveAllButSortInjection
  [ ''HiddenExpressionIsExpression ]
createSortInclusionInfers
  [''HiddenExpressionL]
  [''Parametric.ExpressionL]

-----------------------------------------------------------------------------------
----------------------         Block                       ------------------------
-----------------------------------------------------------------------------------

-- SuiMoveBlockEnd wraps the optional final expression in blocks
data SuiMoveBlockEnd e l where
  SuiMoveBlockEnd :: e (Maybe HiddenExpressionL) -> SuiMoveBlockEnd e Parametric.BlockEndL

deriveAll [''SuiMoveBlockEnd]

-- Sort injection: UseDeclaration can appear as BlockItem
createSortInclusionTypes
  [''UseDeclarationL]
  [''Parametric.BlockItemL]
deriveAllButSortInjection
  [ ''UseDeclarationIsBlockItem ]
createSortInclusionInfers
  [''UseDeclarationL]
  [''Parametric.BlockItemL]

-- Sort injection: BlockItem to parametric BlockItem
createSortInclusionTypes
  [''BlockItemL]
  [''Parametric.BlockItemL]
deriveAllButSortInjection
  [ ''BlockItemIsBlockItem ]
createSortInclusionInfers
  [''BlockItemL]
  [''Parametric.BlockItemL]

-- Sort injection: Block to parametric Block
createSortInclusionTypes
  [''Parametric.BlockL]
  [''BlockL]
deriveAllButSortInjection
  [ ''BlockIsBlock ]
createSortInclusionInfers
  [''Parametric.BlockL]
  [''BlockL]

-----------------------------------------------------------------------------------
----------------------         Unit Expression             ------------------------
-----------------------------------------------------------------------------------

createSortInclusionTypes
  [ ''() ]
  [''UnitExpressionL]
deriveAllButSortInjection
  [ ''UnitIsUnitExpression ]
createSortInclusionInfers
  [ ''() ]
  [''UnitExpressionL]

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
         ]
       suiNewNodes =
         [ ''SuiMoveBlockEnd
         ]
       names =
         (moveSigNames \\ [mkName "Identifier", mkName "BinaryExpression", mkName "Block", mkName "UnitExpression"]) ++
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
         , ''Parametric.Block
         , ''Parametric.EmptyBlockEnd
         , ''Parametric.UnitF
         ]
   runCompTrans $ makeSumType "MSuiMoveSig" names

type MSuiMoveTerm = Term MSuiMoveSig
type MSuiMoveTermLab = TermLab MSuiMoveSig

type MSuiMoveCxt h a = CxtS h MSuiMoveSig a
type MSuiMoveCxtA h a p = AnnCxtS p h MSuiMoveSig a

-----------------------------------------------------------------------------------
----------------------         Sort injections             ------------------------
-----------------------------------------------------------------------------------
