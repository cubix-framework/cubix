{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Language.SuiMove.IPS.Types where

import Data.List ( (\\) )

import Language.Haskell.TH ( Name, mkName, runQ )
import Language.Haskell.TH qualified as TH

import Data.Comp.Multi ( All, HFunctor, Term, project', CxtS, AnnCxtS, inject, project, (:-<:) )
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
---------------       Variable declarations and blocks     ------------------------
-----------------------------------------------------------------------------------

data HiddenTypeIsLocalVarDeclAttrs e l where
   HiddenTypeIsLocalVarDeclAttrs
     :: e HiddenTypeL
     -> HiddenTypeIsLocalVarDeclAttrs e Parametric.LocalVarDeclAttrsL

deriveAllButSortInjection [''HiddenTypeIsLocalVarDeclAttrs]

instance
  ( All HFunctor fs
  , HiddenTypeIsLocalVarDeclAttrs :-<: fs
  ) => InjF fs HiddenTypeL Parametric.LocalVarDeclAttrsL where
  injF = iHiddenTypeIsLocalVarDeclAttrs

  projF' (project' -> Just (HiddenTypeIsLocalVarDeclAttrs ty)) = Just ty
  projF' _ = Nothing

createSortInclusionTypes
  [''Parametric.SingleLocalVarDeclL, ''HiddenExpressionL]
  [''LetStatementL, ''Parametric.LocalVarInitL]
deriveAllButSortInjection
  [''SingleLocalVarDeclIsLetStatement, ''HiddenExpressionIsLocalVarInit]
createSortInclusionInfers
  [''Parametric.SingleLocalVarDeclL, ''HiddenExpressionL]
  [''LetStatementL, ''Parametric.LocalVarInitL]

data BinderIsVarDeclBinder e l where
  BinderIsVarDeclBinder :: e BindListL -> BinderIsVarDeclBinder e Parametric.VarDeclBinderL

deriveAllButSortInjection [''BinderIsVarDeclBinder]

data SuiMoveBlockEnd e l where
  SuiMoveBlockEnd :: e (Maybe HiddenExpressionL) -> SuiMoveBlockEnd e Parametric.BlockEndL

deriveAll [''SuiMoveBlockEnd]

createSortInclusionTypes
  [''UseDeclarationL, ''BlockItemL, ''Parametric.BlockL, ''Parametric.SingleLocalVarDeclL]
  [''Parametric.BlockItemL, ''Parametric.BlockItemL, ''BlockL, ''Parametric.BlockItemL]
deriveAllButSortInjection
  [ ''UseDeclarationIsBlockItem, ''BlockItemIsBlockItem, ''BlockIsBlock, ''SingleLocalVarDeclIsBlockItem ]
createSortInclusionInfers
  [''UseDeclarationL, ''BlockItemL, ''Parametric.BlockL, ''Parametric.SingleLocalVarDeclL]
  [''Parametric.BlockItemL, ''Parametric.BlockItemL, ''BlockL, ''Parametric.BlockItemL]

-----------------------------------------------------------------------------------
----------------------           Expressions               ------------------------
-----------------------------------------------------------------------------------

createSortInclusionTypes
  [''(), ''HiddenUnaryExpressionL, ''HiddenExpressionL, ''Parametric.AssignL, ''Parametric.ExpressionL, ''HiddenExpressionL]
  [''UnitExpressionL, ''Parametric.LhsL, ''Parametric.RhsL, ''HiddenExpressionL, ''HiddenExpressionL, ''Parametric.ExpressionL]
deriveAllButSortInjection
  [''UnitIsUnitExpression, ''HiddenUnaryExpressionIsLhs, ''HiddenExpressionIsRhs, ''AssignIsHiddenExpression, ''ExpressionIsHiddenExpression, ''HiddenExpressionIsExpression]
createSortInclusionInfers
  [''(), ''HiddenUnaryExpressionL, ''HiddenExpressionL, ''Parametric.AssignL, ''Parametric.ExpressionL, ''HiddenExpressionL]
  [''UnitExpressionL, ''Parametric.LhsL, ''Parametric.RhsL, ''HiddenExpressionL, ''HiddenExpressionL, ''Parametric.ExpressionL]

-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------

do let suiSortInjections =
         [ ''HiddenTypeIsLocalVarDeclAttrs
         , ''BinderIsVarDeclBinder
         , ''SingleLocalVarDeclIsLetStatement
         , ''HiddenExpressionIsLocalVarInit
         , ''IdentIsIdentifier
         , ''ExpressionIsHiddenExpression
         , ''HiddenExpressionIsExpression
         , ''UseDeclarationIsBlockItem
         , ''BlockItemIsBlockItem
         , ''SingleLocalVarDeclIsBlockItem
         , ''BlockIsBlock
         , ''UnitIsUnitExpression
         , ''HiddenUnaryExpressionIsLhs
         , ''HiddenExpressionIsRhs
         , ''AssignIsHiddenExpression
         ]
       suiNewNodes =
         [ ''SuiMoveBlockEnd
         ]
       names =
         (moveSigNames \\
          [ mkName "BinaryExpression", mkName "UnaryExpression", mkName "UnitExpression", mkName "AssignExpression"
          , mkName "Identifier", mkName "Block", mkName "LetStatement"
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
         , ''Parametric.OptLocalVarInit
         , ''Parametric.TupleBinder
         , ''Parametric.IdentIsVarDeclBinder
         , ''Parametric.EmptyLocalVarDeclAttrs
         , ''Parametric.SingleLocalVarDecl
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
  injF = iHiddenExpressionIsExpression
       . iHiddenExpressionUnaryExpression
       . iHiddenUnaryExpression
       . iHiddenUnaryExpressionInternal0UnaryExpression
  
  projF' e
    | Just (HiddenExpressionIsExpression e') <- project' e
    , Just (HiddenExpressionUnaryExpression ue) <- project' e'
    , Just (HiddenUnaryExpression ue') <- project' ue
    , Just (HiddenUnaryExpressionInternal0UnaryExpression uexp) <- project' ue'
    = projF' uexp
  projF' _ = Nothing
