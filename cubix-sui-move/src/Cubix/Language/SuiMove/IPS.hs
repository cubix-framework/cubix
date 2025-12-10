{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Cubix.Language.SuiMove.IPS
-- Copyright   :  (c) 2024 Jakub Kopański
-- License     :  BSD3
--
--
-- Incremental Parametric Syntax for Sui Move.
-- Language definition is auto-generated from tree-sitter grammar and modified
-- to replace Move-specific Identifier nodes with generic Cubix Ident nodes.
--
--------------------------------------------------------------------------------

module Cubix.Language.SuiMove.IPS (
    module Types
  , module Trans
  , module M

  , module Para
  ) where

import Cubix.Language.SuiMove.IPS.Types as Types
import Cubix.Language.SuiMove.IPS.Trans as Trans
import Cubix.Language.SuiMove.Modularized as M hiding
  ( Identifier, iIdentifier, jIdentifier, pattern Identifier'
  , IdentifierL
  -- Hide types and operators that conflict with Para exports
  , UnaryOpL, BlockL, BlockItemL, FunctionParameterL
  , Block, iBlock, jBlock, pattern Block'
  , Assign, iAssign, jAssign, pattern Assign'
  , Add, iAdd, jAdd, pattern Add'
  , Sub, iSub, jSub, pattern Sub'
  , Mul, iMul, jMul, pattern Mul'
  , Div, iDiv, jDiv, pattern Div'
  , Mod, iMod, jMod, pattern Mod'
  , Shl, iShl, jShl, pattern Shl'
  , Lt, iLt, jLt, pattern Lt'
  , Gt, iGt, jGt, pattern Gt'
  , Eq, iEq, jEq, pattern Eq'
  , Neq, iNeq, jNeq, pattern Neq'
  )
import Cubix.Language.Parametric.Syntax as Para
