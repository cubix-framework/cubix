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
import Cubix.Language.SuiMove.Modularized as M
import Cubix.Language.Parametric.Syntax as Para hiding
  ( UnaryOpL
  , FunctionParameterL
  , BlockL, BlockItemL
  , Block, iBlock, jBlock, pattern Block'
  , Add, iAdd, jAdd, pattern Add'
  , Assign, iAssign, jAssign, pattern Assign'
  , Div, iDiv, jDiv, pattern Div'
  , Eq, iEq, jEq, pattern Eq'
  , Gt, iGt, jGt, pattern Gt'
  , Lt, iLt, jLt, pattern Lt'
  , Mod, iMod, jMod, pattern Mod'
  , Mul, iMul, jMul, pattern Mul'
  , Neq, iNeq, jNeq, pattern Neq'
  , Shl, iShl, jShl, pattern Shl'
  , Sub, iSub, jSub, pattern Sub'
  )
