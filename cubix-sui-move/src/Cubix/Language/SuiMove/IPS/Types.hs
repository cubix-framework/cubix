{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Incremental Parametric Syntax for Sui Move
-- This module defines the IPS signature where Move-specific nodes
-- are replaced with generic Cubix nodes.

module Cubix.Language.SuiMove.IPS.Types where

import Data.List ( (\\) )

import Language.Haskell.TH ( mkName )

import Data.Comp.Multi ( Term, project', CxtS, AnnCxtS )
import Data.Comp.Trans ( runCompTrans, makeSumType )

import Cubix.Language.SuiMove.Modularized as Move
import Cubix.Language.Info
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax qualified as P

-----------------------------------------------------------------------------------
---------------------     Identifier injection             ------------------------
-----------------------------------------------------------------------------------

-- Create sort inclusion to inject generic Ident into Move's IdentifierL
createSortInclusionTypes
  [''P.IdentL]
  [''IdentifierL]
deriveAllButSortInjection
  [''IdentIsIdentifier]
createSortInclusionInfers
  [''P.IdentL]
  [''IdentifierL]

-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------

do let moveSortInjections =
         [ ''IdentIsIdentifier
         ]
       moveNewNodes = []
       names =
         (moveSigNames \\ [mkName "Identifier"]) ++
         moveSortInjections ++
         moveNewNodes ++
         [ ''P.Ident
         ]
   runCompTrans $ makeSumType "MSuiMoveSig" names


type MSuiMoveTerm = Term MSuiMoveSig
type MSuiMoveTermLab = TermLab MSuiMoveSig

type MSuiMoveCxt h a = CxtS h MSuiMoveSig a
type MSuiMoveCxtA h a p = AnnCxtS p h MSuiMoveSig a


-----------------------------------------------------------------------------------
----------------------         Sort injections             ------------------------
-----------------------------------------------------------------------------------

-- Inject generic Ident into IdentifierL context
instance {-# OVERLAPPING #-} InjF MSuiMoveSig P.IdentL IdentifierL where
    injF = injF . IdentIsIdentifier'

    projF' e
      | Just (IdentIsIdentifier ident) <- project' e
      = Just ident
    projF' _ = Nothing
