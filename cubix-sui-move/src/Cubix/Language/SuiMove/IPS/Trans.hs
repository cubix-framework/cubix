{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cubix.Language.SuiMove.IPS.Trans (
    translate
  , untranslate
  ) where

import Data.List ( (\\) )
import Data.Text qualified as Text
import Language.Haskell.TH ( mkName )
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Data.Comp.Multi ( inject, unTerm, caseCxt, Sum, All, HFunctor(..), (:-<:) )

import Cubix.Language.SuiMove.IPS.Types
import Cubix.Language.SuiMove.Modularized qualified as F
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax

--------------------------------------------------------------------


------------------------------------------------------------------------------------
---------------- Forward translation: Modularized syntax to IPS  -------------------
------------------------------------------------------------------------------------

--------------------------------
------------- Top-level translate
--------------------------------

------ Top-level definition

translate :: F.MoveTerm l -> MSuiMoveTerm l
translate = trans . unTerm @(Sum F.MoveSig)

translate' :: (InjF MSuiMoveSig l l') => F.MoveTerm l -> MSuiMoveTerm l'
translate' = injF . translate

------ Class

class Trans f where
  trans :: f F.MoveTerm l -> MSuiMoveTerm l

------ Default and standard cases

instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt @Trans trans

transDefault :: (HFunctor f, f :-<: MSuiMoveSig, f :-<: F.MoveSig) => f F.MoveTerm l -> MSuiMoveTerm l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MSuiMoveSig, f :-<: F.MoveSig) => Trans f where
  trans = transDefault

---------------------------------
-------------- Per-fragment Instances
---------------------------------

-------- Identifiers

-- Translate Move's Identifier to generic Ident
instance Trans F.Identifier where
  trans (F.Identifier t) = iIdent (Text.unpack t)


------------------------------------------------------------------------------------
---------------- Backward translation: IPS to Modularized syntax  ------------------
------------------------------------------------------------------------------------

--------------------------------
------------- Top-level untranslate
--------------------------------

------ Class

class Untrans f where
  untrans :: f MSuiMoveTerm l -> F.MoveTerm l

------ Default and standard cases

instance {-# OVERLAPPING #-} (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt @Untrans untrans

untransError :: (HFunctor f, f :-<: MSuiMoveSig) => f MSuiMoveTerm l -> F.MoveTerm l
untransError t = error $ "Cannot untranslate root node: " ++ show (inject t)

do let moveSortInjections = [ ''IdentIsIdentifier ]
       moveNewNodes = []
       ipsNames = (F.moveSigNames \\ [mkName "Identifier"]) ++ moveSortInjections ++ moveNewNodes ++ [ ''Ident ]
       -- Generate instances for nodes in IPS but not in modularized sig, excluding sort injections
       targTs = map ConT $ (ipsNames \\ F.moveSigNames) \\ moveSortInjections
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)

untransDefault :: (HFunctor f, f :-<: F.MoveSig) => f MSuiMoveTerm l -> F.MoveTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: F.MoveSig) => Untrans f where
  untrans = untransDefault

------ Top level function on terms

-- Must define this after template Haskell block
untranslate :: MSuiMoveTerm l -> F.MoveTerm l
untranslate = untrans . unTerm

untranslate' :: InjF MSuiMoveSig l l' => MSuiMoveTerm l' -> F.MoveTerm l
untranslate' = untranslate . fromProjF

---------------------------------
-------------- Per-fragment Instances
---------------------------------

-------- Identifiers

-- Helper function to translate generic Ident back to Move's Identifier
untransIdent :: MSuiMoveTerm IdentL -> F.MoveTerm F.IdentifierL
untransIdent (Ident' s) = F.iIdentifier (Text.pack s)

-- Handle the sort injection wrapper
instance {-# OVERLAPPING #-} Untrans IdentIsIdentifier where
  untrans (IdentIsIdentifier ident) = untransIdent ident
