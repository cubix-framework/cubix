{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP                     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE UndecidableInstances    #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Solidity.Parametric.Common.Trans () where
#else
module Cubix.Language.Solidity.Parametric.Common.Trans (
    translate
  , untranslate
  ) where

import Control.Monad.Identity ( Identity(..) )

import Data.Monoid ( Any(..) )
import Data.List( (\\) )
import Data.Text qualified as Text
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Data.Comp.Multi ( project, inject, unTerm, caseCxt, Sum, All, HFunctor(..), (:-<:) )
import Data.Comp.Multi.Strategic ( crushtdT, addFail, promoteTF )

import Cubix.Language.Solidity.Parametric.Common.Types
import qualified Cubix.Language.Solidity.Parametric.Full as F
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

translate :: F.SolidityTerm l -> MSolidityTerm l
translate = trans . unTerm

translate' :: (InjF MSoliditySig l l') => F.SolidityTerm l -> MSolidityTerm l'
translate' = injF . translate

------ Class

class Trans f where
  trans :: f F.SolidityTerm l -> MSolidityTerm l

------ Default and standard cases

instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt @Trans trans

transDefault :: (HFunctor f, f :-<: MSoliditySig, f :-<: F.SoliditySig) => f F.SolidityTerm l -> MSolidityTerm l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MSoliditySig, f :-<: F.SoliditySig) => Trans f where
  trans = transDefault

---------------------------------
-------------- Per-fragment Instances
---------------------------------

-------- Identifiers

transIdent :: F.SolidityTerm F.IdentifierL -> MSolidityTerm IdentL
transIdent (project -> Just (F.Identifier t)) = Ident' (Text.unpack t)

-- Clone of transIdent because type-safe pattern match
instance Trans F.Identifier where
  trans (F.Identifier n) = iIdent (Text.unpack n)

------------------------------------------------------------------------------------
---------------- Reverse translation: IPS to modularized syntax  -------------------
------------------------------------------------------------------------------------

--------------------------------
------------- Top-level untranslate
--------------------------------

------ Class (contains top-level function on sigs)

class Untrans f where
  untrans :: f MSolidityTerm l -> F.SolidityTerm l

------ Default and standard cases

instance {-# OVERLAPPING #-} (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt @Untrans untrans

untransError :: (HFunctor f, f :-<: MSoliditySig) => f MSolidityTerm l -> F.SolidityTerm l
untransError t = error $ "Cannot untranslate root node: " ++ (show $ inject t)

do ipsNames <- sumToNames ''MSoliditySig
   modNames <- sumToNames ''F.SoliditySig
   let targTs = map ConT $ (ipsNames \\ modNames) \\ [ ''IdentIsIdentifier]
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)

untransDefault :: (HFunctor f, f :-<: F.SoliditySig) => f MSolidityTerm l -> F.SolidityTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: F.SoliditySig) => Untrans f where
  untrans = untransDefault

------ Top level function on terms

-- Must define this after template Haskell block
untranslate :: MSolidityTerm l -> F.SolidityTerm l
untranslate = untrans . unTerm

--------------------------------
------------- Per-fragment instances
--------------------------------

-------- Identifiers

untransIdent :: MSolidityTerm IdentL -> F.SolidityTerm F.IdentifierL
untransIdent (Ident' s) = F.iIdentifier (Text.pack s)

instance {-# OVERLAPPING #-} Untrans IdentIsIdentifier where
  untrans (IdentIsIdentifier n) = untransIdent n


#endif
