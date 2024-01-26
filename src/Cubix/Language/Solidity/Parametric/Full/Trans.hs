{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK hide                      #-}

{-# LANGUAGE CPP                              #-}
{-# LANGUAGE TemplateHaskell                  #-}
{-# LANGUAGE UndecidableInstances             #-}

-- This is a separate file due to GHC's phase restriction.

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Solidity.Parametric.Full.Trans () where
#else
module Cubix.Language.Solidity.Parametric.Full.Trans (
    translate
  , untranslate
  ) where

import Data.Text ( Text )
import Data.Typeable ( Typeable )

import qualified Language.Haskell.TH as TH
import qualified Solidity as S

import Data.Comp.Multi ( caseCxt, Sum, All )
import Data.Comp.Trans ( runCompTrans, deriveTrans, deriveUntrans )

import Cubix.Language.Parametric.Syntax.Base
import Cubix.Language.Parametric.Syntax.Functor
import Cubix.Language.Solidity.Parametric.Full.Names
import Cubix.Language.Solidity.Parametric.Full.Types

----------------------------------------------------------------------------


--------------------------------------------------------------------------------------
-------------------- Trans (third-party syntax to modularized syntax) ----------------
--------------------------------------------------------------------------------------

runCompTrans $ deriveTrans origASTTypes (TH.ConT ''SolidityTerm)

translate :: S.Solidity -> SolidityTerm SolidityL
translate = trans

instance (Trans c l, Typeable l) => Trans [c] [l] where
  trans [] = riNilF
  trans (x:xs) = (trans x :: SolidityTerm l) `iConsF` (trans xs)

instance (Trans c l, Typeable l) => Trans (Maybe c) (Maybe l) where
  trans Nothing = riNothingF
  trans (Just x) = iJustF $ (trans x :: SolidityTerm l)

instance (Trans c l, Trans d l', Typeable l, Typeable l') => Trans (c, d) (l, l') where
  trans (x, y) = riPairF (trans x) (trans y)


instance (Trans c l, Trans d l', Trans e l'',
          Typeable l, Typeable l', Typeable l'') => Trans (c, d, e) (l, l', l'') where
  trans (x, y, z) = riTripleF (trans x) (trans y) (trans z)


instance Trans Int IntL where
  trans x = iIntF x

instance Trans Text TextL where
  trans x = iTextF x

instance Trans () () where
  trans _ = iUnitF


--------------------------------------------------------------------------------------
-------------------- Untrans (modularized syntax to third-party syntax) --------------
--------------------------------------------------------------------------------------

runCompTrans $ deriveUntrans origASTTypes (TH.ConT ''SolidityTerm)

type instance Targ [l] = [Targ l]
instance Untrans ListF where
  untrans NilF = T []
  untrans (ConsF a b) = T ((t a) : (t b))

type instance Targ (Maybe l) = Maybe (Targ l)
instance Untrans MaybeF where
  untrans NothingF = T Nothing
  untrans (JustF x) = T (Just (t x))

type instance Targ (l, l') = (Targ l, Targ l')
instance Untrans PairF where
  untrans (PairF x y) = T (t x, t y)

type instance Targ (l, l', l'') = (Targ l, Targ l', Targ l'')
instance Untrans TripleF where
  untrans (TripleF x y z) = T (t x, t y, t z)


type instance Targ IntL = Int
instance Untrans IntF where
  untrans (IntF x) = T x

type instance Targ TextL = Text
instance Untrans TextF where
  untrans (TextF x) = T x

type instance Targ () = ()
instance Untrans UnitF where
  untrans UnitF = T ()

instance (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt @Untrans untrans
  
#endif
