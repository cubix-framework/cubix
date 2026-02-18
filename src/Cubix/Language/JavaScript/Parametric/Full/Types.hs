{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cubix.Language.JavaScript.Parametric.Full.Types where

#ifndef ONLY_ONE_LANGUAGE
import qualified Data.Set as Set
import Data.Type.Equality ( (:~:)(..), gcastWith )

import Data.Comp.Multi ( (:<:), inject, project, Cxt, Term )
import Data.Comp.Multi.Derive ( derive, makeConstrNameHF, makeHFunctor, makeHTraversable, makeHFoldable,
                                    makeEqHF, makeShowHF, makeOrdHF )

import qualified Language.Haskell.TH as TH

import Data.Comp.Multi.Strategy.Classification (DynCase(..) )
import qualified Language.JavaScript.Parser.AST as JS
import Data.Comp.Trans ( runCompTrans, deriveMultiComp, withExcludedNames, standardExcludedNames, makeSumType )

import Cubix.Language.Info
import Cubix.Language.JavaScript.Parametric.Full.Names
import Cubix.Language.Parametric.Derive

-----------------------------------------------------------


do let excludedNames = Set.union standardExcludedNames (Set.fromList [''JS.JSCommaList, ''JS.JSCommaTrailingList])
   runCompTrans $ withExcludedNames excludedNames $ deriveMultiComp ''JS.JSAST

-----------------------------------------------------------


-- I'd prefer to have labels named "JSCommaListL" and "JSCommaList",
-- but the current implementation of comptrans will just use (JS.JSCommaList l) as a label

type JSCommaList l = JS.JSCommaList l
data JSCommaListF e l where
  JSLCons :: e (JSCommaList l) -> e JSAnnotL -> e l -> JSCommaListF e (JSCommaList l)
  JSLOne  :: e l -> JSCommaListF e (JSCommaList l)
  JSLNil  :: JSCommaListF e (JSCommaList t)

type JSCommaTrailingList l = JS.JSCommaTrailingList l
data JSCommaTrailingListF e l where
  JSCTLComma :: e (JSCommaList l) -> e JSAnnotL -> JSCommaTrailingListF e (JSCommaTrailingList l)
  JSCTLNone  :: e (JSCommaList l) -> JSCommaTrailingListF e (JSCommaTrailingList l)



instance {-# OVERLAPPING #-} (JSCommaListF :<: g, DynCase (Cxt h g a) l) => DynCase (Cxt h g a) (JSCommaList l) where
  dyncase (project -> Just JSLNil) = Nothing
  dyncase (project -> Just (JSLOne x)) = do (p :: _ :~: l) <- dyncase x
                                            return $ gcastWith p Refl
  dyncase (project -> Just (JSLCons x _ _)) = dyncase x
  dyncase _ = Nothing



instance (JSCommaTrailingListF :<: g, DynCase (Cxt h g a) (JSCommaList l)) => DynCase (Cxt h g a) (JSCommaTrailingList l) where
  dyncase (project -> Just (JSCTLNone x)) = do (p :: _ :~: (JSCommaList l)) <- dyncase x
                                               return $ gcastWith p Refl
  dyncase (project -> Just (JSCTLComma x _)) = do (p :: _ :~: (JSCommaList l)) <- dyncase x
                                                  return $ gcastWith p Refl
  dyncase _ = Nothing


riJSLCons :: (JSCommaListF :<: f) => Cxt h f a (JSCommaList l) -> Cxt h f a JSAnnotL -> Cxt h f a l -> Cxt h f a (JSCommaList l)
riJSLCons a b c = inject $ JSLCons a b c

riJSLOne :: (JSCommaListF :<: f) => Cxt h f a l -> Cxt h f a (JSCommaList l)
riJSLOne = inject . JSLOne

riJSLNil :: (JSCommaListF :<: f) => Cxt h f a (JSCommaList l)
riJSLNil = inject JSLNil

riJSCTLComma :: (JSCommaTrailingListF :<: f) => Cxt h f a (JSCommaList l) -> Cxt h f a JSAnnotL -> Cxt h f a (JSCommaTrailingList l)
riJSCTLComma a b = inject $ JSCTLComma a b

riJSCTLNone :: (JSCommaTrailingListF :<: f) => Cxt h f a (JSCommaList l) -> Cxt h f a (JSCommaTrailingList l)
riJSCTLNone = inject . JSCTLNone


-----------------------------------------------------------

do let specialSigNames = [''JSCommaListF, ''JSCommaTrailingListF]
   let jsSigNames = jsSigNamesBase ++ specialSigNames

   decs1 <- deriveAll newASTTypes
   decs2 <- derive [makeHFunctor, makeHTraversable, makeHFoldable, makeEqHF, makeShowHF,
                                makeOrdHF, makeConstrNameHF]
                   specialSigNames
   decs3 <- runCompTrans $ makeSumType "JSSig" jsSigNames

   return $ decs1 ++ decs2 ++ decs3

type JSTerm    = Term JSSig
type JSTermLab l = TermLab JSSig l

-- Phase restriction makes eliminating redundancy hard
jsSigNames :: [TH.Name]
jsSigNames = jsSigNamesBase ++ [''JSCommaListF, ''JSCommaTrailingListF]

#endif
