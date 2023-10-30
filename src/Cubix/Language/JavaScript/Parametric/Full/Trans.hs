{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP                     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE UndecidableInstances    #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.JavaScript.Parametric.Full.Trans () where
#else
module Cubix.Language.JavaScript.Parametric.Full.Trans (
    translate
  , untranslate
  ) where

import Data.Proxy
import Data.Typeable ( Typeable )

import qualified Language.Haskell.TH as TH
import qualified Language.JavaScript.Parser.AST as JS

import Data.Comp.Multi ( Sum, All, caseCxt )
import Data.Comp.Trans ( runCompTrans, deriveTrans, deriveUntrans )

import Cubix.Language.JavaScript.Parametric.Full.Names
import Cubix.Language.JavaScript.Parametric.Full.Types
import Cubix.Language.Parametric.Syntax.Functor

runCompTrans $ deriveTrans origASTTypes (TH.ConT ''JSTerm)

translate :: JS.JSAST -> JSTerm JSASTL
translate = trans

instance (Trans c l) => Trans (JS.JSCommaList c) (JSCommaList l) where
  trans (JS.JSLCons a b c) = riJSLCons (trans a) (trans b) (trans c)
  trans (JS.JSLOne a)      = riJSLOne (trans a)
  trans  JS.JSLNil         = riJSLNil

instance (Trans c l, Trans (JS.JSCommaList c) (JSCommaList l))
            => Trans (JS.JSCommaTrailingList c) (JSCommaTrailingList l) where
  trans (JS.JSCTLComma a b) = riJSCTLComma (trans a) (trans b)
  trans (JS.JSCTLNone a)    = riJSCTLNone (trans a)

instance (Trans c l, Typeable l) => Trans [c] [l] where
  trans [] = riNilF
  trans (x:xs) = (trans x :: JSTerm l) `iConsF` (trans xs)

instance (Trans c l, Typeable l) => Trans (Maybe c) (Maybe l) where
  trans Nothing  = riNothingF
  trans (Just x) = iJustF $ (trans x :: JSTerm l)


runCompTrans $ deriveUntrans origASTTypes (TH.ConT ''JSTerm)

type instance Targ (JSCommaList l) = JS.JSCommaList (Targ l)
instance Untrans JSCommaListF where
  untrans (JSLCons a b c) = T $ JS.JSLCons (t a) (t b) (t c)
  untrans (JSLOne a)      = T $ JS.JSLOne (t a)
  untrans  JSLNil         = T $ JS.JSLNil

type instance Targ (JSCommaTrailingList l) = JS.JSCommaTrailingList (Targ l)
instance Untrans JSCommaTrailingListF where
  untrans (JSCTLComma a b) = T $ JS.JSCTLComma (t a) (t b)
  untrans (JSCTLNone a)    = T $ JS.JSCTLNone (t a)

type instance Targ [l] = [Targ l]
instance Untrans ListF where
  untrans NilF = T []
  untrans (ConsF a b) = T ((t a) : (t b))

type instance Targ (Maybe l) = Maybe (Targ l)
instance Untrans MaybeF where
  untrans NothingF = T Nothing
  untrans (JustF x) = T (Just (t x))

instance (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt (Proxy @Untrans) untrans
#endif
