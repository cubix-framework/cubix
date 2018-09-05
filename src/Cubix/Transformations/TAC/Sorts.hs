{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Transformations.TAC.Sorts (
    ExpressionSort
  , BarrierCheck(..)

  , isExpression
  ) where


import Control.Monad ( MonadPlus )
import Data.Proxy ( Proxy(..) )

import Data.Comp.Multi.Strategic ( GRewriteM, guardedT, guardBoolT, isSortT, isSortR, idR, failR )
import Data.Comp.Multi.Strategy.Classification ( DynCase )

import Cubix.Language.Info

import Cubix.Language.C.Parametric.Common as CCommon
import Cubix.Language.Java.Parametric.Common as JCommon
import Cubix.Language.JavaScript.Parametric.Common as JSCommon
import Cubix.Language.Lua.Parametric.Common as LCommon
import Cubix.Language.Python.Parametric.Common as PCommon

--------------------------------------------------------------------------------------

type family ExpressionSort (f :: (* -> *) -> * -> *) :: *
type family BarrierSorts  (f :: (* -> *) -> * -> *) :: [*]

#ifndef ONLY_ONE_LANGUAGE
type instance ExpressionSort MCSig = CCommon.CExpressionL
type instance BarrierSorts   MCSig = [CCommon.CStatementL, CCommon.CDeclarationL, CCommon.CFunctionDefL, SingleLocalVarDeclL]

type instance ExpressionSort MJavaSig = JCommon.ExpL
type instance BarrierSorts   MJavaSig = '[JCommon.StmtL, SingleLocalVarDeclL, JCommon.ClassDeclL]

type instance ExpressionSort MJSSig = JSCommon.JSExpressionL
type instance BarrierSorts   MJSSig = '[JSCommon.JSStatementL]

type instance ExpressionSort MPythonSig = PCommon.ExprL
type instance BarrierSorts   MPythonSig = '[PCommon.StatementL]
#endif

type instance ExpressionSort MLuaSig = LCommon.ExpL
type instance BarrierSorts   MLuaSig = '[LCommon.StatL, LCommon.FunBodyL]

--------------------------------------------------------------------------------------

-- Subexp-to-tmp transform will run on all subexps of some "barrier sort" which are not contained in a sub barrier sort
-- E.g.: Run all all exps in a statement which are not contained in a child statement
class BarrierCheck' f (l :: [*]) where
  barrierCheck' :: (MonadPlus m) => Proxy l -> GRewriteM m (TermLab f)

class BarrierCheck f where
  barrierCheck :: (MonadPlus m) => GRewriteM m (TermLab f)

instance (BarrierCheck' f (BarrierSorts f)) => BarrierCheck f where
  barrierCheck = barrierCheck' (Proxy :: Proxy (BarrierSorts f))

instance BarrierCheck' f '[] where
  barrierCheck' _ = failR

instance (BarrierCheck' f ls, DynCase (TermLab f) l) => BarrierCheck' f (l ': ls) where
  barrierCheck' _ = guardedT (guardBoolT $ isSortT (Proxy :: Proxy l)) idR (barrierCheck' (Proxy :: Proxy ls))

--------------------------------------------------------------------------------------

isExpression :: forall m f. (MonadPlus m, DynCase (TermLab f) (ExpressionSort f)) => GRewriteM m (TermLab f)
isExpression = isSortR (Proxy :: Proxy (ExpressionSort f))
