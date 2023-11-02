{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP                    #-}
{-# LANGUAGE UndecidableInstances   #-}

module Cubix.Transformations.TAC.Custom (
    IsValue(..)
  , RenderGuard(..)

  , ShouldHoist(..)
  , ShouldHoistChild(..)
  ) where

import Control.Monad.Identity ( runIdentity )

import Data.List ( (\\) )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Proxy

import Data.Comp.Multi ( Node, Cxt(..), Term, Sum, All, unTerm, ContextS, project', proj, EqHF, project, runE, HFoldable, htoList, (:-<:), stripA, caseCxt, HFunctor)
import Data.Comp.Multi.Strategic ( Translate, crushtdT, addFail, promoteTF )
import Data.Comp.Multi.Strategy.Classification ( DynCase, subterms )

import Cubix.Language.Info

import Cubix.Language.JavaScript.Parametric.Common as JSCommon
import Cubix.Language.Lua.Parametric.Common as LCommon
import Cubix.Language.Python.Parametric.Common as PCommon

import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax as P

import Cubix.Transformations.TAC.Sorts

import Cubix.Sin.Compdata.Annotation ( MonadAnnotater )

--------------------------------------------------------------------------------------

isLeafExpression ::
  forall fs.
  ( All EqHF fs
  , All HFoldable fs
  , All HFunctor fs
  , DynCase (TermLab fs) (ExpressionSort fs)
  ) => TermLab fs (ExpressionSort fs) -> Bool
isLeafExpression t = ((subterms t :: [TermLab fs (ExpressionSort fs)]) \\ [t]) == []

containsAnyVar ::
  forall fs l.
  ( All HFoldable fs
  , All HFunctor fs
  , Ident :-<: fs
  , DynCase (TermLab fs) IdentL
  ) => Set String -> TermLab fs l -> Bool
containsAnyVar varSet t = any inVarSet (subterms t :: [TermLab fs IdentL])
  where
    inVarSet :: TermLab fs IdentL -> Bool
    inVarSet (project' -> Just (Ident s)) = Set.member s varSet

--------------------------------------------------------------------------------------

-- A term is a value if it contains no computation and hence should not be hoisted
-- However, variables do contain computation (a read),
-- Hence, we'll use this definition of "isValue" for this method: a term is a value if it contains no computation,
-- with the exception of reads that commute with everything else in the containing express
----- and we'll only approximately check that by assuming that assignments are the only expressions that can make reads not commute
----- (so we don't need a frame analysis)
-----
----- And so, this caveat applies to JS, but not Lua or Python, which like inline assigns
class IsValue fs where
  type ModifiedSet fs
  isValue :: ModifiedSet fs -> TermLab fs (ExpressionSort fs) -> Bool
  modifiedVariables :: TermLab fs l -> ModifiedSet fs

#ifndef ONLY_ONE_LANGUAGE
instance IsValue MJSSig where
  type ModifiedSet MJSSig = Set String
  isValue varSet t = isLeafExpression t && not (containsAnyVar varSet t)

  modifiedVariables t = runIdentity $ (crushtdT $ promoteTF $ addFail getAssignVar) (stripA t)
    where
      getAssignVar :: Translate MJSTerm AssignL (Set String)
      getAssignVar (P.Assign' (projF -> Just (P.Ident' s)) _ _) = return $ Set.singleton s
      getAssignVar _ = mempty

instance IsValue MPythonSig where
  type ModifiedSet MPythonSig = ()
  isValue () t = isLeafExpression t
  modifiedVariables _ = ()
#endif

instance IsValue MLuaSig where
  type ModifiedSet MLuaSig = ()
  isValue _ (project' -> Just (LCommon.PrefixExp (project' -> Just (LCommon.PEFunCall _)))) = False
  isValue () t = isLeafExpression t
  modifiedVariables _ = ()


--------------------------------------------------------------------------------------

class RenderGuard fs where
  renderGuard :: (MonadAnnotater Label m) => Bool -> TermLab fs (ExpressionSort fs) -> [TermLab fs BlockItemL] -> m (TermLab fs BlockItemL)

#ifndef ONLY_ONE_LANGUAGE
instance RenderGuard MJSSig where
  renderGuard sign cond body = annotateLabelOuter $ iJSIf noAnn noAnn signedCond noAnn jsBlock
    where
      noAnn :: ContextS MJSSig a JSAnnotL
      noAnn = iJSNoAnnot

      signedCond = if sign then
                     Hole cond
                   else
                     iJSUnaryExpression (iJSUnaryOpNot noAnn) (Hole cond)

      jsBlock = iJSStatementBlock noAnn (insertFHole $ map projBlockItem body) noAnn iJSSemiAuto

      projBlockItem :: MJSTermLab BlockItemL -> MJSTermLab JSStatementL
      projBlockItem (project' -> Just (JSStatementIsBlockItem x)) = x

instance RenderGuard MPythonSig where
  renderGuard sign cond body = annotateLabelOuter $ PCommon.iConditional (insertF [riPairF signedCond suite]) (insertFHole []) iUnitF
    where
      signedCond = if sign then
                     Hole cond
                   else
                     PCommon.iUnaryOp (PCommon.iNot iUnitF) (Hole cond) iUnitF

      suite = insertFHole $ map projBlockItem body

      projBlockItem :: MPythonTermLab BlockItemL -> MPythonTermLab PCommon.StatementL
      projBlockItem (project' -> Just (PCommon.StatementIsBlockItem x)) = x
#endif

instance RenderGuard MLuaSig where -- handle the insertF
  renderGuard sign cond body = annotateLabelOuter $ LCommon.iIf (insertF [riPairF signedCond block]) Nothing'
    where
      signedCond :: ContextS MLuaSig MLuaTermLab LCommon.ExpL
      signedCond = if sign then
                     Hole cond
                   else
                     LCommon.iUnop LCommon.iNot (Hole cond)

      block = iBlock (insertFHole body) (iLuaBlockEnd Nothing')


--------------------------------------------------------------------------------------

data ShouldHoist = ShouldHoist | ShouldntHoist | StopHoisting
  deriving ( Eq, Ord, Show )

class ShouldHoistSelf' gs (f :: Node) where
  shouldHoistSelf' :: f (Term gs) l -> Maybe ShouldHoist

instance {-# OVERLAPPABLE #-} ShouldHoistSelf' gs f where
  shouldHoistSelf' = const Nothing

instance {-# OVERLAPPING #-} (All (ShouldHoistSelf' gs) fs) => ShouldHoistSelf' gs (Sum fs) where
  shouldHoistSelf' = caseCxt @(ShouldHoistSelf' gs) shouldHoistSelf'

#ifndef ONLY_ONE_LANGUAGE
instance {-# OVERLAPPING #-} ShouldHoistSelf' MJSSig JSExpression where
  shouldHoistSelf' (JSCallExpressionDot _ _ _) = Just ShouldntHoist
  shouldHoistSelf' _                           = Nothing

instance {-# OVERLAPPING #-} ShouldHoistSelf' MPythonSig PCommon.Expr where
  shouldHoistSelf' (PCommon.Starred _ _) = Just ShouldntHoist
  shouldHoistSelf' _                     = Nothing
#endif


-- | If there is a hack in here, this is it
-- One of the Lua tests will infinite-loop unless a certain thing is garbage collected
-- Since all the Lua tests run together and TAC can create extra local references that prevent GC,
-- this makes the Lua test suite infinite loop.
--
-- So....this is a hack so that those things will be GCed so the rest of the suite can run
instance {-# OVERLAPPING #-} ShouldHoistSelf' MLuaSig LCommon.Exp where
  shouldHoistSelf' t
    | Just (LCommon.PrefixExp p) <- proj t
    , Just (LCommon.PEFunCall fc) <- project p
    , Just (LCommon.FunctionCallIsFunCall fc') <- project fc
    , Just (P.FunctionCall _ x _) <- project fc'
    , Just (LCommon.PrefixExpIsFunctionExp f) <- project x
    , Just (LCommon.PEVar v) <- project f
    , Just (LCommon.VarName nam) <- project v
    , Just (Ident' ident) <- projF nam
    , ident == "setmetatable"
                                                       = Just ShouldntHoist

  shouldHoistSelf' t = Nothing

shouldHoistSelf :: (All (ShouldHoistSelf' fs) fs) => Term fs l -> Maybe ShouldHoist
shouldHoistSelf = shouldHoistSelf' . unTerm

class ShouldHoistChild' gs f where
  shouldHoistChild' :: ShouldHoist -> f (Term gs) l -> [ShouldHoist]


defaultShouldHoistChild :: (HFoldable f) => ShouldHoist -> f (Term gs) l -> [ShouldHoist]
defaultShouldHoistChild h t = take (length (htoList t)) (repeat h)

shouldHoistAllChildren :: (HFoldable f) => f (Term gs) l -> [ShouldHoist]
shouldHoistAllChildren = defaultShouldHoistChild ShouldHoist

instance {-# OVERLAPPABLE #-} (HFoldable f) => ShouldHoistChild' gs f where
  shouldHoistChild' = defaultShouldHoistChild

instance {-# OVERLAPPING #-} (All (ShouldHoistChild' gs) fs) => ShouldHoistChild' gs (Sum fs) where
  shouldHoistChild' h = caseCxt @(ShouldHoistChild' gs) (shouldHoistChild' h)

class ShouldHoistChild fs where
  shouldHoistChild :: ShouldHoist -> Term fs l -> [ShouldHoist]

instance ( All (ShouldHoistChild' fs) fs
         , All (ShouldHoistSelf' fs) fs
         , All HFoldable fs
         , All HFunctor fs
         ) => ShouldHoistChild fs where
  shouldHoistChild h t =  map chooseShouldHoist $ zip defaultChoice override
    where
      chooseShouldHoist :: (ShouldHoist, Maybe ShouldHoist) -> ShouldHoist
      chooseShouldHoist (x, Nothing) = x
      chooseShouldHoist (_, Just y)  = y


      defaultChoice =shouldHoistChild' h $ unTerm t
      override = map (runE shouldHoistSelf) $ htoList $ unTerm t

instance {-# OVERLAP #-} ShouldHoistChild' g P.Assign where
  shouldHoistChild' _ (P.Assign _ _ _) = [ShouldntHoist, StopHoisting, ShouldntHoist]

instance {-# OVERLAPPING #-} ShouldHoistChild' gs P.SingleLocalVarDecl where
  shouldHoistChild' _ (P.SingleLocalVarDecl _ _ _) = [StopHoisting, ShouldntHoist, ShouldntHoist]

#ifndef ONLY_ONE_LANGUAGE
-- Because Python has more complicated LHSs, but we've taken care of them by changing the representation

instance {-# OVERLAPPING #-} ShouldHoistChild' MPythonSig P.Assign where
  shouldHoistChild' _ (P.Assign _ _ _) = [ShouldHoist, StopHoisting, ShouldntHoist]

instance {-# OVERLAPPING #-} ShouldHoistChild' MJSSig JSStatement where
  shouldHoistChild' _ (JSExpressionStatement _ _) = [ShouldntHoist, StopHoisting]
  shouldHoistChild' h t = defaultShouldHoistChild h t

instance {-# OVERLAPPING #-} ShouldHoistChild' MJSSig JSTryCatch where
  shouldHoistChild' _ (JSCatch _ _ arg _ body) = [StopHoisting, StopHoisting, ShouldntHoist, StopHoisting, ShouldHoist]
  shouldHoistChild' h t = defaultShouldHoistChild h t

instance {-# OVERLAPPING #-} ShouldHoistChild' MJSSig FunctionCall where
  shouldHoistChild' _ (P.FunctionCall _ _ _) = [StopHoisting, ShouldntHoist, ShouldHoist]

instance {-# OVERLAPPING #-} ShouldHoistChild' MJSSig JSExpression where
  -- Quick but coarse -- only most unary operators (delete, ++, --) should result in non-hoisting
  shouldHoistChild' _ (JSUnaryExpression _ _)      = [StopHoisting, ShouldntHoist]
  shouldHoistChild' _ (JSExpressionPostfix _ _)    = [ShouldntHoist, StopHoisting]
  shouldHoistChild' h t = map (runE hoistUnlessDot) $ htoList t
    where
      hoistUnlessDot :: MJSTerm l -> ShouldHoist
      hoistUnlessDot (project -> Just (JSMemberDot _ _ _)) = ShouldntHoist
      hoistUnlessDot _                                     = ShouldHoist

--  TODO: I can improve this by making augmented assign, delete, for all use our new lvalue system
instance {-# OVERLAPPING #-} ShouldHoistChild' MPythonSig PCommon.Statement where
  shouldHoistChild' _ (PCommon.StmtExpr _ _) = [ShouldntHoist, StopHoisting]
  shouldHoistChild' _ (PCommon.AugmentedAssign _ _ _ _) = [ShouldntHoist, StopHoisting, ShouldntHoist, StopHoisting]
  shouldHoistChild' _ (PCommon.Delete _ _) = [ShouldntHoist, StopHoisting]
  shouldHoistChild' _ (PCommon.For _ _ _ _ _) = [StopHoisting, ShouldntHoist, ShouldHoist, ShouldHoist, StopHoisting]

  -- "class Foo(a.b)" and "x = a.b \n class Foo(x)" can behave differently. I don't understand why. Fuck Python.
  ----- The reason is that unit tests like to do reflection to find all classes in scope...and x = a.b makes x a
  ----- class in scope. So, this is a hack that dodges that problem, but....fuck Python.
  shouldHoistChild' _ (PCommon.Class _ _ _ _) = [StopHoisting, StopHoisting, StopHoisting, StopHoisting]
  shouldHoistChild' h t = shouldHoistAllChildren t

instance {-# OVERLAPPING #-} ShouldHoistChild' MPythonSig PCommon.PyClass where
  -- See the above comment for PCommon.Class. When we replaced the original Python Class constructor
  -- with PyClass when doing the docstring change, this didn't get updated. We got burned
  -- by not removing the old constructors from the representation
  --
  -- I am going to go ahead and say "ShouldHoist" for the class body though....we'll see what happens
  shouldHoistChild' _ (PCommon.PyClass _ _ _) = [StopHoisting, StopHoisting, ShouldHoist]

instance {-# OVERLAPPING #-} ShouldHoistChild' MPythonSig PCommon.PyWithBinder where
  shouldHoistChild' _ (PyWithBinder _ _) = [ShouldHoist, ShouldHoist]

-- In something like "@a.b(foo())", the lookup "a.b" can have an effect, and
-- "a.b(x)" is different from "t = a.b; t(x)". I just plain don't see any way to hoist
-- this, period.
instance {-# OVERLAPPING #-} ShouldHoistChild' MPythonSig PCommon.Decorator where
  shouldHoistChild' _ _ = [StopHoisting, StopHoisting, StopHoisting]

-- | We don't support TAC in comprehensions or lambdas;
--   must avoid hoisting outside of binders
instance {-# OVERLAPPING #-} ShouldHoistChild' MPythonSig PCommon.Expr where
  shouldHoistChild' h (PCommon.Paren     _ _) = [h, h]
  shouldHoistChild' h (PCommon.ListComp  _ _) = [StopHoisting, StopHoisting]
  shouldHoistChild' h (PCommon.DictComp  _ _) = [StopHoisting, StopHoisting]
  shouldHoistChild' h (PCommon.SetComp   _ _) = [StopHoisting, StopHoisting]
  shouldHoistChild' h (PCommon.Generator _ _) = [StopHoisting, StopHoisting]
  shouldHoistChild' h (PCommon.Lambda _ _ _)  = [StopHoisting, StopHoisting, StopHoisting]
  shouldHoistChild' h t                       = shouldHoistAllChildren t

instance {-# OVERLAPPING #-} ShouldHoistChild' MPythonSig PCommon.PyCondExpr where
  shouldHoistChild' h t = shouldHoistAllChildren t

-- Do not support expressions in default parameter values
instance {-# OVERLAPPING #-} ShouldHoistChild' MPythonSig FunctionDef where
  shouldHoistChild' h (FunctionDef _ _ _ _) = [StopHoisting, StopHoisting, StopHoisting, StopHoisting]

-- Don't hoist the iterator
instance {-# OVERLAPPING #-} ShouldHoistChild' MPythonSig PCommon.Handler where
  shouldHoistChild' h (PCommon.Handler _ _ _) = [StopHoisting, ShouldntHoist, StopHoisting]
#endif

instance {-# OVERLAPPING #-} ShouldHoistChild' MLuaSig LCommon.Exp where
  shouldHoistChild' h = shouldHoistAllChildren

instance {-# OVERLAPPING #-} ShouldHoistChild' MLuaSig LCommon.Var where
  shouldHoistChild' h = shouldHoistAllChildren

-- | Although for...in loops can loop over tables, more t
instance {-# OVERLAPPING #-} ShouldHoistChild' MLuaSig LCommon.Stat where
  shouldHoistChild' _ (ForIn _ _ _) = [StopHoisting, StopHoisting, StopHoisting]
  shouldHoistChild' h t             = shouldHoistAllChildren t

-- | The only place where [Exp] can occur is in assignments and function calls.
--   For both of these, if the last thing is a function call, should not hoist
--   because Lua will pass along multiple return values
instance {-# OVERLAPPING #-} ShouldHoistChild' MLuaSig ListF where
  shouldHoistChild' _ (ConsF exp NilF')
      | Just (PrefixExp pe) <- project exp
      , Just (PEFunCall _)  <- project pe = [ShouldntHoist, ShouldntHoist] -- FIXME: Update for function calls
  shouldHoistChild' h t = defaultShouldHoistChild h t
