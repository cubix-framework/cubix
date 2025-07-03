{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cubix.Language.Python.Parametric.Common.Types where

#ifndef ONLY_ONE_LANGUAGE
import Data.List ( (\\) )
import Language.Haskell.TH ( mkName )

import Data.Comp.Multi ( Node, Term, AnnTerm, project', project, HFunctor, CxtS, All, (:-<:) )
import Data.Comp.Trans ( runCompTrans, makeSumType )

import Cubix.Language.Info
import Cubix.Language.Python.Parametric.Full.Names
import Cubix.Language.Python.Parametric.Full.Types as Py
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax as P

-----------------------------------------------------------------------------------
---------------       Variable declarations and blocks     ------------------------
-----------------------------------------------------------------------------------

-- Lvalues
-- See https://docs.python.org/3/reference/simple_stmts.html#grammar-token-assignment_stmt

data PyLValueL


-- Python manual is not detailed enough to explain whether we can model augmented
-- and normal assignments with the generic Assign fragment. A more immediate issue though
-- is that they allow different LHSs. It looks like we could use Assign to model either, but
-- we use it to model normal assignments

data PyLhs e l where
  PyLhs :: e [PyLValueL] -> PyLhs e LhsL

data TupleLValue e l where
  TupleLValue :: e [PyLValueL] -> TupleLValue e PyLValueL

data ListLValue e l where
  ListLValue :: e [PyLValueL] -> ListLValue e PyLValueL

data DotLValue e l where
  DotLValue :: e Py.ExprL -> e P.IdentL -> DotLValue e PyLValueL

data StarLValue e l where
  StarLValue :: e PyLValueL -> StarLValue e PyLValueL

data SubscriptLValue e l where
  SubscriptLValue :: e Py.ExprL -> e Py.ExprL -> SubscriptLValue e PyLValueL

data SliceLValue e l where
  SliceLValue :: e Py.ExprL -> e [Py.SliceL] -> SliceLValue e PyLValueL

data ParenLValue e l where
  ParenLValue :: e PyLValueL -> ParenLValue e PyLValueL

deriveAll [''PyLhs, ''TupleLValue, ''ListLValue, ''DotLValue, ''StarLValue, ''SubscriptLValue, ''SliceLValue, ''ParenLValue]




data PyWith e l where
  PyWith :: e [PyWithBinderL] -> e [Py.StatementL] -> PyWith e Py.StatementL

data PyWithBinderL
data PyWithBinder e l where
  PyWithBinder :: e Py.ExprL -> e (Maybe PyLValueL) -> PyWithBinder e PyWithBinderL

-- Extracting out this sort for docstring support

data PyStringLitL
data PyStringLit :: Node where
  PyStrings :: [String] -> PyStringLit e PyStringLitL
  PyUnicodeStrings :: [String] -> PyStringLit e PyStringLitL
  PyByteStrings :: [String] -> PyStringLit e PyStringLitL

-- Supports docstrings
-- Note that only functions and modules have scopes in Python. Those are
-- also the only places that support docstrings
data PyBlockL
data PyBlock e l where
  PyBlock :: e (Maybe PyStringLitL) -> e BlockL -> PyBlock e PyBlockL

data PyClassL -- Creating a new sort, so can tell the CFG it's a suspended computation
data PyClass e l where
  PyClass :: e P.IdentL -> e [ArgumentL] -> e PyBlockL -> PyClass e PyClassL

-- Chained comparison
data PyCompL
data PyComp e l where
  PyBaseComp  :: e Py.OpL -> e Py.ExprL -> e Py.ExprL -> PyComp e PyCompL
  PyChainComp :: e Py.OpL -> e Py.ExprL -> e PyCompL -> PyComp e PyCompL


-- We want the invariant that expressions something on the left of an expression may not
-- depend on something to the right. The "x if y else z" notation violates this; we're reordering.
--
--
-- TODO: Python assignments violate this. In fact, they don't meet the spec of generic assignments
data PyCondExpr e l where
  PyCondExpr :: e Py.ExprL -- condition
             -> e Py.ExprL -- then
             -> e Py.ExprL -- else
             -> PyCondExpr e Py.ExprL

data PyComprehensionExpr e l where
  PyListComprehension :: e PyComprehensionL -> PyComprehensionExpr e Py.ExprL
  PyDictComprehension :: e PyComprehensionL -> PyComprehensionExpr e Py.ExprL
  PySetComprehension :: e PyComprehensionL -> PyComprehensionExpr e Py.ExprL

data PyComprehensionL

data PyComprehension e l where
  PyComprehensionFor :: Bool -> e [Py.ExprL] -> e Py.ExprL -> e PyComprehensionL -> PyComprehension e PyComprehensionL
  PyComprehensionIf  :: e Py.ExprL -> e PyComprehensionL -> PyComprehension e PyComprehensionL
  PyComprehensionBody :: e Py.ComprehensionExprL -> PyComprehension e PyComprehensionL

deriveAll [''PyWith, ''PyWithBinder, ''PyStringLit, ''PyBlock, ''PyClass, ''PyComp, ''PyCondExpr, ''PyComprehensionExpr, ''PyComprehension]


createSortInclusionTypes [ ''P.IdentL,  ''P.AssignL,     ''Py.ExprL, ''Py.StatementL, ''PyCompL,  ''P.IdentL,  ''PyClassL
                         ] [
                           ''Py.IdentL, ''Py.StatementL, ''P.RhsL,   ''P.BlockItemL,  ''Py.ExprL, ''PyLValueL, ''Py.StatementL
                         ]
deriveAllButSortInjection [''IdentIsIdent, ''AssignIsStatement, ''ExprIsRhs, ''StatementIsBlockItem, ''PyCompIsExpr, ''IdentIsPyLValue, ''PyClassIsStatement]
createSortInclusionInfers [ ''P.IdentL,  ''P.AssignL,     ''Py.ExprL, ''Py.StatementL, ''PyCompL,  ''P.IdentL,  ''PyClassL
                          ] [
                            ''Py.IdentL, ''Py.StatementL, ''P.RhsL,   ''P.BlockItemL,  ''Py.ExprL, ''PyLValueL, ''Py.StatementL
                          ]

-----------------------------------------------------------------------------------
---------------       Function calls, decls, defns         ------------------------
-----------------------------------------------------------------------------------


data PythonArg e l where
  PythonArgSplat   :: e Py.ExprL               -> PythonArg e FunctionArgumentL
  PythonArgKeyword :: e P.IdentL -> e Py.ExprL -> PythonArg e FunctionArgumentL
  PythonArgKWSplat :: e Py.ExprL               -> PythonArg e FunctionArgumentL

deriveAll [''PythonArg]

data PyFunDefAttrs e l where
  PyFunDefAttrs :: e (Maybe Py.ExprL) -> PyFunDefAttrs e FunctionDefAttrsL

-- Annotation, default
data PyParamAttrs e l where
  PyParamAttrs :: e (Maybe Py.ExprL) -> e (Maybe Py.ExprL) -> PyParamAttrs e ParameterAttrsL


data PythonParam e l where
  PythonParamVarArgs     :: e P.IdentL       -> e (Maybe Py.ExprL) -> PythonParam e FunctionParameterL
  PythonParamVarArgsKW   :: e P.IdentL       -> e (Maybe Py.ExprL) -> PythonParam e FunctionParameterL
  PythonParamUnpackTuple :: e Py.ParamTupleL -> e (Maybe Py.ExprL) -> PythonParam e FunctionParameterL
  PythonEndPosParam      ::                                           PythonParam e FunctionParameterL

deriveAll [''PyFunDefAttrs, ''PyParamAttrs, ''PythonParam]

-- | FIXME: Python lets you call class/static methods through an instance.
-- I should probably model this similarly to how I do so for Java, or create a more interesting representation
-- that separates syntactic function definitions from their semantic values. Alternatively, since there's a deadline
-- and I only need a prototype, I can also just ignore this issue. I choose the latter.

createSortInclusionTypes [ ''P.FunctionCallL, ''Py.ExprL,       ''Py.ExprL,            ''Py.ExprL,    ''P.FunctionDefL, ''PyBlockL
                         ] [
                           ''Py.ExprL,        ''P.FunctionExpL, ''P.PositionalArgExpL, ''P.ReceiverL, ''Py.StatementL,  ''P.FunctionBodyL
                         ]
deriveAllButSortInjection [ ''FunctionCallIsExpr, ''ExprIsFunctionExp, ''ExprIsPositionalArgExp, ''ExprIsReceiver
          , ''FunctionDefIsStatement, ''PyBlockIsFunctionBody]
createSortInclusionInfers [ ''P.FunctionCallL, ''Py.ExprL,       ''Py.ExprL,            ''Py.ExprL,    ''P.FunctionDefL, ''PyBlockL
                          ] [
                            ''Py.ExprL,        ''P.FunctionExpL, ''P.PositionalArgExpL, ''P.ReceiverL, ''Py.StatementL,  ''P.FunctionBodyL
                          ]

-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------


do let pythonSortInjections = [ ''IdentIsIdent, ''AssignIsStatement, ''ExprIsRhs, ''StatementIsBlockItem, ''PyCompIsExpr
                              , ''IdentIsPyLValue, ''PyClassIsStatement
                              , ''FunctionCallIsExpr, ''ExprIsFunctionExp, ''ExprIsPositionalArgExp, ''ExprIsReceiver
                              , ''FunctionDefIsStatement, ''PyBlockIsFunctionBody]
   let names = (pythonSigNames \\ [mkName "Ident"])
                    ++ pythonSortInjections
                    ++ [ ''PyLhs, ''TupleLValue, ''ListLValue, ''DotLValue, ''StarLValue, ''SubscriptLValue, ''SliceLValue, ''ParenLValue]
                    ++ [ ''PyWith, ''PyWithBinder
                       , ''PythonArg, ''PyFunDefAttrs, ''PyParamAttrs, ''PythonParam
                       , ''PyStringLit, ''PyBlock, ''PyClass, ''PyComp, ''PyCondExpr
                       , ''PyComprehensionExpr, ''PyComprehension]
                    ++ [ ''P.Ident, ''AssignOpEquals, ''Assign, ''P.Block, ''EmptyBlockEnd
                       , ''P.FunctionCall, ''P.EmptyFunctionCallAttrs, ''FunctionIdent, ''FunctionArgumentList, ''PositionalArgument, ''ReceiverArg
                       , ''FunctionDef, ''PositionalParameter]

   sumDec <- runCompTrans $ makeSumType "MPythonSig" names
   return sumDec

type instance InjectableSorts MPythonSig AssignL = '[StatementL]

type MPythonTerm    = Term MPythonSig
type MPythonTermLab = TermLab MPythonSig

type MPythonTermAnn a = AnnTerm a MPythonSig

type MPythonCxt h a = CxtS h MPythonSig a

-----------------------------------------------------------------------------------
----------------------         Sort injections             ------------------------
-----------------------------------------------------------------------------------

instance InjF MPythonSig [PyLValueL] LhsL where
  injF = iPyLhs
  projF' x = do
    PyLhs ls <- project' x
    return ls

instance InjF MPythonSig P.IdentL Py.ExprL where
  injF x = Py.iVar (injF x)
  projF' x
    | Just (Py.Var v) <- project' x
    , Just (IdentIsIdent i) <- project' v
    = return i
  projF' _ = Nothing

instance InjF MPythonSig P.IdentL LhsL where
  injF n = iPyLhs $ insertF [injF n]
  projF' t
    | Just (PyLhs es) <- project' t
    , Just (ConsF e rest) <- project' es
    , Just NilF <- project' rest
    = projF' e
  projF' _ = Nothing

instance InjF MPythonSig AssignL BlockItemL where
  injF = iAssignIsStatement
  projF' t
    | Just (StatementIsBlockItem s) <- project' t
    , Just (AssignIsStatement a) <- project' s
    = return a
  projF' _ = Nothing

instance InjF MPythonSig P.IdentL FunctionExpL where
  injF x = iVar (injF x)

  projF' (project' -> Just (FunctionIdent n)) = Just n
  projF' f
    | Just (ExprIsFunctionExp e) <- project' f
    , Just (Var v) <- project' e = projF' v
  projF' _                                    = Nothing

instance InjF MPythonSig P.IdentL PositionalArgExpL where
  injF = iExprIsPositionalArgExp . injF
  projF' t
   | Just (ExprIsPositionalArgExp e) <- project' t = projF' e
  projF' _ = Nothing

instance InjF MPythonSig FunctionCallL RhsL where
  injF = iFunctionCallIsExpr
  projF' f
   | Just (ExprIsRhs e) <- project' f
   , Just (FunctionCallIsExpr d) <- project' e
   = Just d
  projF' _ = Nothing

#endif
