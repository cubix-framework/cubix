{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cubix.Language.Lua.Parametric.Common.Types where

import Data.List ( (\\) )

import Language.Haskell.TH.Syntax ( mkName )

import Data.Comp.Multi ( Node, Term, project', AnnTerm, (:-<:), All, HFunctor, project, CxtS, AnnCxtS )
import Data.Comp.Trans ( makeSumType, runCompTrans )

import Cubix.Language.Info
import Cubix.Language.Lua.Parametric.Full.Types as L
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax hiding (ExpL)
import Cubix.Language.Parametric.Syntax qualified as P

-----------------------------------------------------------------------------------
---------------       Variable declarations and blocks     ------------------------
-----------------------------------------------------------------------------------

data LuaLocalVarInit e l where
  LuaLocalVarInit :: e [ExpL] -> LuaLocalVarInit e LocalVarInitL

data LuaLhs e l where
  LuaLhs :: e [L.VarL] -> LuaLhs e LhsL

data LuaRhs e l where
  LuaRhs :: e [ExpL] -> LuaRhs e RhsL

data LuaBlockEnd e l where
  LuaBlockEnd :: e (Maybe [ExpL]) -> LuaBlockEnd e BlockEndL

data LuaSpecialFunArg e l where
  LuaTableArg             ::                   e L.TableL -> LuaSpecialFunArg e FunctionArgumentsL
  LuaStringArg            ::                   String     -> LuaSpecialFunArg e FunctionArgumentsL
  LuaReceiverAndTableArg  :: e L.PrefixExpL -> e L.TableL -> LuaSpecialFunArg e FunctionArgumentsL
  LuaReceiverAndStringArg :: e L.PrefixExpL -> String     -> LuaSpecialFunArg e FunctionArgumentsL

deriveAll [''LuaLocalVarInit, ''LuaLhs, ''LuaRhs, ''LuaBlockEnd, ''LuaSpecialFunArg]

createSortInclusionTypes
  [''P.IdentL, ''P.AssignL, ''P.BlockL, ''L.StatL,      ''P.SingleLocalVarDeclL, ''P.FunctionCallL, ''L.ExpL,              ''L.PrefixExpL,   ''L.PrefixExpL]
  [''L.NameL,  ''L.StatL, ''L.BlockL,   ''P.BlockItemL, ''L.StatL,               ''L.FunCallL,      ''P.PositionalArgExpL, ''P.FunctionExpL, ''ReceiverL]
deriveAll [''IdentIsName, ''AssignIsStat, ''BlockIsBlock, ''StatIsBlockItem, ''SingleLocalVarDeclIsStat,
           ''FunctionCallIsFunCall, ''ExpIsPositionalArgExp, ''PrefixExpIsFunctionExp, ''PrefixExpIsReceiver]
createSortInclusionInfers [ ''P.IdentL, ''P.AssignL, ''P.BlockL, ''L.StatL,      ''P.SingleLocalVarDeclL, ''P.FunctionCallL, ''L.ExpL,              ''L.PrefixExpL,   ''L.PrefixExpL
                          ] [
                            ''L.NameL,  ''L.StatL, ''L.BlockL,   ''P.BlockItemL, ''L.StatL,               ''L.FunCallL,      ''P.PositionalArgExpL, ''P.FunctionExpL, ''ReceiverL
                          ]


-----------------------------------------------------------------------------------
---------------       Function calls, decls, defns         ------------------------
-----------------------------------------------------------------------------------

data LuaFunctionDefinedObjL
data LuaFunctionDefinedObj e l where
  LuaFunctionDefinedObj :: e [P.IdentL] -> LuaFunctionDefinedObj e LuaFunctionDefinedObjL

deriveAll [''LuaFunctionDefinedObj]

-- Use this for "fun"; use EmptyFunctionAttrs for "local fun"
data LuaFunctionAttrs e l where
  LuaFunctionAttrs :: e LuaFunctionDefinedObjL -> LuaFunctionAttrs e FunctionDefAttrsL

deriveAll [''LuaFunctionAttrs]

data LuaVarArgsParam :: Node where
  LuaVarArgsParam :: LuaVarArgsParam e FunctionParameterL

deriveAll [''LuaVarArgsParam]

-- When I did the deriveAll's on one line, I got a mysterious GHC crash


createSortInclusionTypes
  [''P.FunctionDefL, ''P.BlockL]
  [''L.StatL,        ''P.FunctionBodyL]
deriveAll [''FunctionDefIsStat, ''BlockIsFunctionBody]
createSortInclusionInfers
  [''P.FunctionDefL, ''P.BlockL]
  [''L.StatL,        ''P.FunctionBodyL]

-----------------------------------------------------------------------------------
---------------               Expressions                  ------------------------
-----------------------------------------------------------------------------------

createSortInclusionTypes
  [''P.ExpressionL, ''ExpL]
  [''ExpL,          ''P.ExpressionL]
deriveAll [''ExpressionIsExp, ''ExpIsExpression]
createSortInclusionInfers
  [''P.ExpressionL, ''ExpL]
  [''ExpL,          ''P.ExpressionL]

-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------

do let newLuaNodes =
         [ ''LuaLocalVarInit, ''LuaLhs, ''LuaRhs, ''LuaBlockEnd
         , ''LuaSpecialFunArg, ''LuaFunctionDefinedObj
         , ''LuaFunctionAttrs, ''LuaVarArgsParam
         ]
       luaSortInjections =
         [ ''IdentIsName, ''AssignIsStat
         , ''BlockIsBlock, ''StatIsBlockItem
         , ''SingleLocalVarDeclIsStat, ''FunctionCallIsFunCall
         , ''ExpIsPositionalArgExp, ''PrefixExpIsFunctionExp
         , ''PrefixExpIsReceiver, ''FunctionDefIsStat
         , ''BlockIsFunctionBody
         , ''ExpressionIsExp, ''ExpIsExpression
         ]
   let names =
         (luaSigNames \\ [mkName "Name", mkName "Block", mkName "FunArg", mkName "FunCall"]) ++
         newLuaNodes ++
         luaSortInjections ++
         [ ''OptLocalVarInit, ''SingleLocalVarDecl, ''P.Ident
         , ''AssignOpEquals, ''Assign, ''P.Block, ''P.TupleBinder
         , ''EmptyLocalVarDeclAttrs
         , ''FunctionCall, ''EmptyFunctionCallAttrs
         , ''FunctionArgumentList, ''PositionalArgument
         , ''ReceiverArg, ''FunctionIdent
         , ''FunctionDef, ''EmptyFunctionDefAttrs, ''SelfParameter
         , ''PositionalParameter, ''EmptyParameterAttrs
         , ''UnaryMinusOp, ''ComplementOp, ''LogicalNegationOp
         , ''ArithBinOp, ''IDivOp, ''ExpOp
         , ''BitwiseBinOp, ''LogicalBinOp, ''LogicalShrOp, ''ShlOp
         , ''RelationalBinOp
         , ''Operator
         ]
   runCompTrans $ makeSumType "MLuaSig" names

type instance InjectableSorts MLuaSig AssignL = '[StatL]
type instance InjectableSorts MLuaSig SingleLocalVarDeclL = '[StatL]

type MLuaTerm = Term MLuaSig
type MLuaTermLab = TermLab MLuaSig

type MLuaCxt h a = CxtS h MLuaSig a
type MLuaCxtA h a p = AnnCxtS p h MLuaSig a

type MLuaTermOptAnn a = AnnTerm (Maybe a) MLuaSig

-----------------------------------------------------------------------------------
----------------------         Sort injections             ------------------------
-----------------------------------------------------------------------------------

instance InjF MLuaSig [ExpL] LocalVarInitL where
  injF = iLuaLocalVarInit

  projF' (project' -> (Just (LuaLocalVarInit x))) = Just x
  projF' _ = Nothing

instance InjF MLuaSig [L.VarL] LhsL where
  injF = iLuaLhs

  projF' (project' -> (Just (LuaLhs x))) = Just x
  projF' _ = Nothing

instance InjF MLuaSig [ExpL] RhsL where
  injF = iLuaRhs

  projF' (project' -> (Just (LuaRhs x))) = Just x
  projF' _ = Nothing

instance InjF MLuaSig (Maybe [ExpL]) BlockEndL where
  injF = iLuaBlockEnd

  projF' (project' -> (Just (LuaBlockEnd x))) = Just x
  projF' _ = Nothing

instance InjF MLuaSig AssignL BlockItemL where
  injF = iAssignIsStat

  projF' t = do
    (a :: MLuaCxtA h a p StatL) <- projF' t
    projF' a


instance InjF MLuaSig SingleLocalVarDeclL BlockItemL where
  injF = iSingleLocalVarDeclIsStat
  projF' t = do
    (a :: MLuaCxtA h a p StatL) <- projF' t
    projF' a

instance InjF MLuaSig IdentL ExpL where
  injF = iPrefixExp . iPEVar . iVarName . injF
  projF' t = do
    PrefixExp a <- project' t
    PEVar b <- project' a
    VarName c <- project' b
    projF' c

instance InjF MLuaSig ExpL LocalVarInitL where
  injF x = injF $ insertF [x]
  projF' t = do
    LuaLocalVarInit x <- project' t
    ConsF e rest <- project' x
    NilF <- project' rest
    return e

instance {-# OVERLAPPING #-} InjF MLuaSig IdentL VarDeclBinderL where
  injF x = iTupleBinder $ insertF [x]

  projF' t = do
    TupleBinder ns <- project' t
    ConsF n rest <- project' ns
    NilF <- project' rest
    return n

instance InjF MLuaSig IdentL LhsL where
  injF x = iLuaLhs $ ConsF' (iVarName $ injF x) riNilF
  projF' t = do
    LuaLhs vs <- project' t
    ConsF v rest <- project' vs
    NilF <- project' rest
    VarName n <- project' v
    projF' n

instance InjF MLuaSig ExpL RhsL where
  injF x = iLuaRhs $ ConsF' x riNilF
  projF' t = do
    LuaRhs es <- project' t
    ConsF e rest <- project' es
    NilF <- project' rest
    return e

instance InjF MLuaSig IdentL PositionalArgExpL where
  injF = iPrefixExp . iPEVar . iVarName . injF

  projF' x
   | Just (ExpIsPositionalArgExp e) <- project' x
   , Just (PrefixExp p) <- project' e
   , Just (PEVar v) <- project' p
   , Just (VarName n) <- project' v
   = projF' n
  projF' _ = Nothing

instance InjF MLuaSig IdentL FunctionExpL where
  injF = iFunctionIdent

  projF' (project' -> Just (FunctionIdent n)) = Just n
  projF' x
    | Just (PrefixExpIsFunctionExp p) <- project' x
    , Just (PEVar v) <- project' p
    , Just (VarName n) <- project' v
    = projF' n
  projF' _                                    = Nothing

instance InjF MLuaSig FunctionCallL RhsL where
  injF x = iLuaRhs $ insertF [iPrefixExp $ iPEFunCall $ iFunctionCallIsFunCall x]
  projF' f
   | Just (LuaRhs (SingletonFA' e)) <- project' f
   , Just (PrefixExp c) <- project' e
   , Just (PEFunCall b) <- project' c
   , Just (FunctionCallIsFunCall a) <- project' b
   = Just a
  projF' _ = Nothing
