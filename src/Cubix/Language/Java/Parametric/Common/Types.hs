{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |

module Cubix.Language.Java.Parametric.Common.Types where

#ifndef ONLY_ONE_LANGUAGE
import Data.List ( (\\) )

import Language.Haskell.TH.Syntax ( mkName )

import Data.Comp.Multi ( Node, Term, project', project, HFunctor, (:-<:), All, CxtS, AnnCxtS )
import Data.Comp.Trans ( makeSumType, runCompTrans )

import Cubix.Language.Info
import Cubix.Language.Java.Parametric.Full.Names
import Cubix.Language.Java.Parametric.Full.Types as J
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax as P

--------------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------
---------------       Variable declarations and blocks     ------------------------
-----------------------------------------------------------------------------------

data ArrayDimVarDeclAttrs :: Node where
  ArrayDimVarDeclAttrs :: Int -> ArrayDimVarDeclAttrs e LocalVarDeclAttrsL


data ModifiersTypeIsMultiLocalVarDeclCommonAttrs e l where
  ModifiersTypeIsMultiLocalVarDeclCommonAttrs :: e ([ModifierL], TypeL) -> ModifiersTypeIsMultiLocalVarDeclCommonAttrs e MultiLocalVarDeclCommonAttrsL

deriveAll [''ArrayDimVarDeclAttrs, ''ModifiersTypeIsMultiLocalVarDeclCommonAttrs]

createSortInclusionTypes [ ''VarInitL,       ''MultiLocalVarDeclL, ''P.IdentL, ''P.BlockL, ''AssignL, ''ExpL, ''J.LhsL, ''BlockStmtL, ''J.AssignOpL
                         ] [
                           ''LocalVarInitL,  ''BlockStmtL,         ''J.IdentL, ''J.BlockL, ''ExpL,    ''RhsL, ''P.LhsL, ''BlockItemL, ''P.AssignOpL
                         ]
deriveAll [''VarInitIsLocalVarInit, ''MultiLocalVarDeclIsBlockStmt, ''IdentIsIdent, ''BlockIsBlock, ''AssignIsExp, ''ExpIsRhs,
           ''LhsIsLhs, ''BlockStmtIsBlockItem, ''AssignOpIsAssignOp]
createSortInclusionInfers [ ''VarInitL,       ''MultiLocalVarDeclL, ''P.IdentL, ''P.BlockL, ''AssignL, ''ExpL, ''J.LhsL, ''BlockStmtL, ''J.AssignOpL
                          ] [
                            ''LocalVarInitL,  ''BlockStmtL,         ''J.IdentL, ''J.BlockL, ''ExpL,    ''RhsL, ''P.LhsL, ''BlockItemL, ''P.AssignOpL
                          ]

-----------------------------------------------------------------------------------
---------------       Function calls, decls, defns         ------------------------
-----------------------------------------------------------------------------------

-- | NOTE: You can call a static method on an instance; then a primary receiver is substituting for
-- a type receiver
data JavaReceiver e l where
  PrimaryReceiver    :: e J.ExpL  -> JavaReceiver e P.ReceiverL
  SuperReceiver      ::              JavaReceiver e P.ReceiverL
  ClassSuperReceiver :: e J.NameL -> JavaReceiver e P.ReceiverL
  TypeReceiver       :: e J.NameL -> JavaReceiver e P.ReceiverL

data JavaTypeArgs e l where
  JavaTypeArgs :: e [J.RefTypeL] -> JavaTypeArgs e P.FunctionCallAttrsL

-- "void" is represented by Nothing
data JavaMethodDeclAttrsL
data JavaMethodDeclAttrs e l where
  JavaMethodDeclAttrs :: e [J.ModifierL] -> e [J.TypeParamL] -> e (Maybe J.TypeL) -> e [J.RefTypeL] -> JavaMethodDeclAttrs e JavaMethodDeclAttrsL

data JavaParamAttrsL
data JavaParamAttrs e l where
  JavaParamAttrs :: e [J.ModifierL] -> e J.TypeL -> Int -> JavaParamAttrs e JavaParamAttrsL

data JavaVarargsParamL
data JavaVarargsParam e l where
  JavaVarargsParam :: e JavaParamAttrsL -> e P.IdentL -> JavaVarargsParam e JavaVarargsParamL


deriveAll [''JavaReceiver, ''JavaTypeArgs, ''JavaMethodDeclAttrs, ''JavaParamAttrs, ''JavaVarargsParam]

pattern JavaMethodDeclAttrs' ::
  ( JavaMethodDeclAttrs :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a [J.ModifierL]
  -> CxtS h fs a [J.TypeParamL]
  -> CxtS h fs a (Maybe J.TypeL)
  -> CxtS h fs a [J.RefTypeL]
  -> CxtS h fs a JavaMethodDeclAttrsL
pattern JavaMethodDeclAttrs' mods tparams retTp exns <- (project -> Just (JavaMethodDeclAttrs mods tparams retTp exns)) where
  JavaMethodDeclAttrs' mods tparams retTp exns = iJavaMethodDeclAttrs mods tparams retTp exns

pattern JavaParamAttrs' ::
  ( JavaParamAttrs :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a [J.ModifierL]
  -> CxtS h fs a J.TypeL
  -> Int
  -> CxtS h fs a JavaParamAttrsL
pattern JavaParamAttrs' mods tp dim <- (project -> Just (JavaParamAttrs mods tp dim)) where
  JavaParamAttrs' mods tp dim = iJavaParamAttrs mods tp dim

pattern JavaVarargsParam' ::
  ( JavaVarargsParam :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a JavaParamAttrsL
  -> CxtS h fs a P.IdentL
  -> CxtS h fs a JavaVarargsParamL
pattern JavaVarargsParam' attrs n <- (project -> Just (JavaVarargsParam attrs n)) where
  JavaVarargsParam' attrs n = iJavaVarargsParam attrs n

createSortInclusionTypes [ ''P.FunctionCallL,     ''J.NameL,        ''J.ExpL,              ''P.FunctionDeclL, ''JavaMethodDeclAttrsL, ''JavaParamAttrsL,               ''JavaVarargsParamL
                         ] [
                           ''J.MethodInvocationL, ''P.FunctionExpL, ''P.PositionalArgExpL, ''J.MemberDeclL,   ''P.FunctionDeclAttrsL, ''P.FunctionParameterDeclAttrsL, ''FunctionParameterDeclL
                         ]
deriveAll [ ''FunctionCallIsMethodInvocation, ''NameIsFunctionExp, ''ExpIsPositionalArgExp, ''FunctionDeclIsMemberDecl
          , ''JavaMethodDeclAttrsIsFunctionDeclAttrs, ''JavaParamAttrsIsFunctionParameterDeclAttrs, ''JavaVarargsParamIsFunctionParameterDecl]
createSortInclusionInfers [ ''P.FunctionCallL,     ''J.NameL,        ''J.ExpL,              ''P.FunctionDeclL, ''JavaMethodDeclAttrsL, ''JavaParamAttrsL,               ''JavaVarargsParamL
                          ] [
                            ''J.MethodInvocationL, ''P.FunctionExpL, ''P.PositionalArgExpL, ''J.MemberDeclL,   ''P.FunctionDeclAttrsL, ''P.FunctionParameterDeclAttrsL, ''FunctionParameterDeclL
                          ]

createSortInclusionTypes [ ''P.FunctionDefL, ''JavaMethodDeclAttrsL, ''JavaParamAttrsL,   ''JavaVarargsParamL,    ''P.BlockL
                         ] [
                           ''J.MemberDeclL,  ''P.FunctionDefAttrsL,  ''P.ParameterAttrsL, ''P.FunctionParameterL, ''P.FunctionBodyL
                         ]
deriveAll [ ''FunctionDefIsMemberDecl, ''JavaMethodDeclAttrsIsFunctionDefAttrs, ''JavaParamAttrsIsParameterAttrs, ''JavaVarargsParamIsFunctionParameter, ''BlockIsFunctionBody]
createSortInclusionInfers [ ''P.FunctionDefL, ''JavaMethodDeclAttrsL, ''JavaParamAttrsL,   ''JavaVarargsParamL,    ''P.BlockL
                          ] [
                            ''J.MemberDeclL,  ''P.FunctionDefAttrsL,  ''P.ParameterAttrsL, ''P.FunctionParameterL, ''P.FunctionBodyL
                          ]

-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------


do let newJavaNodes       = [''ArrayDimVarDeclAttrs, ''JavaReceiver, ''JavaTypeArgs, ''JavaMethodDeclAttrs, ''JavaParamAttrs, ''JavaVarargsParam]
   let javaSortInjections = [''VarInitIsLocalVarInit, ''ModifiersTypeIsMultiLocalVarDeclCommonAttrs, ''IdentIsIdent, ''MultiLocalVarDeclIsBlockStmt,
                             ''BlockIsBlock, ''AssignIsExp, ''ExpIsRhs, ''LhsIsLhs, ''BlockStmtIsBlockItem, ''AssignOpIsAssignOp, ''IdentIsVarDeclBinder,
                             ''FunctionCallIsMethodInvocation, ''NameIsFunctionExp, ''ExpIsPositionalArgExp, ''FunctionDeclIsMemberDecl,
                             ''JavaMethodDeclAttrsIsFunctionDeclAttrs, ''JavaParamAttrsIsFunctionParameterDeclAttrs, ''JavaVarargsParamIsFunctionParameterDecl,
                             ''FunctionDefIsMemberDecl, ''JavaMethodDeclAttrsIsFunctionDefAttrs, ''JavaParamAttrsIsParameterAttrs, ''JavaVarargsParamIsFunctionParameter, ''BlockIsFunctionBody]
   let names = (javaSigNames \\ [mkName "Ident", mkName "Block", mkName "MethodInvocation", mkName "MethodBody"])
                             ++ javaSortInjections
                             ++ newJavaNodes
                             ++ [ ''OptLocalVarInit, ''SingleLocalVarDecl, ''MultiLocalVarDecl, ''P.Ident
                                , ''AssignOpEquals, ''Assign, ''P.Block, ''EmptyBlockEnd
                                , ''FunctionCall, ''FunctionIdent, ''FunctionArgumentList, ''ReceiverArg, ''PositionalArgument
                                , ''FunctionDecl, ''SelfParameterDecl, ''PositionalParameterDeclWithIdent
                                , ''FunctionDef, ''SelfParameter, ''PositionalParameter
                                ]
   sumDec <- runCompTrans $ makeSumType "MJavaSig" names
   return sumDec

type instance InjectableSorts MJavaSig MultiLocalVarDeclL = '[]

type MJavaTerm = Term MJavaSig
type MJavaTermLab = TermLab MJavaSig

type MJavaCxt h a = CxtS h MJavaSig a
type MJavaCxtA h a p = AnnCxtS p h MJavaSig a



-----------------------------------------------------------------------------------
----------------------         Sort injections             ------------------------
-----------------------------------------------------------------------------------

instance InjF MJavaSig ([ModifierL], TypeL) MultiLocalVarDeclCommonAttrsL where
  injF = iModifiersTypeIsMultiLocalVarDeclCommonAttrs

  projF' (project' -> Just (ModifiersTypeIsMultiLocalVarDeclCommonAttrs x)) = Just x
  projF' _ = Nothing

instance InjF MJavaSig AssignL BlockItemL where
  injF = iBlockStmt . iExpStmt . iAssignIsExp

  projF' t = do
    (a :: MJavaCxtA _ _ _ BlockStmtL) <- projF' t
    BlockStmt s <- project' a
    ExpStmt e <- project' s
    projF'  e


instance InjF MJavaSig MultiLocalVarDeclL BlockItemL where
  injF = iMultiLocalVarDeclIsBlockStmt
  projF' t = do
    (a :: MJavaCxtA _ _ _ BlockStmtL) <- projF' t
    projF' a

-- Generalizing to contexts takes more work
instance InjF MJavaSig P.IdentL P.LhsL where
  injF n = iNameLhs $ iName $ insertF [iIdentIsIdent n]
  projF' n = do
    LhsIsLhs l <- project' n
    NameLhs i <- project' l
    Name is <- project' i
    let (SingletonFA' r) = is
    projF' r

-- Generalizing to contexts takes more work
instance InjF MJavaSig P.IdentL ExpL where
  injF x = iExpName $ iName $ insertF [injF x]
  projF' e = do
    ExpName n <- project' e
    Name is <- project' n
    let (SingletonFA' i) = is
    projF' i

instance InjF MJavaSig ExpL LocalVarInitL where
  injF = iInitExp
  projF' i = do
    VarInitIsLocalVarInit v <- project' i
    InitExp e <- project' v
    return e

instance InjF MJavaSig StmtL BlockItemL where
  injF = iBlockStmt
  projF' b = do
    BlockStmtIsBlockItem c <- project' b
    BlockStmt s <- project' c
    return s

instance InjF MJavaSig P.BlockL StmtL where
  injF = iStmtBlock . iBlockIsBlock
  projF' b = do
    StmtBlock s <- project' b
    BlockIsBlock b <- project' s
    return b

instance InjF MJavaSig ExpL P.FunctionArgumentL where
  injF = iPositionalArgument . injF
  projF' a = do
    PositionalArgument p <- project' a
    projF' p

instance InjF MJavaSig P.IdentL FunctionExpL where
  injF = iFunctionIdent

  projF' (project' -> Just (FunctionIdent n)) = Just n
  projF' _                                    = Nothing


instance InjF MJavaSig P.IdentL PositionalArgExpL where
  injF x = iExpName $ iName $ insertF [injF x]
  projF' a
   | Just (ExpIsPositionalArgExp e) <- project' a
   , Just (ExpName n) <- project' e
   , Just (Name (SingletonFA' i)) <- project' n = projF' i
  projF' _ = Nothing

instance InjF MJavaSig FunctionCallL RhsL where
  injF = iMethodInv . injF
  projF' f
   | Just (ExpIsRhs e) <- project' f
   , Just (MethodInv c) <- project' e
   , Just (FunctionCallIsMethodInvocation b) <- project' c
   = Just b
  projF' _ = Nothing


#endif
