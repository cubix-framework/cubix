{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | 

module Cubix.Language.C.Parametric.Common.Types where

#ifndef ONLY_ONE_LANGUAGE
import Data.List ( (\\) )

import Language.Haskell.TH ( mkName )

import Data.Comp.Multi ( Node, HFunctor, Term, project', project, (:-<:), CxtS, All, AnnCxtS )
import Data.Comp.Trans ( runCompTrans, makeSumType )

import Cubix.Language.C.Parametric.Full.Names
import Cubix.Language.C.Parametric.Full.Types as C
import Cubix.Language.Info
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax as P

--------------------------------------------------------------------------------


-----------------------------------------------------------------------------------
---------------       Variable declarations and blocks     ------------------------
-----------------------------------------------------------------------------------

data CDeclarationSpecifiersIsMultiLocalVarDeclCommonAttrs e l where
  CDeclarationSpecifiersIsMultiLocalVarDeclCommonAttrs :: e [CDeclarationSpecifierL] -> CDeclarationSpecifiersIsMultiLocalVarDeclCommonAttrs e MultiLocalVarDeclCommonAttrsL 

data CLocalVarAttrs e l where
  CLocalVarAttrs :: e [CDerivedDeclaratorL] -> e (Maybe CStringLiteralL) -> e [CAttributeL] -> CLocalVarAttrs e LocalVarDeclAttrsL

deriveAll [''CDeclarationSpecifiersIsMultiLocalVarDeclCommonAttrs, ''CLocalVarAttrs]

instance ( CDeclarationSpecifiersIsMultiLocalVarDeclCommonAttrs :-<: fs
         , All HFunctor fs
         ) => InjF fs [CDeclarationSpecifierL] MultiLocalVarDeclCommonAttrsL where
  injF = iCDeclarationSpecifiersIsMultiLocalVarDeclCommonAttrs

  projF' (project' -> Just (CDeclarationSpecifiersIsMultiLocalVarDeclCommonAttrs x)) = Just x
  projF' _                                                                          = Nothing

createSortInclusionTypes [  ''MultiLocalVarDeclL,  ''CInitializerL, ''P.IdentL, ''C.CExpressionL, ''C.CExpressionL, ''C.CAssignOpL, ''P.AssignL,      ''C.CCompoundBlockItemL
                         ] [''CCompoundBlockItemL, ''LocalVarInitL, ''C.IdentL, ''P.LhsL,         ''P.RhsL,         ''P.AssignOpL,  ''C.CExpressionL, ''P.BlockItemL
                         ]
deriveAll [''MultiLocalVarDeclIsCCompoundBlockItem, ''CInitializerIsLocalVarInit, ''IdentIsIdent, ''CExpressionIsLhs,
           ''CExpressionIsRhs, ''CAssignOpIsAssignOp, ''AssignIsCExpression, ''CCompoundBlockItemIsBlockItem]
createSortInclusionInfers [  ''MultiLocalVarDeclL,  ''CInitializerL, ''P.IdentL, ''C.CExpressionL, ''C.CExpressionL, ''C.CAssignOpL, ''P.AssignL,         ''C.CCompoundBlockItemL
                          ] [''CCompoundBlockItemL, ''LocalVarInitL, ''C.IdentL, ''P.LhsL,         ''P.RhsL,         ''P.AssignOpL,  ''C.CExpressionL,    ''P.BlockItemL
                          ]

data CLabeledBlock e l where
  CLabeledBlock :: e [P.IdentL] -> e BlockL -> CLabeledBlock e C.CStatementL

deriveAll [''CLabeledBlock]


-----------------------------------------------------------------------------------
---------------       Function calls, decls, defns         ------------------------
-----------------------------------------------------------------------------------


-- | Modeling note: The language-c AST crams a lot of different kind of declarations together into a single
--   node with too many options. We're breaking it apart so we can e.g.: assume variables have identifiers when
--   it's possible to make that assumption
--  On the other hand, I've learned that "int;" is a valid line of C (at least according to Clang).
--  However, language-c can't parse it.

-- It took me hours to go through language-c's representation of functions/fundecls and understand everything.
-- I now see that much of the complexity is from the spec and not from language-c, though language-c could
-- factor things to make it more clear (and use a tree structure instead of a list structure for the derived declarators, perhaps).
-- Oh, and also it would be nice if it used distinct sorts for parameter declarations, general declarations, and struct declarations;
-- would really clean a lot up.
--
-- Below is the relevant pieces of abstract syntax and my notes on them

{-
data CFunctionDef a
  = CFunDef
    [CDeclarationSpecifier a] -- leaf (though note that it has the type information)
    (CDeclarator a)           -- declarator
    [CDeclaration a]          -- params
    (CStatement a)            -- Is always a CCompound
    a


data CDeclarator a
  = CDeclr (Maybe Ident) [CDerivedDeclarator a] (Maybe (CStringLiteral a)) [CAttribute a] a -- Is a function iff the first CDerivedDeclarator is CFunDeclr

data CDerivedDeclarator a
  = CFunDeclr (Either [Ident] ([CDeclaration a],Bool)) [CAttribute a] a -- CAttribute is leaf. The bool represents varargs.
    -- ^ Function declarator @CFunDeclr declr (old-style-params | new-style-params) c-attrs@
  | ...

data CDeclaration a
  = CDecl
    [CDeclarationSpecifier a] -- leaf
    [(Maybe (CDeclarator a),  -- declarator (may be omitted)
      Maybe (CInitializer a), -- should be nothing for params and fundecls
      Maybe (CExpression a))] -- should be nothing for params and fundecls
    a                         -- annotation
    | ...
-}

-- Note: We totally gloss over how C function decls are multi-decls: there is a return type attached but it's attached
-- to many functions. This is also totally not required by the IPT transformation.

-- According to the C spec (and language-c), a parameter list is either "old-style" (just a list of identifiers),
-- or "new-style" (each parameter must have a type). language-c will fail to parse a declaration that mixes
-- old and new style parameters. Yet Clang accepts them, because why not. C++ apparently removed old-style
-- parameters, and requires a type specifier for all declarations; both Clang in C++ mode and MSVC++ reject old-style
-- parameters.
--
-- As a small sin, I'm going to declare old-style parameters to be just a special case of new-style ones.
-- It makes my life easier, and that appears to be how Clang models them anyway.
--
-- ....except that I just learned about the "int f(a,b) int a; int b; { ... }" syntax. Ahhhhhh! When presented with that,
-- now GCC does treat them differently. Also, those "int a; int b;" decls can come in the other order.
-- No way around this: there is a non-context-free/non-syntactic constraint in the C spec, a constraint
-- between the language-specific and general notions of parameters. At the end of the day: we don't
-- have a good story for handling these kinds of constraints, other than seeing just how far
-- we can get without handling them.
--
-- This syntax can be used in function definitions, but not function declarations. So, we will do the merger
-- as planned for function declarations.

-- |
-- Quoth the spec:
-- If the declarator includes a parameter type list, the declaration of each parameter shall
-- include an identifier, except for the special case of a parameter list consisting of a single
-- parameter of type void, in which case there shall not be an identifier. No declaration list
-- shall follow.
--
-- AHHHHHHHHHHHHH!
-- I'm going to include another argument type for this. Thankfully, the planned
-- design for IPT will not be willing to mess with functions that have non-positional arguments,
-- so we're safe here

-- In function decls, since we don't model that void isn't a type,
-- this becomes just a special case of a normal param, and we don't include it
data CSpecialParamL
data CVoidArg :: Node where
  CVoidArg :: CVoidArg e CSpecialParamL

-- NOTE: I don't derive these all on one line because GHC crashes
-- Try again after upgrading GHC?
deriveAll [''CVoidArg]

pattern CVoidArg' :: (CVoidArg :-<: fs, All HFunctor fs) => CxtS h fs a CSpecialParamL
pattern CVoidArg' <- (project -> Just CVoidArg) where
  CVoidArg' = iCVoidArg

data CVarArgParam :: Node where
  CVarArgParam :: CVarArgParam e CSpecialParamL

deriveAll [''CVarArgParam]

pattern CVarArgParam' :: (CVarArgParam :-<: fs, All HFunctor fs) => CxtS h fs a CSpecialParamL
pattern CVarArgParam' <- (project -> Just CVarArgParam) where
  CVarArgParam' = iCVarArgParam

-- These can be used in function definitions, but not function declarations
data COldStyleParamL
data COldStyleParam e l where
  COldStyleParam :: e P.IdentL -> COldStyleParam e COldStyleParamL

deriveAll [''COldStyleParam]

pattern COldStyleParam' :: (COldStyleParam :-<: fs, All HFunctor fs) => CxtS h fs a P.IdentL -> CxtS h fs a COldStyleParamL
pattern COldStyleParam' n <- (project -> Just (COldStyleParam n)) where
  COldStyleParam' n = iCOldStyleParam n

-- Contains both fields from the first CDerivedDeclarator  in the original abstract syntax, as well as the CDeclarator its
data CFunDeclAttrs e l where
  CFunDeclAttrs :: e [C.CDerivedDeclaratorL] -> e [C.CAttributeL] -> e (Maybe (C.CStringLiteralL)) -> e [C.CAttributeL] -> CFunDeclAttrs e FunctionDeclAttrsL

deriveAll [''CFunDeclAttrs]

pattern CFunDeclAttrs' ::
  ( CFunDeclAttrs :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a [C.CDerivedDeclaratorL]
  -> CxtS h fs a [C.CAttributeL]
  -> CxtS h fs a (Maybe (C.CStringLiteralL))
  -> CxtS h fs a [C.CAttributeL]
  -> CxtS h fs a FunctionDeclAttrsL
pattern CFunDeclAttrs' dds attrs1 asmNm attrs2 <- (project -> Just (CFunDeclAttrs dds attrs1 asmNm attrs2)) where
  CFunDeclAttrs' dds attrs1 asmNm attrs2 = iCFunDeclAttrs dds attrs1 asmNm attrs2

data CFunDefAttrs e l where
  CFunDefAttrs :: e [CDeclarationSpecifierL] -> e FunctionDeclAttrsL -> e [CDeclarationL] -> CFunDefAttrs e FunctionDefAttrsL

deriveAll [''CFunDefAttrs]

pattern CFunDefAttrs' ::
  ( CFunDefAttrs :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a [CDeclarationSpecifierL]
  -> CxtS h fs a FunctionDeclAttrsL
  -> CxtS h fs a [CDeclarationL]
  -> CxtS h fs a FunctionDefAttrsL
pattern CFunDefAttrs' dss fda decls <- (project -> Just (CFunDefAttrs dss fda decls)) where
  CFunDefAttrs' dss fda decls = iCFunDefAttrs dss fda decls

-- Modeling note: The only difference between parameters and parameter decls
-- is that identifiers are optional for decls. Sharing an attributes type
data CFunParamAttrsL
data CFunParamAttrs e l where
  CFunParamAttrs :: e [CDeclarationSpecifierL] -> e [CDerivedDeclaratorL] -> e (Maybe CStringLiteralL) -> e [CAttributeL] -> CFunParamAttrs e CFunParamAttrsL

deriveAll [''CFunParamAttrs]

pattern CFunParamAttrs' ::
  ( CFunParamAttrs :-<: fs
  , All HFunctor fs
  ) => CxtS h fs a [CDeclarationSpecifierL]
  -> CxtS h fs a [CDerivedDeclaratorL]
  -> CxtS h fs a (Maybe CStringLiteralL)
  -> CxtS h fs a [CAttributeL]
  -> CxtS h fs a CFunParamAttrsL
pattern CFunParamAttrs' dss dds asmNm attrs <- (project -> Just (CFunParamAttrs dss dds asmNm attrs)) where
  CFunParamAttrs' dss dds asmNm attrs = iCFunParamAttrs dss dds asmNm attrs

createSortInclusionTypes [  ''P.FunctionCallL, ''C.CExpressionL, ''C.CExpressionL,      ''P.FunctionDeclL, ''CFunParamAttrsL,               ''CSpecialParamL,           ''P.FunctionDefL,  ''CFunParamAttrsL,   ''CSpecialParamL,       ''COldStyleParamL,      ''C.CStatementL
                         ] [''C.CExpressionL,  ''P.FunctionExpL, ''P.PositionalArgExpL, ''C.CDeclaratorL,  ''P.FunctionParameterDeclAttrsL, ''P.FunctionParameterDeclL, ''C.CFunctionDefL, ''P.ParameterAttrsL, ''P.FunctionParameterL, ''P.FunctionParameterL, ''P.FunctionBodyL
                         ]
deriveAll [ ''FunctionCallIsCExpression, ''CExpressionIsFunctionExp, ''CExpressionIsPositionalArgExp
          , ''FunctionDeclIsCDeclarator, ''CFunParamAttrsIsFunctionParameterDeclAttrs, ''CSpecialParamIsFunctionParameterDecl
          , ''FunctionDefIsCFunctionDef, ''CFunParamAttrsIsParameterAttrs, ''CSpecialParamIsFunctionParameter, ''COldStyleParamIsFunctionParameter
          , ''CStatementIsFunctionBody]
createSortInclusionInfers [  ''P.FunctionCallL, ''C.CExpressionL, ''C.CExpressionL,      ''P.FunctionDeclL, ''CFunParamAttrsL,               ''CSpecialParamL,           ''P.FunctionDefL,  ''CFunParamAttrsL,   ''CSpecialParamL,       ''COldStyleParamL,      ''C.CStatementL
                          ] [''C.CExpressionL,  ''P.FunctionExpL, ''P.PositionalArgExpL, ''C.CDeclaratorL,  ''P.FunctionParameterDeclAttrsL, ''P.FunctionParameterDeclL, ''C.CFunctionDefL, ''P.ParameterAttrsL, ''P.FunctionParameterL, ''P.FunctionParameterL, ''P.FunctionBodyL
                          ]



pattern CInteger' ::
  ( CInteger :-<: fs
  , CIntRepr :-<: fs
  , Flags :-<: fs
  , All HFunctor fs
  ) => Integer -> CxtS h fs a CIntegerL
pattern CInteger' n <- (project -> Just (CInteger n _ _)) where
  CInteger' n = iCInteger n iDecRepr (C.iFlags 0)

-- Not a wide string
pattern CString' :: (CString :-<: fs, All HFunctor fs) => String -> CxtS h fs a CStringL
pattern CString' s <- (project -> Just (CString s False)) where
  CString' s = iCString s False

-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------

do let cSortInjections = [ ''CDeclarationSpecifiersIsMultiLocalVarDeclCommonAttrs, ''MultiLocalVarDeclIsCCompoundBlockItem
                         , ''CInitializerIsLocalVarInit, ''IdentIsIdent, ''CExpressionIsLhs
                         , ''CExpressionIsRhs, ''CAssignOpIsAssignOp, ''AssignIsCExpression, ''CCompoundBlockItemIsBlockItem, ''IdentIsVarDeclBinder
                         , ''FunctionCallIsCExpression, ''CExpressionIsFunctionExp, ''CExpressionIsPositionalArgExp
                         , ''FunctionDeclIsCDeclarator, ''CFunParamAttrsIsFunctionParameterDeclAttrs, ''CSpecialParamIsFunctionParameterDecl
                         , ''FunctionDefIsCFunctionDef, ''CFunParamAttrsIsParameterAttrs, ''CSpecialParamIsFunctionParameter, ''COldStyleParamIsFunctionParameter
                         , ''CStatementIsFunctionBody
                         ]
   let cNewNodes       = [''CLocalVarAttrs, ''CLabeledBlock, ''CVoidArg, ''CVarArgParam, ''COldStyleParam, ''CFunDeclAttrs, ''CFunDefAttrs, ''CFunParamAttrs]
   let names = (cSigNames \\ [mkName "Ident", mkName "CFunctionDef"])
                          ++ cSortInjections
                          ++ cNewNodes
                          ++ [ ''OptLocalVarInit, ''SingleLocalVarDecl, ''MultiLocalVarDecl, ''P.Ident, ''Assign
                             , ''AssignOpEquals, ''Block, ''EmptyBlockEnd
                             , ''FunctionCall, ''EmptyFunctionCallAttrs, ''FunctionArgumentList, ''PositionalArgument
                             , ''FunctionDecl, ''PositionalParameterDeclOptionalIdent
                             , ''FunctionDef, ''PositionalParameter
                             ]
   sumDec <- runCompTrans $ makeSumType "MCSig" names
   return sumDec

type instance InjectableSorts MCSig MultiLocalVarDeclL = '[CCompoundBlockItemL]

type MCTerm = Term MCSig
type MCTermLab = TermLab MCSig

type MCCxt h a = CxtS h MCSig a
type MCCxtA h a p = AnnCxtS p h MCSig a


-----------------------------------------------------------------------------------
----------------------         Sort injections             ------------------------
-----------------------------------------------------------------------------------

-- Some extra work needed to make this work on Context's
instance InjF MCSig AssignL BlockItemL where
  injF = C.iCBlockStmt . (flip C.iCExpr iUnitF) . iJustF . (iAssignIsCExpression :: (MCCxt _ _) AssignL -> (MCCxt _ _) CExpressionL)
  projF' t = do
    (b :: MCCxtA _ _ _ CCompoundBlockItemL) <- projF' t
    CBlockStmt s <- project' b
    CExpr me _ <- project' s
    let JustA' e = me
    projF' e

instance InjF MCSig MultiLocalVarDeclL BlockItemL where
  injF = iMultiLocalVarDeclIsCCompoundBlockItem
  projF' t = do
    (s :: (MCCxtA _ _ _) CCompoundBlockItemL) <- projF' t
    projF' s

instance InjF MCSig P.IdentL LhsL where
  injF n = iCVar (injF n) iUnitF
  projF' n = do
    (x :: (MCCxtA _ _ _) CExpressionL) <- projF' n
    CVar y _ <- project' x
    projF' y

instance InjF MCSig P.IdentL CExpressionL where
  injF n = iCVar (injF n) iUnitF
  projF' e = do
    CVar x _ <- project' e
    projF' x

instance InjF MCSig CStatementL BlockItemL where
  injF = iCBlockStmt
  projF' s = do
    CCompoundBlockItemIsBlockItem x <- project' s
    CBlockStmt y <- project' x
    return y

instance InjF MCSig CExpressionL LocalVarInitL where
  injF x = iCInitExpr x iUnitF
  projF' i = do
    CInitializerIsLocalVarInit x <- project' i
    CInitExpr e _ <- project' x
    return e

instance InjF MCSig P.IdentL FunctionExpL where
  injF x = iCVar (injF x) iUnitF

  projF' f
   | Just (CExpressionIsFunctionExp e) <- project' f
   , Just (CVar n _) <- project' e
   , Just (IdentIsIdent i) <- project' n
   = Just i
  projF' _ = Nothing

instance InjF MCSig P.IdentL PositionalArgExpL where
  injF n = iCVar (injF n) iUnitF

  projF' a
   | Just (CExpressionIsPositionalArgExp e) <- project' a
   , Just (CVar n _) <- project' e = projF' n
  projF' _ = Nothing

instance InjF MCSig FunctionCallL RhsL where
  injF = iFunctionCallIsCExpression
  projF' f
   | Just (CExpressionIsRhs e) <- project' f
   , Just (FunctionCallIsCExpression c) <- project' e
   = Just c
  projF' _ = Nothing

#endif
