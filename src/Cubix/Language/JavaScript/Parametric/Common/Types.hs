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

module Cubix.Language.JavaScript.Parametric.Common.Types where

#ifndef ONLY_ONE_LANGUAGE
import Data.List ( (\\) )
import Language.Haskell.TH ( mkName )

import Data.Comp.Multi ( Cxt, Term, project', (:&:), project, (:<:), HFunctor, (:-<:), All, CxtS, AnnCxtS )
import Data.Comp.Trans ( runCompTrans, makeSumType )

import Cubix.Language.Info
import Cubix.Language.JavaScript.Parametric.Full.Types as JS
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax as P

-----------------------------------------------------------------------------------
---------------       Variable declarations and blocks     ------------------------
-----------------------------------------------------------------------------------

-- Biggest gotcha of JavaScript: Scopes are weird. See
-- https://gist.github.com/jkoppel/3f65e316ec921a90ee5228f771da4a5d for an
-- example of why. The big thing to know is that only new functions introduce
-- scopes, and a variable is local if it is in a "var" decl anywhere within the scope,
-- regardless of where that declaration is. Hence, the common "Block" fragment
-- does not model general JS blocks, but does model function bodies
--
-- The JS library we're using lets you put arbitrary expressions as LHSs. JS does not.
-- I don't like this.
--
-- Also note that it has separate "JSAssignStatement" and "JSAssign" (expression) constructors.
-- It seems to somewhat arbitrarily parse an assign as either an AssignStatement or as an ExpressionStatement.
-- The official grammar only has the latter. As an exception to my "isomorphism" rule, I am going to convert all such things
-- to the latter because that's what they should have been in the first place. (Also, strictly speaking, it's still isomorphic
-- to the original tree; it's just that the grammar's redundant)
--
createSortInclusionTypes [ ''P.IdentL,      ''JSExpressionL,   ''JSExpressionL,    ''P.MultiLocalVarDeclL, ''JSExpressionL, ''JSExpressionL, ''JSAssignOpL, ''P.AssignL,      ''P.BlockL,      ''JSStatementL, ''JSBlockL
                         ] [
                           ''JSExpressionL, ''P.LocalVarInitL, ''P.VarDeclBinderL, ''JSStatementL,         ''P.RhsL,        ''P.LhsL,        ''P.AssignOpL, ''JSExpressionL,  ''JSStatementL, ''P.BlockItemL,  ''JSASTL
                         ]
deriveAll [''IdentIsJSExpression, ''JSExpressionIsLocalVarInit, ''JSExpressionIsVarDeclBinder,
           ''MultiLocalVarDeclIsJSStatement, ''JSExpressionIsRhs, ''JSExpressionIsLhs, ''JSAssignOpIsAssignOp,
           ''AssignIsJSExpression, ''BlockIsJSStatement, ''JSStatementIsBlockItem, ''JSBlockIsJSAST]
createSortInclusionInfers [ ''P.IdentL,      ''JSExpressionL,   ''JSExpressionL,    ''P.MultiLocalVarDeclL, ''JSExpressionL, ''JSExpressionL, ''JSAssignOpL, ''P.AssignL,     ''P.BlockL,     ''JSStatementL, ''JSBlockL
                          ] [
                            ''JSExpressionL, ''P.LocalVarInitL, ''P.VarDeclBinderL, ''JSStatementL,         ''P.RhsL,        ''P.LhsL,        ''P.AssignOpL, ''JSExpressionL, ''JSStatementL, ''P.BlockItemL, ''JSASTL
                          ]

data MaybeIdentIsJSIdent e l where
  MaybeIdentIsJSIdent :: e (Maybe IdentL) -> MaybeIdentIsJSIdent e JSIdentL

deriveAll [''MaybeIdentIsJSIdent]

pattern JSIdent' :: (MaybeIdentIsJSIdent :-<: fs, Ident :-<: fs, MaybeF :-<: fs, All HFunctor fs) => String -> CxtS h fs a JSIdentL
pattern JSIdent' ident <- (project -> Just (MaybeIdentIsJSIdent (Just' (Ident' ident)))) where
  JSIdent' ident = iMaybeIdentIsJSIdent $ Just' $ Ident' ident

data JSFor e l where
  JSFor      :: e [JSExpressionL] -> e [JSExpressionL] -> e [JSExpressionL] -> e JSStatementL -> JSFor e JSStatementL
  JSForIn    :: e JSExpressionL -> e JSBinOpL -> e JSExpressionL -> e JSStatementL -> JSFor e JSStatementL
  JSForVar   :: e [SingleLocalVarDeclL] -> e [JSExpressionL] -> e [JSExpressionL] -> e JSStatementL -> JSFor e JSStatementL
  JSForVarIn :: e SingleLocalVarDeclL -> e JSBinOpL -> e JSExpressionL -> e JSStatementL -> JSFor e JSStatementL

deriveAll [''JSFor]

data BlockWithPrelude e l where
  BlockWithPrelude :: [String] -> e BlockL -> BlockWithPrelude e JSBlockL

deriveAll [''BlockWithPrelude]

-----------------------------------------------------------------------------------
---------------       Function calls, decls, defns         ------------------------
-----------------------------------------------------------------------------------


-- There are three ways to give a method call in the language-javascript AST: JSMethodCall,
-- JSCallExpression, and JSMemberExpression. In the spec, there is only one way, and it's called "CallExpression"
-- No qualms about merging these; they are identical.
--
-- For function calls, we choose not to model the receiver, because how "thisValue" works in JS is a bit complicated.
-- In omitting this, we still follow the spec for FunctionCall and ArgumentList

createSortInclusionTypes [ ''FunctionCallL, ''JSExpressionL,     ''JSExpressionL, ''FunctionDefL, ''JSBlockL
                         ] [
                           ''JSExpressionL, ''PositionalArgExpL, ''FunctionExpL,  ''JSStatementL, ''FunctionBodyL
                         ]
deriveAll [ ''FunctionCallIsJSExpression, ''JSExpressionIsPositionalArgExp, ''JSExpressionIsFunctionExp
          , ''FunctionDefIsJSStatement, ''JSBlockIsFunctionBody]
createSortInclusionInfers [ ''FunctionCallL, ''JSExpressionL,     ''JSExpressionL, ''FunctionDefL, ''JSBlockL
                          ] [
                            ''JSExpressionL, ''PositionalArgExpL, ''FunctionExpL,  ''JSStatementL, ''FunctionBodyL
                          ]


-----------------------------------------------------------------------------------
----------------------         Declaring the IPS           ------------------------
-----------------------------------------------------------------------------------

do let jsSortInjections = [''IdentIsJSExpression, ''JSExpressionIsLocalVarInit, ''JSExpressionIsVarDeclBinder,
                           ''MultiLocalVarDeclIsJSStatement, ''JSExpressionIsRhs, ''JSExpressionIsLhs, ''JSAssignOpIsAssignOp,
                           ''AssignIsJSExpression, ''BlockIsJSStatement, ''JSStatementIsBlockItem, ''BlockWithPrelude, ''JSBlockIsJSAST,
                           ''FunctionCallIsJSExpression, ''JSExpressionIsPositionalArgExp, ''JSExpressionIsFunctionExp, ''FunctionDefIsJSStatement, ''JSBlockIsFunctionBody]
   let names = (jsSigNames \\ [mkName "JSIdent", mkName "JSVarInitializer"])
                    ++ jsSortInjections
                    ++ [''MaybeIdentIsJSIdent, ''JSFor]
                    ++ [ ''OptLocalVarInit, ''SingleLocalVarDecl, ''P.Ident
                       , ''AssignOpEquals, ''Assign, ''P.Block, ''P.TupleBinder
                       , ''EmptyLocalVarDeclAttrs, ''MultiLocalVarDecl
                       , ''EmptyBlockEnd, ''EmptyMultiLocalVarDeclCommonAttrs
                       , ''FunctionCall, ''EmptyFunctionCallAttrs, ''FunctionArgumentList, ''PositionalArgument
                       , ''FunctionDef, ''EmptyFunctionDefAttrs, ''PositionalParameter, ''EmptyParameterAttrs
                       ]

   sumDec <- runCompTrans $ makeSumType "MJSSig" names
   return sumDec

type instance InjectableSorts MJSSig AssignL = [JSExpressionL, BlockItemL]
type instance InjectableSorts MJSSig MultiLocalVarDeclL = [JSStatementL, BlockItemL]

type MJSTerm = Term MJSSig
type MJSTermLab = TermLab MJSSig

type MJSCxt h a = CxtS h MJSSig a
type MJSCxtA h a p = AnnCxtS p h MJSSig a


-----------------------------------------------------------------------------------
----------------------         Sort injections             ------------------------
-----------------------------------------------------------------------------------

instance InjF MJSSig (Maybe IdentL) JSIdentL where
  injF = iMaybeIdentIsJSIdent
  projF' (project' -> Just (MaybeIdentIsJSIdent x)) = Just x
  projF' _                                         = Nothing

instance InjF MJSSig MultiLocalVarDeclL BlockItemL where
  injF = iJSStatementIsBlockItem . injF
  projF' x = do
    (y :: MJSCxtA h a _ JSStatementL) <- projF' x
    projF' y

instance InjF MJSSig SingleLocalVarDeclL BlockItemL where
  injF x = iMultiLocalVarDeclIsJSStatement $ iMultiLocalVarDecl iEmptyMultiLocalVarDeclCommonAttrs (SingletonF' x)
  projF' x = do
    (y :: MJSCxtA h a _ JSStatementL) <- projF' x
    (z :: MJSCxtA h a _ MultiLocalVarDeclL) <- projF' y
    MultiLocalVarDecl _ (SingletonFA' r) <- project' z
    return r

instance InjF MJSSig AssignL BlockItemL where
  injF x = iJSExpressionStatement (injF x) (iJSSemi iJSNoAnnot)
  projF' x = do
    (y :: MJSCxtA _ _ _ JSStatementL) <- projF' x
    JSExpressionStatement e _ <- project' y
    projF' e

instance InjF MJSSig AssignL JSStatementL where
  injF x = iJSExpressionStatement (injF x) (iJSSemi iJSNoAnnot)
  projF' x = do
    JSExpressionStatement e _ <- project' x
    projF' e

instance InjF MJSSig IdentL VarDeclBinderL where
  injF = iIdentIsJSExpression
  projF' x = do
    (y :: MJSCxtA _ _ _ JSExpressionL) <- projF' x
    projF' y

instance InjF MJSSig IdentL LhsL where
  injF x = injF $ (injF x :: MJSCxt _ _ JSExpressionL)

  projF' x
   | Just (y :: MJSCxtA _ _ _ JSExpressionL) <- projF' x
   , Just z <- projF' y
   = Just z
  projF' _ = Nothing

instance InjF MJSSig IdentL FunctionExpL where
  injF = iIdentIsJSExpression

  projF' x = do
    JSExpressionIsFunctionExp e <- project' x
    projF' e

instance InjF MJSSig IdentL PositionalArgExpL where
  injF = iIdentIsJSExpression

  projF' x
   | Just (JSExpressionIsPositionalArgExp e) <- project' x = projF' e
  projF' _ = Nothing

#endif
