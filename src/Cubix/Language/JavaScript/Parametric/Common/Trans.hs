{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PartialTypeSignatures   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE ViewPatterns            #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.JavaScript.Parametric.Common.Trans () where
#else
module Cubix.Language.JavaScript.Parametric.Common.Trans
  (
    translate
  , untranslate
  ) where

import Data.List ( (\\) )
import Data.Proxy
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Control.Lens ( (&), (%~), _1 )

import Data.Comp.Multi ( Term, project, inject, unTerm, caseCxt, HFunctor(..), (:-<:), All, Sum )
import Data.Comp.Multi.Show ()

import Cubix.Language.JavaScript.Parametric.Common.Types
import qualified Cubix.Language.JavaScript.Parametric.Full as F
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax

-------------------------------------------------------------------------------

noAnn :: (F.JSAnnot :-<: fs, All HFunctor fs) => Term fs F.JSAnnotL
noAnn = F.iJSNoAnnot

semi :: (F.JSSemi :-<: fs, F.JSAnnot :-<: fs, All HFunctor fs) => Term fs F.JSSemiL
semi = F.iJSSemi F.iJSNoAnnot

jsCommaListToList :: F.JSTerm (F.JSCommaList l) -> [F.JSTerm l]
jsCommaListToList (project -> Just (F.JSLCons xs _ x)) = jsCommaListToList xs ++ [x]
jsCommaListToList (project -> Just (F.JSLOne x))       = [x]
jsCommaListToList (project -> Just F.JSLNil)           = []


listToCommaList :: [F.JSTerm l] -> F.JSTerm (F.JSCommaList l)
listToCommaList x = listToCommaList' $ reverse x -- They have left-associative lists
  where
    listToCommaList' []     = F.riJSLNil
    listToCommaList' [x]    = F.riJSLOne x
    listToCommaList' (x:xs) = F.riJSLCons (listToCommaList' xs) noAnn x

-------------------------------------------------------------------------------

translate :: F.JSTerm l -> MJSTerm l
translate = trans . unTerm

translate' :: (InjF MJSSig l l') => F.JSTerm l -> MJSTerm l'
translate' = injF . translate

class Trans f where
  trans :: f F.JSTerm l -> MJSTerm l

instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt (Proxy @Trans) trans

transDefault :: (HFunctor f, f :-<: MJSSig, f :-<: F.JSSig) => f F.JSTerm l -> MJSTerm l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MJSSig, f :-<: F.JSSig) => Trans f where
  trans = transDefault


instance {-# OVERLAPPABLE #-} Trans F.JSVarInitializer where
  trans = error "JSVarInitializer found not within variable declaration"

translateAssign :: F.JSTerm F.JSExpressionL -> F.JSTerm F.JSAssignOpL -> F.JSTerm F.JSExpressionL -> MJSTerm F.JSExpressionL
translateAssign lhs op rhs = iAssign (injF $ translate lhs) op' (injF $ translate rhs)
  where
    op' = case project op of
      Just (F.JSAssign _) -> AssignOpEquals'
      _                   -> injF $ translate op

-- This takes out things like "use strict";, which aren't really part of the computation,
-- and have to be at the front
splitDirectivePrelude :: [F.JSTerm F.JSStatementL] -> ([String], [F.JSTerm F.JSStatementL])
splitDirectivePrelude []     = ([], [])
splitDirectivePrelude (s:ss) = case toDirective s of
                                 Nothing  -> ([], s:ss)
                                 Just dir -> (splitDirectivePrelude ss) & _1 %~ (dir:)
  where
    toDirective :: F.JSTerm F.JSStatementL -> Maybe String
    toDirective (project -> Just (F.JSExpressionStatement (project -> Just (F.JSStringLiteral _ str)) _)) = Just str
    toDirective _ = Nothing

translateBlock :: F.JSTerm F.JSBlockL -> MJSTerm F.JSBlockL
translateBlock (project -> Just (F.JSBlock _ ss _)) = iBlockWithPrelude dirPrelude block
  where
    (dirPrelude, blockStmts) = splitDirectivePrelude $ extractF ss

    block = iBlock (insertF $ map (injF.translate) blockStmts) EmptyBlockEnd'

translateFunCall :: F.JSTerm F.JSExpressionL -> F.JSTerm (F.JSCommaList F.JSExpressionL) -> MJSTerm FunctionCallL
translateFunCall e args = FunctionCall' EmptyFunctionCallAttrs'
                                        (injF $ translate e)
                                        (FunctionArgumentList' $ insertF $ map (PositionalArgument' . injF . translate)
                                                                               (jsCommaListToList args))

instance Trans F.JSExpression where
  trans (F.JSIdentifier _ n)                     = iIdent n
  trans (F.JSAssignExpression lhs op rhs)        = translateAssign lhs op rhs
  trans (F.JSFunctionExpression _ a _ b _ block) = F.iJSFunctionExpression noAnn (translate a) noAnn (translate b) noAnn (translateBlock block)
  trans (F.JSMemberExpression f _ args _)        = injF $ translateFunCall f args
  trans (F.JSCallExpression   f _ args _)        = injF $ translateFunCall f args
  trans x                                        = transDefault x

transIdent :: F.JSTerm F.JSIdentL -> MJSTerm IdentL
transIdent (project -> (Just (F.JSIdentName _ n))) = Ident' n
transIdent (project -> (Just F.JSIdentNone))       = error "JSIdentNone discovered where identifier was required"


instance Trans F.JSIdent where
  trans (F.JSIdentName _ n) = injF $ Just' $ Ident' n
  trans F.JSIdentNone       = injF $ (Nothing' :: MJSTerm (Maybe IdentL))

varInitToDecl :: F.JSTerm F.JSExpressionL -> MJSTerm SingleLocalVarDeclL
varInitToDecl (project -> (Just (F.JSVarInitExpression lhs init))) =
          SingleLocalVarDecl' EmptyLocalVarDeclAttrs' (injF $ translate lhs) rhs
  where
    rhs = case project init of
      Just (F.JSVarInit _ e) -> JustLocalVarInit' $ injF $ translate e
      Just F.JSVarInitNone   -> NoLocalVarInit'

extractVarDecls :: F.JSTerm (F.JSCommaList F.JSExpressionL) -> MJSTerm [SingleLocalVarDeclL]
extractVarDecls commaList = insertF $ map varInitToDecl exps
  where
    exps = jsCommaListToList commaList

transCommaExpList :: F.JSTerm (F.JSCommaList F.JSExpressionL) -> MJSTerm [F.JSExpressionL]
transCommaExpList = insertF . map translate . jsCommaListToList

transParams :: F.JSTerm (F.JSCommaList F.JSIdentL) -> MJSTerm [FunctionParameterL]
transParams args = insertF $ map (makeParam.transIdent) (jsCommaListToList args)
  where
    makeParam :: MJSTerm IdentL -> MJSTerm FunctionParameterL
    makeParam n = PositionalParameter' EmptyParameterAttrs' n

instance Trans F.JSStatement where
  trans (F.JSVariable _ exprs _)      = iMultiLocalVarDecl EmptyMultiLocalVarDeclCommonAttrs'
                                                         (extractVarDecls exprs)
  trans (F.JSFor _ _ init _ exp _ upd _ s)      = iJSFor (transCommaExpList init) (transCommaExpList exp) (transCommaExpList upd) (translate s)
  trans (F.JSForIn _ _ v op exp _ s)            = iJSForIn (translate v) (translate op) (translate exp) (translate s)
  trans (F.JSForVar _ _ _ init _ exp _ upd _ s) = iJSForVar (extractVarDecls init) (transCommaExpList exp) (transCommaExpList upd) (translate s)
  trans (F.JSForVarIn _ _ _ v op exp _ s)       = iJSForVarIn (varInitToDecl v) (translate op) (translate exp) (translate s)
  trans (F.JSConstant _ _ _)                    = error "ES7 Const not supported"
  trans (F.JSAssignStatement lhs op rhs s)      = F.iJSExpressionStatement (translateAssign lhs op rhs) semi
  trans (F.JSFunction _ n _ args _ block _)     = iFunctionDef EmptyFunctionDefAttrs' (transIdent n) (transParams args) (injF $ translateBlock block)
  trans (F.JSMethodCall f _ args _ _)           = F.iJSExpressionStatement (injF $ translateFunCall f args) semi
  trans x                                       = transDefault x

instance Trans F.JSAST where
  trans (F.JSAstProgram stmts _) = injF $ translateBlock (F.iJSBlock noAnn stmts noAnn)
  trans x                        = transDefault x

-------------------------------------------------------------------------------

class Untrans f where
  untrans :: f MJSTerm l -> F.JSTerm l

instance {-# OVERLAPPING #-} (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt (Proxy @Untrans) untrans

untransError :: (HFunctor f, f :-<: MJSSig) => f MJSTerm l -> F.JSTerm l
untransError t = error $ "Cannot untranslate root node: " ++ (show $ (inject t :: MJSTerm _))

do ipsNames <- sumToNames ''MJSSig
   modNames <- sumToNames ''F.JSSig
   let targTs = map ConT $ (ipsNames \\ modNames)
                           \\ [ ''JSBlockIsJSAST, ''MaybeIdentIsJSIdent, ''JSFor, ''BlockIsJSStatement, ''BlockWithPrelude
                              , ''AssignIsJSExpression, ''MultiLocalVarDeclIsJSStatement, ''IdentIsJSExpression
                              , ''FunctionCallIsJSExpression, ''FunctionDefIsJSStatement]
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)


untransDefault :: (HFunctor f, f :-<: F.JSSig) => f MJSTerm l -> F.JSTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: F.JSSig) => Untrans f where
  untrans = untransDefault

instance {-# OVERLAPPING #-} Untrans IdentIsJSExpression where
  untrans (IdentIsJSExpression (Ident' n)) = F.iJSIdentifier noAnn n


untransDecl :: MJSTerm SingleLocalVarDeclL -> F.JSTerm F.JSExpressionL
untransDecl (SingleLocalVarDecl' _ lhs init) =
       F.iJSVarInitExpression (untranslate $ fromProjF lhs) varInit
  where
    varInit = case init of
      JustLocalVarInit' e -> F.iJSVarInit noAnn (untranslate $ fromProjF e)
      NoLocalVarInit'     -> F.iJSVarInitNone

untransDecls :: MJSTerm [SingleLocalVarDeclL] -> F.JSTerm (F.JSCommaList F.JSExpressionL)
untransDecls x = listToCommaList $ map untransDecl $ extractF x


instance {-# OVERLAPPING #-} Untrans MultiLocalVarDeclIsJSStatement where
  untrans (MultiLocalVarDeclIsJSStatement (MultiLocalVarDecl' _ decs)) = F.iJSVariable noAnn (untransDecls decs) semi

untransOp :: MJSTerm AssignOpL -> F.JSTerm F.JSAssignOpL
untransOp AssignOpEquals' = F.iJSAssign noAnn
untransOp x               = untranslate $ fromProjF x

instance {-# OVERLAPPING #-} Untrans AssignIsJSExpression where
  untrans (AssignIsJSExpression (Assign' lhs op rhs)) = F.iJSAssignExpression (untranslate $ fromProjF lhs) (untransOp op) (untranslate $ fromProjF rhs)


toJSString :: String -> MJSTerm F.JSStatementL
toJSString s = F.iJSExpressionStatement (F.iJSStringLiteral noAnn s) semi

instance {-# OVERLAPPING #-} Untrans BlockWithPrelude where
  untrans (BlockWithPrelude dirPrelude (Block' ss _)) = F.iJSBlock noAnn (insertF $ map untranslate stmts) noAnn
    where
      stmts = (map toJSString dirPrelude) ++ (map fromProjF $ extractF ss)


instance {-# OVERLAPPING #-} Untrans BlockIsJSStatement where
  untrans (BlockIsJSStatement (Block' ss _)) = F.iJSStatementBlock noAnn (mapF (untranslate.fromProjF) ss) noAnn semi


untransCommaListExp :: MJSTerm [F.JSExpressionL] -> F.JSTerm (F.JSCommaList F.JSExpressionL)
untransCommaListExp = listToCommaList . map untranslate . extractF

instance {-# OVERLAPPING #-} Untrans JSFor where
  untrans (JSFor init exp upd s)   = F.iJSFor noAnn noAnn (untransCommaListExp init) noAnn (untransCommaListExp exp) noAnn (untransCommaListExp upd) noAnn (untranslate s)
  untrans (JSForIn lhs b e s)      = F.iJSForIn noAnn noAnn (untranslate lhs) (untranslate b) (untranslate e) noAnn (untranslate s)
  untrans (JSForVar dec exp upd s) = F.iJSForVar noAnn noAnn noAnn (untransDecls dec) noAnn (untransCommaListExp exp) noAnn (untransCommaListExp upd) noAnn (untranslate s)
  untrans (JSForVarIn dec b e s)   = F.iJSForVarIn noAnn noAnn noAnn (untransDecl dec) (untranslate b) (untranslate e) noAnn (untranslate s)

untransIdent :: MJSTerm IdentL -> F.JSTerm F.JSIdentL
untransIdent (Ident' s) = F.iJSIdentName noAnn s

instance {-# OVERLAPPING #-} Untrans MaybeIdentIsJSIdent where
  untrans (MaybeIdentIsJSIdent x) = case extractF x of
    Just (Ident' s) -> F.iJSIdentName noAnn s
    Nothing         -> F.iJSIdentNone

instance {-# OVERLAPPING #-} Untrans JSBlockIsJSAST where
  untrans (JSBlockIsJSAST (project -> (Just (BlockWithPrelude dirPrelude (Block' ss _))))) = F.iJSAstProgram (insertF $ map untranslate stmts) noAnn
    where
      stmts :: [MJSTerm F.JSStatementL]
      stmts = (map toJSString dirPrelude) ++ (map fromProjF $ extractF ss)

untransArgs :: MJSTerm [FunctionArgumentL] -> F.JSTerm (F.JSCommaList F.JSExpressionL)
untransArgs = untransCommaListExp . mapF untransArg
  where
    untransArg :: MJSTerm FunctionArgumentL -> MJSTerm F.JSExpressionL
    untransArg (PositionalArgument' x) = fromProjF x

instance {-# OVERLAPPING #-} Untrans FunctionCallIsJSExpression where
  untrans (FunctionCallIsJSExpression (FunctionCall' EmptyFunctionCallAttrs' f (FunctionArgumentList' args))) = F.iJSCallExpression (untranslate $ fromProjF f) noAnn (untransArgs args) noAnn

untransParams :: MJSTerm [FunctionParameterL] -> F.JSTerm (F.JSCommaList F.JSIdentL)
untransParams params = listToCommaList $ map (untransIdent.paramToIdent) $ extractF params
  where
    paramToIdent :: MJSTerm FunctionParameterL -> MJSTerm IdentL
    paramToIdent (PositionalParameter' EmptyParameterAttrs' n) = n

instance {-# OVERLAPPING #-} Untrans FunctionDefIsJSStatement where
  untrans (FunctionDefIsJSStatement (FunctionDef' EmptyFunctionDefAttrs' n params block)) = F.iJSFunction noAnn (untransIdent n) noAnn (untransParams params) noAnn (untranslate $ fromProjF block) semi

untranslate :: MJSTerm l -> F.JSTerm l
untranslate = untrans . unTerm

#endif
