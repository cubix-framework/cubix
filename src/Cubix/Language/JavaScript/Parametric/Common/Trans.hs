{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP                     #-}
{-# LANGUAGE PartialTypeSignatures   #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE UndecidableInstances    #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.JavaScript.Parametric.Common.Trans () where
#else
module Cubix.Language.JavaScript.Parametric.Common.Trans
  (
    translate
  , untranslate
  ) where

import Data.Default ( Default(..) )
import Data.List ( (\\) )
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Control.Lens ( (&), (%~), _1 )

import Data.Comp.Multi ( Term, project, pattern (::&::), inject, unTerm, caseCxt, caseCxt'
                       , HFunctor(..), (:-<:), All, Sum, (:&:)(..), AnnTerm )
import Data.Comp.Multi.Show ()

import Cubix.Language.JavaScript.Parametric.Common.Types
import qualified Cubix.Language.JavaScript.Parametric.Full as F
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax
import Cubix.Sin.Compdata.Annotation ( AnnotationInfo, getAnn )

-------------------------------------------------------------------------------

-- Unannotated placeholders used by the (unannotated) untranslate path.
noAnn :: (F.JSAnnot :-<: fs, All HFunctor fs) => Term fs F.JSAnnotL
noAnn = F.iJSNoAnnot

semi :: (F.JSSemi :-<: fs, F.JSAnnot :-<: fs, All HFunctor fs) => Term fs F.JSSemiL
semi = F.iJSSemi F.iJSNoAnnot

-- Annotated placeholders used by the forward translate path.
noAnnA :: forall fs a. (F.JSAnnot :-<: fs, Default a) => AnnTerm a fs F.JSAnnotL
noAnnA = F.JSNoAnnot ::&:: def

semiA :: forall fs a. (F.JSSemi :-<: fs, F.JSAnnot :-<: fs, Default a) => AnnTerm a fs F.JSSemiL
semiA = F.JSSemi noAnnA ::&:: def

jsCommaListToList :: (Default a) => F.JSTermAnn a (F.JSCommaList l) -> [F.JSTermAnn a l]
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

translate :: (Default a, AnnotationInfo a) => F.JSTermAnn a l -> MJSTermAnn a l
translate = trans . unTerm

translate' :: (InjF MJSSig l l', Default a, AnnotationInfo a) => F.JSTermAnn a l -> MJSTermAnn a l'
translate' = injFAnnDef . translate

class Trans f where
  trans :: (Default a, AnnotationInfo a) => (f :&: a) (F.JSTermAnn a) l -> MJSTermAnn a l

instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt' @Trans trans

transDefault :: (HFunctor f, f :-<: MJSSig, f :-<: F.JSSig, Default a, AnnotationInfo a)
             => (f :&: a) (F.JSTermAnn a) l -> MJSTermAnn a l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MJSSig, f :-<: F.JSSig) => Trans f where
  trans = transDefault


instance {-# OVERLAPPABLE #-} Trans F.JSVarInitializer where
  trans = error "JSVarInitializer found not within variable declaration"

translateAssign :: (Default a, AnnotationInfo a)
                => F.JSTermAnn a F.JSExpressionL
                -> F.JSTermAnn a F.JSAssignOpL
                -> F.JSTermAnn a F.JSExpressionL
                -> a
                -> MJSTermAnn a F.JSExpressionL
translateAssign lhs op rhs a =
    injFAnnDef $ Assign (injFAnnDef $ translate lhs) op' (injFAnnDef $ translate rhs) ::&:: a
  where
    op' = case op of
      (F.JSAssign _ ::&:: opA) -> AssignOpEquals ::&:: opA
      _                        -> injFAnnDef $ translate op

-- This takes out things like "use strict";, which aren't really part of the computation,
-- and have to be at the front
splitDirectivePrelude
  :: forall a. (Default a, AnnotationInfo a)
  => [F.JSTermAnn a F.JSStatementL] -> ([String], [F.JSTermAnn a F.JSStatementL])
splitDirectivePrelude []     = ([], [])
splitDirectivePrelude (s:ss) = case toDirective s of
                                 Nothing  -> ([], s:ss)
                                 Just dir -> (splitDirectivePrelude ss) & _1 %~ (dir:)
  where
    toDirective :: F.JSTermAnn a F.JSStatementL -> Maybe String
    toDirective (project -> Just (F.JSExpressionStatement (project -> Just (F.JSStringLiteral _ str)) _)) = Just str
    toDirective _ = Nothing

translateBlockStmts
  :: forall a. (Default a, AnnotationInfo a)
  => F.JSTermAnn a [F.JSStatementL] -> a -> MJSTermAnn a F.JSBlockL
translateBlockStmts ss a = BlockWithPrelude dirPrelude block ::&:: a
  where
    (dirPrelude, blockStmts) = splitDirectivePrelude $ extractF ss

    block :: MJSTermAnn a BlockL
    block = Block (insertF $ map (injFAnnDef . translate) blockStmts) jEmptyBlockEnd ::&:: a

translateBlock :: (Default a, AnnotationInfo a) => F.JSTermAnn a F.JSBlockL -> MJSTermAnn a F.JSBlockL
translateBlock (F.JSBlock _ ss _ ::&:: a) = translateBlockStmts ss a

translateFunCall
  :: (Default a, AnnotationInfo a)
  => F.JSTermAnn a F.JSExpressionL
  -> F.JSTermAnn a (F.JSCommaList F.JSExpressionL)
  -> a
  -> MJSTermAnn a FunctionCallL
-- The outer 'FunctionCall' carries the parent's whole-call annotation 'a',
-- but the 'FunctionArgumentList' sub-node must NOT inherit it — that would
-- make the args list claim a span covering the function expression too.
-- We propagate the JSCommaList's own annotation instead, which spans only
-- the arguments (empty arg lists fall back to 'def').
translateFunCall e args a =
  FunctionCall jEmptyFunctionCallAttrs
               (injFAnnDef $ translate e)
               (FunctionArgumentList (insertF $ map (\x -> PositionalArgument (injFAnnDef $ translate x) ::&:: def)
                                                    (jsCommaListToList args))
                  ::&:: getAnn args)
    ::&:: a

instance Trans F.JSExpression where
  trans (F.JSIdentifier _ n :&: a)                     = injectFAnnDef (Ident n :&: a)
  trans (F.JSAssignExpression lhs op rhs :&: a)        = translateAssign lhs op rhs a
  trans (F.JSFunctionExpression _ na _ params _ block :&: a) =
      F.JSFunctionExpression noAnnA (translate na) noAnnA (translate params) noAnnA (translateBlock block) ::&:: a
  trans (F.JSMemberExpression f _ args _ :&: a)        = injFAnnDef $ translateFunCall f args a
  trans (F.JSCallExpression   f _ args _ :&: a)        = injFAnnDef $ translateFunCall f args a
  trans x                                              = transDefault x

transIdent :: (Default a, AnnotationInfo a) => F.JSTermAnn a F.JSIdentL -> MJSTermAnn a IdentL
transIdent (F.JSIdentName _ n ::&:: a) = Ident n ::&:: a
transIdent (project -> (Just F.JSIdentNone)) = error "JSIdentNone discovered where identifier was required"


instance Trans F.JSIdent where
  trans (F.JSIdentName _ n :&: a) = injFAnnDef $ JustF (Ident n ::&:: a) ::&:: a
  trans (F.JSIdentNone     :&: a) = injFAnnDef $ (NothingF ::&:: a :: MJSTermAnn _ (Maybe IdentL))

varInitToDecl :: (Default a, AnnotationInfo a) => F.JSTermAnn a F.JSExpressionL -> MJSTermAnn a SingleLocalVarDeclL
varInitToDecl (F.JSVarInitExpression lhs init ::&:: a) =
          SingleLocalVarDecl jEmptyLocalVarDeclAttrs (injFAnnDef $ translate lhs) rhs ::&:: a
  where
    rhs = case init of
      (F.JSVarInit _ e ::&:: rA) -> JustLocalVarInit (injFAnnDef $ translate e) ::&:: rA
      (F.JSVarInitNone ::&:: rA) -> NoLocalVarInit ::&:: rA
      _ -> error "varInitToDecl: JSVarInitializer subterm is neither JSVarInit nor JSVarInitNone"

extractVarDecls
  :: (Default a, AnnotationInfo a)
  => F.JSTermAnn a (F.JSCommaList F.JSExpressionL) -> MJSTermAnn a [SingleLocalVarDeclL]
extractVarDecls commaList = insertF $ map varInitToDecl exps
  where
    exps = jsCommaListToList commaList

transCommaExpList
  :: (Default a, AnnotationInfo a)
  => F.JSTermAnn a (F.JSCommaList F.JSExpressionL) -> MJSTermAnn a [F.JSExpressionL]
transCommaExpList = insertF . map translate . jsCommaListToList

transParams
  :: (Default a, AnnotationInfo a)
  => F.JSTermAnn a (F.JSCommaList F.JSIdentL) -> MJSTermAnn a [FunctionParameterL]
transParams args = insertF $ map (makeParam . transIdent) (jsCommaListToList args)
  where
    makeParam :: (Default a, AnnotationInfo a) => MJSTermAnn a IdentL -> MJSTermAnn a FunctionParameterL
    makeParam n = PositionalParameter jEmptyParameterAttrs n ::&:: def

instance Trans F.JSStatement where
  trans (F.JSVariable _ exprs _ :&: a) =
      injFAnnDef $ MultiLocalVarDecl jEmptyMultiLocalVarDeclCommonAttrs (extractVarDecls exprs) ::&:: a
  trans (F.JSFor _ _ init _ exp _ upd _ s :&: a) =
      injFAnnDef $ JSFor (transCommaExpList init) (transCommaExpList exp) (transCommaExpList upd) (translate s) ::&:: a
  trans (F.JSForIn _ _ v op exp _ s :&: a) =
      injFAnnDef $ JSForIn (translate v) (translate op) (translate exp) (translate s) ::&:: a
  trans (F.JSForVar _ _ _ init _ exp _ upd _ s :&: a) =
      injFAnnDef $ JSForVar (extractVarDecls init) (transCommaExpList exp) (transCommaExpList upd) (translate s) ::&:: a
  trans (F.JSForVarIn _ _ _ v op exp _ s :&: a) =
      injFAnnDef $ JSForVarIn (varInitToDecl v) (translate op) (translate exp) (translate s) ::&:: a
  trans (F.JSConstant _ _ _ :&: _) = error "ES7 Const not supported"
  -- The inner assign-expression's annotation is the parent's annotation
  -- /minus/ the trailing semi position; we don't have a generic way to
  -- subtract that off, so we fall back to 'def' here. The
  -- 'JSExpressionStatement' wrapper still carries the statement-level
  -- span, which is what a reparse-at-JSStatementL target needs.
  trans (F.JSAssignStatement lhs op rhs _ :&: a) =
      F.JSExpressionStatement (translateAssign lhs op rhs def) semiA ::&:: a
  trans (F.JSFunction _ n _ args _ block _ :&: a) =
      injFAnnDef $ FunctionDef jEmptyFunctionDefAttrs (transIdent n) (transParams args) (injFAnnDef $ translateBlock block) ::&:: a
  -- See note on JSAssignStatement: the inner call-expression's
  -- annotation comes out as 'def' rather than the parent's, so a
  -- reparse target at JSExpressionL doesn't include the trailing
  -- @;@ in its slice.
  trans (F.JSMethodCall f _ args _ _ :&: a) =
      F.JSExpressionStatement (injFAnnDef $ translateFunCall f args def) semiA ::&:: a
  trans x = transDefault x

instance Trans F.JSAST where
  trans (F.JSAstProgram stmts _ :&: a) =
      injFAnnDef $ translateBlockStmts stmts a
  trans x = transDefault x

-------------------------------------------------------------------------------

class Untrans f where
  untrans :: f MJSTerm l -> F.JSTerm l

instance {-# OVERLAPPING #-} (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt @Untrans untrans

untransError :: (HFunctor f, f :-<: MJSSig) => f MJSTerm l -> F.JSTerm l
untransError t = error $ "Cannot untranslate root node: " ++ (show $ inject t)

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
