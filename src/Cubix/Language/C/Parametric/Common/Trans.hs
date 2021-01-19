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
module Cubix.Language.C.Parametric.Common.Trans () where
#else
module Cubix.Language.C.Parametric.Common.Trans (
    translate
  , untranslate
  ) where

import Control.Monad.Identity ( Identity(..) )

import Data.Monoid ( Any(..) )
import Data.Proxy
import Data.List( (\\) )
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Data.Comp.Multi ( project, inject, unTerm, caseCxt, Sum, All, HFunctor(..), (:-<:) )
import Data.Comp.Multi.Strategic ( crushtdT, addFail, promoteTF )

import qualified Language.C as COrig

import Cubix.Language.C.Parametric.Common.Types
import qualified Cubix.Language.C.Parametric.Full as F
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax

translate :: F.CTerm l -> MCTerm l
translate = trans . unTerm

translate' :: (InjF MCSig l l') => F.CTerm l -> MCTerm l'
translate' = injF . translate

class Trans f where
  trans :: f F.CTerm l -> MCTerm l

instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt (Proxy @Trans) trans

transDefault :: (HFunctor f, f :-<: MCSig, f :-<: F.CSig) => f F.CTerm l -> MCTerm l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MCSig, f :-<: F.CSig) => Trans f where
  trans = transDefault

-- Damn language-c mixing in annotations. We drop them for the pure tree
transIdent :: F.CTerm F.IdentL -> MCTerm IdentL
transIdent (project -> Just (F.Ident s _ _)) = Ident' s

-- Clone of transIdent because type-safe pattern match
instance Trans F.Ident where
  trans (F.Ident n _ _) = iIdent n

transSingleDec :: F.CTerm (Maybe F.CDeclaratorL, Maybe F.CInitializerL, Maybe F.CExpressionL) ->  MCTerm SingleLocalVarDeclL
transSingleDec (extractF3 -> (declaratorOpt, initOpt, bitFieldOpt)) = checkBitField `seq` SingleLocalVarDecl' localOpts id init
  where
    checkBitField = case bitFieldOpt of
      Just' _  -> error "Attempting to translate bitfield var declaration as if it were a local decl"
      Nothing' -> ()

    (id, localOpts) = case declaratorOpt of
      Just' (project -> Just (F.CDeclr (Just' vid) decls asmName attrs _)) -> (injF $ transIdent vid, iCLocalVarAttrs (translate decls) (translate asmName) (translate attrs))
      _ -> error "Attempting to translate nameless bitfield var declaration as if it were a local decl"

    init = case initOpt of
      Nothing' -> NoLocalVarInit'
      Just' x  -> JustLocalVarInit' $ injF $ translate x

containsStatic :: F.CTerm F.CDeclarationL -> Bool
containsStatic = getAny . runIdentity . (crushtdT $ promoteTF $ addFail isStatic)
  where
    isStatic :: (Monad m) => F.CTerm F.CStorageSpecifierL -> m Any
    isStatic (project -> Just (F.CStatic _)) = return $ Any True
    isStatic _                               = return $ Any False

transMultiDec :: F.CTerm F.CDeclarationL -> MCTerm MultiLocalVarDeclL
transMultiDec (project -> Just (F.CDecl specs single_decs _)) =
        MultiLocalVarDecl' (injF $ translate specs) (mapF transSingleDec single_decs)

instance {-# OVERLAPPING #-} Trans F.CCompoundBlockItem where
  trans t@(F.CBlockStmt _)    = transDefault t
  trans t@(F.CNestedFunDef _) = transDefault t
  trans t@(F.CBlockDecl decl) =
    if containsStatic decl then
      transDefault t
    else
      injF $ transMultiDec decl

transOp :: F.CTerm F.CAssignOpL -> MCTerm AssignOpL
transOp (project -> Just F.CAssignOp) = AssignOpEquals'
transOp x                             = injF $ translate x

translateArgs :: F.CTerm [F.CExpressionL] -> MCTerm FunctionArgumentsL
translateArgs args = FunctionArgumentList' $ mapF (PositionalArgument' . injF . translate) args

instance {-# OVERLAPPING #-} Trans F.CExpression where
  trans t@(F.CAssign op lhs rhs _) = iAssign (injF $ translate lhs) (transOp op) (injF $ translate rhs)
  trans t@(F.CCall f args _)       = iFunctionCall EmptyFunctionCallAttrs' (injF $ translate f) (translateArgs args)
  trans t                          = transDefault t

transForInit :: F.CTerm (Either (Maybe F.CExpressionL) F.CDeclarationL) -> MCTerm CForInitL
transForInit (Left' Nothing')  = iCForInitEmpty
transForInit (Left' (Just' e)) = iCForInitExp $ translate e
transForInit (Right' dec)      = iCForInitDecl $ transMultiDec dec

instance {-# OVERLAPPING #-} Trans F.CStatement where
  trans t@(F.CCompound ids items _)        = iCLabeledBlock (mapF transIdent ids) (Block' (mapF (injF.translate) items) EmptyBlockEnd')
  trans t@(F.CFor init cond update body _) = iCFor (transForInit init) (translate cond) (translate update) (translate body)
  trans t                                  = transDefault t

paramDeclFromId :: F.CTerm F.IdentL -> MCTerm FunctionParameterDeclL
paramDeclFromId n = PositionalParameterDeclOptionalIdent' (iCFunParamAttrs NilF' NilF' Nothing' NilF') (Just' $ transIdent n)

paramDeclFromDecl :: F.CTerm F.CDeclarationL -> MCTerm FunctionParameterDeclL
paramDeclFromDecl (project -> Just (F.CDecl (SingletonF' (project -> Just (F.CTypeSpec (project -> Just (F.CVoidType _))))) NilF' _)) = iCVoidArg

paramDeclFromDecl (project -> Just (F.CDecl dss inf _)) =
  case inf of
    NilF' -> PositionalParameterDeclOptionalIdent' (iCFunParamAttrs (translate dss) NilF' Nothing' NilF') Nothing'
    (SingletonF' (TripleF' (Just' (project -> Just (F.CDeclr nOpt dds asmNm attrs _))) Nothing' Nothing')) ->
                PositionalParameterDeclOptionalIdent' (iCFunParamAttrs (translate dss) (translate dds) (translate asmNm) (translate attrs)) (mapF transIdent nOpt)

instance {-# OVERLAPPING #-} Trans F.CDeclarator where
  trans t@(F.CDeclr (Just' n) (ConsF' (project -> Just (F.CFunDeclr pars attrs1 _)) dds) asmNm attrs2 _) =
    iFunctionDecl (iCFunDeclAttrs (translate dds) (translate attrs1) (translate asmNm) (translate attrs2))
                  (transIdent n)
                  (case pars of
                    Left' ps                                -> mapF paramDeclFromId ps
                    Right' (PairF' decls (BoolF' isVarArg)) -> insertF $ (map paramDeclFromDecl $ extractF decls) ++ (if isVarArg then [iCVarArgParam] else []))
  trans t = transDefault t

-- Duplication is better than the wrong abstraction
-- Problem here is that I don't really know how to abstract these overlapping trees.
-- I think the solution is going to be some way to "partially merge" the param decl/param hierarchies
paramFromDecl :: F.CTerm F.CDeclarationL -> MCTerm FunctionParameterL
paramFromDecl (project -> Just (F.CDecl (SingletonF' (project -> Just (F.CTypeSpec (project -> Just (F.CVoidType _))))) NilF' _)) = iCVoidArg
paramFromDecl (project -> Just (F.CDecl dss (SingletonF' (TripleF' declOpt Nothing' Nothing')) _)) =
  case declOpt of
    Nothing' -> error "Unnamed parameter in function definition"
    Just' (project -> Just (F.CDeclr (Just' n) dds asmNm attrs _)) ->
                PositionalParameter' (iCFunParamAttrs (translate dss) (translate dds) (translate asmNm) (translate attrs)) (transIdent n)

instance {-# OVERLAPPING #-} Trans F.CFunctionDef where
  trans (F.CFunDef dss (project -> Just (F.CDeclr (Just' n)
                                                  (ConsF' (project -> Just (F.CFunDeclr pars attrs1 _)) dds)
                                                  asmNm attrs2 _))
                   oldStyleDecls body _) =
      iFunctionDef (iCFunDefAttrs (translate dss)
                                  (iCFunDeclAttrs (translate dds)
                                                  (translate attrs1)
                                                  (translate asmNm)
                                                  (translate attrs2))
                                  (translate oldStyleDecls))
                   (transIdent n)
                   (case pars of
                     Left' ps -> mapF (iCOldStyleParam.transIdent) ps
                     Right' (PairF' decls (BoolF' isVarArg)) -> insertF $ (map paramFromDecl $ extractF decls) ++ (if isVarArg then [iCVarArgParam] else []))
                   (injF $ translate body)

untranslate :: MCTerm l -> F.CTerm l
untranslate = untrans . unTerm

class Untrans f where
  untrans :: f MCTerm l -> F.CTerm l

instance {-# OVERLAPPING #-} (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt (Proxy @Untrans) untrans

untransDefault :: (HFunctor f, f :-<: F.CSig) => f MCTerm l -> F.CTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: F.CSig) => Untrans f where
  untrans = untransDefault

dummyNodeInfo :: F.CTerm F.NodeInfoL
dummyNodeInfo = F.translateNodeInfo $ COrig.mkNodeInfoOnlyPos COrig.nopos

untransIdent :: MCTerm IdentL -> F.CTerm F.IdentL
untransIdent (Ident' s) = F.iIdent s 0 dummyNodeInfo

instance {-# OVERLAPPING #-} Untrans IdentIsIdent where
  untrans (IdentIsIdent n) = untransIdent n

untransSingleDec :: MCTerm SingleLocalVarDeclL -> F.CTerm (Maybe F.CDeclaratorL, Maybe F.CInitializerL, Maybe F.CExpressionL)
untransSingleDec (SingleLocalVarDecl' attrs id init) = riTripleF (Just' declarator) cinit Nothing'
  where
    Just (CLocalVarAttrs decs asmName cAttrs) = project attrs

    declarator = F.iCDeclr (Just' $ untransIdent $ fromProjF id) (untranslate decs) (untranslate asmName) (untranslate cAttrs) iUnitF

    cinit = case init of
      NoLocalVarInit'     -> Nothing'
      JustLocalVarInit' x -> Just' $ untranslate $ fromProjF x

untransMultiDec :: MCTerm MultiLocalVarDeclL -> F.CTerm F.CDeclarationL
untransMultiDec (MultiLocalVarDecl' attrs decls) = F.iCDecl (untranslate $ fromProjF attrs) (mapF untransSingleDec decls) iUnitF

instance {-# OVERLAPPING #-} Untrans MultiLocalVarDeclIsCCompoundBlockItem where
  untrans (MultiLocalVarDeclIsCCompoundBlockItem dec) = F.iCBlockDecl $ untransMultiDec dec

untransOp :: MCTerm AssignOpL -> F.CTerm F.CAssignOpL
untransOp AssignOpEquals' = F.iCAssignOp
untransOp x               = untranslate $ fromProjF x

instance {-# OVERLAPPING #-} Untrans AssignIsCExpression where
  untrans (AssignIsCExpression (Assign' lhs op rhs)) = F.iCAssign (untransOp op) (untranslate $ fromProjF lhs)  (untranslate $ fromProjF rhs) iUnitF

instance {-# OVERLAPPING #-} Untrans CLabeledBlock where
  untrans (CLabeledBlock ids (Block' items _)) = F.iCCompound (mapF untransIdent ids) (mapF (untranslate.fromProjF) items) iUnitF

untransForInit :: MCTerm CForInitL -> F.CTerm (Either (Maybe F.CExpressionL) F.CDeclarationL)
untransForInit (project -> Just CForInitEmpty)      = Left' Nothing'
untransForInit (project -> Just (CForInitExp  e))   = Left' (Just' $ untranslate e)
untransForInit (project -> Just (CForInitDecl dec)) = Right' $ untransMultiDec dec

instance {-# OVERLAPPING #-} Untrans CFor where
  untrans (CFor init cond update body) = F.iCFor (untransForInit init) (untranslate cond) (untranslate update) (untranslate body) iUnitF

untranslateArg :: MCTerm FunctionArgumentL -> F.CTerm F.CExpressionL
untranslateArg (PositionalArgument' a) = untranslate $ fromProjF a

instance {-# OVERLAPPING #-} Untrans FunctionCallIsCExpression where
  untrans (FunctionCallIsCExpression (FunctionCall' EmptyFunctionCallAttrs' f (FunctionArgumentList' args))) = F.iCCall (untranslate $ fromProjF f) (mapF untranslateArg args) iUnitF

instance {-# OVERLAPPING #-} Untrans FunctionDeclIsCDeclarator where
  untrans (FunctionDeclIsCDeclarator (FunctionDecl' (CFunDeclAttrs' dds attrs1 asmNm attrs2) n pars)) =
      F.iCDeclr (Just' $ untransIdent n) (ConsF' (F.iCFunDeclr (Right' $ untransPars pars) (untranslate attrs1) iUnitF) (untranslate dds)) (untranslate asmNm) (untranslate attrs2) iUnitF
    where
      untransPars :: MCTerm [FunctionParameterDeclL] -> F.CTerm ([F.CDeclarationL], BoolL)
      untransPars t = let (ds, b) = go (extractF t) ([], False) in PairF' (insertF $ reverse ds) (BoolF' b)
        where
          go :: [MCTerm FunctionParameterDeclL] -> ([F.CTerm F.CDeclarationL], Bool) -> ([F.CTerm F.CDeclarationL], Bool)
          go ((PositionalParameterDeclOptionalIdent' (fromProjF -> (CFunParamAttrs' dss NilF' Nothing' NilF')) Nothing') : ps) (l, b) =
               go ps (F.iCDecl (untranslate dss) NilF' iUnitF : l, b)
          go ((PositionalParameterDeclOptionalIdent' (fromProjF -> (CFunParamAttrs' dss dds asmNm attrs)) nOpt) : ps) (l, b) =
               go ps ((F.iCDecl (untranslate dss) (SingletonF' (TripleF' (Just' $ F.iCDeclr (mapF untransIdent nOpt) (untranslate dds) (untranslate asmNm) (untranslate attrs) iUnitF) Nothing' Nothing')) iUnitF) : l, b)
          go [fromProjF -> CVoidArg'] ([], b) = ([F.iCDecl (SingletonF' $ F.iCTypeSpec (F.iCVoidType iUnitF)) NilF' iUnitF], False)
          go [fromProjF -> CVarArgParam'] (l, b) = (l, True)
          go [] (l, b) = (l, b)

instance {-# OVERLAPPING #-} Untrans FunctionDefIsCFunctionDef where
  untrans (FunctionDefIsCFunctionDef
              (FunctionDef' (CFunDefAttrs' dss
                                           (CFunDeclAttrs' dds attrs1 asmNm attrs2)
                                           oldStyleParams)
                            n params body)) =
      F.iCFunDef (untranslate dss)
                 (F.iCDeclr (Just' $ untransIdent n)
                            (ConsF' (F.iCFunDeclr (untransPars params) (untranslate attrs1) iUnitF)
                                    (untranslate dds))
                            (untranslate asmNm) (untranslate attrs2) iUnitF)
                 (untranslate oldStyleParams) (untranslate $ fromProjF body) iUnitF
    where
      untransPars :: MCTerm [FunctionParameterL] -> F.CTerm (Either [F.IdentL] ([F.CDeclarationL], BoolL))
      untransPars t = case extractF t of
                        [] -> Right' (PairF' NilF' (BoolF' False))
                        ns@((projF -> Just (COldStyleParam' _)) : _) -> Left' (insertF $ map untransOldStyle ns)
                        ns@([projF -> Just CVoidArg']) -> Right' (PairF' (SingletonF' $ F.iCDecl (SingletonF' $ F.iCTypeSpec (F.iCVoidType iUnitF)) NilF' iUnitF) (BoolF' False))
                        ps -> let (ds, b) = go (extractF t) ([], False) in Right' (PairF' (insertF $ reverse ds) (BoolF' b))
        where
          untransOldStyle :: MCTerm FunctionParameterL -> F.CTerm F.IdentL
          untransOldStyle (fromProjF -> COldStyleParam' n) = untransIdent n

          go :: [MCTerm FunctionParameterL] -> ([F.CTerm F.CDeclarationL], Bool) -> ([F.CTerm F.CDeclarationL], Bool)
          go ((PositionalParameter' (fromProjF -> (CFunParamAttrs' dss dds asmNm attrs)) n) : ps) (l, b) =
               go ps ((F.iCDecl (untranslate dss) (SingletonF' (TripleF' (Just' $ F.iCDeclr (Just' $ untransIdent n) (untranslate dds) (untranslate asmNm) (untranslate attrs) iUnitF) Nothing' Nothing')) iUnitF) : l, b)
          go [fromProjF -> CVarArgParam'] (l, b) = (l, True)
          go [] (l, b) = (l, b)

untransError :: (HFunctor f, f :-<: MCSig) => f MCTerm l -> F.CTerm l
untransError t = error $ "Cannot untranslate root node: " ++ (show $ (inject t :: MCTerm _))

do ipsNames <- sumToNames ''MCSig
   modNames <- sumToNames ''F.CSig
   let targTs = map ConT $ (ipsNames \\ modNames) \\ [ ''CLabeledBlock, ''CFor, ''AssignIsCExpression, ''MultiLocalVarDeclIsCCompoundBlockItem, ''IdentIsIdent
                                                     , ''FunctionCallIsCExpression, ''FunctionDeclIsCDeclarator, ''FunctionDefIsCFunctionDef]
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)

#endif
