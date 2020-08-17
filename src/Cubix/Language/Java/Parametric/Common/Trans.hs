{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -freduction-depth=200 #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PartialTypeSignatures    #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ViewPatterns             #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Java.Parametric.Common.Trans () where
#else
module Cubix.Language.Java.Parametric.Common.Trans (
    translate
  , untranslate
  ) where

import Data.List( (\\) )
import Data.Maybe ( fromJust )
import Data.Proxy
import Data.Typeable ( Typeable )
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Data.Comp.Multi ( unTerm , inject, project, HFunctor, hfmap, (:-<:), All, Sum, caseCxt )

import Cubix.Language.Java.Parametric.Common.Types
import qualified Cubix.Language.Java.Parametric.Full as F
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax

-- Problem: Some parts of the translation can tear down multiple layers. A VarDeclID
-- is only translated when its parent VarDecl is translated. It's nonsense to just be asked
-- to translate a VarDeclID on its own.
-- 
-- I did want to be able to avoid writing cases that throw errors,
-- while being able to have the type system know that the translation is total.
-- Inspired by how you'd write a hand-traversal to do this, I think the approach to doing this is to have
-- a type-level set of labels at which you can apply the transformation. A translation instance for VarDecl might take the form
--
-- instance (Trans (IdentL :@: ls)) => Trans (IdentL :@: VarDeclL :@: ls) where....
--
-- I think Haskell could do this if it had AC-matching (errr....AC+idempotent) in its typeclass resolver. That would let it infer
-- a fixpoint for the set of labels at which you could apply the translation.
--
-- Overall, this sounds quite infeasible without ACI-matching. At the very least, it seems painful, especially if
-- you have to hand-write the set of applicable labels


translate :: F.JavaTerm l -> MJavaTerm l
translate = trans . unTerm

translate' :: (InjF MJavaSig l l') => F.JavaTerm l -> MJavaTerm l'
translate' = injF . translate

class Trans f where
  trans :: f F.JavaTerm l -> MJavaTerm l

instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt (Proxy @Trans) trans

transDefault :: (HFunctor f, f :-<: MJavaSig, f :-<: F.JavaSig) => f F.JavaTerm l -> MJavaTerm l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MJavaSig, f :-<: F.JavaSig) => Trans f where
  trans = transDefault

transIdent :: F.JavaTerm F.IdentL -> MJavaTerm IdentL
transIdent (project -> Just (F.Ident s)) = Ident' s

-- Clone of transIdent because type-safe pattern match
instance Trans F.Ident where
  trans (F.Ident n) = iIdent n

splitVarDeclId :: F.JavaTerm F.VarDeclIdL -> (Int, MJavaTerm IdentL)
splitVarDeclId (project -> Just (F.VarId ident))      = (0, transIdent ident)
splitVarDeclId (project -> Just (F.VarDeclArray sub)) = (dim + 1, id)
  where
    (dim, id) = splitVarDeclId sub
                                                          

transSingleDecl :: F.JavaTerm F.VarDeclL -> MJavaTerm SingleLocalVarDeclL
transSingleDecl (project -> Just (F.VarDecl vid init)) = SingleLocalVarDecl' (iArrayDimVarDeclAttrs dim) (injF id) optInit
  where
    (dim, id) = splitVarDeclId vid

    optInit :: MJavaTerm OptLocalVarInitL
    optInit = case init of
      Just' x  -> JustLocalVarInit' $ injF $ translate x
      Nothing' -> NoLocalVarInit'
                                                                                                                   

instance {-# OVERLAPPING #-} Trans F.BlockStmt where
  trans t@(F.BlockStmt _)              = transDefault t
  trans t@(F.LocalClass _)             = transDefault t
  trans t@(F.LocalVars mods typ decls) = iMultiLocalVarDecl (injF $ translate (riPairF mods typ)) $ mapF transSingleDecl decls

transOp :: F.JavaTerm F.AssignOpL -> MJavaTerm AssignOpL
transOp (project -> (Just F.EqualA)) = AssignOpEquals'
transOp x                            = injF $ translate x

instance {-# OVERLAPPING #-} Trans F.Exp where
  trans t@(F.Assign lhs op rhs) = iAssign (injF $ translate lhs) (transOp op) (injF $ translate rhs)
  trans t                       = transDefault t

instance {-# OVERLAPPING #-} Trans F.MethodInvocation where
  trans (F.MethodCall nam args) = iFunctionCall (iJavaTypeArgs riNilF)
                                                f
                                                (FunctionArgumentList' $ ConsF' (ReceiverArg' iImplicitReceiver)
                                                                                (mapF (injF . translate) args))
    where
      f = case nam of
            (project -> Just (F.Name (SingletonF' n))) -> injF $ transIdent n
            _                                          -> injF $ translate nam
  trans (F.PrimaryMethodCall e targs n args) = iFunctionCall (iJavaTypeArgs $ translate targs)
                                                             (injF $ transIdent n)
                                                             (FunctionArgumentList' $ ConsF' (ReceiverArg' $ iPrimaryReceiver $ translate e)
                                                                                                             (mapF (injF . translate) args))
  trans (F.SuperMethodCall targs n args)     = iFunctionCall (iJavaTypeArgs $ translate targs)
                                                             (injF $ transIdent n)
                                                             (FunctionArgumentList' $ ConsF' (ReceiverArg' $ iSuperReceiver)
                                                                                             (mapF (injF . translate) args))
  trans (F.ClassMethodCall typ targs n args) = iFunctionCall (iJavaTypeArgs $ translate targs)
                                                             (injF $ transIdent n)
                                                             (FunctionArgumentList' $ ConsF' (ReceiverArg' $ iClassSuperReceiver $ translate typ)
                                                                                             (mapF (injF . translate) args))
  trans (F.TypeMethodCall  typ targs n args) = iFunctionCall (iJavaTypeArgs $ translate targs)
                                                             (injF $ transIdent n)
                                                             (FunctionArgumentList' $ ConsF' (ReceiverArg' $ iTypeReceiver $ translate typ)
                                                                                                             (mapF (injF . translate) args))

transBlock :: F.JavaTerm F.BlockL -> MJavaTerm BlockL
transBlock (project -> Just (F.Block stmts)) = Block' (mapF (injF.translate) stmts) EmptyBlockEnd'

instance {-# OVERLAPPING #-} Trans F.Block where
  trans b@(F.Block _) = injF $ transBlock $ inject b

transParamDecl :: F.JavaTerm F.FormalParamL -> MJavaTerm FunctionParameterDeclL
transParamDecl (project -> Just (F.FormalParam mods tp isVarargs vid)) =
    if isVarargs then
      iJavaVarargsParam attrs n
    else
      iPositionalParameterDeclWithIdent (injF attrs) n
  where
    (dim, n) = splitVarDeclId vid

    attrs :: MJavaTerm JavaParamAttrsL
    attrs = iJavaParamAttrs (translate mods) (translate tp) dim


-- Yes, this is almost copy+paste of the previous function. It's
-- not obvious how to share code effectively between them. Basically, the issue
-- is that the ParameterDecl/Parameter type hierarchy is identical for Java, but we've
-- split them because they're in general different (i.e.: in C)
transParam :: F.JavaTerm F.FormalParamL -> MJavaTerm FunctionParameterL
transParam (project -> Just (F.FormalParam mods tp isVarargs vid)) =
    if isVarargs then
      iJavaVarargsParam attrs n
    else
      iPositionalParameter (injF attrs) n
  where
    (dim, n) = splitVarDeclId vid

    attrs :: MJavaTerm JavaParamAttrsL
    attrs = iJavaParamAttrs (translate mods) (translate tp) dim

-- The Nothing' field is for default methods; Java 8 only
instance {-# OVERLAPPING #-} Trans F.MemberDecl where
  trans (F.MethodDecl mods tparams typ n params ex Nothing' body) =
      case project body of
        Just (F.MethodBody Nothing') -> iFunctionDecl (injF attrs) (transIdent n) (addUnlessStatic SelfParameterDecl' $ mapF transParamDecl params)
        Just (F.MethodBody (Just' b)) -> iFunctionDef  (injF attrs) (transIdent n) (addUnlessStatic SelfParameter' $ mapF transParam params) (injF $ transBlock b)
    where
      attrs :: MJavaTerm JavaMethodDeclAttrsL
      attrs = iJavaMethodDeclAttrs (translate mods) (translate tparams) (translate typ) (translate ex)

      addUnlessStatic :: Typeable l => MJavaTerm l -> MJavaTerm [l] -> MJavaTerm [l]
      addUnlessStatic x l = if elem F.iStatic (extractF mods) then
                              l
                            else
                              ConsF' x l

  trans t = transDefault t

instance {-# OVERLAPPING #-} Trans F.MethodBody where
  trans = error "MethodBody found not within MethodDecl"

untranslate :: MJavaTerm l -> F.JavaTerm l
untranslate = untrans . unTerm

class Untrans f where
  untrans :: f MJavaTerm l -> F.JavaTerm l

instance {-# OVERLAPPING #-} (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt (Proxy @Untrans) untrans

untransDefault :: (HFunctor f, f :-<: F.JavaSig) => f MJavaTerm l -> F.JavaTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: F.JavaSig) => Untrans f where
  untrans = untransDefault

untransIdent :: MJavaTerm IdentL -> F.JavaTerm F.IdentL
untransIdent (Ident' s) = F.iIdent s

instance {-# OVERLAPPING #-} Untrans IdentIsIdent where
  untrans (IdentIsIdent n) = untransIdent n

combineVarDeclId :: Int -> MJavaTerm IdentL -> F.JavaTerm F.VarDeclIdL
combineVarDeclId 0 id = F.iVarId $ untransIdent id
combineVarDeclId n id = F.iVarDeclArray (combineVarDeclId (n-1) id)

untransSingleDecl :: MJavaTerm SingleLocalVarDeclL -> F.JavaTerm F.VarDeclL
untransSingleDecl (SingleLocalVarDecl' attrs id optInit) = F.iVarDecl vid vinit
  where
    (project -> Just (ArrayDimVarDeclAttrs dim)) = attrs
    vid = combineVarDeclId dim $ fromProjF id

    vinit :: F.JavaTerm (Maybe F.VarInitL)
    vinit = case optInit of
      JustLocalVarInit' e -> Just' $ untranslate $ fromJust $ projF (e :: MJavaTerm LocalVarInitL)
      NoLocalVarInit'     -> Nothing'
    

instance {-# OVERLAPPING #-} Untrans MultiLocalVarDeclIsBlockStmt where
  untrans (MultiLocalVarDeclIsBlockStmt (MultiLocalVarDecl' attrs decls)) = F.iLocalVars (untranslate mods) (untranslate typ) $ mapF untransSingleDecl decls
    where
      (mods, typ) = extractF2 $ fromProjF attrs

untransOp :: MJavaTerm AssignOpL -> F.JavaTerm F.AssignOpL
untransOp AssignOpEquals' = F.iEqualA
untransOp x               = untranslate $ fromProjF x

instance {-# OVERLAPPING #-} Untrans AssignIsExp where
  untrans (AssignIsExp (Assign' lhs op rhs)) = F.iAssign (untranslate $ fromProjF lhs) (untransOp op) (untranslate $ fromProjF rhs)

untransBlock :: MJavaTerm BlockL -> F.JavaTerm F.BlockL
untransBlock (Block' items _) = F.iBlock $ mapF (untranslate.fromProjF) items

instance {-# OVERLAPPING #-} Untrans BlockIsBlock where
  untrans (BlockIsBlock b) = untransBlock b

instance {-# OVERLAPPING #-} Untrans FunctionCallIsMethodInvocation where
  untrans (FunctionCallIsMethodInvocation (FunctionCall' (project -> Just (JavaTypeArgs targs))
                                                         (FunctionIdent' n)
                                                         (FunctionArgumentList' (ConsF' (ReceiverArg' rec) args)))) =
      case project rec of
        Just ImplicitReceiver -> case targs of
          NilF' -> F.iMethodCall (F.iName $ SingletonF' n') args'
          _     -> error "Illegal Java term constructed: type args passed to implicit receiver"
        Just (PrimaryReceiver e)    -> F.iPrimaryMethodCall (untranslate e) (untranslate targs) n' args'
        Just SuperReceiver          -> F.iSuperMethodCall                   (untranslate targs) n' args'
        Just (ClassSuperReceiver c) -> F.iClassMethodCall (untranslate c)   (untranslate targs) n' args'
        Just (TypeReceiver c)       -> F.iTypeMethodCall  (untranslate c)   (untranslate targs) n' args'
    where
      n' = untransIdent n
      args' = mapF (untranslate.fromProjF) args

untransParamDecl :: MJavaTerm FunctionParameterDeclL -> F.JavaTerm F.FormalParamL
untransParamDecl (projF -> Just (JavaVarargsParam'               (JavaParamAttrs' mods tp dim) n)) = F.iFormalParam (untranslate mods) (untranslate tp) True  (combineVarDeclId dim n)
untransParamDecl (PositionalParameterDeclWithIdent' (fromProjF -> JavaParamAttrs' mods tp dim) n)  = F.iFormalParam (untranslate mods) (untranslate tp) False (combineVarDeclId dim n)

untransParam :: MJavaTerm FunctionParameterL -> F.JavaTerm F.FormalParamL
untransParam (projF -> Just (JavaVarargsParam'  (JavaParamAttrs' mods tp dim) n)) = F.iFormalParam (untranslate mods) (untranslate tp) True  (combineVarDeclId dim n)
untransParam (PositionalParameter' (fromProjF -> JavaParamAttrs' mods tp dim) n)  = F.iFormalParam (untranslate mods) (untranslate tp) False (combineVarDeclId dim n)


instance {-# OVERLAPPING #-} Untrans FunctionDeclIsMemberDecl where
  untrans (FunctionDeclIsMemberDecl (FunctionDecl' (project -> Just (JavaMethodDeclAttrsIsFunctionDeclAttrs (JavaMethodDeclAttrs' mods tparams tp ex)))
                                                   n
                                                   params))
           = F.iMethodDecl (untranslate mods)
                           (untranslate tparams)
                           (untranslate tp)
                           (untransIdent n)
                           (insertF $ map untransParamDecl (dropWhile (== SelfParameterDecl') $ extractF params))
                           (untranslate ex)
                           Nothing'
                           (F.iMethodBody Nothing')

instance {-# OVERLAPPING #-} Untrans FunctionDefIsMemberDecl where
  untrans (FunctionDefIsMemberDecl (FunctionDef' (project -> Just (JavaMethodDeclAttrsIsFunctionDefAttrs (JavaMethodDeclAttrs' mods tparams tp ex)))
                                                  n
                                                  params
                                                  body))
           = F.iMethodDecl (untranslate mods)
                           (untranslate tparams)
                           (untranslate tp)
                           (untransIdent n)
                           (insertF $ map untransParam (dropWhile (== SelfParameter') $ extractF params))
                           (untranslate ex)
                           Nothing'
                           (F.iMethodBody (Just' (untransBlock $ fromProjF body)))

untransError :: (HFunctor f, f :-<: MJavaSig) => f MJavaTerm l -> F.JavaTerm l
untransError t = error $ "Cannot untranslate root node: " ++ (show $ (inject t :: MJavaTerm _))

do ipsNames <- sumToNames ''MJavaSig
   modNames <- sumToNames ''F.JavaSig
   let targTs = map ConT $ (ipsNames \\ modNames) \\ [ ''BlockIsBlock, ''AssignIsExp, ''MultiLocalVarDeclIsBlockStmt, ''IdentIsIdent
                                                     , ''FunctionCallIsMethodInvocation, ''FunctionDeclIsMemberDecl, ''FunctionDefIsMemberDecl]
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)



#endif
