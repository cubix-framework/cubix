{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures    #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE UndecidableInstances     #-}

module Cubix.Language.Lua.Parametric.Common.Trans (
    translate
  , untranslate
  ) where

import Data.List( (\\) )
import Data.Maybe ( fromJust )
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )
import Data.Text ( pack, unpack )

import Data.Comp.Multi ( project, inject, unTerm, (:-<:), Sum, All, caseCxt
                       , HFunctor(..) )

import Cubix.Language.Lua.Parametric.Common.Types
import qualified Cubix.Language.Lua.Parametric.Full as F
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax

translate :: F.LuaTerm l -> MLuaTerm l
translate = trans . unTerm

class Trans f where
  trans :: f F.LuaTerm l -> MLuaTerm l

instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt @Trans trans

transDefault :: (HFunctor f, f :-<: MLuaSig, f :-<: F.LuaSig) => f F.LuaTerm l -> MLuaTerm l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MLuaSig, f :-<: F.LuaSig) => Trans f where
  trans = transDefault

transIdent :: F.LuaTerm F.NameL -> MLuaTerm IdentL
transIdent (project -> Just (F.Name n)) = Ident' $ unpack n


transBlock :: F.LuaTerm F.BlockL -> MLuaTerm BlockL
transBlock (project -> Just (F.Block s e)) = Block' (mapF (injF.translate) s) (injF $ translate e)

instance Trans F.Block where
  trans b@(F.Block _ _) = injF $ transBlock $ inject b

instance Trans F.Name where
  trans (F.Name n) = iIdent $ unpack n

transParams :: F.LuaTerm [F.NameL] -> Bool -> MLuaTerm [FunctionParameterL]
transParams nms varArg = insertF $ (map makeParam $ extractF nms) ++ (if varArg then [iLuaVarArgsParam] else [])
  where
    makeParam :: F.LuaTerm F.NameL -> MLuaTerm FunctionParameterL
    makeParam n = PositionalParameter' EmptyParameterAttrs' (transIdent n)

splitLuaFunName :: F.LuaTerm F.FunNameL -> (MLuaTerm [IdentL], MLuaTerm IdentL, Bool)
splitLuaFunName (project -> Just (F.FunName n1 n2s n3opt)) =
    case n3opt of
      Just' n3 -> (insertF $ n1' ++ n2s', transIdent n3, True)
      Nothing' -> (insertF $ init (n1' ++ n2s'), last (n1' ++ n2s'), False)
  where
    n1'  = [transIdent n1]
    n2s' = map transIdent $ extractF n2s

instance Trans F.Stat where
  trans (F.Assign vs es) = iAssign (injF $ translate vs) AssignOpEquals' (injF $ translate es)
  trans (F.LocalAssign vs init) = injF $ SingleLocalVarDecl' EmptyLocalVarDeclAttrs' (injF $ mapF transIdent vs) tInit
    where
      tInit = case init of
        Just' x  -> JustLocalVarInit' $ injF $ translate x
        Nothing' -> NoLocalVarInit'
  trans (F.FunAssign fn (project -> Just (F.FunBody pars varArg body))) = iFunctionDef (iLuaFunctionAttrs $ iLuaFunctionDefinedObj fnDefObjNms)
                                                                                       funNm
                                                                                       (addReceiverParam $ transParams pars varArg)
                                                                                       (injF $ transBlock body)
    where
      (fnDefObjNms, funNm, isMethod) = splitLuaFunName fn

      addReceiverParam :: MLuaTerm [FunctionParameterL] -> MLuaTerm [FunctionParameterL]
      addReceiverParam x = if isMethod then ConsF' SelfParameter' x else x

  trans (F.LocalFunAssign n (project -> Just (F.FunBody pars varArg body))) = iFunctionDef EmptyFunctionDefAttrs' (transIdent n) (transParams pars varArg) (injF $ transBlock body)
  trans x = transDefault x


translateFunArg :: F.LuaTerm F.FunArgL -> MLuaTerm FunctionArgumentsL
translateFunArg (project -> Just (F.Args args))   = FunctionArgumentList' (mapF (PositionalArgument' . injF . translate) args)
translateFunArg (project -> Just (F.TableArg t))  = iLuaTableArg $ translate t
translateFunArg (project -> Just (F.StringArg s)) = iLuaStringArg $ unpack s

instance Trans F.FunCall where
  trans (F.NormalFunCall  f arg) = iFunctionCall EmptyFunctionCallAttrs' (injF $ translate f) (translateFunArg arg)
  trans (F.MethodCall rec f arg) = iFunctionCall EmptyFunctionCallAttrs' (injF $ transIdent f) receiverArg
    where
      receiverArg :: MLuaTerm FunctionArgumentsL
      receiverArg = case arg of
                      (project -> Just (F.Args as)) -> FunctionArgumentList' (ConsF' (ReceiverArg' $ injF $ translate rec)
                                                                                      (mapF (PositionalArgument' . injF . translate) as))
                      (project -> Just (F.TableArg  t)) -> iLuaReceiverAndTableArg  (translate rec) (translate t)
                      (project -> Just (F.StringArg s)) -> iLuaReceiverAndStringArg (translate rec) (unpack s)

instance Trans F.FunArg where
  trans _ = error "Lua FunArg found not with FunCall"


class Untrans f where
  untrans :: f MLuaTerm l -> F.LuaTerm l

instance {-# OVERLAPPING #-} (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt @Untrans untrans
  
untransError :: (HFunctor f, f :-<: MLuaSig) => f MLuaTerm l -> F.LuaTerm l
untransError t = error $ "Cannot untranslate root node: " ++ (show $ inject t)
  
do ipsNames <- sumToNames ''MLuaSig
   modNames <- sumToNames ''F.LuaSig
   let targTs = map ConT $ (ipsNames \\ modNames) \\ [''BlockIsBlock, ''SingleLocalVarDeclIsStat, ''AssignIsStat, ''IdentIsName, ''FunctionCallIsFunCall, ''FunctionDefIsStat]
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)

untransDefault :: (HFunctor f, f :-<: F.LuaSig) => f MLuaTerm l -> F.LuaTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: F.LuaSig) => Untrans f where
  untrans = untransDefault

untransIdent :: MLuaTerm IdentL -> F.LuaTerm F.NameL
untransIdent (Ident' n) = F.iName $ pack n

instance {-# OVERLAPPING #-} Untrans IdentIsName where
  untrans (IdentIsName (Ident' n)) = F.iName $ pack n

instance {-# OVERLAPPING #-} Untrans AssignIsStat where
  untrans (AssignIsStat (Assign' l _ r)) = F.iAssign (untranslate $ fromProjF l) (untranslate $ fromProjF r)

instance {-# OVERLAPPING #-} Untrans SingleLocalVarDeclIsStat where
  untrans (SingleLocalVarDeclIsStat (SingleLocalVarDecl' _ b init)) = F.iLocalAssign (mapF untransIdent $ fromProjF b) linit
    where
      linit = case init of
        NoLocalVarInit'     -> Nothing'
        JustLocalVarInit' x -> Just' $ untranslate $ fromProjF x

untransBlock :: MLuaTerm BlockL -> F.LuaTerm F.BlockL
untransBlock (Block' n e) = F.iBlock (mapF (untranslate.fromProjF) n) (untranslate $ fromProjF e)

instance {-# OVERLAPPING #-} Untrans BlockIsBlock where
  untrans (BlockIsBlock b) = untransBlock b


-- This thing could probably be blown down a lot, but I was tired and didn't want to figure
-- out the correlation between cases
instance {-# OVERLAPPING #-} Untrans FunctionCallIsFunCall where
  untrans (FunctionCallIsFunCall (FunctionCall' EmptyFunctionCallAttrs' f args)) =
      case receiver of
        Nothing -> F.iNormalFunCall (getFunPE f) fa
        Just r  -> F.iMethodCall (fromJust receiver) (fromJust funNm) fa
    where
      getFunPE :: MLuaTerm FunctionExpL -> F.LuaTerm F.PrefixExpL
      getFunPE (project -> Just (PrefixExpIsFunctionExp x)) = untranslate x
      getFunPE (project -> Just (FunctionIdent n))          = F.iPEVar $ F.iVarName $ injF $ untransIdent n

      funNm :: Maybe (F.LuaTerm F.NameL)
      funNm = case project f of
              Just (FunctionIdent i) -> Just (untransIdent i)
              _                      -> Nothing

      receiver :: Maybe (F.LuaTerm F.PrefixExpL)
      receiver = case args of
                   FunctionArgumentList' (ConsF' (ReceiverArg' r) _) -> fmap untranslate $ projF r
                   _     -> case project args of
                                Just (LuaReceiverAndTableArg  rec _) -> Just (untranslate rec)
                                Just (LuaReceiverAndStringArg rec _) -> Just (untranslate rec)
                                _                                    -> Nothing

      untransArgs :: MLuaTerm [FunctionArgumentL] -> F.LuaTerm [F.ExpL]
      untransArgs = mapF untransArg
        where
          untransArg (PositionalArgument' p) = untranslate $ fromProjF p

      fa :: F.LuaTerm F.FunArgL
      fa = case args of
             FunctionArgumentList' (ConsF' (ReceiverArg' _) as) -> F.iArgs (untransArgs as)
             FunctionArgumentList' as                           -> F.iArgs (untransArgs as)
             _      -> case project args of
                          Just (LuaTableArg               x) -> F.iTableArg  (untranslate x)
                          Just (LuaStringArg              s) -> F.iStringArg (pack s)
                          Just (LuaReceiverAndTableArg  _ x) -> F.iTableArg  (untranslate x)
                          Just (LuaReceiverAndStringArg _ s) -> F.iStringArg (pack s)

instance {-# OVERLAPPING #-} Untrans FunctionDefIsStat where
  untrans (FunctionDefIsStat (FunctionDef' attrs n params body)) =
      case attrs of
        EmptyFunctionDefAttrs'-> F.iLocalFunAssign (untransIdent n) (F.iFunBody params'' isVarargs (untransBlock $ fromProjF body))
        (LuaFunctionAttrs' (LuaFunctionDefinedObj' nms)) -> F.iFunAssign (congealFunName $ extractF nms) (F.iFunBody params'' isVarargs (untransBlock $ fromProjF body))
    where
      paramsList = extractF params
      (params', isVarargs) = if length paramsList > 0 then
                               case last paramsList of
                                 LuaVarArgsParam' -> (init paramsList, True)
                                 _                -> (paramsList,      False)
                             else
                               ([], False)

      params'' = insertF $ map unwrapParam $ removeSelf params'
        where
          unwrapParam :: MLuaTerm FunctionParameterL -> F.LuaTerm F.NameL
          unwrapParam (PositionalParameter' EmptyParameterAttrs' n) = untransIdent n

      hasSelf :: [MLuaTerm FunctionParameterL] -> Bool
      hasSelf (SelfParameter' : l) = True
      hasSelf l                    = False

      congealFunName :: [MLuaTerm IdentL] -> F.LuaTerm F.FunNameL
      congealFunName nms = if hasSelf params' then
                             F.iFunName (head nms') (insertF $ tail nms') (Just' n')
                           else
                             -- "drop 1" is a safe version of tail
                             F.iFunName (head (nms' ++ [n'])) (insertF $ drop 1 (nms' ++ [n'])) Nothing'
        where
          nms' = map untransIdent nms
          n' = untransIdent n


      removeSelf :: [MLuaTerm FunctionParameterL] -> [MLuaTerm FunctionParameterL]
      removeSelf l | hasSelf l = tail  l
                   | otherwise = l


untranslate :: MLuaTerm l -> F.LuaTerm l
untranslate = untrans . unTerm