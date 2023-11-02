{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP                    #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Python.Parametric.Common.Trans () where
#else
module Cubix.Language.Python.Parametric.Common.Trans
  (
    translate
  , untranslate
  ) where

import Data.List( (\\) )
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Data.Comp.Multi ( project, inject, unTerm, caseCxt, HFunctor(..), All, Sum, (:-<:) )

import Cubix.Language.Python.Parametric.Common.Types
import qualified Cubix.Language.Python.Parametric.Full as F
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax

-------------------------------------------------------------------------------

translate :: F.PythonTerm l -> MPythonTerm l
translate = trans . unTerm

translate' :: (InjF MPythonSig l l') => F.PythonTerm l -> MPythonTerm l'
translate' = injF . translate

class Trans f where
  trans :: f F.PythonTerm l -> MPythonTerm l

instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt @Trans trans

transDefault :: (HFunctor f, f :-<: MPythonSig, f :-<: F.PythonSig) => f F.PythonTerm l -> MPythonTerm l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MPythonSig, f :-<: F.PythonSig) => Trans f where
  trans = transDefault


transIdent :: F.PythonTerm F.IdentL -> MPythonTerm IdentL
transIdent (project -> Just (F.Ident s _)) = iIdent s

instance Trans F.Ident where
  trans (F.Ident s _) = injF $ Ident' s

transArgs :: F.PythonTerm [F.ArgumentL] -> MPythonTerm [FunctionArgumentL]
transArgs = mapF transArg
  where
    transArg :: F.PythonTerm F.ArgumentL -> MPythonTerm FunctionArgumentL
    transArg (project -> Just (F.ArgExpr           e _)) = PositionalArgument' $ injF $ translate e
    transArg (project -> Just (F.ArgVarArgsPos     e _)) = iPythonArgSplat $ translate e
    transArg (project -> Just (F.ArgVarArgsKeyword e _)) = iPythonArgKWSplat $ translate  e
    transArg (project -> Just (F.ArgKeyword      n e _)) = iPythonArgKeyword (transIdent n) (translate e)

isCompOp :: F.PythonTerm F.OpL -> Bool
isCompOp (project -> (Just (F.LessThan _)))          = True
isCompOp (project -> (Just (F.LessThanEquals _)))    = True
isCompOp (project -> (Just (F.GreaterThan _)))       = True
isCompOp (project -> (Just (F.GreaterThanEquals _))) = True
isCompOp (project -> (Just (F.Equality _)))          = True
isCompOp (project -> (Just (F.NotEquals _)))         = True
isCompOp (project -> (Just (F.In _)))                = True
isCompOp (project -> (Just (F.Is _)))                = True
isCompOp (project -> (Just (F.IsNot _)))             = True
isCompOp (project -> (Just (F.NotIn _)))             = True
isCompOp _                                           = False

-- The parser groups "a > b > c" as "(a > b) > c". However, PyComp needs the
-- left element to be an expression, and not another comparison. This function
-- flips the grouping to "a > (b > c)".
reassociateComp :: F.PythonTerm F.OpL -> F.PythonTerm F.ExprL -> F.PythonTerm F.ExprL -> MPythonTerm F.ExprL
reassociateComp op l r = injF $ reassociate (translate op) (translate l) (translate r)
  where
    reassociate outer_op (project -> (Just (PyCompIsExpr (PyChainComp' inner_op left inner_right)))) outer_right =
      iPyChainComp inner_op left (reassociate outer_op (injF inner_right) outer_right)
    reassociate outer_op (project -> (Just (PyCompIsExpr (PyBaseComp' inner_op left inner_right)))) outer_right =
      iPyChainComp inner_op left (iPyBaseComp outer_op inner_right outer_right)
    reassociate op left right = iPyBaseComp op left right

-- FIXME: This can erroneously capture a module/class as a receiver
-- FIXME: I think I intended to translate "f(x)" to use a FunctionIdent for f
instance Trans F.Expr where
  trans (F.Call f args _) = iFunctionCall EmptyFunctionCallAttrs' fun (FunctionArgumentList' args')
    where
      (fun, args') = case project f of
        Just (F.Dot l n _) -> (iFunctionIdent (transIdent n), ConsF' (ReceiverArg' $ injF $ translate l) (transArgs args))
        _                  -> (injF $ translate f, transArgs args)

  trans t@(F.BinaryOp op l r _) =
    if isCompOp op
    then reassociateComp op l r
    else transDefault t
  trans (F.CondExpr t c e _) = iPyCondExpr (translate c) (translate t) (translate e)
  trans (F.ListComp comp _) = iPyListComprehension (transComprehension comp)
  trans (F.DictComp comp _) = iPyDictComprehension (transComprehension comp)
  trans (F.SetComp comp _) = iPySetComprehension (transComprehension comp)
  trans x = transDefault x

transComprehension :: F.PythonTerm F.ComprehensionL -> MPythonTerm PyComprehensionL
transComprehension (project -> Just (F.Comprehension cexpr cfor _)) = transComprehensionFor cexpr cfor

transComprehensionIf :: F.PythonTerm F.ComprehensionExprL -> F.PythonTerm F.CompIfL -> MPythonTerm PyComprehensionL
transComprehensionIf cexp (project -> Just (F.CompIf e mi _)) =
  let comp = case extractF mi of
        Just (project -> Just (F.IterFor i _)) -> transComprehensionFor cexp i
        Just (project -> Just (F.IterIf i _)) -> transComprehensionIf cexp i
        Nothing -> iPyComprehensionBody (translate cexp)
  in iPyComprehensionIf (translate e) comp

transComprehensionFor :: F.PythonTerm F.ComprehensionExprL -> F.PythonTerm F.CompForL -> MPythonTerm PyComprehensionL
transComprehensionFor cexp (project -> Just (F.CompFor b es v mi _)) =
  let comp = case extractF mi of
        Just (project -> Just (F.IterFor i _)) -> transComprehensionFor cexp i
        Just (project -> Just (F.IterIf i _)) -> transComprehensionIf cexp i
        Nothing -> iPyComprehensionBody (translate cexp)
  in iPyComprehensionFor b (mapF translate es) (translate v) comp

translateLValue :: F.PythonTerm F.ExprL -> MPythonTerm PyLValueL
translateLValue (project -> Just (F.Tuple xs _))        = iTupleLValue (mapF translateLValue xs)
translateLValue (project -> Just (F.List xs _))         = iListLValue  (mapF translateLValue xs)
translateLValue (project -> Just (F.Dot e i _))         = iDotLValue (translate e) (transIdent i)
translateLValue (project -> Just (F.Starred e _))       = iStarLValue (translateLValue e)
translateLValue (project -> Just (F.Subscript e x _))   = iSubscriptLValue (translate e) (translate x)
translateLValue (project -> Just (F.SlicedExpr e sl _)) = iSliceLValue (translate e) (translate sl)
translateLValue (project -> Just (F.Paren e _))         = iParenLValue (translateLValue e)
translateLValue (project -> Just (F.Var n _))           = injF (transIdent n)
translateLValue x                                       = error ("Invalid expression appeared in lvalue: " ++ show x)


transBinder :: F.PythonTerm (F.ExprL, Maybe F.ExprL) -> MPythonTerm PyWithBinderL
transBinder (PairF' x y) = iPyWithBinder (translate x) (mapF translateLValue y)

transParam :: F.PythonTerm F.ParameterL -> MPythonTerm FunctionParameterL
transParam (project -> Just (F.Param          n ann d _)) = PositionalParameter' (iPyParamAttrs (translate ann) (translate d)) (transIdent n)
transParam (project -> Just (F.VarArgsPos     n ann   _)) = iPythonParamVarArgs     (transIdent n) (translate ann)
transParam (project -> Just (F.VarArgsKeyword n ann   _)) = iPythonParamVarArgsKW   (transIdent n) (translate ann)
transParam (project -> Just (F.EndPositional _))          = iPythonEndPosParam
transParam (project -> Just (F.UnPackTuple t    ann   _)) = iPythonParamUnpackTuple (translate t) (translate ann)

extractString :: F.PythonTerm [CharL] -> String
extractString cs = map getChar $ extractF cs
  where
    getChar :: F.PythonTerm CharL -> Char
    getChar (CharF' c) = c

extractStrings :: F.PythonTerm [[CharL]] -> [String]
extractStrings strs = map extractString $ extractF strs

makePyBlock :: F.PythonTerm [F.StatementL] -> MPythonTerm PyBlockL
makePyBlock stmts = iPyBlock docstring (iBlock (insertF $ map (injF.translate) body) EmptyBlockEnd')
  where
    docstring :: MPythonTerm (Maybe PyStringLitL)
    body :: [F.PythonTerm F.StatementL]
    (docstring, body) = case extractF stmts of
      [] -> (Nothing', [])
      (fst : rest) ->
        case fst of
          (project -> Just (F.StmtExpr exp _)) ->
            case exp of
              (project -> Just (F.Strings        strs _)) -> (Just' $ iPyStrings        (extractStrings strs), rest)
              (project -> Just (F.UnicodeStrings strs _)) -> (Just' $ iPyUnicodeStrings (extractStrings strs), rest)
              (project -> Just (F.ByteStrings    strs _)) -> (Just' $ iPyByteStrings    (extractStrings strs), rest)
              _                                            -> (Nothing', fst : rest)
          _ -> (Nothing', fst : rest)

instance Trans F.Statement where
  trans (F.Assign es e _)           = injF $ Assign' (iPyLhs $ mapF translateLValue es) AssignOpEquals' (injF $ translate e)
  trans (F.Fun n params ann body _) = iFunctionDef (iPyFunDefAttrs $ translate ann) (transIdent n) (mapF transParam params) (injF $ makePyBlock body)
  trans (F.With binders body _  )   = iPyWith (mapF transBinder binders) (translate body)
  trans (F.Class id args body _)    = iPyClass (transIdent id) (translate args) (makePyBlock body)
  trans x                           = transDefault x

-------------------------------------------------------------------------------

class Untrans f where
  untrans :: f MPythonTerm l -> F.PythonTerm l

instance {-# OVERLAPPING #-} (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt @Untrans untrans

untransError :: (HFunctor f, f :-<: MPythonSig) => f MPythonTerm l -> F.PythonTerm l
untransError t = error $ "Cannot untranslate root node: " ++ (show $ (inject t :: MPythonTerm _))

do ipsNames <- sumToNames ''MPythonSig
   modNames <- sumToNames ''F.PythonSig
   let targTs = map ConT $ (ipsNames \\ modNames) \\ [''PyWith, ''PyClassIsStatement, ''AssignIsStatement, ''IdentIsIdent, ''FunctionCallIsExpr, ''FunctionDefIsStatement, ''PyCompIsExpr, ''PyCondExpr, ''PyComprehensionExpr]
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)


untransDefault :: (HFunctor f, f :-<: F.PythonSig) => f MPythonTerm l -> F.PythonTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: F.PythonSig) => Untrans f where
  untrans = untransDefault

untransIdent :: MPythonTerm IdentL -> F.PythonTerm F.IdentL
untransIdent (Ident' x) = F.iIdent x iUnitF

instance {-# OVERLAPPING #-} Untrans IdentIsIdent where
  untrans (IdentIsIdent (Ident' x)) = F.iIdent x iUnitF

untransLValue :: MPythonTerm PyLValueL -> F.PythonTerm F.ExprL
untransLValue (project -> Just (TupleLValue xs))      = F.iTuple (mapF untransLValue xs) iUnitF
untransLValue (project -> Just (ListLValue xs))       = F.iList  (mapF untransLValue xs) iUnitF
untransLValue (project -> Just (DotLValue e i))       = F.iDot (untranslate e) (untransIdent i) iUnitF
untransLValue (project -> Just (StarLValue e))        = F.iStarred (untransLValue e) iUnitF
untransLValue (project -> Just (SubscriptLValue e x)) = F.iSubscript (untranslate e) (untranslate x) iUnitF
untransLValue (project -> Just (SliceLValue e sl))    = F.iSlicedExpr (untranslate e) (untranslate sl) iUnitF
untransLValue (project -> Just (ParenLValue e))       = F.iParen (untransLValue e) iUnitF
untransLValue (project -> Just (IdentIsPyLValue n))   = F.iVar (untransIdent n) iUnitF

instance {-# OVERLAPPING #-} Untrans AssignIsStatement where
  untrans (AssignIsStatement (Assign' l AssignOpEquals' r)) = F.iAssign (mapF untransLValue $ fromProjF l) (untranslate $ fromProjF r) iUnitF


untransBinder :: MPythonTerm PyWithBinderL -> F.PythonTerm (F.ExprL, Maybe F.ExprL)
untransBinder (project -> Just (PyWithBinder x y)) = PairF' (untranslate x) (mapF untransLValue y)

instance {-# OVERLAPPING #-} Untrans PyWith where
  untrans (PyWith binders body) = F.iWith (mapF untransBinder binders) (untranslate body) iUnitF

untransPyStrLit :: MPythonTerm PyStringLitL -> F.PythonTerm F.ExprL
untransPyStrLit (project -> Just (PyStrings        strs)) = F.iStrings        (insertStrings strs) iUnitF
untransPyStrLit (project -> Just (PyUnicodeStrings strs)) = F.iUnicodeStrings (insertStrings strs) iUnitF
untransPyStrLit (project -> Just (PyByteStrings    strs)) = F.iByteStrings    (insertStrings strs) iUnitF

untransBlock :: MPythonTerm BlockL -> F.PythonTerm [F.StatementL]
untransBlock (Block' ss EmptyBlockEnd') = mapF (untranslate.fromProjF) ss

untransPyBlock :: MPythonTerm PyBlockL -> F.PythonTerm [F.StatementL]
untransPyBlock (PyBlock' docStr body) = insertF $ docStrStmt ++ (extractF $ untransBlock body)
  where
    docStrStmt = case docStr of
      Nothing' -> []
      Just' lit -> [F.iStmtExpr (untransPyStrLit lit) iUnitF]

instance {-# OVERLAPPING #-} Untrans PyClassIsStatement where
  untrans (PyClassIsStatement (project -> Just (PyClass id args body))) = F.iClass (untransIdent id) (untranslate args) (untransPyBlock body) iUnitF

undoReassociateComp :: F.PythonTerm F.OpL -> F.PythonTerm F.ExprL -> F.PythonTerm F.ExprL -> F.PythonTerm F.ExprL
undoReassociateComp outer_op outer_left (project -> (Just (F.BinaryOp inner_op inner_left right _))) =
  F.iBinaryOp inner_op (undoReassociateComp outer_op outer_left inner_left) right iUnitF
undoReassociateComp op left right = F.iBinaryOp op left right iUnitF

instance {-# OVERLAPPING #-} Untrans PyCompIsExpr where
  untrans (PyCompIsExpr (PyBaseComp' op l r)) = undoReassociateComp (untranslate op) (untranslate l) (untranslate r)
  untrans (PyCompIsExpr (PyChainComp' op l r)) = undoReassociateComp (untranslate op) (untranslate l) (untranslate $ iPyCompIsExpr r)

untransArg :: MPythonTerm FunctionArgumentL -> F.PythonTerm F.ArgumentL
untransArg (PositionalArgument' e) = F.iArgExpr (untranslate $ fromProjF e) iUnitF
untransArg (project -> Just (PythonArgSplat     e)) = F.iArgVarArgsPos (untranslate e) iUnitF
untransArg (project -> Just (PythonArgKWSplat   e)) = F.iArgVarArgsKeyword (untranslate e) iUnitF
untransArg (project -> Just (PythonArgKeyword n e)) = F.iArgKeyword (untransIdent n) (untranslate e) iUnitF

instance {-# OVERLAPPING #-} Untrans FunctionCallIsExpr where
  untrans (FunctionCallIsExpr (FunctionCall' EmptyFunctionCallAttrs' f args)) = F.iCall lhs args' iUnitF
    where
      (lhs, args') = case (f, args) of
                       (FunctionIdent' n, FunctionArgumentList' (ConsF' (ReceiverArg' r) as)) -> ( F.iDot (untranslate $ fromProjF r) (untransIdent n) iUnitF
                                                                                                 , mapF untransArg as)
                       (e, FunctionArgumentList' as) -> (untranslate $ fromProjF e, mapF untransArg as)

insertString :: String -> F.PythonTerm [CharL]
insertString str = insertF $ map CharF' str

insertStrings :: [String] -> F.PythonTerm [[CharL]]
insertStrings strs = insertF $ map insertString strs

untransParam :: MPythonTerm FunctionParameterL -> F.PythonTerm F.ParameterL
untransParam (PositionalParameter' (PyParamAttrs'     ann def) n) = F.iParam          (untransIdent n) (untranslate ann) (untranslate def) iUnitF
untransParam (project -> Just (PythonParamVarArgs   n ann))       = F.iVarArgsPos     (untransIdent n) (untranslate ann) iUnitF
untransParam (project -> Just (PythonParamVarArgsKW n ann))       = F.iVarArgsKeyword (untransIdent n) (untranslate ann) iUnitF
untransParam (project -> Just (PythonParamUnpackTuple pt ann))    = F.iUnPackTuple (untranslate pt) (untranslate ann) iUnitF
untransParam (project -> Just (PythonEndPosParam))                = F.iEndPositional iUnitF


instance {-# OVERLAPPING #-} Untrans FunctionDefIsStatement where
  untrans (FunctionDefIsStatement (FunctionDef' (PyFunDefAttrs' ann) n params body)) = F.iFun (untransIdent n) (mapF untransParam params) (untranslate ann) (untransPyBlock $ fromProjF body) iUnitF

instance {-# OVERLAPPING #-} Untrans PyCondExpr where
  untrans (PyCondExpr c t e) = F.iCondExpr (untranslate t) (untranslate c) (untranslate e) iUnitF

instance {-# OVERLAPPING #-} Untrans PyComprehensionExpr where
  untrans (PyListComprehension c) = F.iListComp (untransComprehension c) iUnitF
  untrans (PyDictComprehension c) = F.iDictComp (untransComprehension c) iUnitF
  untrans (PySetComprehension c) = F.iSetComp (untransComprehension c) iUnitF

-- instance {-# OVERLAPPING #-} Untrans PyComprehension where
--   untrans = untransComprehension

untransComprehension :: MPythonTerm PyComprehensionL -> F.PythonTerm F.ComprehensionL
untransComprehension t =
  let (expr, comp) = untransComprehension0 t
  in  F.iComprehension expr comp iUnitF

  where untransComprehension0 :: MPythonTerm PyComprehensionL -> (F.PythonTerm F.ComprehensionExprL, F.PythonTerm F.CompForL)
        untransComprehension0 (project -> Just (PyComprehensionFor b es v comp)) =
          compFor <$> go comp

          where compFor mIter = F.iCompFor b (mapF untranslate es) (untranslate v) mIter iUnitF

        go :: MPythonTerm PyComprehensionL -> (F.PythonTerm F.ComprehensionExprL, F.PythonTerm (Maybe F.CompIterL))
        go (project -> Just (PyComprehensionFor b es v comp)) =
          iterFor <$> go comp
          
          where iterFor mIter =
                  let compFor = F.iCompFor b (mapF untranslate es) (untranslate v) mIter iUnitF
                  in  Just' (F.iIterFor compFor iUnitF)
          
        go (project -> Just (PyComprehensionIf e comp)) =
          iterIf <$> go comp

          where iterIf mIter =
                  let compIf = F.iCompIf (untranslate e) mIter iUnitF
                  in Just' (F.iIterIf compIf iUnitF)

        go (project -> Just (PyComprehensionBody b)) =
          (untranslate b, Nothing')

untranslate :: MPythonTerm l -> F.PythonTerm l
untranslate = untrans . unTerm

#endif
