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

import Data.Default ( Default(..) )
import Data.List( (\\) )
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Data.Comp.Multi ( project, stripA, pattern (::&::), inject, unTerm, caseCxt, caseCxt', HFunctor(..), All, Sum, (:-<:), (:&:)(..) )

import Cubix.Language.Python.Parametric.Common.Types
import qualified Cubix.Language.Python.Parametric.Full as F
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax

-------------------------------------------------------------------------------


------------------------------------------------------------------------------------
---------------- Forward translation: Modularized syntax to IPS  -------------------
------------------------------------------------------------------------------------

--------------------------------
------------- Top-level translate
--------------------------------

------ Top-level definition

translate :: (Default a) => F.PythonTermAnn a l -> MPythonTermAnn a l
translate = trans . unTerm

translate' :: (InjF MPythonSig l l', Default a) => F.PythonTermAnn a l -> MPythonTermAnn a l'
translate' = injFAnnDef . translate

------ Class

class Trans f where
  trans :: (Default a) => (f :&: a) (F.PythonTermAnn a) l -> MPythonTermAnn a l

------ Default and standard cases

instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt' @Trans trans

transDefault :: (HFunctor f, f :-<: MPythonSig, f :-<: F.PythonSig, Default a) => (f :&: a) (F.PythonTermAnn a) l -> MPythonTermAnn a l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MPythonSig, f :-<: F.PythonSig) => Trans f where
  trans = transDefault


---------------------------------
-------------- Per-fragment Instances
---------------------------------


-------- Identifiers

transIdent :: (Default a) => F.PythonTermAnn a F.IdentL -> MPythonTermAnn a IdentL
transIdent (F.Ident s ::&:: a) = Ident s ::&:: a

instance Trans F.Ident where
  trans (F.Ident s :&: a) = injectFAnnDef (Ident s :&: a)

-------- Functions

transArgs :: (Default a) => F.PythonTermAnn a [F.ArgumentL] -> MPythonTermAnn a [FunctionArgumentL]
transArgs = mapF transArg
  where
    transArg :: (Default a) => F.PythonTermAnn a F.ArgumentL -> MPythonTermAnn a FunctionArgumentL
    transArg (F.ArgExpr           e ::&:: a) = PositionalArgument (injFAnnDef $ translate e) ::&:: a
    transArg (F.ArgVarArgsPos     e ::&:: a) = PythonArgSplat (translate e) ::&:: a
    transArg (F.ArgVarArgsKeyword e ::&:: a) = PythonArgKWSplat (translate e) ::&:: a
    transArg (F.ArgKeyword      n e ::&:: a) = PythonArgKeyword (transIdent n) (translate e) ::&:: a

isCompOp :: (Default a) => F.PythonTermAnn a F.OpL -> Bool
isCompOp (F.LessThan          ::&:: _) = True
isCompOp (F.LessThanEquals    ::&:: _) = True
isCompOp (F.GreaterThan       ::&:: _) = True
isCompOp (F.GreaterThanEquals ::&:: _) = True
isCompOp (F.Equality          ::&:: _) = True
isCompOp (F.NotEquals         ::&:: _) = True
isCompOp (F.In                ::&:: _) = True
isCompOp (F.Is                ::&:: _) = True
isCompOp (F.IsNot             ::&:: _) = True
isCompOp (F.NotIn             ::&:: _) = True
isCompOp _                             = False

-- The parser groups "a > b > c" as "(a > b) > c". However, PyComp needs the
-- left element to be an expression, and not another comparison. This function
-- flips the grouping to "a > (b > c)".
-- NOTE 2023.11.13: I'm just going to let this translation drop the annotations on the comparisons rather than trying to figure this out.
--                  Note that, in the present case where the annotations are actually source spans, the reassociation will mess with things.
reassociateComp :: (Default a) => F.PythonTermAnn a F.OpL -> F.PythonTermAnn a F.ExprL -> F.PythonTermAnn a F.ExprL -> a -> MPythonTermAnn a F.ExprL
reassociateComp op l r _ = injFAnnDef $ reassociate (translate op) (translate l) (translate r)
  where
    reassociate outer_op (PyCompIsExpr (PyChainComp inner_op left inner_right ::&:: _) ::&:: _) outer_right =
      jPyChainComp inner_op left (reassociate outer_op (injFAnnDef inner_right) outer_right)
    reassociate outer_op (PyCompIsExpr (PyBaseComp inner_op left inner_right ::&:: _) ::&:: _) outer_right =
      jPyChainComp inner_op left (jPyBaseComp outer_op inner_right outer_right)
    reassociate op left right = jPyBaseComp op left right

-- FIXME: This can erroneously capture a module/class as a receiver
-- FIXME: I think I intended to translate "f(x)" to use a FunctionIdent for f
instance Trans F.Expr where
  trans (F.Call f args :&: a) = injFAnnDef $ FunctionCall jEmptyFunctionCallAttrs fun (jFunctionArgumentList args') ::&:: a
    where
      (fun, args') = case f of
        (F.Dot l n ::&:: _) -> (jFunctionIdent (transIdent n), jConsF (jReceiverArg $ injFAnnDef $ translate l) (transArgs args))
        _                   -> (injFAnnDef $ translate f, transArgs args)

  trans t@(F.BinaryOp op l r :&: a) =
    if isCompOp op
    then reassociateComp op l r a
    else transDefault t
  trans (F.CondExpr t c e :&: a) = injFAnnDef $ PyCondExpr (translate c) (translate t) (translate e) ::&:: a
  trans (F.ListComp comp  :&: a) = injFAnnDef $ PyListComprehension (transComprehension comp) ::&:: a
  trans (F.DictComp comp  :&: a) = injFAnnDef $ PyDictComprehension (transComprehension comp) ::&:: a
  trans (F.SetComp comp   :&: a) = injFAnnDef $ PySetComprehension (transComprehension comp) ::&:: a
  trans x                        = transDefault x

transComprehension :: (Default a) => F.PythonTermAnn a F.ComprehensionL -> MPythonTermAnn a PyComprehensionL
transComprehension (F.Comprehension cexpr cfor ::&:: a) = transComprehensionFor cexpr cfor a

-- NOTE 2023.11.13: Totally unsure I got the annotation threading correct. What is the comp_if_iter case in language-python anyhow?
transComprehensionIf :: (Default a) => F.PythonTermAnn a F.ComprehensionExprL -> F.PythonTermAnn a F.CompIfL -> a -> MPythonTermAnn a PyComprehensionL
transComprehensionIf cexp (F.CompIf e mi ::&:: _) a =
  let comp = case extractF mi of
        Just (F.IterFor i ::&:: a') -> transComprehensionFor cexp i a'
        Just (F.IterIf  i ::&:: a') -> transComprehensionIf  cexp i a'
        Nothing -> jPyComprehensionBody (translate cexp) -- Very unsure of annotations here
  in PyComprehensionIf (translate e) comp ::&:: a

transComprehensionFor :: (Default a) => F.PythonTermAnn a F.ComprehensionExprL -> F.PythonTermAnn a F.CompForL -> a -> MPythonTermAnn a PyComprehensionL
transComprehensionFor cexp (F.CompFor b es v mi ::&:: a) a' =
  let comp = case extractF mi of
        Just (F.IterFor i ::&:: a'') -> transComprehensionFor cexp i a''
        Just (F.IterIf  i ::&:: a'') -> transComprehensionIf  cexp i a''
        Nothing -> jPyComprehensionBody (translate cexp) -- Very unsure of annotations here
  in PyComprehensionFor b (mapF translate es) (translate v) comp ::&:: a

translateLValue :: (Default a) => F.PythonTermAnn a F.ExprL -> MPythonTermAnn a PyLValueL
translateLValue (F.Tuple xs        ::&:: a) = TupleLValue (mapF translateLValue xs)       ::&:: a
translateLValue (F.List xs         ::&:: a) = ListLValue  (mapF translateLValue xs)       ::&:: a
translateLValue (F.Dot e i         ::&:: a) = DotLValue (translate e) (transIdent i)      ::&:: a
translateLValue (F.Starred e       ::&:: a) = StarLValue (translateLValue e)              ::&:: a
translateLValue (F.Subscript e x   ::&:: a) = SubscriptLValue (translate e) (translate x) ::&:: a
translateLValue (F.SlicedExpr e sl ::&:: a) = SliceLValue (translate e) (translate sl)    ::&:: a
translateLValue (F.Paren e         ::&:: a) = ParenLValue (translateLValue e)             ::&:: a
translateLValue (F.Var n           ::&:: a) = injFAnnDef (transIdent n)
translateLValue x                           = error ("Invalid expression appeared in lvalue: " ++ show (stripA x))


transBinder :: (Default a) => F.PythonTermAnn a (F.ExprL, Maybe F.ExprL) -> MPythonTermAnn a PyWithBinderL
transBinder (PairF x y ::&:: a) = injFAnnDef $ PyWithBinder (translate x) (mapF translateLValue y) ::&:: a

transParam :: (Default a) => F.PythonTermAnn a F.ParameterL -> MPythonTermAnn a FunctionParameterL
transParam (F.Param          n ann d ::&:: a) = injFAnnDef $ PositionalParameter (jPyParamAttrs (translate ann) (translate d)) (transIdent n) ::&:: a
transParam (F.VarArgsPos     n ann   ::&:: a) = injFAnnDef $ PythonParamVarArgs     (transIdent n) (translate ann) ::&:: a
transParam (F.VarArgsKeyword n ann   ::&:: a) = injFAnnDef $ PythonParamVarArgsKW   (transIdent n) (translate ann) ::&:: a
transParam (F.EndPositional          ::&:: a) = injFAnnDef $ PythonEndPosParam                                     ::&:: a
transParam (F.UnPackTuple t    ann   ::&:: a) = injFAnnDef $ PythonParamUnpackTuple (translate t) (translate ann)  ::&:: a

extractString :: (Default a) => F.PythonTermAnn a [CharL] -> String
extractString cs = map getChar $ extractF cs
  where
    getChar :: (Default a) => F.PythonTermAnn a CharL -> Char
    getChar (CharF' c) = c

extractStrings :: (Default a) => F.PythonTermAnn a [[CharL]] -> [String]
extractStrings strs = map extractString $ extractF strs

makePyBlock :: forall a. (Default a) => F.PythonTermAnn a [F.StatementL] -> MPythonTermAnn a PyBlockL
makePyBlock stmts = jPyBlock docstring (jBlock (insertF $ map (injFAnnDef.translate) body) jEmptyBlockEnd)
  where
    docstring :: MPythonTermAnn a (Maybe PyStringLitL)
    body :: [F.PythonTermAnn a F.StatementL]
    (docstring, body) = case extractF stmts of
      [] -> (Nothing', [])
      (fst : rest) ->
        case fst of
          (F.StmtExpr exp ::&:: _) ->
            case exp of
              (project -> Just (F.Strings        strs :&: a)) -> (Just' $ PyStrings        (extractStrings strs) ::&:: a, rest)
              (project -> Just (F.UnicodeStrings strs :&: a)) -> (Just' $ PyUnicodeStrings (extractStrings strs) ::&:: a, rest)
              (project -> Just (F.ByteStrings    strs :&: a)) -> (Just' $ PyByteStrings    (extractStrings strs) ::&:: a, rest)
              _                                              -> (Nothing', fst : rest)
          _ -> (Nothing', fst : rest)

instance Trans F.Statement where
  trans (F.Assign es e           :&: a) = injFAnnDef $ Assign (jPyLhs $ mapF translateLValue es) jAssignOpEquals (injFAnnDef $ translate e) ::&:: a
  trans (F.Fun n params ann body :&: a) = injFAnnDef $ FunctionDef (jPyFunDefAttrs $ translate ann) (transIdent n) (mapF transParam params) (injFAnnDef $ makePyBlock body) ::&:: a
  trans (F.With binders body     :&: a) = injFAnnDef $ PyWith (mapF transBinder binders) (translate body) ::&:: a
  trans (F.Class id args body    :&: a) = injFAnnDef $ PyClass (transIdent id) (translate args) (makePyBlock body) ::&:: a
  trans x                               = transDefault x

-------------------------------------------------------------------------------

class Untrans f where
  untrans :: f MPythonTerm l -> F.PythonTerm l

instance {-# OVERLAPPING #-} (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt @Untrans untrans

untransError :: (HFunctor f, f :-<: MPythonSig) => f MPythonTerm l -> F.PythonTerm l
untransError t = error $ "Cannot untranslate root node: " ++ (show $ inject t)

do ipsNames <- sumToNames ''MPythonSig
   modNames <- sumToNames ''F.PythonSig
   let targTs = map ConT $ (ipsNames \\ modNames) \\ [''PyWith, ''PyClassIsStatement, ''AssignIsStatement, ''IdentIsIdent, ''FunctionCallIsExpr, ''FunctionDefIsStatement, ''PyCompIsExpr, ''PyCondExpr, ''PyComprehensionExpr]
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)


untransDefault :: (HFunctor f, f :-<: F.PythonSig) => f MPythonTerm l -> F.PythonTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: F.PythonSig) => Untrans f where
  untrans = untransDefault

untransIdent :: MPythonTerm IdentL -> F.PythonTerm F.IdentL
untransIdent (Ident' x) = F.iIdent x

instance {-# OVERLAPPING #-} Untrans IdentIsIdent where
  untrans (IdentIsIdent (Ident' x)) = F.iIdent x

untransLValue :: MPythonTerm PyLValueL -> F.PythonTerm F.ExprL
untransLValue (project -> Just (TupleLValue xs))      = F.iTuple (mapF untransLValue xs)
untransLValue (project -> Just (ListLValue xs))       = F.iList  (mapF untransLValue xs)
untransLValue (project -> Just (DotLValue e i))       = F.iDot (untranslate e) (untransIdent i)
untransLValue (project -> Just (StarLValue e))        = F.iStarred (untransLValue e)
untransLValue (project -> Just (SubscriptLValue e x)) = F.iSubscript (untranslate e) (untranslate x)
untransLValue (project -> Just (SliceLValue e sl))    = F.iSlicedExpr (untranslate e) (untranslate sl)
untransLValue (project -> Just (ParenLValue e))       = F.iParen (untransLValue e)
untransLValue (project -> Just (IdentIsPyLValue n))   = F.iVar (untransIdent n)

instance {-# OVERLAPPING #-} Untrans AssignIsStatement where
  untrans (AssignIsStatement (Assign' l AssignOpEquals' r)) = F.iAssign (mapF untransLValue $ fromProjF l) (untranslate $ fromProjF r)


untransBinder :: MPythonTerm PyWithBinderL -> F.PythonTerm (F.ExprL, Maybe F.ExprL)
untransBinder (project -> Just (PyWithBinder x y)) = PairF' (untranslate x) (mapF untransLValue y)

instance {-# OVERLAPPING #-} Untrans PyWith where
  untrans (PyWith binders body) = F.iWith (mapF untransBinder binders) (untranslate body)

untransPyStrLit :: MPythonTerm PyStringLitL -> F.PythonTerm F.ExprL
untransPyStrLit (project -> Just (PyStrings        strs)) = F.iStrings        (insertStrings strs)
untransPyStrLit (project -> Just (PyUnicodeStrings strs)) = F.iUnicodeStrings (insertStrings strs)
untransPyStrLit (project -> Just (PyByteStrings    strs)) = F.iByteStrings    (insertStrings strs)

untransBlock :: MPythonTerm BlockL -> F.PythonTerm [F.StatementL]
untransBlock (Block' ss EmptyBlockEnd') = mapF (untranslate.fromProjF) ss

untransPyBlock :: MPythonTerm PyBlockL -> F.PythonTerm [F.StatementL]
untransPyBlock (PyBlock' docStr body) = insertF $ docStrStmt ++ (extractF $ untransBlock body)
  where
    docStrStmt = case docStr of
      Nothing' -> []
      Just' lit -> [F.iStmtExpr (untransPyStrLit lit)]

instance {-# OVERLAPPING #-} Untrans PyClassIsStatement where
  untrans (PyClassIsStatement (project -> Just (PyClass id args body))) = F.iClass (untransIdent id) (untranslate args) (untransPyBlock body)

undoReassociateComp :: F.PythonTerm F.OpL -> F.PythonTerm F.ExprL -> F.PythonTerm F.ExprL -> F.PythonTerm F.ExprL
undoReassociateComp outer_op outer_left (project -> (Just (F.BinaryOp inner_op inner_left right))) =
  F.iBinaryOp inner_op (undoReassociateComp outer_op outer_left inner_left) right
undoReassociateComp op left right = F.iBinaryOp op left right

instance {-# OVERLAPPING #-} Untrans PyCompIsExpr where
  untrans (PyCompIsExpr (PyBaseComp' op l r)) = undoReassociateComp (untranslate op) (untranslate l) (untranslate r)
  untrans (PyCompIsExpr (PyChainComp' op l r)) = undoReassociateComp (untranslate op) (untranslate l) (untranslate $ iPyCompIsExpr r)

untransArg :: MPythonTerm FunctionArgumentL -> F.PythonTerm F.ArgumentL
untransArg (PositionalArgument' e) = F.iArgExpr (untranslate $ fromProjF e)
untransArg (project -> Just (PythonArgSplat     e)) = F.iArgVarArgsPos (untranslate e)
untransArg (project -> Just (PythonArgKWSplat   e)) = F.iArgVarArgsKeyword (untranslate e)
untransArg (project -> Just (PythonArgKeyword n e)) = F.iArgKeyword (untransIdent n) (untranslate e)

instance {-# OVERLAPPING #-} Untrans FunctionCallIsExpr where
  untrans (FunctionCallIsExpr (FunctionCall' EmptyFunctionCallAttrs' f args)) = F.iCall lhs args'
    where
      (lhs, args') = case (f, args) of
                       (FunctionIdent' n, FunctionArgumentList' (ConsF' (ReceiverArg' r) as)) -> ( F.iDot (untranslate $ fromProjF r) (untransIdent n)
                                                                                                 , mapF untransArg as)
                       (e, FunctionArgumentList' as) -> (untranslate $ fromProjF e, mapF untransArg as)

insertString :: String -> F.PythonTerm [CharL]
insertString str = insertF $ map CharF' str

insertStrings :: [String] -> F.PythonTerm [[CharL]]
insertStrings strs = insertF $ map insertString strs

untransParam :: MPythonTerm FunctionParameterL -> F.PythonTerm F.ParameterL
untransParam (PositionalParameter' (PyParamAttrs'     ann def) n) = F.iParam          (untransIdent n) (untranslate ann) (untranslate def)
untransParam (project -> Just (PythonParamVarArgs   n ann))       = F.iVarArgsPos     (untransIdent n) (untranslate ann)
untransParam (project -> Just (PythonParamVarArgsKW n ann))       = F.iVarArgsKeyword (untransIdent n) (untranslate ann)
untransParam (project -> Just (PythonParamUnpackTuple pt ann))    = F.iUnPackTuple (untranslate pt) (untranslate ann)
untransParam (project -> Just (PythonEndPosParam))                = F.iEndPositional


instance {-# OVERLAPPING #-} Untrans FunctionDefIsStatement where
  untrans (FunctionDefIsStatement (FunctionDef' (PyFunDefAttrs' ann) n params body)) = F.iFun (untransIdent n) (mapF untransParam params) (untranslate ann) (untransPyBlock $ fromProjF body)

instance {-# OVERLAPPING #-} Untrans PyCondExpr where
  untrans (PyCondExpr c t e) = F.iCondExpr (untranslate t) (untranslate c) (untranslate e)

instance {-# OVERLAPPING #-} Untrans PyComprehensionExpr where
  untrans (PyListComprehension c) = F.iListComp (untransComprehension c)
  untrans (PyDictComprehension c) = F.iDictComp (untransComprehension c)
  untrans (PySetComprehension c) = F.iSetComp (untransComprehension c)

-- instance {-# OVERLAPPING #-} Untrans PyComprehension where
--   untrans = untransComprehension

untransComprehension :: MPythonTerm PyComprehensionL -> F.PythonTerm F.ComprehensionL
untransComprehension t =
  let (expr, comp) = untransComprehension0 t
  in  F.iComprehension expr comp

  where untransComprehension0 :: MPythonTerm PyComprehensionL -> (F.PythonTerm F.ComprehensionExprL, F.PythonTerm F.CompForL)
        untransComprehension0 (project -> Just (PyComprehensionFor b es v comp)) =
          compFor <$> go comp

          where compFor mIter = F.iCompFor b (mapF untranslate es) (untranslate v) mIter

        go :: MPythonTerm PyComprehensionL -> (F.PythonTerm F.ComprehensionExprL, F.PythonTerm (Maybe F.CompIterL))
        go (project -> Just (PyComprehensionFor b es v comp)) =
          iterFor <$> go comp
          
          where iterFor mIter =
                  let compFor = F.iCompFor b (mapF untranslate es) (untranslate v) mIter
                  in  Just' (F.iIterFor compFor)
          
        go (project -> Just (PyComprehensionIf e comp)) =
          iterIf <$> go comp

          where iterIf mIter =
                  let compIf = F.iCompIf (untranslate e) mIter
                  in Just' (F.iIterIf compIf)

        go (project -> Just (PyComprehensionBody b)) =
          (untranslate b, Nothing')

untranslate :: MPythonTerm l -> F.PythonTerm l
untranslate = untrans . unTerm

#endif
