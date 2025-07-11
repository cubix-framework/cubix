{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cubix.Language.Solidity.IPS.Trans (
    translate
  , untranslate
  ) where

import Data.List( (\\) )
import Data.Text qualified as Text
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Data.Comp.Multi ( project, inject, unTerm, caseCxt, Sum, All, HFunctor(..), (:-<:) )

import Cubix.Language.Solidity.IPS.Types
import Cubix.Language.Solidity.Modularized qualified as F
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax
import Data.Typeable (Typeable)

--------------------------------------------------------------------


------------------------------------------------------------------------------------
---------------- Forward translation: Modularized syntax to IPS  -------------------
------------------------------------------------------------------------------------

--------------------------------
------------- Top-level translate
--------------------------------

------ Top-level definition

translate :: F.SolidityTerm l -> MSolidityTerm l
translate = trans . unTerm @(Sum F.SoliditySig)

translate' :: (InjF MSoliditySig l l') => F.SolidityTerm l -> MSolidityTerm l'
translate' = injF . translate

------ Class

class Trans f where
  trans :: f F.SolidityTerm l -> MSolidityTerm l

------ Default and standard cases

instance {-# OVERLAPPING #-} (All Trans fs) => Trans (Sum fs) where
  trans = caseCxt @Trans trans

transDefault :: (HFunctor f, f :-<: MSoliditySig, f :-<: F.SoliditySig) => f F.SolidityTerm l -> MSolidityTerm l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: MSoliditySig, f :-<: F.SoliditySig) => Trans f where
  trans = transDefault

---------------------------------
-------------- Per-fragment Instances
---------------------------------

-------- Identifiers

transIdent :: F.SolidityTerm F.IdentifierL -> MSolidityTerm IdentL
transIdent (project -> Just (F.Identifier t)) = Ident' (Text.unpack t)

-- Clone of transIdent because type-safe pattern match
instance Trans F.Identifier where
  trans (F.Identifier n) = iIdent (Text.unpack n)

transUnop :: F.SolidityTerm F.UnaryOpL -> Maybe (MSolidityTerm UnaryOpL)
transUnop F.UPreSub' = Just UnaryMinus'
transUnop F.UPreBitNot' = Just Complement'
transUnop F.UPreNot' = Just Not'
transUnop _ = Nothing

translateUnary
  :: F.SolidityTerm F.UnaryOpL
  -> F.SolidityTerm F.ExpressionL
  -> MSolidityTerm ExpressionL
translateUnary (transUnop -> Just op) exp = Unary' op (translate' exp)
translateUnary op exp = F.iUnaryExpression (translate' op) (translate exp)

transBinOp :: F.SolidityTerm F.BinaryOpL -> Maybe (MSolidityTerm BinaryOpL)
transBinOp F.Exp' = Just Pow'
transBinOp F.Mul' = Just Mul'
transBinOp F.Div' = Just Div'
transBinOp F.Mod' = Just Mod'
transBinOp F.Add' = Just Add'
transBinOp F.Sub' = Just Sub'
transBinOp F.Shl' = Just Shl'
transBinOp F.Shr' = Just LogicShr'
transBinOp F.Sar' = Just ArithShr'
transBinOp F.BitAnd' = Just BitAnd'
transBinOp F.BitXor' = Just BitXor'
transBinOp F.BitOr' = Just BitOr'
transBinOp F.LessThan' = Just Lt'
transBinOp F.GreaterThan' = Just Gt'
transBinOp F.LessEqual' = Just Lte'
transBinOp F.GreaterEqual' = Just Gte'
transBinOp F.Equal' = Just Eq'
transBinOp F.NotEqual' = Just Neq'
transBinOp F.And' = Just LogicAnd'
transBinOp F.Or' = Just LogicOr'
transBinOp _ = Nothing

transAssignOp :: F.SolidityTerm F.BinaryOpL -> Maybe (MSolidityTerm AssignOpL)
transAssignOp F.Assign' = Just AssignOpEquals'
transAssignOp _ = Nothing

translateLValue
  :: F.SolidityTerm F.ExpressionL
  -> Maybe (MSolidityTerm LhsL)
translateLValue (F.IndexExpression' arr idx) =
  Just $ IndexLValue' (translateExpression arr) (translateExpression idx)
translateLValue (F.IdentifierExpression' id) =
  Just $ IdentifierLValue' (transIdent id)
translateLValue (F.MemberAccess' struct access) =
  Just $ MemberAccessLValue' (translateExpression struct) (translate' access)
translateLValue (F.TupleExpression' (extractF -> exprs)) =
  Just $ TupleLValue' $ insertF $ map (mapF translateExpression) exprs
translateLValue _ = Nothing

translateBinary
  :: F.SolidityTerm F.BinaryOpL
  -> F.SolidityTerm F.ExpressionL
  -> F.SolidityTerm F.ExpressionL
  -> MSolidityTerm ExpressionL
translateBinary (transBinOp -> Just op) a b =
  Binary' op (translateExpression a) (translateExpression b)
translateBinary (transAssignOp -> Just op) a b
  | Just (lhs :: MSolidityTerm LhsL) <- translateLValue a
  = iAssign lhs op (injF $ translateExpression b)
translateBinary op a b =
  F.iBinaryExpression (translate' op) (injF $ translateExpression a) (injF $ translateExpression b)

translateExpression
  :: F.SolidityTerm F.ExpressionL
  -> MSolidityTerm ExpressionL
transalteExpression (F.UnaryExpression' f a) =
  translateUnary f a
transalteExpression (F.BinaryExpression' f a b) =
  translateBinary f a b
transalteExpression (F.ConditionalExpression' cond a b) =
  Ternary' ITE' (translateExpression cond) (translateExpression a) (translateExpression b)
translateExpression =
  translate'

instance Trans F.Expression where
  trans (F.UnaryExpression f a) =
    injF $ translateUnary f a
  trans (F.BinaryExpression f a b) =
    injF $ translateBinary f a b
  trans (F.ConditionalExpression cond a b) =
    iTernary ITE' (translate' cond) (translate' a) (translate' b)
  trans x =
    transDefault x

------------------------------------------------------------------------------------
---------------- Reverse translation: IPS to modularized syntax  -------------------
------------------------------------------------------------------------------------

--------------------------------
------------- Top-level untranslate
--------------------------------

------ Class (contains top-level function on sigs)

class Untrans f where
  untrans :: f MSolidityTerm l -> F.SolidityTerm l

------ Default and standard cases

instance {-# OVERLAPPING #-} (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt @Untrans untrans

untransError :: (HFunctor f, f :-<: MSoliditySig) => f MSolidityTerm l -> F.SolidityTerm l
untransError t = error $ "Cannot untranslate root node: " ++ show (inject t)

do ipsNames <- sumToNames ''MSoliditySig
   modNames <- sumToNames ''F.SoliditySig
   let targTs = map ConT $ (ipsNames \\ modNames) \\ [ ''IdentIsIdentifier, ''AssignIsExpression, ''ExpressionIsSolExp, ''SolExpIsExpression ]
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)

untransDefault :: (HFunctor f, f :-<: F.SoliditySig) => f MSolidityTerm l -> F.SolidityTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :-<: F.SoliditySig) => Untrans f where
  untrans = untransDefault

------ Top level function on terms

-- Must define this after template Haskell block
untranslate :: MSolidityTerm l -> F.SolidityTerm l
untranslate = untrans . unTerm

untranslate' :: InjF MSoliditySig l l' => MSolidityTerm l' -> F.SolidityTerm l
untranslate' = untranslate . fromProjF

--------------------------------
------------- Per-fragment instances
--------------------------------

-------- Identifiers

untransIdent :: MSolidityTerm IdentL -> F.SolidityTerm F.IdentifierL
untransIdent (Ident' s) = F.iIdentifier (Text.pack s)

instance {-# OVERLAPPING #-} Untrans IdentIsIdentifier where
  untrans (IdentIsIdentifier n) = untransIdent n

untransUnaryOp :: MSolidityTerm UnaryOpL -> F.SolidityTerm F.UnaryOpL
untransUnaryOp UnaryMinus' = F.UPreSub'
untransUnaryOp Complement' = F.UPreBitNot'
untransUnaryOp Not' = F.UPreNot'

untransUnary
  :: MSolidityTerm UnaryOpL
  -> MSolidityTerm ExpressionL
  -> F.SolidityTerm F.ExpressionL
untransUnary op e = F.iUnaryExpression (untransUnaryOp op) (untransExpression e)

untransBinOp :: MSolidityTerm BinaryOpL -> F.SolidityTerm F.BinaryOpL
untransBinOp Pow' = F.Exp'
untransBinOp Mul' = F.Mul'
untransBinOp Div' = F.Div'
untransBinOp Mod' = F.Mod'
untransBinOp Add' = F.Add'
untransBinOp Sub' = F.Sub'
untransBinOp Shl'      = F.Shl'
untransBinOp LogicShr' = F.Shr'
untransBinOp ArithShr' = F.Sar'
untransBinOp BitAnd' = F.BitAnd'
untransBinOp BitXor' = F.BitXor'
untransBinOp BitOr'  = F.BitOr'
untransBinOp Lt'  = F.LessThan'
untransBinOp Gt'  = F.GreaterThan'
untransBinOp Lte' = F.LessEqual'
untransBinOp Gte' = F.GreaterEqual'
untransBinOp Eq'  = F.Equal'
untransBinOp Neq' = F.NotEqual'
untransBinOp LogicAnd' = F.And'
untransBinOp LogicOr'  = F.Or'

untransBinary
  :: MSolidityTerm BinaryOpL
  -> MSolidityTerm ExpressionL
  -> MSolidityTerm ExpressionL
  -> F.SolidityTerm F.ExpressionL
untransBinary op a b = F.iBinaryExpression (untransBinOp op) (untransExpression a) (untransExpression b)

untransTernary
  :: MSolidityTerm TernaryOpL
  -> MSolidityTerm ExpressionL
  -> MSolidityTerm ExpressionL
  -> MSolidityTerm ExpressionL
  -> F.SolidityTerm F.ExpressionL
untransTernary ITE' cond a b =
  F.iConditionalExpression (untransExpression cond) (untransExpression a) (untransExpression b)

untransExpression :: MSolidityTerm ExpressionL -> F.SolidityTerm F.ExpressionL
untransExpression (Unary' op e) = untransUnary op e
untransExpression (Binary' op a b) = untransBinary op a b
untransExpression (Ternary' op cond a b) = untransTernary op cond a b
untransExpression (SolExpIsExpression' e) = untranslate e

instance {-# OVERLAPPING #-} Untrans ExpressionIsSolExp where
  untrans (ExpressionIsSolExp e) = untransExpression e

untransSolExp :: MSolidityTerm F.ExpressionL -> F.SolidityTerm ExpressionL
untransSolExp (ExpressionIsSolExp' e) = untranslate e

instance {-# OVERLAPPING #-} Untrans SolExpIsExpression where
  untrans (SolExpIsExpression e) = untransSolExp e

untransLValue :: MSolidityTerm LhsL -> MSolidityTerm F.ExpressionL
untransLValue (IndexLValue' arr idx) =
  F.iIndexExpression (injF arr) (injF idx)
untransLValue (IdentifierLValue' (Ident' s)) =
  F.iIdentifierExpression $ iIdent s
untransLValue (MemberAccessLValue' struct mem) =
  F.iMemberAccess (injF struct) (injF mem)
untransLValue (TupleLValue' (extractF -> exprs)) =
  F.iTupleExpression $ insertF $ map (insertF . fmap injF . extractF) exprs

instance Untrans AssignIsExpression where
  untrans :: AssignIsExpression MSolidityTerm l -> F.SolidityTerm l
  untrans (AssignIsExpression (Assign' lhs AssignOpEquals' rhs))
    | lval :: MSolidityTerm LhsL <- fromProjF lhs
    , arg :: MSolidityTerm ExpressionL <- fromProjF rhs
    = untranslate' $ SolExpIsExpression' $
      F.iBinaryExpression F.iAssign (injF $ untransLValue lval) (injF arg)
