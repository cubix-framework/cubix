{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE UndecidableInstances    #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.JavaScript.Parametric.Full.Trans () where
#else
module Cubix.Language.JavaScript.Parametric.Full.Trans (
    translate
  , translateStatement
  , translateExpression
  , untranslate
  , annotateWithSpans
  ) where

import Data.Typeable ( Typeable )

import qualified Language.Haskell.TH as TH
import qualified Language.JavaScript.Parser.AST as JS

import Data.Comp.Multi ( Sum, All, Cxt(..), HFunctor(..), (:&:)(..), caseCxt, cata, inj, proj )
import Data.Comp.Multi.Algebra ( Alg )
import Data.Comp.Multi.Annotation ( pattern (::&::) )
import Data.Comp.Multi.HFoldable ( hfoldMap )
import Data.Comp.Trans ( runCompTrans, deriveTrans, deriveUntrans )

import Cubix.Language.Info ( SourcePos(..), SourceSpan(..), mkSourceSpan )
import Cubix.Language.JavaScript.Parametric.Full.Names
import Cubix.Language.JavaScript.Parametric.Full.Types
import Cubix.Language.Parametric.Syntax.Functor
import Cubix.Sin.Compdata.Annotation ( getAnn )

runCompTrans $ deriveTrans origASTTypes (TH.ConT ''JSTerm)

translate :: JS.JSAST -> JSTerm JSASTL
translate = trans

translateStatement :: JS.JSStatement -> JSTerm JSStatementL
translateStatement = trans

translateExpression :: JS.JSExpression -> JSTerm JSExpressionL
translateExpression = trans

instance (Trans c l) => Trans (JS.JSCommaList c) (JSCommaList l) where
  trans (JS.JSLCons a b c) = riJSLCons (trans a) (trans b) (trans c)
  trans (JS.JSLOne a)      = riJSLOne (trans a)
  trans  JS.JSLNil         = riJSLNil

instance (Trans c l, Trans (JS.JSCommaList c) (JSCommaList l))
            => Trans (JS.JSCommaTrailingList c) (JSCommaTrailingList l) where
  trans (JS.JSCTLComma a b) = riJSCTLComma (trans a) (trans b)
  trans (JS.JSCTLNone a)    = riJSCTLNone (trans a)

instance (Trans c l, Typeable l) => Trans [c] [l] where
  trans [] = riNilF
  trans (x:xs) = (trans x :: JSTerm l) `iConsF` (trans xs)

instance (Trans c l, Typeable l) => Trans (Maybe c) (Maybe l) where
  trans Nothing  = riNothingF
  trans (Just x) = iJustF $ (trans x :: JSTerm l)


runCompTrans $ deriveUntrans origASTTypes (TH.ConT ''JSTerm)

type instance Targ (JSCommaList l) = JS.JSCommaList (Targ l)
instance Untrans JSCommaListF where
  untrans (JSLCons a b c) = T $ JS.JSLCons (t a) (t b) (t c)
  untrans (JSLOne a)      = T $ JS.JSLOne (t a)
  untrans  JSLNil         = T $ JS.JSLNil

type instance Targ (JSCommaTrailingList l) = JS.JSCommaTrailingList (Targ l)
instance Untrans JSCommaTrailingListF where
  untrans (JSCTLComma a b) = T $ JS.JSCTLComma (t a) (t b)
  untrans (JSCTLNone a)    = T $ JS.JSCTLNone (t a)

type instance Targ [l] = [Targ l]
instance Untrans ListF where
  untrans NilF = T []
  untrans (ConsF a b) = T ((t a) : (t b))

type instance Targ (Maybe l) = Maybe (Targ l)
instance Untrans MaybeF where
  untrans NothingF = T Nothing
  untrans (JustF x) = T (Just (t x))

instance (All Untrans fs) => Untrans (Sum fs) where
  untrans = caseCxt @Untrans untrans

-- @language-javascript@ records token start positions on 'JS.JSAnnot'
-- sub-sort nodes rather than as the top-level @a@ parameter that
-- @comptrans@ threads for other languages. 'annotateWithSpans'
-- lifts those positions to a 'Maybe SourceSpan' annotation per node
-- and collapses every 'JSAnnot' to 'JSNoAnnot' so that 'TokenPn'
-- coordinates (which differ between in-context and slice-isolation
-- parses) don't confuse 'stripA'-based comparisons downstream.
annotateWithSpans :: JSTerm l -> JSTermAnn (Maybe SourceSpan) l
annotateWithSpans = clearEmptyAnn . cata alg
  where
    alg :: Alg (Sum JSSig) (JSTermAnn (Maybe SourceSpan))
    alg f
      | Just (JSAnnot pn _) <- proj @JSAnnot f
      = Term (inj JSNoAnnot :&: tokenPosnSpan pn)
      | otherwise
      = Term (f :&: maybe childSpan (\w -> extendSpanBy (w - 1) childSpan) (tokenWidth f))
      where
        childSpan = unionSpans (hfoldMap (\c -> [getAnn c]) f)

    tokenPosnSpan :: JSTermAnn (Maybe SourceSpan) TokenPosnL -> Maybe SourceSpan
    tokenPosnSpan (TokenPn _ row col ::&:: _)
      | (row, col) /= (0, 0) = Just (mkSourceSpan "" (row, col) (row, col))
    tokenPosnSpan _          = Nothing

    -- Atomic token widths: 'JSAnnot' only records start positions, so
    -- for nodes whose textual payload is observable we extend the
    -- synthesised end by the payload's length.
    tokenWidth f
      | Just e <- proj @JSExpression f = jsExprTokenWidth e
      | Just p <- proj @JSPropertyName f = jsPropertyNameWidth p
      | Just op <- proj @JSUnaryOp f   = Just (jsUnaryOpWidth op)
      | Just op <- proj @JSBinOp f     = Just (jsBinOpWidth op)
      | Just op <- proj @JSAssignOp f  = Just (jsAssignOpWidth op)
      | Just (JSIdentName _ s) <- proj @JSIdent f = Just (length s)
      | otherwise = Nothing

    jsExprTokenWidth :: JSExpression e l -> Maybe Int
    jsExprTokenWidth (JSIdentifier    _ s) = Just (length s)
    jsExprTokenWidth (JSDecimal       _ s) = Just (length s)
    jsExprTokenWidth (JSLiteral       _ s) = Just (length s)
    jsExprTokenWidth (JSHexInteger    _ s) = Just (length s)
    jsExprTokenWidth (JSOctal         _ s) = Just (length s)
    jsExprTokenWidth (JSStringLiteral _ s) = Just (length s)
    jsExprTokenWidth (JSRegEx         _ s) = Just (length s)
    jsExprTokenWidth _                     = Nothing

    jsPropertyNameWidth :: JSPropertyName e l -> Maybe Int
    jsPropertyNameWidth (JSPropertyIdent  _ s) = Just (length s)
    jsPropertyNameWidth (JSPropertyString _ s) = Just (length s)
    jsPropertyNameWidth (JSPropertyNumber _ s) = Just (length s)

    jsUnaryOpWidth :: JSUnaryOp e l -> Int
    jsUnaryOpWidth (JSUnaryOpDecr   _) = 2
    jsUnaryOpWidth (JSUnaryOpDelete _) = 6
    jsUnaryOpWidth (JSUnaryOpIncr   _) = 2
    jsUnaryOpWidth (JSUnaryOpMinus  _) = 1
    jsUnaryOpWidth (JSUnaryOpNot    _) = 1
    jsUnaryOpWidth (JSUnaryOpPlus   _) = 1
    jsUnaryOpWidth (JSUnaryOpTilde  _) = 1
    jsUnaryOpWidth (JSUnaryOpTypeof _) = 6
    jsUnaryOpWidth (JSUnaryOpVoid   _) = 4

    jsBinOpWidth :: JSBinOp e l -> Int
    jsBinOpWidth (JSBinOpAnd       _) = 2
    jsBinOpWidth (JSBinOpBitAnd    _) = 1
    jsBinOpWidth (JSBinOpBitOr     _) = 1
    jsBinOpWidth (JSBinOpBitXor    _) = 1
    jsBinOpWidth (JSBinOpDivide    _) = 1
    jsBinOpWidth (JSBinOpEq        _) = 2
    jsBinOpWidth (JSBinOpGe        _) = 2
    jsBinOpWidth (JSBinOpGt        _) = 1
    jsBinOpWidth (JSBinOpIn        _) = 2
    jsBinOpWidth (JSBinOpInstanceOf _) = 10
    jsBinOpWidth (JSBinOpLe        _) = 2
    jsBinOpWidth (JSBinOpLsh       _) = 2
    jsBinOpWidth (JSBinOpLt        _) = 1
    jsBinOpWidth (JSBinOpMinus     _) = 1
    jsBinOpWidth (JSBinOpMod       _) = 1
    jsBinOpWidth (JSBinOpNeq       _) = 2
    jsBinOpWidth (JSBinOpOr        _) = 2
    jsBinOpWidth (JSBinOpPlus      _) = 1
    jsBinOpWidth (JSBinOpRsh       _) = 2
    jsBinOpWidth (JSBinOpStrictEq  _) = 3
    jsBinOpWidth (JSBinOpStrictNeq _) = 3
    jsBinOpWidth (JSBinOpTimes     _) = 1
    jsBinOpWidth (JSBinOpUrsh      _) = 3

    jsAssignOpWidth :: JSAssignOp e l -> Int
    jsAssignOpWidth (JSAssign       _) = 1
    jsAssignOpWidth (JSTimesAssign  _) = 2
    jsAssignOpWidth (JSDivideAssign _) = 2
    jsAssignOpWidth (JSModAssign    _) = 2
    jsAssignOpWidth (JSPlusAssign   _) = 2
    jsAssignOpWidth (JSMinusAssign  _) = 2
    jsAssignOpWidth (JSLshAssign    _) = 3
    jsAssignOpWidth (JSRshAssign    _) = 3
    jsAssignOpWidth (JSUrshAssign   _) = 4
    jsAssignOpWidth (JSBwAndAssign  _) = 2
    jsAssignOpWidth (JSBwXorAssign  _) = 2
    jsAssignOpWidth (JSBwOrAssign   _) = 2

    -- JSAnnot nodes carry punctuation token positions that parent
    -- spans need during the bottom-up synthesis above, but the
    -- collapsed JSNoAnnot placeholders have no textual payload of
    -- their own. Rewrite them to degenerate empty spans after the
    -- parent spans have already been computed.
    clearEmptyAnn :: JSTermAnn (Maybe SourceSpan) l' -> JSTermAnn (Maybe SourceSpan) l'
    clearEmptyAnn (Term (node :&: a)) =
      Term (node' :&: if isNoAnnot node' then fmap emptySpan a else a)
      where
        node' = hfmap clearEmptyAnn node

    isNoAnnot node = case proj @JSAnnot node of
      Just JSNoAnnot -> True
      _              -> False

    emptySpan :: SourceSpan -> SourceSpan
    emptySpan (SourceSpan s _) = SourceSpan s (prevPos s)

    prevPos :: SourcePos -> SourcePos
    prevPos (SourcePos f r c) = SourcePos f r (c - 1)

extendSpanBy :: Int -> Maybe SourceSpan -> Maybe SourceSpan
extendSpanBy n (Just (SourceSpan s (SourcePos f r c))) =
  Just (SourceSpan s (SourcePos f r (c + max 0 n)))
extendSpanBy _ Nothing = Nothing

unionSpans :: [Maybe SourceSpan] -> Maybe SourceSpan
unionSpans = foldr step Nothing
  where
    step Nothing s = s
    step s Nothing = s
    step (Just (SourceSpan s1 e1)) (Just (SourceSpan s2 e2)) =
      Just $ SourceSpan (minPos s1 s2) (maxPos e1 e2)

    minPos a@(SourcePos _ r1 c1) b@(SourcePos _ r2 c2) =
      if (r1, c1) <= (r2, c2) then a else b
    maxPos a@(SourcePos _ r1 c1) b@(SourcePos _ r2 c2) =
      if (r1, c1) >= (r2, c2) then a else b
#endif
