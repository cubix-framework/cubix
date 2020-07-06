{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fcontext-stack=300     #-}

module Cubix.Language.JavaScript.Cfg.Test where

import           Control.Lens hiding ( para )
import           Control.Monad ( when )
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Proxy
import           Data.Typeable
import qualified Data.Set as Set
import           Hedgehog
import qualified Hedgehog.Internal.Property as H


import           Cubix.Language.Info
import           Cubix.Language.JavaScript.Parametric.Common as JSCommon
import qualified Cubix.Language.JavaScript.Parametric.Full as JSFull
import           Cubix.Language.Parametric.Semantics.Cfg hiding ( enter, exit )
import           Cubix.ParsePretty
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, (:*:), HFix, (:->), NatM, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), inj', inject', EqHF, remA, (:-<:), proj, Cxt (..), inj, hfoldMap, K (..), cata, hfold, HFoldable )
import           Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )

import           Cubix.Language.Parametric.Cfg.Test
import qualified Cubix.Language.Parametric.Syntax as S

unit_js_cfg :: FilePath -> Property
unit_js_cfg fp =
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile fp
    (tLab, cfg) <- makeJavascriptEnv t
    runReaderT (assertCfgWellFormedness tLab) cfg
    pure ()

makeJavascriptEnv :: (MonadIO m) => MJSTerm JSASTL -> m (MJSTermLab JSASTL, Cfg MJSSig)
makeJavascriptEnv t = do
  gen <- mkCSLabelGen
  let tLab = labelProg gen t
      cfg = makeCfg tLab
  pure (tLab, cfg)

instance AssertCfgWellFormed MJSSig S.MaybeF
instance AssertCfgWellFormed MJSSig S.ListF where
  assertCfgWellFormed t@(inject' -> stripA -> t0)
    | isSort (Proxy :: Proxy [BlockItemL]) t0 ||
      isSort (Proxy :: Proxy [JSStatementL]) t0 = do
        case t of
          (remA -> S.ConsF v vs) -> assertCfgIsGenericAuto (inject' t) [E v, E vs]
          (remA -> S.NilF)       -> assertCfgIsGeneric (inject' t) []
    | otherwise = assertCfgNoCfgNode (inject' t)  
  
instance AssertCfgWellFormed MJSSig BlockWithPrelude
instance AssertCfgWellFormed MJSSig CommentAnnotation
instance AssertCfgWellFormed MJSSig TokenPosn
instance AssertCfgWellFormed MJSSig JSCommaTrailingListF
instance AssertCfgWellFormed MJSSig JSCommaListF
instance AssertCfgWellFormed MJSSig JSUnaryOp
instance AssertCfgWellFormed MJSSig JSTryFinally
instance AssertCfgWellFormed MJSSig JSTryCatch
instance AssertCfgWellFormed MJSSig JSSwitchParts
instance AssertCfgWellFormed MJSSig JSSemi
instance AssertCfgWellFormed MJSSig JSPropertyName
instance AssertCfgWellFormed MJSSig JSObjectProperty
instance AssertCfgWellFormed MJSSig JSBlock
instance AssertCfgWellFormed MJSSig JSBinOp
instance AssertCfgWellFormed MJSSig JSAssignOp
instance AssertCfgWellFormed MJSSig JSArrayElement
instance AssertCfgWellFormed MJSSig JSAnnot
instance AssertCfgWellFormed MJSSig JSAccessor
instance AssertCfgWellFormed MJSSig JSAST

instance AssertCfgWellFormed MJSSig S.EmptyParameterAttrs
instance AssertCfgWellFormed MJSSig S.PositionalParameter
instance AssertCfgWellFormed MJSSig S.EmptyFunctionDefAttrs
instance AssertCfgWellFormed MJSSig S.FunctionDef where
  assertCfgWellFormed t@(remA -> S.FunctionDef {}) =
    -- NOTE: def is suspended, so check that
    assertCfgIsGenericAuto (inject' t) []
    
  
instance AssertCfgWellFormed MJSSig S.PositionalArgument
instance AssertCfgWellFormed MJSSig S.FunctionArgumentList
instance AssertCfgWellFormed MJSSig S.EmptyFunctionCallAttrs
instance AssertCfgWellFormed MJSSig S.FunctionCall
instance AssertCfgWellFormed MJSSig S.EmptyMultiLocalVarDeclCommonAttrs
instance AssertCfgWellFormed MJSSig S.EmptyBlockEnd
instance AssertCfgWellFormed MJSSig S.MultiLocalVarDecl
instance AssertCfgWellFormed MJSSig S.EmptyLocalVarDeclAttrs
instance AssertCfgWellFormed MJSSig S.TupleBinder
instance AssertCfgWellFormed MJSSig S.Block
instance AssertCfgWellFormed MJSSig S.Assign
instance AssertCfgWellFormed MJSSig S.AssignOpEquals
instance AssertCfgWellFormed MJSSig S.Ident
instance AssertCfgWellFormed MJSSig S.SingleLocalVarDecl
instance AssertCfgWellFormed MJSSig S.OptLocalVarInit

-- Is
instance AssertCfgWellFormed MJSSig MaybeIdentIsJSIdent
instance AssertCfgWellFormed MJSSig JSBlockIsFunctionBody
instance AssertCfgWellFormed MJSSig FunctionDefIsJSStatement where
  assertCfgWellFormed t@(remA -> FunctionDefIsJSStatement def) =
    assertCfgIsGenericAuto (inject' t) [E def]
  
instance AssertCfgWellFormed MJSSig JSExpressionIsFunctionExp
instance AssertCfgWellFormed MJSSig JSExpressionIsPositionalArgExp
instance AssertCfgWellFormed MJSSig JSBlockIsJSAST
instance AssertCfgWellFormed MJSSig JSStatementIsBlockItem
instance AssertCfgWellFormed MJSSig JSAssignOpIsAssignOp
instance AssertCfgWellFormed MJSSig JSExpressionIsLhs
instance AssertCfgWellFormed MJSSig JSExpressionIsRhs
instance AssertCfgWellFormed MJSSig JSExpressionIsVarDeclBinder
instance AssertCfgWellFormed MJSSig JSExpressionIsLocalVarInit

instance AssertCfgWellFormed MJSSig IdentIsJSExpression where
  assertCfgWellFormed t@(remA -> IdentIsJSExpression e) = do
    let t0 = inject' t
    -- NOTE: This lookup is necessary, because we ignore
    --       Idents in the for loop in CFGs
    mlab <- lookupCfgNodeLabel t0 EnterNode
    case mlab of
      Just _ -> assertCfgIsGenericAuto (inject' t) []
      _      -> pure ()
instance AssertCfgWellFormed MJSSig FunctionCallIsJSExpression where
  assertCfgWellFormed t@(remA -> FunctionCallIsJSExpression call) =
    assertCfgIsGenericAuto (inject' t) [E call]
  
instance AssertCfgWellFormed MJSSig BlockIsJSStatement
instance AssertCfgWellFormed MJSSig AssignIsJSExpression where
  assertCfgWellFormed t@(remA -> AssignIsJSExpression asn) =
    case project' asn of
      Just (remA -> Assign lhs _ rhs) -> assertCfgIsGeneric (inject' t) [eLhs lhs, eRhs rhs]

    where eLhs :: MJSTermLab LhsL -> E MJSTermLab
          eLhs (project' -> Just (remA -> JSExpressionIsLhs e)) = E e

          eRhs :: MJSTermLab RhsL -> E MJSTermLab
          eRhs (project' -> Just (remA -> JSExpressionIsRhs e)) = E e

instance AssertCfgWellFormed MJSSig MultiLocalVarDeclIsJSStatement where
  assertCfgWellFormed t@(remA -> MultiLocalVarDeclIsJSStatement dec) =
    assertCfgIsGenericAuto (inject' t) [E dec]

instance AssertCfgWellFormed MJSSig JSExpression where
  assertCfgWellFormed t@(JSExpressionTernary test _ succ _ fail :&: _) = do
    assertCfgCondOp (inject' t) test succ fail
  assertCfgWellFormed t@(JSExpressionBinary e1 op e2 :&: _) = do
    case extractOp op of
      JSBinOpAnd _ -> assertCfgShortCircuit (inject' t) e1 e2
      JSBinOpOr _  -> assertCfgShortCircuit (inject' t) e1 e2
      _   -> assertCfgIsGeneric (inject' t) [E e1, E e2]

    where extractOp :: MJSTermLab JSBinOpL -> JSBinOp MJSTerm JSBinOpL
          extractOp (stripA -> project -> Just bp) = bp

  assertCfgWellFormed t@(remA -> JSIdentifier {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> JSDecimal {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> JSLiteral {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> JSHexInteger {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> JSOctal {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> JSStringLiteral {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> JSRegEx {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> JSArrayLiteral _ elems _) =
    assertCfgIsGeneric (inject' t) (concatMap go $ S.extractF elems)

    where go :: MJSTermLab JSArrayElementL -> [E MJSTermLab]
          go t = case project' t of
            Just (remA -> JSArrayElement e) -> [E e]
            Just (remA -> JSArrayComma {})  -> []
  -- JSAssignExpression does not occur
  -- JSCallExpression does not occur
  assertCfgWellFormed t@(remA -> JSCallExpressionDot b _ a) = assertCfgIsGeneric (inject' t) [E b, E a]
  assertCfgWellFormed t@(remA -> JSCallExpressionSquare b _ a _) = assertCfgIsGeneric (inject' t) [E b, E a]  
  assertCfgWellFormed t@(remA -> JSCommaExpression l _ r) = assertCfgIsGeneric (inject' t) [E l, E r]
  assertCfgWellFormed t@(remA -> JSExpressionParen _ e _) = assertCfgIsGeneric (inject' t) [E e]  
  assertCfgWellFormed t@(remA -> JSExpressionPostfix e _) = assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t@(remA -> JSFunctionExpression _ _ _ _ _ blk) =
    assertCfgFunction (inject' t) (extractJSBlock blk)
  assertCfgWellFormed t@(remA -> JSMemberDot l _ r) = assertCfgIsGeneric (inject' t) [E l, E r]
  assertCfgWellFormed t@(remA -> JSNewExpression _ e) = assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t@(remA -> JSUnaryExpression _ e) = assertCfgIsGeneric (inject' t) [E e]  
  assertCfgWellFormed t@(remA -> JSObjectLiteral _ objPropList _) = assertCfgIsGenericAuto (inject' t) [E objPropList]
  assertCfgWellFormed t@(remA -> JSMemberExpression e _ es _) = assertCfgIsGenericAuto (inject' t) [E e, E es]
  assertCfgWellFormed t@(remA -> JSMemberNew _ e _ es _) = assertCfgIsGenericAuto (inject' t) [E e, E es]
  assertCfgWellFormed t@(remA -> JSMemberSquare e1 _ e2 _) = assertCfgIsGenericAuto (inject' t) [E e1, E e2]
  assertCfgWellFormed t@(remA -> JSVarInitExpression e init) = assertCfgIsGenericAuto (inject' t) [E e, E init]
  assertCfgWellFormed t = error $ "Impossible case: " ++ show (inject' t)

instance AssertCfgWellFormed MJSSig JSStatement where
  assertCfgWellFormed t@(remA -> JSDoWhile _ body _ _ cond _ _) =
    assertCfgDoWhile (inject' t) (extractBlock body) cond
  assertCfgWellFormed t@(remA -> JSIf _ _ cond _ thn) =
    assertCfgIf (inject' t) cond (extractBlock thn)
  assertCfgWellFormed t@(remA -> JSIfElse _ _ cond _ thn _ els) =
    assertCfgIfElse (inject' t) cond (extractBlock thn) (extractBlock els)
  assertCfgWellFormed t@(remA -> JSReturn _ e _) =
    assertCfgReturn (inject' t) (S.extractF e)
  assertCfgWellFormed t@(remA -> JSThrow _ e _) =
    assertCfgReturn (inject' t) (Just e)
  assertCfgWellFormed t@(remA -> JSTry _ block catchs finally) =
    assertCfgTry (inject' t) (extractJSBlock block) (map extractJSCatch $ S.extractF catchs) (extractJSFinally finally)
  assertCfgWellFormed t@(remA -> JSWhile _ _ e _ b) =
    assertCfgWhile (inject' t) e (extractBlock b)
  assertCfgWellFormed t@(remA -> JSSwitch _ _ exp _ _ cases _ _) =
    assertCfgSwitch (inject' t) exp (S.extractF cases)
  assertCfgWellFormed t@(remA -> JSBreak _ (stripA -> JSIdent' targ) _) =
    assertCfgBreakLabeled (inject' t) targ
  assertCfgWellFormed t@(remA -> JSBreak {}) =
    assertCfgBreak (inject' t)
  assertCfgWellFormed t@(remA -> JSContinue _ (stripA -> JSIdent' targ) _) =
    assertCfgContinueLabeled (inject' t) targ
  assertCfgWellFormed t@(remA -> JSContinue {}) =
    assertCfgContinue (inject' t)
  assertCfgWellFormed t@(remA -> JSLabelled _ _ stmt) =
    assertCfgIsGeneric (inject' t) [extractBlock stmt]    
  assertCfgWellFormed t@(remA -> JSEmptyStatement {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> JSExpressionStatement e _) =
    assertCfgIsGeneric (inject' t) [E e]    
  assertCfgWellFormed t@(remA -> JSStatementBlock _ stmts _ _) =
    case S.extractF stmts of
      [] -> assertCfgIsGeneric (inject' t) [E stmts]
      _  -> assertCfgSubtermsAuto (inject' t) [E stmts]
  assertCfgWellFormed t@(remA -> JSWith _ _ e _ stmt _) =
    assertCfgIsGeneric (inject' t) [E e, extractBlock stmt]
  -- JSStatementBlock does not exist
  -- JSConstant does not exist
  -- JSAssignStatement does not exist
  -- JSFunction  does not exist
  -- JSMethodCall does not exist
  -- JSVariable does not exist
  -- JSFor, JSForIn, JSForVar, JSForVarIn does not exist
  assertCfgWellFormed t = error $ "Impossible case: " ++ show (inject' t)

instance AssertCfgWellFormed MJSSig JSCommon.JSFor where
  assertCfgWellFormed t@(JSCommon.JSFor init cond step body :&: _) = do
    let inits = fmap (\t -> (E t, E t)) (toMaybe $ S.extractF init)
    assertCfgFor (inject' t) inits (toMaybe $ S.extractF cond) (toMaybe $ S.extractF step) (extractBlock body)
      where toMaybe [] = Nothing
            toMaybe [x] = Just x
            toMaybe xs = error $ "Panic: unexpected list: " ++ show xs
  assertCfgWellFormed t@(JSCommon.JSForVar init cond step body :&: _) = do
    inits0 <- subtermWithCfg (inject' t) init
    assertCfgFor (inject' t) inits0 (toMaybe $ S.extractF cond) (toMaybe $ S.extractF step) (extractBlock body)
      where toMaybe [] = Nothing
            toMaybe [x] = Just x
            toMaybe xs = error $ "Panic: unexpected list: " ++ show xs
  assertCfgWellFormed t@(JSCommon.JSForIn _ _ e0 body :&: _) = do
    assertCfgWhile (inject' t) e0 (extractBlock body)
  assertCfgWellFormed t@(JSCommon.JSForVarIn _ _ e0 body :&: _) = do
    assertCfgWhile (inject' t) e0 (extractBlock body)

assertCfgShortCircuit ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs
  ) => TermLab fs l -> TermLab fs e1 -> TermLab fs e2 -> m ()
assertCfgShortCircuit t e1 e2 = do
  (enSExp, exSExp) <- getEnterExitPair t t
  (enE1, exE1) <- getEnterExitPair t e1
  (enE2, exE2) <- getEnterExitPair t e2

  assertEdges t [ (enSExp, enE1)
                , (exE1, exSExp)
                , (exE1, enE2)
                , (exE2, exSExp)
                ]
                [ enSExp, exSExp, enE1
                , exE1, enE2, exE2
                ]

assertCfgCondOp ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs  
  ) => TermLab fs e -> TermLab fs e -> TermLab fs e -> TermLab fs e -> m ()
assertCfgCondOp t test succ fail = do
  (enTop, exTop) <- getEnterExitPair t t
  (enTest, exTest) <- getEnterExitPair t test
  (enSucc, exSucc) <- getEnterExitPair t succ
  (enFail, exFail) <- getEnterExitPair t fail

  assertEdges t [(enTop, enTest), (exTest, enSucc), (exTest, enFail), (exSucc, exTop), (exFail, exTop)]
                [enTop, exTop, enTest, exTest, enSucc, exSucc, enFail, exFail]

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `break`.
--       * that outgoing edge is to a node which has an `ExitNode` type.
--       * that outgoing edge is to a node which has a label with this name
--       * there are no incoming nodes in exit node of `break`.
assertCfgBreakLabeled ::
  ( MonadTest m
  , MonadReader (Cfg MJSSig) m
  ) => TermLab MJSSig a -> String -> m ()
assertCfgBreakLabeled b labName = do
  (enBreak, exBreak) <- getEnterExitPair b b
  let jmpNodeLabs = enBreak ^. cfg_node_succs
  assert (length jmpNodeLabs == 1)

  let enJmpLab = head (Set.toList jmpNodeLabs)
  menJump <- preview (cur_cfg.cfg_nodes.(ix enJmpLab))
  enJmp <- assertJust "Cfg label lookup: " menJump
  assert (checkNodeType enJmp)
  assert (checkLab (enJmp ^.  cfg_node_term))  
  assert (length (exBreak ^. cfg_node_prevs) == 0)

    where
      checkNodeType :: CfgNode MJSSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == ExitNode

      checkLab ::  E (TermLab MJSSig) -> Bool
      checkLab (E (project' -> Just (remA -> JSLabelled (stripA -> JSIdent' n) _ _)))
        | labName == n = True
        | otherwise = False
      checkLab _ = False

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `continue`.
--       * that outgoing edge is to a node which has an `LoopEntry` type.
--       * that outgoing edge is to a node which has a label with this name [ TODO: need to be added; but how to get to the label? ]
--       * there are no incoming nodes in exit node of `continue`.
assertCfgContinueLabeled ::
  ( MonadTest m
  , MonadReader (Cfg MJSSig) m
  ) => TermLab MJSSig a -> String -> m ()
assertCfgContinueLabeled b labName = do
  (enContinue, exContinue) <- getEnterExitPair b b
  let jmpNodeLabs = enContinue ^. cfg_node_succs
  assert (length jmpNodeLabs == 1)

  let enJmpLab = head (Set.toList jmpNodeLabs)
  menJump <- preview (cur_cfg.cfg_nodes.(ix enJmpLab))
  enJmp <- assertJust "Cfg label lookup: " menJump
  assert (checkNodeType enJmp)
  -- assert (checkLab (enJmp ^.  cfg_node_term)) 
  assert (length (exContinue ^. cfg_node_prevs) == 0)

    where
      checkNodeType :: CfgNode MJSSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == LoopEntryNode

      checkLab ::  E (TermLab MJSSig) -> Bool
      checkLab (E (project' -> Just (remA -> JSLabelled (stripA -> JSIdent' n) _ _)))
        | labName == n = True
        | otherwise = False
      checkLab _ = False

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `break`.
--       * that outgoing edge is to a node which has an `ExitNode` type.
--       * that outgoing edge is to a node which is switch or loop-like (is one of `Do while/For*/While`)
--       * there are no incoming nodes in exit node of `break`.
assertCfgBreak ::
  ( MonadTest m
  , MonadReader (Cfg MJSSig) m
  ) => TermLab MJSSig a -> m ()
assertCfgBreak b = do
  (enBreak, exBreak) <- getEnterExitPair b b
  let jmpNodeLabs = enBreak ^. cfg_node_succs
  assert (length jmpNodeLabs == 1)

  let enJmpLab = head (Set.toList jmpNodeLabs)
  menJump <- preview (cur_cfg.cfg_nodes.(ix enJmpLab))
  enJmp <- assertJust "Cfg label lookup: " menJump
  assert (checkNodeType enJmp)
  assert (length (exBreak ^. cfg_node_prevs) == 0)

    where
      checkNodeType :: CfgNode MJSSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == ExitNode                    &&
        (\(E t) -> isLoopLikeNode t || isSwitchNode t) (node ^. cfg_node_term)

assertCfgWellFormedDefault :: (MonadTest m, MonadReader (Cfg MJSSig) m, f :-<: MJSSig) => (f :&: Label) MJSTermLab l -> m ()
assertCfgWellFormedDefault p = pure ()

assertCfgDoWhile ::
  ( MonadTest m
  , MonadReader (Cfg gs) m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  , All HFoldable gs
  ) => TermLab gs l -> E (TermLab gs) -> TermLab gs j -> m ()
assertCfgDoWhile t (E b) e = do
  (enDoWhile, exDoWhile) <- getEnterExitPair t t
  loDoWhile <- getLoopEntry t t

  (enBody, exBody) <- getEnterExitPair t b
  (enExp, exExp) <- getEnterExitPair t e

  assertEdges t [ (enDoWhile, enBody)
                , (exExp, enBody)
                , (exExp, exDoWhile)
                , (exBody, loDoWhile)
                , (loDoWhile, enExp)
                ]
                [ enDoWhile, exDoWhile, loDoWhile
                , enBody, exBody, enExp, exExp
                ]

assertCfgIfElse ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs
  ) => TermLab fs a -> TermLab fs e -> E (TermLab fs) -> E (TermLab fs) -> m ()
assertCfgIfElse t cond (E thn) (E els) = do
  (enIf, exIf) <- getEnterExitPair t t
  (enCond, exCond) <- getEnterExitPair t cond
  (enThn, exThn) <- getEnterExitPair t thn
  (enEls, exEls) <- getEnterExitPair t els
  midNode <- getIEP 0 t t
  assertEdges t [ (enIf, midNode), (midNode, enCond), (exCond, enThn), (exThn, exIf), (exCond, enEls), (exEls, exIf)]
                [enIf, exIf, midNode, enCond, exCond, enThn, exThn, enEls, exEls]

assertCfgIf ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs
  ) => TermLab fs a -> TermLab fs e -> E (TermLab fs) -> m ()
assertCfgIf t cond (E thn) = do
  (enIf, exIf) <- getEnterExitPair t t
  (enCond, exCond) <- getEnterExitPair t cond
  (enThn, exThn) <- getEnterExitPair t thn
  midNode <- getIEP 0 t t
  assertEdges t [ (enIf, midNode), (midNode, enCond), (exCond, enThn), (exCond, exIf), (exThn, exIf), (exCond, exIf) ]
                [enIf, exIf, midNode, enCond, exCond, enThn, exThn]

-- NOTE: Asserts that
--        * If there is no expression, then return's entry and exit node are *not* connected.
--        * If there is an expression, then return's entry is
--          connected to expression's entry, and expression's exit
--          is not connected to return's exit, and return's entry
--          is not connected to return's exit.
assertCfgReturn ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs l -> Maybe (TermLab fs e) -> m ()
assertCfgReturn t me = do
  (enReturn, exReturn) <- getEnterExitPair t t
  assertNoEdge t enReturn exReturn
  case me of
    Nothing -> pure ()
    Just exp -> do
      (enExp, exExp) <- getEnterExitPair t exp
      assertEdge t enReturn enExp
      assertNoEdge t exExp exReturn

assertCfgWhile ::
  forall fs a e b m.
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs
  ) => TermLab fs a -> TermLab fs e -> E (TermLab fs) -> m ()
assertCfgWhile t e (E b) = do
  (enWhile, exWhile) <- getEnterExitPair t t
  loWhile <- getLoopEntry t t
  (enCond, exCond) <- getEnterExitPair t e
  (enBody, exBody) <- getEnterExitPair t b

  assertEdges t ([ (enWhile, loWhile)
                 , (loWhile, enCond)
                 , (exBody, loWhile)
                 , (exCond, enBody)
                 , (exCond, exWhile)
                 ])
                [ enWhile, exWhile, loWhile
                , enCond, exCond, enBody, exBody
                ]

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `continue`.
--       * that outgoing edge is to a node which has an `LoopEntryNode` type.
--       * that outgoing edge is to a node which is loop-like (is one of `Do while/For*/While`)
--       * there are no incoming nodes in exit node of `continue`.
assertCfgContinue ::
  ( MonadTest m
  , MonadReader (Cfg MJSSig) m
  ) => TermLab MJSSig a -> m ()
assertCfgContinue c = do
  (enContinue, exContinue) <- getEnterExitPair c c
  let jmpNodeLabs = enContinue ^. cfg_node_succs
  assert (length jmpNodeLabs == 1)

  let enJmpLab = head (Set.toList jmpNodeLabs)
  menJump <- preview (cur_cfg.cfg_nodes.(ix enJmpLab))
  enJmp <- assertJust "Cfg label lookup: " menJump
  assert (checkNodeType enJmp)
  assert (length (exContinue ^. cfg_node_prevs) == 0)

    where
      checkNodeType :: CfgNode MJSSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == LoopEntryNode                &&
        (\(E t) -> isLoopLikeNode t) (node ^. cfg_node_term)

assertCfgFor ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs
  ) => TermLab fs l -> Maybe (E (TermLab fs), E (TermLab fs)) -> Maybe (TermLab fs e) -> Maybe (TermLab fs e) -> E (TermLab fs) -> m ()
assertCfgFor t mInit mCond mStep (E body) = do
  eepFor <- getEnterExitPair t t
  loFor <- getLoopEntry t t
  eepBody <- getEnterExitPair t body
  mEnInit <- traverse (getEnterNodeE t . fst) mInit
  mExInit <- traverse (getExitNodeE t . snd) mInit
  let mEEPInit = (,) <$> mEnInit <*> mExInit
  mEEPCond <- traverse (getEnterExitPair t) mCond
  mEEPStep <- traverse (getEnterExitPair t) mStep

  let edges =
        let (e0, vs0) = mJoinEdgesR [] (identEdge $ enter eepFor) mEEPInit
            (e1, vs1) = mJoinEdgesR vs0 e0 mEEPCond
            (_ , vs2) = joinEdges   vs1 e1 eepBody

            (e3, vs3) = joinEdges   vs2 eepBody (identEdge loFor)
            (e4, vs4) = mJoinEdgesR vs3 e3 mEEPStep
            (e5, vs5) = mJoinEdgesR vs4 e4 mEEPCond
            (_,  vs6) = joinEdges  vs5 e5 eepBody
            (_,  vs7) = mJoinEdgesL vs6 mEEPCond (identEdge $ exit eepFor)

        in  vs7
      nodes =
        let ns = catMaybes [mEEPInit, mEEPCond, mEEPStep]
        in loFor : map enter ns ++ map exit ns

  assertEdges t (nub edges) nodes

assertCfgTry ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All HFunctor fs
  ) => TermLab fs l -> E (TermLab fs) -> [(E (TermLab fs), E (TermLab fs))] -> Maybe (E (TermLab fs)) -> m ()
assertCfgTry t (E blk) catchs finally = do
  assertCfgIsSuspended t blk
  mapM_ (uncurryE (assertCfgIsSuspended' t)) catchs
  mapM_ (\(E t0) -> (assertCfgIsSuspended t) t0) finally

  where
    uncurryE :: (forall l1 l2. TermLab fs l1 -> TermLab fs l2 -> m ()) -> (E (TermLab fs), E (TermLab fs)) -> m ()
    uncurryE f (E t1, E t2) = f t1 t2

assertCfgSwitch ::
  forall m l.
  ( MonadReader (Cfg MJSSig) m
  , MonadTest m
  ) => TermLab MJSSig l -> TermLab MJSSig JSExpressionL -> [TermLab MJSSig JSSwitchPartsL] -> m ()
assertCfgSwitch t e cs = do
  (swEn, swEx) <- getEnterExitPair t t
  (eEn, eEx) <- getEnterExitPair t e
  csEEPs <- mapM getEEPCase cs

  let jumps = map (\cEEP -> (eEx, fst cEEP)) csEEPs
      fallthrough = zipWith (\p n -> (snd p, fst n)) csEEPs (tail csEEPs) ++ [(snd (last  csEEPs), swEx)]
  assertEdges t ([(swEn, eEn), (eEn, eEx)] ++ jumps ++ fallthrough)
                ([swEn, swEx, eEn, eEx] ++ map fst csEEPs ++ map snd csEEPs)
  
  where getEEPCase :: TermLab MJSSig JSSwitchPartsL -> m (CfgNode MJSSig, CfgNode MJSSig)
        getEEPCase t0 = case project' t0 of
          Just (remA -> JSCase _ e _ es) -> do
            eEEP <- getEnterExitPair t e
            esEEP <- getEnterExitPair t es
            pure (fst eEEP, snd esEEP)
          Just (remA -> JSDefault _ _ es) ->
            getEnterExitPair t es

assertCfgFunction ::
  ( MonadTest m
  , MonadReader (Cfg MJSSig) m
  ) => TermLab MJSSig a -> E (TermLab MJSSig) -> m ()
assertCfgFunction t (E body) = do
  assertCfgIsSuspended t body
  assertCfgSubtermsAuto t []

extractBlock :: TermLab MJSSig JSStatementL -> E (TermLab MJSSig)
extractBlock t@(project' -> Just (JSStatementBlock _ stmts _ _ :&: _)) =
  case S.extractF stmts of
    [] -> E t
    _  -> E stmts
extractBlock t                                                       = E t

extractJSBlock :: TermLab MJSSig JSBlockL -> E (TermLab MJSSig)
extractJSBlock (project' -> Just (JSBlock _ stmts _ :&: _)) = E stmts
extractJSBlock (project' -> Just (BlockWithPrelude _ blk :&: _)) =
  case project' blk of
    Just (remA -> Block stmts _) -> E stmts

extractJSCatch :: TermLab MJSSig JSTryCatchL -> (E (TermLab MJSSig), E (TermLab MJSSig))
extractJSCatch t@(project' -> Just (JSCatch _ _ e _ b :&: _)) = (E e, extractJSBlock b)
extractJSCatch t@(project' -> Just (JSCatchIf _ _ e _ cond _ b :&: _)) = (E cond, extractJSBlock b)

extractJSFinally :: TermLab MJSSig JSTryFinallyL -> Maybe (E (TermLab MJSSig))
extractJSFinally t@(project' -> Just (JSFinally _ b :&: _)) = Just (extractJSBlock b)
extractJSFinally t@(project' -> Just (JSNoFinally :&: _)) = Nothing

isLoopLikeNode :: TermLab MJSSig l -> Bool
isLoopLikeNode (project' -> Just (JSWhile {} :&: _))= True
isLoopLikeNode (project' -> Just (JSDoWhile {} :&: _)) = True
isLoopLikeNode (project' -> Just (JSFor {} :&: _)) = True
isLoopLikeNode (project' -> Just (JSForVar {} :&: _)) = True
isLoopLikeNode (project' -> Just (JSForIn {} :&: _)) = True
isLoopLikeNode (project' -> Just (JSForVarIn {} :&: _)) = True
isLoopLikeNode _ = False

isSwitchNode :: TermLab MJSSig l -> Bool
isSwitchNode (project' -> Just (JSSwitch {} :&: _))= True
isSwitchNode _ = False


-- hardcoded integration tests
integration_javascript_cfg :: FilePath -> Property
integration_javascript_cfg path = 
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile path
    edges <- liftIO $ parseEdges path
    
    (_, cfg) <- makeJavascriptEnv t
    integration_cfg edges cfg
