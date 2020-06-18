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

module Cubix.Language.Python.Cfg.Test where

import           Control.Lens hiding ( para, List )
import           Control.Monad ( when )
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Proxy
import           Data.Typeable
import qualified Data.Set as Set
import           Hedgehog hiding ( Var )
import qualified Hedgehog.Internal.Property as H


import           Cubix.Language.Info
import           Cubix.Language.Python.Parametric.Common as Common
import qualified Cubix.Language.Python.Parametric.Full as Full
import           Cubix.Language.Parametric.Semantics.Cfg hiding ( enter, exit )
import           Cubix.ParsePretty
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, (:*:), HFix, (:->), NatM, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), inj', inject', EqHF, remA, (:-<:), proj, Cxt (..), inj, hfoldMap, K (..), cata, hfold )
import           Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )

import           Cubix.Language.Parametric.Cfg.Test
import qualified Cubix.Language.Parametric.Syntax as S
import           Cubix.Language.Parametric.InjF ( projF )

unit_python_cfg :: FilePath -> Property
unit_python_cfg fp =
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile fp
    (tLab, cfg) <- makePyEnv t
    runReaderT (assertCfgWellFormedness tLab) cfg
    pure ()

makePyEnv :: (MonadIO m) => MPythonTerm ModuleL -> m (MPythonTermLab ModuleL, Cfg MPythonSig)
makePyEnv t = do
  gen <- mkCSLabelGen
  let tLab = labelProg gen t
      cfg = makeCfg tLab
  pure (tLab, cfg)

instance AssertCfgWellFormed MPythonSig PyBlockIsFunctionBody
instance AssertCfgWellFormed MPythonSig FunctionDefIsStatement where
  assertCfgWellFormed t@(remA -> FunctionDefIsStatement def) =
    assertCfgIsGeneric (inject' t) [E def]

instance AssertCfgWellFormed MPythonSig ExprIsReceiver
instance AssertCfgWellFormed MPythonSig ExprIsPositionalArgExp
instance AssertCfgWellFormed MPythonSig ExprIsFunctionExp
instance AssertCfgWellFormed MPythonSig FunctionCallIsExpr where
  assertCfgWellFormed t@(remA -> FunctionCallIsExpr call) =
    assertCfgIsGenericAuto (inject' t) [E call]
  
instance AssertCfgWellFormed MPythonSig PyClassIsStatement where
  assertCfgWellFormed t@(remA -> PyClassIsStatement call) =
    assertCfgIsGenericAuto (inject' t) [E call]
    
instance AssertCfgWellFormed MPythonSig IdentIsPyLValue
instance AssertCfgWellFormed MPythonSig PyCompIsExpr where
  assertCfgWellFormed t@(remA -> PyCompIsExpr comp) =
    assertCfgIsGenericAuto (inject' t) [E comp]
    
  
instance AssertCfgWellFormed MPythonSig StatementIsBlockItem
instance AssertCfgWellFormed MPythonSig ExprIsRhs

instance AssertCfgWellFormed MPythonSig AssignIsStatement where
  assertCfgWellFormed t@(remA -> AssignIsStatement asn) =  
    assertCfgIsGenericAuto (inject' t) [E asn]
  
instance AssertCfgWellFormed MPythonSig IdentIsIdent


instance AssertCfgWellFormed MPythonSig PositionalParameter
instance AssertCfgWellFormed MPythonSig FunctionDef where
  assertCfgWellFormed t@(remA -> S.FunctionDef _ _ _ body) =
    -- NOTE: suspended
    assertCfgIsGeneric (inject' t) []
    
instance AssertCfgWellFormed MPythonSig ReceiverArg
instance AssertCfgWellFormed MPythonSig PositionalArgument
instance AssertCfgWellFormed MPythonSig FunctionArgumentList
instance AssertCfgWellFormed MPythonSig FunctionIdent
instance AssertCfgWellFormed MPythonSig EmptyFunctionCallAttrs
instance AssertCfgWellFormed MPythonSig FunctionCall
instance AssertCfgWellFormed MPythonSig EmptyBlockEnd
instance AssertCfgWellFormed MPythonSig Block

-- TODO: Assign is supposed to work the other way.
instance AssertCfgWellFormed MPythonSig Assign
instance AssertCfgWellFormed MPythonSig AssignOpEquals
instance AssertCfgWellFormed MPythonSig S.Ident

instance AssertCfgWellFormed MPythonSig PyCondExpr
instance AssertCfgWellFormed MPythonSig PyComp
instance AssertCfgWellFormed MPythonSig PyClass where
  assertCfgWellFormed t@(remA -> PyClass _ args body) =
    assertCfgPyClass (inject' t) args body

assertCfgPyClass ::
  ( MonadTest m
  , MonadReader (Cfg MPythonSig) m
  ) => TermLab MPythonSig a -> TermLab MPythonSig b -> TermLab MPythonSig c -> m ()
assertCfgPyClass t =
  error "TODO: PyClass"
  
instance AssertCfgWellFormed MPythonSig PyBlock
instance AssertCfgWellFormed MPythonSig PyStringLit
instance AssertCfgWellFormed MPythonSig PythonParam
instance AssertCfgWellFormed MPythonSig PyParamAttrs
instance AssertCfgWellFormed MPythonSig PyFunDefAttrs
instance AssertCfgWellFormed MPythonSig PythonArg
instance AssertCfgWellFormed MPythonSig PyWithBinder

instance AssertCfgWellFormed MPythonSig PyWith where
  assertCfgWellFormed t@(remA -> PyWith binds body) = do
    assertCfgPyWith (inject' t) (map extractBinder $ S.extractF binds) body
    
    where
      extractBinder :: MPythonTermLab PyWithBinderL -> MPythonTermLab ExprL
      extractBinder (project' -> Just (remA -> PyWithBinder e _)) = e 
    
instance AssertCfgWellFormed MPythonSig PyLhs

instance AssertCfgWellFormed MPythonSig ParenLValue
instance AssertCfgWellFormed MPythonSig SliceLValue
instance AssertCfgWellFormed MPythonSig SubscriptLValue
instance AssertCfgWellFormed MPythonSig StarLValue
instance AssertCfgWellFormed MPythonSig DotLValue
instance AssertCfgWellFormed MPythonSig ListLValue
instance AssertCfgWellFormed MPythonSig TupleLValue
instance AssertCfgWellFormed MPythonSig YieldArg
instance AssertCfgWellFormed MPythonSig Slice
instance AssertCfgWellFormed MPythonSig RaiseExpr
instance AssertCfgWellFormed MPythonSig Parameter
instance AssertCfgWellFormed MPythonSig ParamTuple
instance AssertCfgWellFormed MPythonSig Op
instance AssertCfgWellFormed MPythonSig Module
instance AssertCfgWellFormed MPythonSig ImportRelative
instance AssertCfgWellFormed MPythonSig ImportItem
instance AssertCfgWellFormed MPythonSig Handler
instance AssertCfgWellFormed MPythonSig FromItem
instance AssertCfgWellFormed MPythonSig FromItems
instance AssertCfgWellFormed MPythonSig ExceptClause
instance AssertCfgWellFormed MPythonSig DictKeyDatumList
instance AssertCfgWellFormed MPythonSig Decorator
instance AssertCfgWellFormed MPythonSig ComprehensionExpr
instance AssertCfgWellFormed MPythonSig Comprehension
instance AssertCfgWellFormed MPythonSig CompIter
instance AssertCfgWellFormed MPythonSig CompIf
instance AssertCfgWellFormed MPythonSig CompFor
instance AssertCfgWellFormed MPythonSig Full.AssignOp
instance AssertCfgWellFormed MPythonSig Argument

instance AssertCfgWellFormed MPythonSig S.CharF
instance AssertCfgWellFormed MPythonSig S.UnitF


-- containers
instance AssertCfgWellFormed MPythonSig S.MaybeF
instance AssertCfgWellFormed MPythonSig S.ListF where
  -- TODO: ListF
  assertCfgWellFormed _ = pure ()
instance AssertCfgWellFormed MPythonSig S.PairF

instance AssertCfgWellFormed MPythonSig Statement where
  assertCfgWellFormed t@(remA -> While cond body els _) =
    assertCfgWhileOrForElse (inject' t) cond body els
  assertCfgWellFormed t@(remA -> For _ cond body els _) =
    assertCfgWhileOrForElse (inject' t) cond body els
  assertCfgWellFormed t@(remA -> Conditional clauses els _) =
    assertCfgIfElse (inject' t) (map S.extractF2 (S.extractF clauses)) els
  assertCfgWellFormed t@(remA -> Return e _) =
    assertCfgReturn (inject' t) (S.extractF e) 
  assertCfgWellFormed t@(remA -> Raise e _) =
    assertCfgRaise (inject' t) (extractRaiseExprs e)
  assertCfgWellFormed t@(remA -> Break _) =
    assertCfgBreak (inject' t)
  assertCfgWellFormed t@(remA -> Continue _) =
    assertCfgContinue (inject' t)
  -- NOTE: Assign and Fun does not exist
  assertCfgWellFormed t@(remA -> AsyncFor {}) =
    error "TODO: AsyncFor"
  assertCfgWellFormed t@(remA -> AsyncFun {}) =
    error "TODO: AsyncFun"
  assertCfgWellFormed t@(remA -> AsyncWith {}) =
    error "TODO: AsyncWith"    
  assertCfgWellFormed t@(remA -> Import {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> FromImport {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> AugmentedAssign el _ er _) =
    assertCfgIsGeneric (inject' t) [E el, E er]
  assertCfgWellFormed t@(remA -> AnnotatedAssign ann el er _) =
    assertCfgIsGeneric (inject' t) exps
    where exps = case S.extractF er of
            Just er0 -> [E ann, E el, E er0]
            Nothing -> [E ann, E el]
  assertCfgWellFormed t@(remA -> Decorated decs def _) =
    assertCfgIsGenericAuto (inject' t) [E decs, E def]
  assertCfgWellFormed t@(remA -> Try body handlers els finally _) =
    assertCfgTry (inject' t) body (map extractHandler $ S.extractF handlers) els finally
  assertCfgWellFormed t@(remA -> Pass {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> Delete es _) =
    assertCfgIsGeneric (inject' t) (map E $ S.extractF es)
  assertCfgWellFormed t@(remA -> StmtExpr e _) =
    assertCfgIsGeneric (inject' t) [E e]    
  assertCfgWellFormed t@(remA -> Global {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> NonLocal {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> Assert exprs _) =
    assertCfgIsGeneric (inject' t) (map E $ S.extractF exprs)
  assertCfgWellFormed t = error $ "Impossible case: " ++ show (inject' t)
  
instance AssertCfgWellFormed MPythonSig Expr where
  assertCfgWellFormed t@(remA -> Var {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> Int {}) =
    assertCfgIsGeneric (inject' t) []
  -- skipping LongInt (v2 only)
  assertCfgWellFormed t@(remA -> Float {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> Imaginary {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> Bool {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> None {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> Ellipsis {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> ByteStrings {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> Strings {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> UnicodeStrings {}) =
    assertCfgIsGeneric (inject' t) []
  -- skipping call
  assertCfgWellFormed t@(remA -> SlicedExpr e es _) =
    assertCfgIsGenericAuto (inject' t) [E e, E es]
  assertCfgWellFormed t@(remA -> CondExpr tru cond fls _) =
    assertCfgCondOp (inject' t) cond tru fls
  assertCfgWellFormed t@(remA -> BinaryOp op l r _) =
    case extractOp op of
      And _ -> assertCfgShortCircuit (inject' t) l r
      Or _  -> assertCfgShortCircuit (inject' t) l r
      _   -> assertCfgIsGeneric (inject' t) [E l, E r]

    where extractOp :: MPythonTermLab OpL -> Op MPythonTerm OpL
          extractOp (stripA -> project -> Just op) = op
  assertCfgWellFormed t@(remA -> UnaryOp _ e _) =
    assertCfgIsGeneric (inject' t) [E e]    
  assertCfgWellFormed t@(remA -> Dot e _ _) =
    assertCfgIsGeneric (inject' t) [E e]    
  assertCfgWellFormed t@(remA -> Lambda {}) =
    error "TODO: Lambda"
  assertCfgWellFormed t@(remA -> Tuple es _) =
    assertCfgIsGeneric (inject' t) (map E $ S.extractF es)    
  assertCfgWellFormed t@(remA -> Yield {}) =
    error "TODO: Yield"
  assertCfgWellFormed t@(remA -> Generator {}) =
    error "TODO: Generator"
  assertCfgWellFormed t@(remA -> Await {}) =
    error "TODO: Await"
  assertCfgWellFormed t@(remA -> List es _) =
    assertCfgIsGeneric (inject' t) (map E $ S.extractF es)        
  assertCfgWellFormed t@(remA -> ListComp _ _) =
    error "TODO: ListComp"
  assertCfgWellFormed t@(remA -> Dictionary ds _) =
    assertCfgIsGenericAuto (inject' t) (map E $ S.extractF ds)
  assertCfgWellFormed t@(remA -> DictComp {}) =
    error "TODO: DictComp"
  assertCfgWellFormed t@(remA -> Set es _) =
    assertCfgIsGeneric (inject' t) (map E $ S.extractF es)        
  assertCfgWellFormed t@(remA -> SetComp {}) =
    error "TODO: SetComp"
  assertCfgWellFormed t@(remA -> Starred e _) =
    assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t@(remA -> Paren e _) =
    assertCfgIsGeneric (inject' t) [E e]          
  assertCfgWellFormed t = error $ "Impossible case: " ++ show (inject' t)

assertCfgWhileOrForElse ::
  ( MonadReader (Cfg MPythonSig) m
  , MonadTest m
  ) => MPythonTermLab l -> MPythonTermLab e -> MPythonTermLab b -> MPythonTermLab s -> m ()
assertCfgWhileOrForElse t cond body els = do
  (enWhile, exWhile) <- getEnterExitPair t t
  loWhile <- getLoopEntry t t
  (enCond, exCond) <- getEnterExitPair t cond
  (enBody, exBody) <- getEnterExitPair t body
  (enEls, exEls) <- getEnterExitPair t els
  assertEdges t [(enWhile, loWhile), (loWhile, enCond), (exCond, enBody), (exBody, loWhile), (exCond, enEls), (exEls, exWhile)]
                [enWhile, exWhile, loWhile, enCond, exCond, enBody, exBody, enEls, exEls]  

assertCfgIfElse ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs  
  ) => TermLab fs a -> [(TermLab fs e, TermLab fs b)] -> TermLab fs b -> m ()
assertCfgIfElse t cs els = do
  (enIf, exIf) <- getEnterExitPair t t
  eepExps <- mapM (getEnterExitPair t . fst) cs
  eepBodies <- mapM (getEnterExitPair t . snd) cs
  midNodes <- mapM (\i -> getIEP i t t) [0 .. length cs - 1]

  let evalEdges = concatMap (\( (_, exExp), (enBody, exBody)) ->
                         [ (exExp, enBody)
                         , (exBody, exIf)
                         ]
                      ) (zip eepExps eepBodies)
  
  let condEdges = map (\(midN, (enExp, _)) -> (midN, enExp)) condNodes
      condNodes = zip midNodes eepExps
      condJmpEdges = map (\( snd.snd -> exExp , (midN, _)) -> (exExp, midN)) (zip condNodes (tail condNodes))

  (elseNodes, exitEdges) <- do
    (enElse, exElse) <- getEnterExitPair t els
    pure ([enElse, exElse], [(snd $ last eepExps, enElse), (exElse, exIf)])

  assertEdges t ([(enIf, head midNodes)] ++ evalEdges ++ condEdges ++ condJmpEdges ++ exitEdges)
                ([enIf, exIf] ++ map fst eepExps ++ map snd eepExps ++ map fst eepBodies ++ map snd eepBodies ++ midNodes ++ elseNodes)

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

-- NOTE: Asserts that
--        * If there is no expression, then raise's entry and exit node are *not* connected.
--        * If there is an expression, then return's entry is
--          connected to expression's entry, and expression's exit
--          is not connected to return's exit, and return's entry
--          is not connected to return's exit.
assertCfgRaise ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs  
  ) => TermLab fs l -> [TermLab fs e] -> m ()
assertCfgRaise t exps = do
  (enReturn, exReturn) <- getEnterExitPair t t
  assertNoEdge t enReturn exReturn  
  case exps of
    [] -> pure ()
    _ -> do
      (enExp, exExp) <- getEnterExitPair t (head exps)
      (enExp, exExp) <- getEnterExitPair t (last exps)      
      assertEdge t enReturn enExp
      assertNoEdge t exExp exReturn
  
extractRaiseExprs :: MPythonTermLab RaiseExprL -> [MPythonTermLab ExprL]
extractRaiseExprs (project' -> Just (remA -> RaiseV3 r)) =
  case S.extractF r of
    Nothing -> []
    Just (S.extractF2 -> (e, e0)) -> case S.extractF e0 of
      Nothing -> [e]
      Just e1 -> [e, e1]
extractRaiseExprs (project' -> Just (remA -> RaiseV2 r)) =
  case S.extractF r of
    Nothing -> []
    Just (S.extractF2 -> (e, e0)) -> case S.extractF e0 of
      Nothing -> [e]
      Just (S.extractF2 -> (e1, e2)) -> case S.extractF e2 of
        Just e02 -> [e, e1, e02]
        Nothing -> [e, e1]
      
-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `break`.
--       * that outgoing edge is to a node which has an `ExitNode` type.
--       * that outgoing edge is to a node which is loop-like (is one of `For/While`)
--         (TODO: assert that it is the *nearest* such loop-like AST)
--       * there are no incoming nodes in exit node of `break`.
assertCfgBreak ::
  ( MonadTest m
  , MonadReader (Cfg MPythonSig) m
  ) => TermLab MPythonSig a -> m ()
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
      checkNodeType :: CfgNode MPythonSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == ExitNode                    &&
        (\(E t) -> isLoopLikeNode t) (node ^. cfg_node_term)

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `continue`.
--       * that outgoing edge is to a node which has an `LoopEntryNode` type.
--       * that outgoing edge is to a node which is loop-like (is one of `For/ While`)
--         (TODO: assert that it is the *nearest* such loop-like AST)
--       * there are no incoming nodes in exit node of `continue`.
assertCfgContinue ::
  ( MonadTest m
  , MonadReader (Cfg MPythonSig) m
  ) => MPythonTermLab a -> m ()
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
      checkNodeType :: CfgNode MPythonSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == LoopEntryNode                &&
        (\(E t) -> isLoopLikeNode t) (node ^. cfg_node_term)

assertCfgPyWith  ::
  ( MonadTest m
  , MonadReader (Cfg MPythonSig) m
  ) => MPythonTermLab w -> [MPythonTermLab b] -> MPythonTermLab a -> m ()
assertCfgPyWith t bs body = do
  (enWith, exWith) <- getEnterExitPair t t
  bsEEP <- mapM (getEnterExitPair t) bs
  midNodes <- mapM (\i -> getIEP i t t) [0..(length bs - 1)]
  (enBody, exBody) <- getEnterExitPair t body

  let bmsEdges = zipWith (\p n -> (snd p, n)) bsEEP (tail midNodes)
      mbsEdges = zipWith (\p n -> (p, fst n)) midNodes bsEEP                         
  assertEdges t ([(enWith, head midNodes)] ++ bmsEdges ++ mbsEdges ++ [(snd (last bsEEP), enBody), (exBody, exWith)])
                ([enWith, exWith, enBody, exBody] ++ map fst bsEEP ++ map snd bsEEP)

isLoopLikeNode :: MPythonTermLab l -> Bool
isLoopLikeNode (project' -> Just (While {} :&: _))= True
isLoopLikeNode (project' -> Just (For {} :&: _)) = True
isLoopLikeNode _ = False

-- NOTE: Asserts that
--        * catch(s) block and finally blocks are suspended
--        * try body is connected to els
assertCfgTry ::
  ( MonadTest m
  , MonadReader (Cfg MPythonSig) m
  ) => TermLab MPythonSig t -> TermLab MPythonSig b -> [(E (TermLab MPythonSig), E (TermLab MPythonSig))] -> TermLab MPythonSig b -> TermLab MPythonSig b -> m ()
assertCfgTry t body catchs els finally = do
  (enTry, exTry) <- getEnterExitPair t t
  (enBody, exBody) <- getEnterExitPair t body
  catchEnters <- mapM (\(E a) -> getEnterNode t a) (map fst catchs)
  catchExits <- mapM (\(E a) -> getEnterNode t a) (map snd catchs)
  let catchEEPs = zip catchEnters catchExits
  (enEls, exEls) <- getEnterExitPair t els  
  (enFinally, exFinally) <- getEnterExitPair t finally

  mapM_ (uncurryE (assertCfgIsSuspended' t)) catchs
  assertCfgIsSuspended t finally

  assertEdges t [(enTry, enBody), (exBody, enEls), (exEls, exTry)]
                ([enTry, exTry, enBody, exBody, enEls, exEls, enFinally
                , exFinally ] ++ (map fst catchEEPs) ++ map snd catchEEPs)

  where
    uncurryE :: (forall l1 l2. TermLab fs l1 -> TermLab fs l2 -> m ()) -> (E (TermLab fs), E (TermLab fs)) -> m ()
    uncurryE f (E t1, E t2) = f t1 t2


extractHandler :: MPythonTermLab HandlerL -> (E MPythonTermLab, E MPythonTermLab)
extractHandler (project' -> Just (remA -> Handler e s _)) =
  let eClause = case project' e of
        Just (remA -> ExceptClause e0 _) -> case S.extractF e0 of
          Just (S.extractF2 -> (e1, e2)) -> case S.extractF e2 of
            Just e3 -> [E e1, E e3]
            Nothing -> [E e1]
          Nothing -> []
  in  case eClause of
    [] -> (E s, E s)
    _  -> (head eClause, E s)
  
assertCfgCondOp ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs e -> TermLab fs e -> TermLab fs e -> TermLab fs e -> m ()
assertCfgCondOp t test succ fail = do
  (enTop, exTop) <- getEnterExitPair t t
  (enTest, exTest) <- getEnterExitPair t test
  (enSucc, exSucc) <- getEnterExitPair t succ
  (enFail, exFail) <- getEnterExitPair t fail

  assertEdges t [(enTop, enTest), (exTest, enSucc), (exTest, enFail), (exSucc, exTop), (exFail, exTop)]
                [enTop, exTop, enTest, exTest, enSucc, exSucc, enFail, exFail]

assertCfgShortCircuit ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
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
