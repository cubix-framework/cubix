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

module Cubix.Language.Java.Cfg.Test where

import           Control.Lens hiding ( para, Empty )
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
import           Cubix.Language.Java.Parametric.Common as JCommon
import qualified Cubix.Language.Java.Parametric.Full as JFull
import           Cubix.Language.Parametric.Semantics.Cfg hiding ( enter, exit )
import           Cubix.ParsePretty
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, (:*:), HFix, (:->), NatM, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), inj', inject', EqHF, remA, (:-<:), proj, Cxt (..), inj, hfoldMap, K (..), cata, hfold, HFoldable )
import           Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )

import           Cubix.Language.Parametric.Cfg.Test
import qualified Cubix.Language.Parametric.Syntax as S

unit_java_cfg :: FilePath -> Property
unit_java_cfg fp =
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile fp
    (tLab, cfg) <- makeJavaEnv t
    runReaderT (assertCfgWellFormedness tLab) cfg
    pure ()

makeJavaEnv :: (MonadIO m) => MJavaTerm CompilationUnitL -> m (MJavaTermLab CompilationUnitL, Cfg MJavaSig)
makeJavaEnv t = do
  gen <- mkConcurrentSupplyLabelGen
  let tLab = labelProg gen t
      cfg = makeCfg tLab
  pure (tLab, cfg)

instance AssertCfgWellFormed MJavaSig BlockStmt where
  assertCfgWellFormed t@(remA -> BlockStmt stmt) =
    assertCfgIsGenericAuto' (inject' t) isSuspended [E stmt]
  assertCfgWellFormed t@(remA -> LocalClass cls) =
    assertCfgIsGenericAuto' (inject' t) isSuspended [E cls]    
  assertCfgWellFormed t@(remA -> LocalVars _ _ var) =
    assertCfgIsGenericAuto' (inject' t) isSuspended [E var]

instance AssertCfgWellFormed MJavaSig ListF where
  assertCfgWellFormed t@(inject' -> stripA -> t0)
    | isSort @[BlockItemL] t0 =
        case t of
          (remA -> S.ConsF v vs) -> assertCfgIsGenericAuto' (inject' t) isSuspended [E v, E vs]
          (remA -> S.NilF)       -> assertCfgIsGeneric (inject' t) []
    | otherwise = assertCfgNoCfgNode (inject' t)  

-- containers
instance AssertCfgWellFormed MJavaSig MaybeF
instance AssertCfgWellFormed MJavaSig PairF
instance AssertCfgWellFormed MJavaSig SwitchBlock

instance AssertCfgWellFormed MJavaSig Name
instance AssertCfgWellFormed MJavaSig Op
instance AssertCfgWellFormed MJavaSig Literal
instance AssertCfgWellFormed MJavaSig AssignOp
instance AssertCfgWellFormed MJavaSig VarInit
instance AssertCfgWellFormed MJavaSig VarDeclId
instance AssertCfgWellFormed MJavaSig VarDecl
instance AssertCfgWellFormed MJavaSig SwitchLabel
instance AssertCfgWellFormed MJavaSig Modifier
instance AssertCfgWellFormed MJavaSig MemberDecl
instance AssertCfgWellFormed MJavaSig Lhs
instance AssertCfgWellFormed MJavaSig LambdaParams
instance AssertCfgWellFormed MJavaSig LambdaExpression
instance AssertCfgWellFormed MJavaSig InterfaceKind
instance AssertCfgWellFormed MJavaSig InterfaceDecl
instance AssertCfgWellFormed MJavaSig InterfaceBody
instance AssertCfgWellFormed MJavaSig ImportDecl
instance AssertCfgWellFormed MJavaSig FormalParam
instance AssertCfgWellFormed MJavaSig ForInit
instance AssertCfgWellFormed MJavaSig FieldAccess
instance AssertCfgWellFormed MJavaSig ExplConstrInv
instance AssertCfgWellFormed MJavaSig EnumConstant
instance AssertCfgWellFormed MJavaSig EnumBody
instance AssertCfgWellFormed MJavaSig ElementValue
instance AssertCfgWellFormed MJavaSig Decl
instance AssertCfgWellFormed MJavaSig ConstructorBody where
  assertCfgWellFormed t@(remA -> ConstructorBody p e) =
    assertCfgConstructor (inject' t) p e
instance AssertCfgWellFormed MJavaSig CompilationUnit
instance AssertCfgWellFormed MJavaSig ClassDecl
instance AssertCfgWellFormed MJavaSig ClassBody
instance AssertCfgWellFormed MJavaSig Catch
instance AssertCfgWellFormed MJavaSig ArrayInit
instance AssertCfgWellFormed MJavaSig ArrayIndex
instance AssertCfgWellFormed MJavaSig Annotation
instance AssertCfgWellFormed MJavaSig PackageDecl
instance AssertCfgWellFormed MJavaSig TypeDecl
instance AssertCfgWellFormed MJavaSig ClassType
instance AssertCfgWellFormed MJavaSig Diamond
instance AssertCfgWellFormed MJavaSig WildcardBound
instance AssertCfgWellFormed MJavaSig TypeParam
instance AssertCfgWellFormed MJavaSig TypeDeclSpecifier
instance AssertCfgWellFormed MJavaSig TypeArgument
instance AssertCfgWellFormed MJavaSig Type
instance AssertCfgWellFormed MJavaSig RefType
instance AssertCfgWellFormed MJavaSig PrimType
instance AssertCfgWellFormed MJavaSig PositionalParameter
instance AssertCfgWellFormed MJavaSig SelfParameter
instance AssertCfgWellFormed MJavaSig PositionalParameterDeclWithIdent
instance AssertCfgWellFormed MJavaSig SelfParameterDecl
instance AssertCfgWellFormed MJavaSig FunctionDecl
instance AssertCfgWellFormed MJavaSig PositionalArgument
instance AssertCfgWellFormed MJavaSig ReceiverArg
instance AssertCfgWellFormed MJavaSig FunctionArgumentList
instance AssertCfgWellFormed MJavaSig FunctionIdent
instance AssertCfgWellFormed MJavaSig FunctionCall
instance AssertCfgWellFormed MJavaSig EmptyBlockEnd
instance AssertCfgWellFormed MJavaSig S.Block
instance AssertCfgWellFormed MJavaSig S.Assign
instance AssertCfgWellFormed MJavaSig AssignOpEquals
instance AssertCfgWellFormed MJavaSig Ident
instance AssertCfgWellFormed MJavaSig MultiLocalVarDecl
instance AssertCfgWellFormed MJavaSig SingleLocalVarDecl
instance AssertCfgWellFormed MJavaSig OptLocalVarInit
instance AssertCfgWellFormed MJavaSig JavaVarargsParam
instance AssertCfgWellFormed MJavaSig JavaParamAttrs
instance AssertCfgWellFormed MJavaSig JavaMethodDeclAttrs
instance AssertCfgWellFormed MJavaSig JavaTypeArgs
instance AssertCfgWellFormed MJavaSig JavaReceiver
instance AssertCfgWellFormed MJavaSig ArrayDimVarDeclAttrs

-- Is
instance AssertCfgWellFormed MJavaSig BlockIsFunctionBody
instance AssertCfgWellFormed MJavaSig JavaVarargsParamIsFunctionParameter
instance AssertCfgWellFormed MJavaSig JavaParamAttrsIsFunctionParameterDeclAttrs
instance AssertCfgWellFormed MJavaSig JavaParamAttrsIsParameterAttrs
instance AssertCfgWellFormed MJavaSig JavaMethodDeclAttrsIsFunctionDefAttrs
instance AssertCfgWellFormed MJavaSig FunctionDefIsMemberDecl
instance AssertCfgWellFormed MJavaSig JavaVarargsParamIsFunctionParameterDecl
instance AssertCfgWellFormed MJavaSig JavaMethodDeclAttrsIsFunctionDeclAttrs
instance AssertCfgWellFormed MJavaSig FunctionDeclIsMemberDecl
instance AssertCfgWellFormed MJavaSig ExpIsPositionalArgExp
instance AssertCfgWellFormed MJavaSig NameIsFunctionExp
instance AssertCfgWellFormed MJavaSig FunctionCallIsMethodInvocation
instance AssertCfgWellFormed MJavaSig IdentIsVarDeclBinder
instance AssertCfgWellFormed MJavaSig AssignOpIsAssignOp
instance AssertCfgWellFormed MJavaSig BlockStmtIsBlockItem
instance AssertCfgWellFormed MJavaSig LhsIsLhs
instance AssertCfgWellFormed MJavaSig ExpIsRhs
instance AssertCfgWellFormed MJavaSig BlockIsBlock
instance AssertCfgWellFormed MJavaSig IdentIsIdent
instance AssertCfgWellFormed MJavaSig ModifiersTypeIsMultiLocalVarDeclCommonAttrs
instance AssertCfgWellFormed MJavaSig VarInitIsLocalVarInit

instance AssertCfgWellFormed MJavaSig FunctionDef where
  assertCfgWellFormed t@(remA -> FunctionDef _ _ _ e) =
    assertCfgFunction (inject' t) e  

instance AssertCfgWellFormed MJavaSig AssignIsExp where
  assertCfgWellFormed t = assertCfgIsGenericFullyAuto' (inject' t) isSuspended

instance AssertCfgWellFormed MJavaSig MultiLocalVarDeclIsBlockStmt where
  assertCfgWellFormed t = assertCfgIsGenericFullyAuto' (inject' t) isSuspended

instance AssertCfgWellFormed MJavaSig Exp where
  assertCfgWellFormed t@(remA -> Cond test succ fail) = do
    assertCfgCondOp (inject' t) test succ fail
  assertCfgWellFormed t@(remA -> BinOp e1 op e2) = do
    case extractOp op of
      CAnd -> assertCfgShortCircuit (inject' t) e1 e2
      COr  -> assertCfgShortCircuit (inject' t) e1 e2
      _   -> assertCfgIsGeneric (inject' t) [E e1, E e2]
    where extractOp :: MJavaTermLab OpL -> Op MJavaTerm OpL
          extractOp (stripA -> project -> Just bp) = bp

  assertCfgWellFormed t@(remA -> Lit {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> ClassLit {}) = assertCfgIsGeneric (inject' t) []  
  assertCfgWellFormed t@(remA -> This) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> ThisClass {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> InstanceCreation _ _ args cb) = assertCfgIsGenericAuto' (inject' t) isSuspended [E args, E cb]
  assertCfgWellFormed t@(remA -> QualInstanceCreation e _ _ args cb) = assertCfgIsGenericAuto' (inject' t) isSuspended [E e, E args, E cb]
  assertCfgWellFormed t@(remA -> ArrayCreate _ exps _) = assertCfgIsGeneric (inject' t) (map E . S.extractF $ exps)
  assertCfgWellFormed t@(remA -> ArrayCreateInit _ _ inits) =
    assertCfgIsGeneric (inject' t) exps

    where exps = go0 inits

          go0 :: MJavaTermLab ArrayInitL -> [E MJavaTermLab]    
          go0 (project' -> Just (remA -> ArrayInit es)) = concatMap go (S.extractF es)
          
          go :: MJavaTermLab VarInitL -> [E MJavaTermLab]
          go (project' -> Just (remA -> InitExp e)) = [E e]
          go (project' -> Just (remA -> InitArray es)) = go0 es
              
  assertCfgWellFormed t@(remA -> FieldAccess fld) =
    assertCfgIsGeneric (inject' t) exps

    where exps = go fld

          go :: MJavaTermLab FieldAccessL -> [E MJavaTermLab]
          go t = case project' t of
            Just (remA -> PrimaryFieldAccess e _) -> [E e]
            Just _ -> []                      
          
  assertCfgWellFormed t@(remA -> MethodInv {}) =
    assertCfgIsGenericFullyAuto' (inject' t) isSuspended
  assertCfgWellFormed t@(remA -> InstanceOf {}) =
    assertCfgIsGenericFullyAuto' (inject' t) isSuspended
  assertCfgWellFormed t@(remA -> ArrayAccess ix) =
    assertCfgIsGeneric (inject' t) exps

    where exps = case project' ix of
            Just (remA -> ArrayIndex e es) -> E e : map E (S.extractF es)
            
  assertCfgWellFormed t@(remA -> ExpName {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> PostIncrement exp) = assertCfgIsGeneric (inject' t) [E exp]
  assertCfgWellFormed t@(remA -> PostDecrement exp) = assertCfgIsGeneric (inject' t) [E exp]
  assertCfgWellFormed t@(remA -> PreIncrement exp) = assertCfgIsGeneric (inject' t) [E exp]  
  assertCfgWellFormed t@(remA -> PreDecrement exp) = assertCfgIsGeneric (inject' t) [E exp]
  assertCfgWellFormed t@(remA -> PrePlus exp) = assertCfgIsGeneric (inject' t) [E exp]
  assertCfgWellFormed t@(remA -> PreMinus exp) = assertCfgIsGeneric (inject' t) [E exp]
  assertCfgWellFormed t@(remA -> PreBitCompl exp) = assertCfgIsGeneric (inject' t) [E exp]
  assertCfgWellFormed t@(remA -> PreNot exp) = assertCfgIsGeneric (inject' t) [E exp]
  assertCfgWellFormed t@(remA -> Cast _ exp) = assertCfgIsGeneric (inject' t) [E exp]
  assertCfgWellFormed t@(remA -> Lambda _ e) =
    assertCfgLambda (inject' t) e

  assertCfgWellFormed t@(remA -> MethodRef {}) = assertCfgIsGeneric (inject' t) []
  
  assertCfgWellFormed t = error $ "Impossible case: " ++ show (inject' t)  

instance AssertCfgWellFormed MJavaSig Stmt where
  assertCfgWellFormed t@(remA -> StmtBlock b) = assertCfgSubterms (inject' t) [blk]
    where blk = extractBlockFromBlock b
  assertCfgWellFormed t@(remA -> IfThen cond thn) =
    assertCfgIf (inject' t) cond (extractBlock thn)
  assertCfgWellFormed t@(remA -> IfThenElse cond thn els) =
    assertCfgIfElse (inject' t) cond (extractBlock thn) (extractBlock els)
  assertCfgWellFormed t@(remA -> While e b) =
    assertCfgWhile (inject' t) e (extractBlock b)
  assertCfgWellFormed t@(remA -> BasicFor init cond step b) =
    assertCfgFor (inject' t)
                 (extractForInits <$> extractMaybe init)
                 (extractMaybe cond)
                 (extractList <$> extractMaybe step)
                 (extractBlock b)
  assertCfgWellFormed t@(remA -> EnhancedFor _ _ _ e b) =
    assertCfgEnhancedFor (inject' t) e (extractBlock b)
  assertCfgWellFormed t@(remA -> Switch exp switchBlocks) =
    assertCfgSwitch (inject' t) exp (S.extractF switchBlocks)
  assertCfgWellFormed t@(remA -> Do b e) =
    assertCfgDoWhile (inject' t) (extractBlock b) e
  assertCfgWellFormed t@(remA -> Break ml) =
    case S.extractF ml of
      Just l -> assertCfgBreakLabeled (inject' t) (nameString l)
      Nothing -> assertCfgBreak (inject' t)
  assertCfgWellFormed t@(remA -> Continue ml) =
    case S.extractF ml of
      Just l -> assertCfgContinueLabeled (inject' t) (nameString l)
      Nothing -> assertCfgContinue (inject' t)
  assertCfgWellFormed t@(remA -> Return e) =
    assertCfgReturn (inject' t) (extractMaybe e)
  assertCfgWellFormed t@(remA -> Throw e) =
    assertCfgReturn (inject' t) (Just e)
  assertCfgWellFormed t@(remA -> Try block catchs finally) =
    assertCfgTry (inject' t)
                 (extractBlockFromBlock block)
                 (extractBlockFromCatch <$> extractList catchs)
                 (extractBlockFromBlock <$> extractMaybe finally)
  assertCfgWellFormed t@(remA -> Labeled _ stmt) =
    assertCfgIsGeneric (inject' t) [extractBlock stmt]
  assertCfgWellFormed t@(remA -> Empty) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> ExpStmt e) =
    assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t@(remA -> Assert e me0) = do
    let ns = case S.extractF me0 of
          Just e0 -> [E e, E e0]
          Nothing -> [E e]
    assertCfgIsGeneric (inject' t) ns
  assertCfgWellFormed t@(remA -> Synchronized e b) =
    assertCfgIsGeneric (inject' t) [E e, blk]
    where blk = extractBlockFromBlock b
    
  assertCfgWellFormed t = error $ "Impossible case: " ++ show (inject' t)  

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
  assertEdges t [ (enIf, midNode), (midNode, enCond), (exCond, enThn), (exCond, exIf), (exThn, exIf) ]
                [enIf, exIf, midNode, enCond, exCond, enThn, exThn]

assertCfgWhile ::
  forall fs a e b m.
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , Exp :-<: fs
  , Literal :-<: fs
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
                 ] ++
                 staticLoopCheck enCond exCond enBody exWhile
                )

                [ enWhile, exWhile, loWhile
                , enCond, exCond, enBody, exBody
                ]

    where staticLoopCheck enCond exCond enBody exWhile =
            case evaluateExp e of
              Just True -> [(exCond, enBody)]
              Just False -> [(exCond, exWhile)]
              Nothing -> [(exCond, enBody), (exCond, exWhile)] 

          evaluateExp :: (Exp :-<: fs, Literal :-<: fs) => TermLab fs e -> Maybe Bool
          evaluateExp (stripA -> project -> Just t@(Lit lit)) =
            case project lit of
              Just (Boolean True) -> Just True
              Just (Boolean False) -> Just False
              _ -> Nothing
          evaluateExp _ = Nothing

assertCfgEnhancedFor ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs 
  , All HFoldable fs
  ) => TermLab fs a -> TermLab fs e -> E (TermLab fs) -> m ()
assertCfgEnhancedFor t e (E b) = do
  (enEnFor, exEnFor) <- getEnterExitPair t t
  loEnFor <- getLoopEntry t t
  (enExp, exExp) <- getEnterExitPair t e
  (enBody, exBody) <- getEnterExitPair t b

  assertEdges t ([ (enEnFor, loEnFor)
                 , (loEnFor, enExp)
                 , (exBody, loEnFor)
                 , (exExp, enBody)
                 , (exExp, exEnFor)
                 ]                 
                )
                [ enEnFor, exEnFor, loEnFor
                , enExp, exExp, enBody, exBody
                ]

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


-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `break`.
--       * that outgoing edge is to a node which has an `ExitNode` type.
--       * that outgoing edge is to a node which has a label with this name
--       * there are no incoming nodes in exit node of `break`.
assertCfgBreakLabeled ::
  ( MonadTest m
  , MonadReader (Cfg MJavaSig) m
  ) => TermLab MJavaSig a -> String -> m ()
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
      checkNodeType :: CfgNode MJavaSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == ExitNode

      checkLab ::  E (TermLab MJavaSig) -> Bool
      checkLab (E (project' -> Just (remA -> Labeled (nameString -> n) _)))
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
  , MonadReader (Cfg MJavaSig) m
  ) => TermLab MJavaSig a -> String -> m ()
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
      checkNodeType :: CfgNode MJavaSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == LoopEntryNode

      checkLab ::  E (TermLab MJavaSig) -> Bool
      checkLab (E (project' -> Just (remA -> Labeled (nameString -> n) _)))
        | labName == n = True
        | otherwise = False
      checkLab _ = False

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `break`.
--       * that outgoing edge is to a node which has an `ExitNode` type.
--       * that outgoing edge is to a node which is switch or loop-like (is one of `Do while/EnhancedFor/BasicFor/While`)
--       * there are no incoming nodes in exit node of `break`.
assertCfgBreak ::
  ( MonadTest m
  , MonadReader (Cfg MJavaSig) m
  ) => TermLab MJavaSig a -> m ()
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
      checkNodeType :: CfgNode MJavaSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == ExitNode                    &&
        (\(E t) -> isLoopLikeNode t || isSwitchNode t) (node ^. cfg_node_term)

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `continue`.
--       * that outgoing edge is to a node which has an `LoopEntryNode` type.
--       * that outgoing edge is to a node which is loop-like (is one of `Do while/EnhancedFor/BasicFor/ While`)
--       * there are no incoming nodes in exit node of `continue`.
assertCfgContinue ::
  ( MonadTest m
  , MonadReader (Cfg MJavaSig) m
  ) => TermLab MJavaSig a -> m ()
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
      checkNodeType :: CfgNode MJavaSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == LoopEntryNode                &&
        (\(E t) -> isLoopLikeNode t) (node ^. cfg_node_term)

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

assertCfgFor ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs  
  ) => TermLab fs l -> Maybe [TermLab fs i] -> Maybe (TermLab fs e) -> Maybe [TermLab fs e] -> E (TermLab fs) -> m ()
assertCfgFor t mInit mCond mStep (E body) = do
  eepFor <- getEnterExitPair t t
  loFor <- getLoopEntry t t
  eepBody <- getEnterExitPair t body
  mEEPInits <- maybe (pure Nothing) (fmap Just . mapM (getEnterExitPair t)) mInit
  mEEPCond <- maybe (pure Nothing) (fmap Just . (getEnterExitPair t)) mCond
  mEEPSteps <- maybe (pure Nothing) (fmap Just . mapM (getEnterExitPair t)) mStep

  let edges =
        let (e0, vs0) = mJoinEdgesR [] (identEdge $ enter eepFor) minitOutEdge
            (e1, vs1) = mJoinEdgesR vs0 e0 mEEPCond
            (_ , vs2) = joinEdges   vs1 e1 eepBody

            (e3, vs3) = joinEdges   vs2 eepBody (identEdge loFor)
            (e4, vs4) = mJoinEdgesR vs3 e3 mstepOutEdge
            (e5, vs5) = mJoinEdgesR vs4 e4 mEEPCond
            (_,  vs6) = joinEdges  vs5 e5 eepBody
            
            (_,  vs7) = mJoinEdgesL vs6 mEEPCond (identEdge $ exit eepFor)
        in  vs7
      nodes =
        let ns = catMaybes [mEEPCond] ++ [ eepFor, eepBody ]
            both0 p = [fst p, snd p]
        in loFor : map enter ns ++ map exit ns ++ maybe [] both0 mstepOutEdge ++ maybe [] both0 minitOutEdge
        
      mstepOutEdge = go mEEPSteps
        where go Nothing   = Nothing
              go (Just []) = Nothing
              go (Just xs) = Just (enter (head xs), exit (last xs))

      minitOutEdge = go mEEPInits
        where go Nothing   = Nothing
              go (Just []) = Nothing
              go (Just xs) = Just (enter (head xs), exit (last xs))
  assertEdges t (nub edges) nodes

-- NOTE: Asserts that
--        * try block, catch(s) block and finally blocks are not connected
assertCfgTry ::
  ( MonadTest m
  , MonadReader (Cfg MJavaSig) m
  ) => TermLab MJavaSig t -> E (TermLab MJavaSig) -> [E (TermLab MJavaSig)] -> Maybe (E (TermLab MJavaSig)) -> m ()
assertCfgTry t (E block) catchs finally = do
  (enTry, exTry) <- getEnterExitPair t t
  (enTryBlk, exTryBlk) <- getEnterExitPair t block
  catchEEPs <- mapM (\(E a) -> getEnterExitPair t a) catchs
  mfinallyEEP <- mapM (\(E a) -> getEnterExitPair t a) finally

  assertNoEdge t enTry enTryBlk
  
  case null catchEEPs of
    True -> do
      case mfinallyEEP of
        Nothing -> pure ()
        Just finallyEEP -> assertNoEdge t exTryBlk (fst $ finallyEEP)
    False -> do     
      assertNoEdge t exTryBlk (fst $ head catchEEPs)
      case mfinallyEEP of
        Nothing -> pure ()
        Just finallyEEP -> assertNoEdge t (snd $ last catchEEPs) (fst $ finallyEEP)
  
  pure ()

assertCfgSwitch ::
  forall m l.
  ( MonadReader (Cfg MJavaSig) m
  , MonadTest m
  ) => TermLab MJavaSig l -> TermLab MJavaSig ExpL -> [TermLab MJavaSig SwitchBlockL] -> m ()
assertCfgSwitch t e cs = do
  (swEn, swEx) <- getEnterExitPair t t
  (eEn, eEx) <- getEnterExitPair t e
  mcsEEPs <- mapM getEEPCase cs

  let jumps = map (\cEEP -> (eEx, fst cEEP)) csEEPs
      csEEPs = catMaybes mcsEEPs
      fallthrough = zipWith (\p n -> (snd p, fst n)) csEEPs (tail csEEPs) ++ [(snd (last csEEPs), swEx)]
  assertEdges t ([(swEn, eEn), (eEn, eEx)] ++ jumps ++ fallthrough)
                ([swEn, swEx, eEn, eEx] ++ map fst csEEPs ++ map snd csEEPs)
  
  where getEEPCase :: TermLab MJavaSig SwitchBlockL -> m (Maybe (CfgNode MJavaSig, CfgNode MJavaSig))
        getEEPCase t0 = case project' t0 of
          Just (remA -> SwitchBlock _ es) -> do
            esEEP <- mapM (getEnterExitPair t) (extractF es)            
            case null esEEP of
                True  -> pure Nothing
                False -> pure (Just (fst (head esEEP), snd (last esEEP)))

        -- getEEPLabel :: TermLab MJavaSig SwitchLabelL -> m (Maybe (CfgNode MJavaSig, CfgNode MJavaSig))
        -- getEEPLabel t0 = case project' t0 of
        --   Just (remA -> SwitchCase e) -> Just <$> getEnterExitPair t e
        --   Just (remA -> Default)      -> pure Nothing

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

isLoopLikeNode :: TermLab MJavaSig l -> Bool
isLoopLikeNode (project' -> Just (While {} :&: _))= True
isLoopLikeNode (project' -> Just (Do {} :&: _)) = True
isLoopLikeNode (project' -> Just (EnhancedFor {} :&: _)) = True
isLoopLikeNode (project' -> Just (BasicFor {} :&: _)) = True
isLoopLikeNode _ = False

isSwitchNode :: TermLab MJavaSig l -> Bool
isSwitchNode (project' -> Just (Switch {} :&: _))= True
isSwitchNode _ = False


assertCfgWellFormedDefault :: (MonadTest m, MonadReader (Cfg MJavaSig) m, f :-<: MJavaSig) => (f :&: Label) MJavaTermLab l -> m ()
assertCfgWellFormedDefault p = pure ()

extractMaybe :: TermLab MJavaSig (Maybe l) -> Maybe (TermLab MJavaSig l)
extractMaybe (project' -> Just (JustF t :&: _)) = Just t
extractMaybe (project' -> Just (NothingF :&: _)) = Nothing

extractList :: TermLab MJavaSig [l] -> [TermLab MJavaSig l]
extractList (project' -> Just (ConsF x xs :&: _)) = x : extractList xs
extractList (project' -> Just (NilF :&: _)) = []

extractBlock :: TermLab MJavaSig StmtL -> E (TermLab MJavaSig)
extractBlock (project' -> Just (StmtBlock blkIsBlk :&: _)) = extractBlockFromBlock blkIsBlk
extractBlock t = E t

extractBlockFromBlock :: TermLab MJavaSig JFull.BlockL -> E (TermLab MJavaSig)
extractBlockFromBlock (project' -> Just (BlockIsBlock blk :&: _)) = fromJust $ do
  (remA -> S.Block blkItems _) <- project' blk
  pure (E blkItems)

extractForInits :: TermLab MJavaSig ForInitL -> [TermLab MJavaSig ExpL]
extractForInits (project' -> Just (JFull.ForInitExps exps :&: _)) = S.extractF exps
extractForInits (project' -> Just (JFull.ForLocalVars _ _ decls :&: _)) = concatMap extractExps (S.extractF decls)

  where extractExps :: TermLab MJavaSig VarDeclL -> [TermLab MJavaSig ExpL]
        extractExps (project' -> Just (remA -> (VarDecl _ e))) = case S.extractF e of
          Just e0 -> extractVarInit e0
          Nothing -> []

        extractVarInit :: TermLab MJavaSig VarInitL -> [TermLab MJavaSig ExpL]
        extractVarInit v = case project' v of
            Just (remA -> InitExp e) -> [e]
            Just (remA -> InitArray arr) -> extractArrInit arr

        extractArrInit :: TermLab MJavaSig ArrayInitL -> [TermLab MJavaSig ExpL]
        extractArrInit (project' -> Just (remA -> ArrayInit vs)) = concatMap extractVarInit (S.extractF vs)
          
                 
extractBlockFromCatch :: TermLab MJavaSig CatchL -> E (TermLab MJavaSig)
extractBlockFromCatch (project' -> Just (Catch _ blkIsBlk :&: _)) = fromJust $ do
  (BlockIsBlock blk :&: _) <- project' blkIsBlk
  (S.Block blkItems _ :&: _) <- project' blk
  pure (E blkItems)

nameString :: MJavaTermLab JFull.IdentL -> String
nameString (stripA -> project -> Just (IdentIsIdent (Ident' s))) = s

-- hardcoded integration tests
integration_java_cfg :: FilePath -> Property
integration_java_cfg path = 
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile path
    edges <- liftIO $ parseEdges path
    
    (_, cfg) <- makeJavaEnv t
    integration_cfg edges cfg

assertCfgFunction ::
  ( MonadTest m
  , MonadReader (Cfg MJavaSig) m
  ) => TermLab MJavaSig a -> TermLab MJavaSig b -> m ()
assertCfgFunction t body = do
  assertCfgIsSuspendedAuto' t isSuspended body
  assertCfgSubtermsAuto' t isSuspended []

assertCfgConstructor ::
  ( MonadTest m
  , MonadReader (Cfg MJavaSig) m
  ) => TermLab MJavaSig a -> TermLab MJavaSig p -> TermLab MJavaSig b -> m ()
assertCfgConstructor t param body = do
  assertCfgIsSuspendedPiecesAuto' t isSuspended [E param, E body]
  assertCfgSubtermsAuto' t isSuspended []

assertCfgLambda ::
  ( MonadTest m
  , MonadReader (Cfg MJavaSig) m
  ) => TermLab MJavaSig a -> TermLab MJavaSig b -> m ()
assertCfgLambda t body = do
  assertCfgIsSuspendedAuto' t isSuspended body
  assertCfgIsGenericAuto' t isSuspended []

-- NOTE: This is necessary because Java
--       is not assigned a node for
--       suspended computations of
--       FunctionDef and ConstructorBody
isSuspended :: TermLab MJavaSig a -> Bool
isSuspended (stripA -> t0)
  | isSort @FunctionDefL t0 = True
  | isSort @ConstructorBodyL t0 = True
  | isSort @LambdaExpressionL t0 = True
  | isSort @CatchL t0 = True
  | otherwise = False
