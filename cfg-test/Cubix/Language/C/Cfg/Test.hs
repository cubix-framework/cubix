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

module Cubix.Language.C.Cfg.Test where

import           Control.Lens hiding ( para, children )
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
import           Cubix.Language.C.Parametric.Common as CCommon
import qualified Cubix.Language.C.Parametric.Full as CFull
import           Cubix.Language.Parametric.Semantics.Cfg hiding ( enter, exit )
import           Cubix.ParsePretty
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, (:*:), HFix, (:->), NatM, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), inj', inject', EqHF, remA, (:-<:), proj, Cxt (..), inj, hfoldMap, K (..), cata, hfold, subterms, HFoldable )
import           Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )

import           Cubix.Language.Parametric.Cfg.Test
import qualified Cubix.Language.Parametric.Syntax as S
import           Cubix.Language.Parametric.InjF ( projF )

unit_c_cfg :: FilePath -> Property
unit_c_cfg fp =
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile fp
    (tLab, cfg) <- makeCEnv t
    runReaderT (assertCfgWellFormedness tLab) cfg
    pure ()

makeCEnv :: (MonadIO m) => MCTerm CTranslationUnitL -> m (MCTermLab CTranslationUnitL, Cfg MCSig)
makeCEnv t = do
  gen <- mkConcurrentSupplyLabelGen
  let tLab = labelProg gen t
      cfg = makeCfg tLab
  pure (tLab, cfg)

instance AssertCfgWellFormed MCSig UnitF
instance AssertCfgWellFormed MCSig IntegerF
instance AssertCfgWellFormed MCSig IntF
instance AssertCfgWellFormed MCSig BoolF
instance AssertCfgWellFormed MCSig EitherF
instance AssertCfgWellFormed MCSig MaybeF
instance AssertCfgWellFormed MCSig TripleF
instance AssertCfgWellFormed MCSig PairF

instance AssertCfgWellFormed MCSig ListF where
  assertCfgWellFormed t
    | isSort (Proxy :: Proxy [BlockItemL]) (stripA (inject' t)) = do
        case t of
          (remA -> ConsF v vs) -> assertCfgIsGenericAuto (inject' t) [E v, E vs]
          (remA -> NilF)       -> assertCfgIsGeneric (inject' t) []
    | otherwise = assertCfgNoCfgNode (inject' t)
  

instance AssertCfgWellFormed MCSig CStatementIsFunctionBody
instance AssertCfgWellFormed MCSig COldStyleParamIsFunctionParameter
instance AssertCfgWellFormed MCSig CSpecialParamIsFunctionParameter
instance AssertCfgWellFormed MCSig CFunParamAttrsIsParameterAttrs
instance AssertCfgWellFormed MCSig FunctionDefIsCFunctionDef
instance AssertCfgWellFormed MCSig CSpecialParamIsFunctionParameterDecl
instance AssertCfgWellFormed MCSig CFunParamAttrsIsFunctionParameterDeclAttrs
instance AssertCfgWellFormed MCSig FunctionDeclIsCDeclarator
instance AssertCfgWellFormed MCSig CExpressionIsPositionalArgExp
instance AssertCfgWellFormed MCSig CExpressionIsFunctionExp
instance AssertCfgWellFormed MCSig FunctionCallIsCExpression where
  assertCfgWellFormed t@(remA -> FunctionCallIsCExpression cexp) = assertCfgIsGenericAuto (inject' t) [E cexp]
  
instance AssertCfgWellFormed MCSig IdentIsVarDeclBinder
instance AssertCfgWellFormed MCSig CCompoundBlockItemIsBlockItem
instance AssertCfgWellFormed MCSig AssignIsCExpression where
  assertCfgWellFormed t@(remA -> AssignIsCExpression cexp) = assertCfgIsGenericAuto (inject' t) [E cexp]

instance AssertCfgWellFormed MCSig CAssignOpIsAssignOp
instance AssertCfgWellFormed MCSig CExpressionIsRhs
instance AssertCfgWellFormed MCSig CExpressionIsLhs
instance AssertCfgWellFormed MCSig IdentIsIdent
instance AssertCfgWellFormed MCSig CInitializerIsLocalVarInit
instance AssertCfgWellFormed MCSig MultiLocalVarDeclIsCCompoundBlockItem where
  assertCfgWellFormed t@(remA -> MultiLocalVarDeclIsCCompoundBlockItem dec) = assertCfgIsGenericAuto (inject' t) [E dec]
  
instance AssertCfgWellFormed MCSig CDeclarationSpecifiersIsMultiLocalVarDeclCommonAttrs

instance AssertCfgWellFormed MCSig Flags
instance AssertCfgWellFormed MCSig CAssignOp
instance AssertCfgWellFormed MCSig CBinaryOp
instance AssertCfgWellFormed MCSig CUnaryOp
instance AssertCfgWellFormed MCSig CFunParamAttrs
instance AssertCfgWellFormed MCSig CFunDefAttrs
instance AssertCfgWellFormed MCSig CFunDeclAttrs
instance AssertCfgWellFormed MCSig COldStyleParam
instance AssertCfgWellFormed MCSig CVarArgParam
instance AssertCfgWellFormed MCSig CVoidArg
instance AssertCfgWellFormed MCSig CLabeledBlock
instance AssertCfgWellFormed MCSig CLocalVarAttrs
instance AssertCfgWellFormed MCSig CString
instance AssertCfgWellFormed MCSig CInteger
instance AssertCfgWellFormed MCSig CIntRepr
instance AssertCfgWellFormed MCSig CIntFlag
instance AssertCfgWellFormed MCSig CFloat
instance AssertCfgWellFormed MCSig CChar
instance AssertCfgWellFormed MCSig CTypeSpecifier
instance AssertCfgWellFormed MCSig CTypeQualifier
instance AssertCfgWellFormed MCSig CTranslationUnit
instance AssertCfgWellFormed MCSig CStructureUnion
instance AssertCfgWellFormed MCSig CStructTag
instance AssertCfgWellFormed MCSig CStringLiteral
instance AssertCfgWellFormed MCSig CStorageSpecifier
instance AssertCfgWellFormed MCSig CPartDesignator
instance AssertCfgWellFormed MCSig CInitializer
instance AssertCfgWellFormed MCSig CFunctionSpecifier
instance AssertCfgWellFormed MCSig CExternalDeclaration
instance AssertCfgWellFormed MCSig CEnumeration
instance AssertCfgWellFormed MCSig CDerivedDeclarator
instance AssertCfgWellFormed MCSig CDeclarator
instance AssertCfgWellFormed MCSig CDeclarationSpecifier
instance AssertCfgWellFormed MCSig CDeclaration
instance AssertCfgWellFormed MCSig CConstant
instance AssertCfgWellFormed MCSig CCompoundBlockItem where
  assertCfgWellFormed t@(remA -> CBlockStmt stmt) = assertCfgIsGeneric (inject' t) (extractBlock stmt)
  assertCfgWellFormed t@(remA -> CBlockDecl decl) = assertCfgIsGenericAuto (inject' t) [E decl]
  assertCfgWellFormed t@(remA -> CNestedFunDef _def) =
    -- NOTE: functiondef is suspended, so we just check whether the start and end are connected
    assertCfgIsGeneric (inject' t) []
  
instance AssertCfgWellFormed MCSig CBuiltinThing
instance AssertCfgWellFormed MCSig CAttribute
instance AssertCfgWellFormed MCSig CAssemblyStatement
instance AssertCfgWellFormed MCSig CAssemblyOperand
instance AssertCfgWellFormed MCSig CArraySize
instance AssertCfgWellFormed MCSig CAlignmentSpecifier
instance AssertCfgWellFormed MCSig Position
instance AssertCfgWellFormed MCSig FilePosition
instance AssertCfgWellFormed MCSig NodeInfo
instance AssertCfgWellFormed MCSig Name

instance AssertCfgWellFormed MCSig PositionalParameter
instance AssertCfgWellFormed MCSig FunctionDef where
  assertCfgWellFormed t@(remA -> FunctionDef attrs _ params e) =
    assertCfgFunction (inject' t) attrs params e

instance AssertCfgWellFormed MCSig PositionalParameterDeclOptionalIdent
instance AssertCfgWellFormed MCSig FunctionDecl
instance AssertCfgWellFormed MCSig PositionalArgument
instance AssertCfgWellFormed MCSig FunctionArgumentList
instance AssertCfgWellFormed MCSig EmptyFunctionCallAttrs
instance AssertCfgWellFormed MCSig FunctionCall
instance AssertCfgWellFormed MCSig EmptyBlockEnd
instance AssertCfgWellFormed MCSig S.Block
instance AssertCfgWellFormed MCSig AssignOpEquals
instance AssertCfgWellFormed MCSig Assign
instance AssertCfgWellFormed MCSig Ident
instance AssertCfgWellFormed MCSig MultiLocalVarDecl
instance AssertCfgWellFormed MCSig SingleLocalVarDecl
instance AssertCfgWellFormed MCSig OptLocalVarInit

instance AssertCfgWellFormed MCSig CExpression where
  assertCfgWellFormed t@(CCond test succ fail _ :&: _) = do
    assertCfgCondOp (inject' t) test (S.extractF succ) fail
  assertCfgWellFormed t@(CBinary op e1 e2 _ :&: _) = do
    case extractOp op of
      CLndOp -> assertCfgShortCircuit (inject' t) e1 e2
      CLorOp  -> assertCfgShortCircuit (inject' t) e1 e2
      _   -> assertCfgIsGeneric (inject' t) [E e1, E e2]

    where extractOp :: MCTermLab CBinaryOpL -> CBinaryOp MCTerm CBinaryOpL
          extractOp (stripA -> project -> Just bp) = bp

  assertCfgWellFormed t@(remA -> CComma es _) = assertCfgIsGeneric (inject' t) (map E $ extractF es)
  -- Ignoring Assign
  assertCfgWellFormed t@(remA -> CCast decs e _) = assertCfgIsGenericAuto (inject' t) [E decs, E e]
  assertCfgWellFormed t@(remA -> CUnary _ e _) = assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t@(remA -> CSizeofExpr e _) = assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t@(remA -> CSizeofType dec _) = assertCfgIsGenericAuto (inject' t) [E dec]
  assertCfgWellFormed t@(remA -> CAlignofExpr e _) = assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t@(remA -> CAlignofType dec _) = assertCfgIsGenericAuto (inject' t) [E dec]  
  assertCfgWellFormed t@(remA -> CComplexReal e _) = assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t@(remA -> CComplexImag e _) = assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t@(remA -> CIndex i e _) = assertCfgIsGeneric (inject' t) [E i, E e]
  assertCfgWellFormed t@(remA -> CIndex i e _) = assertCfgIsGeneric (inject' t) [E i, E e]
  -- Ignoring CCall
  assertCfgWellFormed t@(remA -> CMember e _ _ _) = assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t@(remA -> CVar {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> CConst {}) = assertCfgIsGeneric (inject' t) [] 
  assertCfgWellFormed t@(remA -> CCompoundLit dec init _) = assertCfgIsGenericAuto (inject' t) [E dec, E init]
  assertCfgWellFormed t@(remA -> CGenericSelection e es _) = assertCfgIsGenericAuto (inject' t) [E e, E es]  
  assertCfgWellFormed t@(remA -> CStatExpr s _) = assertCfgIsGeneric (inject' t) (extractBlock s)
  assertCfgWellFormed t@(remA -> CLabAddrExpr {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> CBuiltinExpr e) = assertCfgIsGenericAuto (inject' t) [E e]
  assertCfgWellFormed t = error $ "Impossible case: " ++ show (inject' t)
  
instance AssertCfgWellFormed MCSig CStatement where
  assertCfgWellFormed t@(remA -> CLabel _ stat _ _) =
    assertCfgLabel (inject' t) (extractBlock' stat)
  assertCfgWellFormed t@(remA -> CCase _ stat _) =
    assertCfgSubtermsAuto (inject' t) (extractBlock stat)
  assertCfgWellFormed t@(remA -> CCases _ _ stat _) =
    assertCfgSubtermsAuto (inject' t) (extractBlock stat)
  assertCfgWellFormed t@(remA -> CDefault stat _) =
    assertCfgSubtermsAuto (inject' t) (extractBlock stat)
  assertCfgWellFormed t@(remA -> CExpr mexp _) =
    case extractF mexp of
      Just e -> assertCfgIsGeneric (inject' t) [E e]
      Nothing -> assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(CIf cond thn mels _ :&: _) =
    case S.extractF mels of
      Just els -> assertCfgIfElse (inject' t) cond (extractBlock thn) (extractBlock els)
      Nothing  -> assertCfgIf (inject' t) cond (extractBlock thn)
  assertCfgWellFormed t@(remA -> CWhile e b False _) =
    assertCfgWhile (inject' t) e (extractBlock b)
  assertCfgWellFormed t@(remA -> CWhile e b True _) =
    assertCfgDoWhile (inject' t) (extractBlock b) e
  assertCfgWellFormed t@(remA -> CGoto n _) =    
    assertCfgGoto (inject' t) (nameString n)
  assertCfgWellFormed t@(remA -> CGotoPtr e _) =    
    assertCfgReturn (inject' t) (Just e)
  assertCfgWellFormed t@(remA -> CCont _) =
    assertCfgContinue (inject' t)
  assertCfgWellFormed t@(remA -> CBreak _) =
    assertCfgBreak (inject' t)
  assertCfgWellFormed t@(remA -> CReturn e _) =
    assertCfgReturn (inject' t) (S.extractF e)
  assertCfgWellFormed t@(remA -> CSwitch exp body _) =
    assertCfgSwitch (inject' t) exp (extractBlock body)
  assertCfgWellFormed t@(remA -> CAsm stmt _) = assertCfgIsGenericAuto (inject' t) [E stmt]
  assertCfgWellFormed t = error $ "Impossible case: " ++ show (inject' t)

instance AssertCfgWellFormed MCSig CForInit

instance AssertCfgWellFormed MCSig CFor where
  assertCfgWellFormed t@(remA -> CFor init cond step body) = do
    inits0 <- inits
    assertCfgFor (inject' t) inits0 (S.extractF cond) (S.extractF step) (extractBlock body)
    where inits = subtermWithCfg (inject' t) init

assertCfgSwitch ::
  ( MonadTest m
  , MonadReader (Cfg MCSig) m
  ) => MCTermLab a -> MCTermLab e -> [E MCTermLab] -> m ()
assertCfgSwitch t cond bodys = do
  (enSwitch, exSwitch) <- getEnterExitPair t t
  (enCond, exCond) <- getEnterExitPair t cond
  bodyEEPs <- mapM (getEnterExitPairE t) bodys
  let cases = extractCases t
      enBody = fst (head bodyEEPs)
      exBody = snd (last bodyEEPs)  
  
  caseEEPs <- mapM (getEnterExitPairE t) cases
  let caseEdges = map (\caseEEP -> (exCond, fst caseEEP)) caseEEPs
  assertEdges t ([ (enSwitch, enCond)
                , (exCond, enBody)
                , (exBody, exSwitch)
                ] ++ caseEdges)
                ([enSwitch, exSwitch, enBody, exBody, enCond, exCond] ++
                 map fst caseEEPs ++
                 map snd caseEEPs)

  where extractCases t0 =
              let subs = subterms t0
                  cases0 = filter isCase subs
                  switches = filter (\(E s) -> isSwitchNode s) subs
                  subcases = filter isCase (concatMap (\(E e0) -> subterms e0) switches)
              in  cases0 \\ subcases

        isCase :: E MCTermLab -> Bool
        isCase (E (project' -> Just (remA -> CCase {}))) = True
        isCase (E (project' -> Just (remA -> CDefault {}))) = True
        isCase _ = False
        
assertCfgLabel ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs  
  ) => TermLab fs a -> E (TermLab fs) -> m ()
assertCfgLabel t (E body) = do
  (enLabel, exLabel) <- getEnterExitPair t t
  (enBody, exBody) <- getEnterExitPair t body
  assertEdges t [(enLabel, exLabel), (exLabel, enBody)]
                [enLabel, exLabel, enBody, exBody]

assertCfgIfElse ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs
  ) => TermLab fs a -> TermLab fs e -> [E (TermLab fs)] -> [E (TermLab fs)] -> m ()
assertCfgIfElse t cond thns elss = do
  (enIf, exIf) <- getEnterExitPair t t
  (enCond, exCond) <- getEnterExitPair t cond
  thnEEPs <- mapM (getEnterExitPairE t) thns
  elsEEPs <- mapM (getEnterExitPairE t) elss
  midNode <- getIEP 0 t t
  let edges = [ (enIf, midNode), (midNode, enCond)
              , (exCond, enThn), (exThn, exIf)
              , (exCond, enEls), (exEls, exIf)]
      nodes = [enIf, exIf, midNode, enCond, exCond, enThn, exThn, enEls, exEls]           
      enThn = fst (head thnEEPs)
      exThn = snd (last thnEEPs)              
      enEls = fst (head elsEEPs)
      exEls = snd (last elsEEPs)              
  assertEdges t edges nodes

assertCfgIf ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs 
  ) => TermLab fs a -> TermLab fs e -> [E (TermLab fs)] -> m ()
assertCfgIf t cond thns = do
  (enIf, exIf) <- getEnterExitPair t t
  (enCond, exCond) <- getEnterExitPair t cond
  thnEEPs <- mapM (getEnterExitPairE t) thns
  midNode <- getIEP 0 t t
  let edges = [ (enIf, midNode), (midNode, enCond)
              , (exCond, enThn), (exThn, exIf)
              , (exCond, exIf)
              ]
      nodes = [enIf, exIf, midNode, enCond, exCond] ++
              map fst thnEEPs ++
              map snd thnEEPs
      enThn = fst (head thnEEPs)
      exThn = snd (last thnEEPs)              
  assertEdges t edges nodes

assertCfgWhile ::
  forall fs a e b m.
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs  
  ) => TermLab fs a -> TermLab fs e -> [E (TermLab fs)] -> m ()
assertCfgWhile t e bs = do
  (enWhile, exWhile) <- getEnterExitPair t t
  loWhile <- getLoopEntry t t
  (enCond, exCond) <- getEnterExitPair t e
  bodyEEPs <- mapM (getEnterExitPairE t) bs

  let enBody = fst (head bodyEEPs)
      exBody = snd (last bodyEEPs)    

  assertEdges t ([ (enWhile, loWhile)
                 , (loWhile, enCond)
                 , (exBody, loWhile)
                 , (exCond, enBody)
                 , (exCond, exWhile)                 
                 ])
                 [ enWhile, exWhile, loWhile, enCond, exCond, enBody, exBody]

assertCfgDoWhile ::
  ( MonadTest m
  , MonadReader (Cfg gs) m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  , All HFoldable gs  
  ) => TermLab gs l -> [E (TermLab gs)] -> TermLab gs j -> m ()
assertCfgDoWhile t bs e = do
  (enDoWhile, exDoWhile) <- getEnterExitPair t t
  loDoWhile <- getLoopEntry t t

  bodyEEPs <- mapM (getEnterExitPairE t) bs
  (enExp, exExp) <- getEnterExitPair t e
  let enBody = fst (head bodyEEPs)
      exBody = snd (last bodyEEPs)
      edges = [ (enDoWhile, enBody)
              , (exExp, enBody)
              , (exExp, exDoWhile)
              , (exBody, loDoWhile)
              , (loDoWhile, enExp)
              ]
  assertEdges t edges 
                [ enDoWhile, exDoWhile, loDoWhile
                , enBody, exBody, enExp, exExp
                ]

assertCfgFor ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs 
  ) => TermLab fs l -> Maybe (E (TermLab fs), E (TermLab fs)) -> Maybe (TermLab fs e) -> Maybe (TermLab fs e) -> [E (TermLab fs)] -> m ()
assertCfgFor t mInit mCond mStep bs = do
  eepFor <- getEnterExitPair t t
  loFor <- getLoopEntry t t
  bodyEEPs <- mapM (getEnterExitPairE t) bs
  let eepBody = (fst (head bodyEEPs), snd (last bodyEEPs))
  mEEPInitS <- traverse (getEnterExitPairE t) (fmap fst mInit)
  mEEPInitE <- traverse (getEnterExitPairE t) (fmap snd mInit)
  let mEEPInit = (,) <$> (fmap fst mEEPInitS) <*> (fmap snd mEEPInitE)
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
        let ns = catMaybes [mEEPCond, mEEPStep, mEEPInit] ++ [ eepFor, eepBody ]
        in loFor : map enter ns ++ map exit ns

  assertEdges t (nub edges) nodes

isLoopLikeNode :: TermLab MCSig l -> Bool
isLoopLikeNode (project' -> Just (CWhile {} :&: _))= True
isLoopLikeNode (project' -> Just (CFor {} :&: _)) = True
isLoopLikeNode _ = False

isSwitchNode :: TermLab MCSig l -> Bool
isSwitchNode (project' -> Just (CSwitch {} :&: _))= True
isSwitchNode _ = False

assertCfgCondOp ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  , All HFoldable fs  
  ) => TermLab fs e -> TermLab fs e -> Maybe (TermLab fs e) -> TermLab fs e -> m ()
assertCfgCondOp t test succ fail = do
  (enTop, exTop) <- getEnterExitPair t t
  (enTest, exTest) <- getEnterExitPair t test
  mEEPsucc <- traverse (getEnterExitPair t) succ
  (enFail, exFail) <- getEnterExitPair t fail

  case mEEPsucc of
    Just (enSucc, exSucc) -> do
      assertEdges t [(enTop, enTest), (exTest, enSucc), (exTest, enFail), (exSucc, exTop), (exFail, exTop)]
                    [enTop, exTop, enTest, exTest, enSucc, exSucc, enFail, exFail]
    Nothing -> do
      assertEdges t [(enTop, enTest), (exTest, exTop), (exTest, enFail), (exFail, exTop)]
                    [enTop, exTop, enTest, exTest, enFail, exFail]

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

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `continue`.
--       * that outgoing edge is to a node which has an `LoopEntryNode` type.
--       * that outgoing edge is to a node which is loop-like (is one of `Do while/For/While`)
--       * there are no incoming nodes in exit node of `continue`.
assertCfgContinue ::
  ( MonadTest m
  , MonadReader (Cfg MCSig) m
  ) => TermLab MCSig a -> m ()
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
      checkNodeType :: CfgNode MCSig -> Bool
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
  , All HFoldable fs  
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
--       * there in only one outgoing edge from entry node of `break`.
--       * that outgoing edge is to a node which has an `ExitNode` type.
--       * that outgoing edge is to a node which is switch or loop-like (is one of `Do while/For/While`)
--       * there are no incoming nodes in exit node of `break`.
assertCfgBreak ::
  ( MonadTest m
  , MonadReader (Cfg MCSig) m
  ) => TermLab MCSig a -> m ()
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
      checkNodeType :: CfgNode MCSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == ExitNode &&
        (\(E t) -> isLoopLikeNode t || isSwitchNode t) (node ^. cfg_node_term)

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `goto`
--       * that outgoing edge is to a node which has is a `Label` with right label name
--       * there are no incoming nodes in exit node of `goto`.
assertCfgGoto ::
  forall a m.
  ( MonadTest m
  , MonadReader (Cfg MCSig) m
  ) => TermLab MCSig a -> String -> m ()
assertCfgGoto t labName = do
  (enGoto, exGoto) <- getEnterExitPair t t
  let jmpNodeLabs = enGoto ^. cfg_node_succs
  assert (length jmpNodeLabs == 1)

  let enJmpLab = head (Set.toList jmpNodeLabs)
  menJump <- preview (cur_cfg.cfg_nodes.(ix enJmpLab))
  enJmp <- assertJust "Cfg label lookup: " menJump
  
  assert (checkLab (enJmp ^.  cfg_node_term))
  assert (length (exGoto ^. cfg_node_prevs) == 0)

  where
    checkLab ::  E (TermLab MCSig) -> Bool
    checkLab (E (project' -> Just (CLabel (nameString -> n) _ _ _ :&: _)))
          | labName == n = True
          | otherwise = False
    checkLab _ = False

assertCfgFunction ::
  ( MonadTest m
  , MonadReader (Cfg MCSig) m
  ) => TermLab MCSig a -> TermLab MCSig as -> TermLab MCSig ps -> TermLab MCSig b -> m ()
assertCfgFunction t attrs params body = do
  assertCfgIsSuspendedPiecesAuto t [E attrs, E params, E body]
  assertCfgSubtermsAuto t []

-- NOTE: Label adjustment is necessary because
--       Label node sort of does not respect compositionality
extractBlock :: TermLab MCSig CStatementL -> [E (TermLab MCSig)]
extractBlock (project' -> Just (CLabeledBlock _ blk :&: _)) =
  case project' blk of
    Just (S.Block items _ :&: _) -> [E items]
extractBlock t@(project' -> Just (remA -> CLabel _ stat _ _)) =
  E t : extractBlock stat
extractBlock t = [E t]

extractBlock' :: TermLab MCSig CStatementL -> E (TermLab MCSig)
extractBlock' (project' -> Just (CLabeledBlock _ blk :&: _)) =
  case project' blk of
    Just (S.Block items _ :&: _) -> E items
extractBlock' t = E t


nameString :: MCTermLab CFull.IdentL -> String
nameString (stripA -> projF -> Just (Ident' n)) = n

-- hardcoded integration tests
integration_c_cfg :: FilePath -> Property
integration_c_cfg path = 
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile path
    edges <- liftIO $ parseEdges path
    
    (_, cfg) <- makeCEnv t
    integration_cfg edges cfg
