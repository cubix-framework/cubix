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

module Cubix.Language.Lua.Cfg.Test where

import           Control.Lens hiding ( para )
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import qualified Data.Map as Map
import           Data.Maybe ( fromJust )
import           Data.Proxy
import           Data.Typeable
import qualified Data.Set as Set
import           Hedgehog hiding ( Var )
import qualified Hedgehog.Internal.Property as H

import           Cubix.Language.Info
import           Cubix.Language.Lua.Parametric.Common as LCommon
import qualified Cubix.Language.Lua.Parametric.Full as LFull
import           Cubix.Language.Parametric.Semantics.Cfg
import           Cubix.ParsePretty
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, (:*:), HFix, (:->), NatM, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), inj', inject', EqHF, remA, (:-<:), proj, Cxt (..), inj, hfoldMap, K (..), cata, hfold )
import           Data.Comp.Multi.Strategy.Classification ( DynCase, isSort )

import           Cubix.Language.Parametric.Cfg.Test
import qualified Cubix.Language.Parametric.Syntax as S

unit_lua_cfg :: FilePath -> Property
unit_lua_cfg fp =
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile fp
    (tLab, cfg) <- makeLuaEnv t
    runReaderT (assertCfgWellFormedness tLab) cfg
    pure ()

makeLuaEnv :: (MonadIO m) => MLuaTerm LBlockL -> m (MLuaTermLab LBlockL, Cfg MLuaSig)
makeLuaEnv t = do
  gen <- mkCSLabelGen
  let tLab = labelProg gen t
      cfg = makeCfg tLab
  pure (cfg `seq` (tLab, cfg))

instance AssertCfgWellFormed MLuaSig EmptyParameterAttrs
instance AssertCfgWellFormed MLuaSig PositionalParameter

instance AssertCfgWellFormed MLuaSig SelfParameter
instance AssertCfgWellFormed MLuaSig EmptyFunctionDefAttrs

instance AssertCfgWellFormed MLuaSig FunctionDef where
  assertCfgWellFormed t@(remA -> FunctionDef _ _ _ b) =
    case project' b of
      Just (BlockIsFunctionBody blk :&: _) ->
        case extractBlock' blk of
          (E t1, E t2) -> assertCfgIsSuspended' (inject' t) t1 t2

-- NOTE: couldn't find FunctionIdent in output
instance AssertCfgWellFormed MLuaSig FunctionIdent
instance AssertCfgWellFormed MLuaSig ReceiverArg
instance AssertCfgWellFormed MLuaSig PositionalArgument
instance AssertCfgWellFormed MLuaSig FunctionArgumentList
instance AssertCfgWellFormed MLuaSig EmptyFunctionCallAttrs
instance AssertCfgWellFormed MLuaSig FunctionCall
instance AssertCfgWellFormed MLuaSig EmptyLocalVarDeclAttrs
instance AssertCfgWellFormed MLuaSig AssignOpEquals
instance AssertCfgWellFormed MLuaSig Ident

-- NOTE: investigate following
-- NOTE: why dooes tuplebinder not have cfg node
--       but assign does?
-- NOTE: shouldn't TupleBinder check that its expressions are connected?
instance AssertCfgWellFormed MLuaSig TupleBinder

instance AssertCfgWellFormed MLuaSig Assign where
  assertCfgWellFormed t@(remA -> Assign (project' -> Just (LuaLhs lhs :&: _)) _ (project' -> Just (LuaRhs rhs :&: _))) = do
    assertCfgIsGeneric (inject' t) $ (map E (S.extractF lhs)) ++ (map E (S.extractF rhs))

instance AssertCfgWellFormed MLuaSig SingleLocalVarDecl
instance AssertCfgWellFormed MLuaSig OptLocalVarInit

instance AssertCfgWellFormed MLuaSig Block where
  assertCfgWellFormed t@(remA -> Block body r) = do
    case (extractF body, project' r) of
      ([], Just (LuaBlockEnd e :&: _)) -> case extractList <$> extractMaybe e of
        Nothing -> assertCfgIsGeneric (inject' t) [E body]
        Just [] -> assertCfgIsGeneric (inject' t) [E body]
        Just xs -> assertCfgIsGeneric (inject' t) $ [E body] ++ map E xs
      (_, Just (LuaBlockEnd e :&: _)) -> assertCfgNoCfgNode (inject' t)

instance AssertCfgWellFormed MLuaSig Stat where
  assertCfgWellFormed t@(remA -> Break) =
    assertCfgBreak (inject' t)
  assertCfgWellFormed t@(remA -> Repeat b e) =    
    assertCfgDoWhile (inject' t) (extractBlock b) e
  assertCfgWellFormed t@(remA -> While e b) =
    assertCfgWhile (inject' t) e (extractBlock b)
  assertCfgWellFormed t@(remA -> ForIn _ e b) = do
    assertCfgFor (inject' t) (extractList e) (extractBlock b)
  assertCfgWellFormed t@(remA -> Goto n) =
    assertCfgGoto (inject' t) (nameString n)
  assertCfgWellFormed t@(remA -> Label _) =
    assertCfgLabel (inject' t)
  assertCfgWellFormed t@(remA -> ForRange _ init final optStep body) = do
    assertCfgForRange (inject' t) init final (extractMaybe optStep) (extractBlock body)
  assertCfgWellFormed t@(remA -> If clauses optElse) =
    assertCfgIfElse (inject' t) (extractClauses clauses) (extractBlock <$> extractMaybe optElse)
  assertCfgWellFormed t@(remA -> FunCall c) = do
    assertCfgIsGeneric (inject' t) [E c]
  assertCfgWellFormed t@(remA -> EmptyStat) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> Do b) =
    assertCfgIsGeneric (inject' t) (extractBlockAll b)
  -- AssignIsStat covers Assign
  -- FunctionDefIsStat covers FunAssign and LocalFunAssign
  assertCfgWellFormed t = error $ "Impossible case: " ++ show (inject' t)

instance AssertCfgWellFormed MLuaSig Exp where
  assertCfgWellFormed t@(Binop op e1 e2 :&: _) = do
    case extractOp op of
      And -> assertCfgShortCircuit (inject' t) e1 e2
      Or  -> assertCfgShortCircuit (inject' t) e1 e2
      _   -> assertCfgIsGeneric (inject' t) [E e1, E e2]

    where extractOp :: MLuaTermLab BinopL -> Binop MLuaTerm BinopL
          extractOp (stripA -> project -> Just bp) = bp
  assertCfgWellFormed t@(remA -> Nil) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> Bool {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> String {}) =
    assertCfgIsGeneric (inject' t) []    
  assertCfgWellFormed t@(remA -> Number {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> Vararg {}) =
    assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> EFunDef _def) =
    -- TODO: Fix EFunDef
    pure ()
  assertCfgWellFormed t@(remA -> PrefixExp pexp) =
    assertCfgIsGeneric (inject' t) [E pexp]    
  assertCfgWellFormed t@(remA -> TableConst (project' -> Just (remA -> Table flds))) =
    assertCfgIsGeneric (inject' t) (map E $ S.extractF flds)
  assertCfgWellFormed t@(remA -> Unop _ e) =
    assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t = error $ "Impossible case: " ++ show (inject' t)

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `break`.
--       * that outgoing edge is to a node which has an `ExitNode` type.
--       * that outgoing edge is to a node which is loop-like (is one of `ForRange/ForIn/ While/Repeat`)
--         (TODO: assert that it is the *nearest* such loop-like AST)
--       * there are no incoming nodes in exit node of `break`.
assertCfgBreak ::
  ( MonadTest m
  , MonadReader (Cfg MLuaSig) m
  ) => TermLab MLuaSig a -> m ()
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
      checkNodeType :: CfgNode MLuaSig -> Bool
      checkNodeType node =
        node ^. cfg_node_type == ExitNode                    &&
        (\(E t) -> isLoopLikeNode t) (node ^. cfg_node_term)

      isLoopLikeNode :: TermLab MLuaSig l -> Bool
      isLoopLikeNode (project' -> Just (While {} :&: _))= True
      isLoopLikeNode (project' -> Just (Repeat {} :&: _)) = True
      isLoopLikeNode (project' -> Just (ForIn {} :&: _)) = True
      isLoopLikeNode (project' -> Just (ForRange {} :&: _)) = True
      isLoopLikeNode _ = False

      -- addCount t | isLoopNode (inject' t) = fmap (+ 1) <$> hfold t
      --            | unAnn t == getAnn b     = Just 0

-- NOTE: Asserts that
--       * there in only one outgoing edge from entry node of `goto`
--       * that outgoing edge is to a node which has is a `Label` with right label name
--       * there are no incoming nodes in exit node of `goto`.
assertCfgGoto ::
  forall a m.
  ( MonadTest m
  , MonadReader (Cfg MLuaSig) m
  ) => TermLab MLuaSig a -> String -> m ()
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
    checkLab ::  E (TermLab MLuaSig) -> Bool
    checkLab (E (project' -> Just (Label (nameString -> n) :&: _)))
          | labName == n = True
          | otherwise = False
    checkLab _ = False
  
assertCfgLabel ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs
  ) => TermLab fs a -> m ()
assertCfgLabel t = do
  (enLab, exLab) <- getEnterExitPair t t
  assertEdges t [(enLab, exLab)] [enLab, exLab]

assertCfgForRange ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs  
  ) => TermLab fs a -> TermLab fs b -> TermLab fs c -> Maybe (TermLab fs d) -> BlockTermPairLab fs -> m ()
assertCfgForRange t init final optStep body = do
  (enForRange, exForRange) <- getEnterExitPair t t
  (enInit, exInit) <- getEnterExitPair t init
  (enFinal, exFinal) <- getEnterExitPair t final
  optStepN <- maybe (pure Nothing) (fmap Just) (getEnterExitPair t <$> optStep)
  (enBody, exBody) <- getEEPBlock t body

  assertEdges t ([ (exInit, enFinal)
                , (enForRange, enInit)
                , (exBody, enBody)
                ] ++ optStepEntry exFinal optStepN ++ optStepExit exFinal enBody exForRange optStepN)

                ([ enForRange, exForRange, enInit, exInit
                , enFinal, exFinal 
                , enBody, exBody
                ] ++ maybe [] (\(en, ex) -> [en, ex]) optStepN)

  where optStepEntry _ Nothing = []
        optStepEntry exFinal (Just (enStep, _)) = [(exFinal, enStep)]

        optStepExit exFinal enBody exForRange Nothing =
          [ (exFinal, enBody)
          , (exFinal, exForRange)
          ]
        optStepExit _ enBody exForRange (Just (_, exStep)) =
          [ (exStep, enBody)
          , (exStep, exForRange)
          ]
          

assertCfgIfElse ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs  
  ) => TermLab fs a -> [(TermLab fs e, BlockTermPairLab fs)] -> Maybe (BlockTermPairLab fs) -> m ()
assertCfgIfElse t cs optElse = do
  (enIf, exIf) <- getEnterExitPair t t
  eepExps <- mapM (getEnterExitPair t . fst) cs
  eepBodies <- mapM (getEEPBlock t . snd) cs
  midNodes <- mapM (\i -> getIEP i t t) [0 .. length cs - 1]

  let evalEdges = concatMap (\( (_, exExp), (enBody, exBody)) ->
                         [ (exExp, enBody)
                         , (exBody, exIf)
                         ]
                      ) (zip eepExps eepBodies)
  
  let condEdges = map (\(midN, (enExp, _)) -> (midN, enExp)) condNodes
      condNodes = zip midNodes eepExps
      condJmpEdges = map (\( snd.snd -> exExp , (midN, _)) -> (exExp, midN)) (zip condNodes (tail condNodes))

  (elseNodes, exitEdges) <- case optElse of
        Nothing -> pure ([], [(snd $ last eepExps, exIf)])
        Just e  -> do
          (enElse, exElse) <- getEEPBlock t e
          pure ([enElse, exElse], [(snd $ last eepExps, enElse), (exElse, exIf)])

  assertEdges t ([(enIf, head midNodes)] ++ evalEdges ++ condEdges ++ condJmpEdges ++ exitEdges)
                ([enIf, exIf] ++ map fst eepExps ++ map snd eepExps ++ map fst eepBodies ++ map snd eepBodies ++ midNodes ++ elseNodes)
  

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

assertCfgWhile ::
  ( MonadTest m
  , MonadReader (Cfg gs) m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  ) => TermLab gs l -> TermLab gs i -> BlockTermPairLab gs -> m ()
assertCfgWhile t e b = do
  (enWhile, exWhile) <- getEnterExitPair t t
  loWhile <- getLoopEntry t t

  (enExp, exExp) <- getEnterExitPair t e
  (enBody, exBody) <- getEEPBlock t b
  assertEdges t [ (enWhile, loWhile)
                , (loWhile, enExp)
                , (exExp, enBody)
                , (exExp, exWhile)
                , (exBody, loWhile)
                ]
                [ enWhile, exWhile, loWhile
                , enExp, exExp, enBody, exBody
                ]

assertCfgFor ::
  ( MonadTest m
  , MonadReader (Cfg gs) m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  ) => TermLab gs l -> [TermLab gs i] -> BlockTermPairLab gs -> m ()
assertCfgFor t es b = do
  (enFor, exFor) <- getEnterExitPair t t
  loFor <- getLoopEntry t t

  enExps <- getEnterNode t (head es)
  exExps <- getExitNode t (last es)

  (enBody, exBody) <- getEEPBlock t b  
  assertEdges t [ (enFor, loFor)
                , (loFor, enExps)
                , (exExps, enBody)
                , (exExps, exFor)
                , (exBody, loFor)
                ]
                [ enFor, exFor, loFor
                , enExps, exExps, enBody, exBody
                ]


assertCfgDoWhile ::
  ( MonadTest m
  , MonadReader (Cfg gs) m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  ) => TermLab gs l -> BlockTermPairLab gs -> TermLab gs j -> m ()
assertCfgDoWhile t b e = do
  (enDoWhile, exDoWhile) <- getEnterExitPair t t
  loDoWhile <- getLoopEntry t t

  (enBody, exBody) <- getEEPBlock t b  
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

extractClauses :: TermLab MLuaSig [(ExpL, LFull.BlockL)] -> [(TermLab MLuaSig ExpL, BlockTermPairLab MLuaSig)]
extractClauses (project' -> Just (ConsF (project' -> Just (PairF e b :&: _)) xs :&: _)) = (inj e, extractBlock $ inj b) : extractClauses xs
extractClauses (project' -> Just (NilF :&: _))       = []

type BlockTermPairLab fs = (E (TermLab fs), E (TermLab fs)) -- (Term with label for blockstart, Term with label for blockend)

extractBlock :: TermLab MLuaSig LFull.BlockL -> BlockTermPairLab MLuaSig
extractBlock (project' -> Just (BlockIsBlock blk :&: _)) =
  extractBlock' blk

extractBlock' :: TermLab MLuaSig BlockL -> BlockTermPairLab MLuaSig
extractBlock' blk =
  case project' blk of
    Just b@(Block body r :&: _) -> case (extractF body, project' r) of
      ([], Just (LuaBlockEnd e :&: _)) -> (E blk, E blk)
      (_, Just (LuaBlockEnd e :&: _))  ->  case extractList <$> extractMaybe e of
        Nothing -> (E body, E body)
        Just [] -> (E body, E body)
        Just xs -> (E body, E (last xs))

extractBlockAll :: TermLab MLuaSig LFull.BlockL -> [E MLuaTermLab]
extractBlockAll (project' -> Just (BlockIsBlock blk :&: _)) =
  case project' blk of
    Just b@(Block body r :&: _) -> case (extractF body, project' r) of
      ([], Just (LuaBlockEnd e :&: _)) -> [E blk]
      (_, Just (LuaBlockEnd e :&: _))  ->  case extractList <$> extractMaybe e of
        Nothing -> [E body]
        Just [] -> [E body]
        Just xs -> (E body :  map E xs)
  

extractMaybe :: MLuaTermLab (Maybe l) -> Maybe (MLuaTermLab l)
extractMaybe (project' -> Just (JustF e :&: _)) = Just e        
extractMaybe _ = Nothing

extractList :: MLuaTermLab [l] -> [MLuaTermLab l]
extractList (project' -> Just (ConsF x xs :&: _)) = x : extractList xs
extractList (project' -> Just (NilF :&: _)) = []

nameString :: MLuaTermLab LFull.NameL -> String
nameString (stripA -> project -> Just (IdentIsName (Ident' s))) = s

getEEPBlock ::
  ( MonadReader (Cfg fs) m
  , MonadTest m
  , All ShowHF fs
  , All EqHF fs
  , All HFunctor fs
  ) => TermLab fs a -> BlockTermPairLab fs -> m (CfgNode fs, CfgNode fs)
getEEPBlock t (bs, be) = do
  enBody <- getEnterNodeE t bs
  exBody <- getExitNodeE t be
  return (enBody, exBody)

-- hardcoded integration tests
integration_lua_cfg :: Map.Map Int Int -> FilePath -> Property
integration_lua_cfg edges path = 
  withTests 1 $
  property $ do
    Just t <- liftIO $ parseFile path
    (_, cfg) <- makeLuaEnv t
    assertEdgesEqual edges (concatMap nodeEdges (nodes cfg))

    where nodes cfg = map snd $ Map.toList (cfg ^. cfg_nodes)
          nodeEdges n = map ((,) (n ^. cfg_node_lab)) (Set.toList $ n ^. cfg_node_succs)

          assertEdgesEqual es as =
            map (\(a, b) -> (show a, show b)) (Map.toList es) === map (\(a, b) -> (ppLabel a, ppLabel b)) as

instance AssertCfgWellFormed MLuaSig IdentIsName

instance AssertCfgWellFormed MLuaSig AssignIsStat where
  assertCfgWellFormed t@(remA -> AssignIsStat s) = assertCfgIsGeneric (inject' t) [E s]

instance AssertCfgWellFormed MLuaSig BlockIsBlock
instance AssertCfgWellFormed MLuaSig StatIsBlockItem

instance AssertCfgWellFormed MLuaSig SingleLocalVarDeclIsStat where
  assertCfgWellFormed t@(remA -> SingleLocalVarDeclIsStat (project' -> Just (remA -> SingleLocalVarDecl _ _ init))) =
    case project' init of
      Just (remA -> JustLocalVarInit (project' -> Just (remA -> LuaLocalVarInit is))) -> do
        assertCfgIsGeneric (inject' t) (map E (S.extractF is))
      Just (remA -> NoLocalVarInit) -> do
        assertCfgIsGeneric (inject' t) []

instance AssertCfgWellFormed MLuaSig FunctionCallIsFunCall where
  assertCfgWellFormed t@(remA -> FunctionCallIsFunCall fc) = do
    let pes = maybe (error $ show (inject' t)) id $ do
          (remA -> FunctionCall _ pefe args)  <- project' fc
          pure $ go1 pefe ++ go args
    assertCfgIsGeneric (inject' t) pes

    where go2 :: MLuaTermLab FunctionArgumentL -> E MLuaTermLab
          go2 (project' -> Just (remA -> PositionalArgument exp)) =
            case project' exp of
              Just (remA -> ExpIsPositionalArgExp e) -> E e
          go2 (project' -> Just (remA -> ReceiverArg exp)) =
              case project' exp of
                Just (remA -> PrefixExpIsReceiver e) -> E e

          go1 :: MLuaTermLab FunctionExpL -> [E MLuaTermLab]
          go1 (project' -> Just (remA -> PrefixExpIsFunctionExp pe)) = [E pe]
          go1 (project' -> Just (remA -> FunctionIdent _)) = []

          go :: MLuaTermLab FunctionArgumentsL -> [E MLuaTermLab]
          go (project' -> Just (remA -> FunctionArgumentList as)) = map go2 (S.extractF as)
          go (project' -> Just (remA -> LuaTableArg tab)) =
            case project' tab of
              Just (remA -> Table flds) -> (map E $ S.extractF flds)
          go (project' -> Just (remA -> LuaStringArg {})) = []
          go (project' -> Just (remA -> LuaReceiverAndTableArg pe t)) = do
            case project' t of
              Just (remA -> Table flds) -> E pe : (map E $ S.extractF flds)
          go (project' -> Just (remA -> LuaReceiverAndStringArg pe _)) = [E pe]          
          
instance AssertCfgWellFormed MLuaSig ExpIsPositionalArgExp
instance AssertCfgWellFormed MLuaSig PrefixExpIsFunctionExp
instance AssertCfgWellFormed MLuaSig PrefixExpIsReceiver

instance AssertCfgWellFormed MLuaSig FunctionDefIsStat where
  assertCfgWellFormed t@(remA -> FunctionDefIsStat def) =
    -- NOTE: FunctionDef is a suspended computation, so just check
    -- if start and end of this node are connected
    assertCfgIsGeneric (inject' t) []

instance AssertCfgWellFormed MLuaSig BlockIsFunctionBody

-- lua specific

instance AssertCfgWellFormed MLuaSig LuaVarArgsParam
instance AssertCfgWellFormed MLuaSig LuaFunctionAttrs
instance AssertCfgWellFormed MLuaSig LuaFunctionDefinedObj
instance AssertCfgWellFormed MLuaSig LuaSpecialFunArg

instance AssertCfgWellFormed MLuaSig LuaBlockEnd

instance AssertCfgWellFormed MLuaSig LuaRhs
instance AssertCfgWellFormed MLuaSig LuaLhs
instance AssertCfgWellFormed MLuaSig LuaLocalVarInit

instance AssertCfgWellFormed MLuaSig NumberType

instance AssertCfgWellFormed MLuaSig Var where
  assertCfgWellFormed t@(remA -> VarName {}) = assertCfgIsGeneric (inject' t) []
  assertCfgWellFormed t@(remA -> Select pe e) = assertCfgIsGeneric (inject' t) [E pe, E e]
  assertCfgWellFormed t@(remA -> SelectName pe _) = assertCfgIsGeneric (inject' t) [E pe]

instance AssertCfgWellFormed MLuaSig Unop

instance AssertCfgWellFormed MLuaSig TableField where
  assertCfgWellFormed t@(remA -> ExpField e1 e2) = assertCfgIsGeneric (inject' t) [E e1, E e2]
  assertCfgWellFormed t@(remA -> NamedField _ e) = assertCfgIsGeneric (inject' t) [E e]
  assertCfgWellFormed t@(remA -> Field e) = assertCfgIsGeneric (inject' t) [E e]
  
instance AssertCfgWellFormed MLuaSig Table

instance AssertCfgWellFormed MLuaSig PrefixExp where
  assertCfgWellFormed t@(remA -> PEVar v) = assertCfgIsGeneric (inject' t) [E v]
  assertCfgWellFormed t@(remA -> PEFunCall fcall) = assertCfgIsGeneric (inject' t) [E fcall]
  assertCfgWellFormed t@(remA -> Paren v) = assertCfgIsGeneric (inject' t) [E v]

instance AssertCfgWellFormed MLuaSig FunName

-- NOTE: seems like FunDef and FunBody is only used for lambdas
-- get more information about this
instance AssertCfgWellFormed MLuaSig FunDef
instance AssertCfgWellFormed MLuaSig FunBody

instance AssertCfgWellFormed MLuaSig Binop 


-- containers
instance AssertCfgWellFormed MLuaSig PairF
instance AssertCfgWellFormed MLuaSig ListF where
  assertCfgWellFormed _ = pure () -- error "TODO"

instance AssertCfgWellFormed MLuaSig MaybeF
instance AssertCfgWellFormed MLuaSig UnitF
