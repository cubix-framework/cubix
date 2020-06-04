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
import           Data.Proxy
import           Data.Typeable
import qualified Data.Set as Set
import           Hedgehog
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
  pure (tLab, cfg)

instance AssertCfgWellFormed MLuaSig Block where
  assertCfgWellFormed t@(Block {} :&: _) = pure ()

instance AssertCfgWellFormed MLuaSig Stat where
  assertCfgWellFormed t@(Break :&: _) =
    assertCfgBreak (inject' t)
  assertCfgWellFormed t@(Repeat b e :&: _) =    
    assertCfgDoWhile (inject' t) (extractBlock b) e
  assertCfgWellFormed t@(While e b :&: _) =
    assertCfgWhile (inject' t) e (extractBlock b)
  assertCfgWellFormed t@(ForIn _ e b :&: _) = do
    assertCfgFor (inject' t) (extractList e) (extractBlock b)
  assertCfgWellFormed t@(Goto n :&: _) =
    assertCfgGoto (inject' t) (nameString n)
  assertCfgWellFormed t@(Label _ :&: _) =
    assertCfgLabel (inject' t)
  assertCfgWellFormed t@(ForRange _ init final optStep body :&: _) = do
    assertCfgForRange (inject' t) init final (extractMaybe optStep) (extractBlock body)
  assertCfgWellFormed t@(If clauses optElse :&: _) =
    assertCfgIfElse (inject' t) (extractClauses clauses) (extractBlock <$> extractMaybe optElse)
  assertCfgWellFormed t = assertCfgWellFormedDefault t

instance AssertCfgWellFormed MLuaSig Exp where
  assertCfgWellFormed t@(Binop op e1 e2 :&: _) = do
    case extractOp op of
      And -> assertCfgShortCircuit (inject' t) e1 e2
      Or  -> assertCfgShortCircuit (inject' t) e1 e2
      _   -> pure ()

    where extractOp :: MLuaTermLab BinopL -> Binop MLuaTerm BinopL
          extractOp (stripA -> project -> Just bp) = bp

  assertCfgWellFormed t = assertCfgWellFormedDefault t

instance {-# OVERLAPPABLE #-}
  ( f :-<: MLuaSig
  ) => AssertCfgWellFormed MLuaSig f where
  assertCfgWellFormed = assertCfgWellFormedDefault

assertCfgWellFormedDefault :: (MonadTest m, MonadReader (Cfg MLuaSig) m, f :-<: MLuaSig) => (f :&: Label) MLuaTermLab l -> m ()
assertCfgWellFormedDefault p@(inject' -> t) =
  if labeledIsComputationSort t then
    -- TODO: What has to be checked here?
    pure ()
  else if labeledIsSuspendedComputationSort t then
    -- TODO: What has to be checked here?
    pure ()
  else if labeledIsContainer t then
    assertCfgContainer t
  else
    pure ()

-- NOTE: Asserts that a CFG node is not created for this AST node.
assertCfgContainer ::
  ( MonadTest m
  , MonadReader (Cfg MLuaSig) m
  ) => MLuaTermLab l -> m ()
assertCfgContainer t = do
  mnodeLab <- preview (cur_cfg.cfg_ast_nodes.(ix astLab))
  case mnodeLab of
    Nothing -> pure ()
    Just l  -> H.failWith Nothing (msg l)

    where astLab = getAnn t
          msg l = "unexpected CfgNode created for AST: " ++
                  show t ++ "\n CfgNode is: " ++ show l

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
  (enBreak, exBreak) <- getEnterExitPair b
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
  (enGoto, exGoto) <- getEnterExitPair t
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
  (enLab, exLab) <- getEnterExitPair t
  assertEdges t [(enLab, exLab)] [enLab, exLab]

assertCfgForRange ::
  ( MonadTest m
  , MonadReader (Cfg fs) m
  , All ShowHF fs
  , All HFunctor fs
  , All EqHF fs  
  ) => TermLab fs a -> TermLab fs b -> TermLab fs c -> Maybe (TermLab fs d) -> BlockTermPairLab fs -> m ()
assertCfgForRange t init final optStep body = do
  (enForRange, exForRange) <- getEnterExitPair t
  (enInit, exInit) <- getEnterExitPair init
  (enFinal, exFinal) <- getEnterExitPair final
  optStepN <- maybe (pure Nothing) (fmap Just) (getEnterExitPair <$> optStep)
  (enBody, exBody) <- getEEPBlock body

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
  (enIf, exIf) <- getEnterExitPair t
  eepExps <- mapM (getEnterExitPair . fst) cs
  eepBodies <- mapM (getEEPBlock . snd) cs
  midNodes <- mapM (flip getIEP t) [0 .. length cs - 1]

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
          (enElse, exElse) <- getEEPBlock e
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
  (enSExp, exSExp) <- getEnterExitPair t
  (enE1, exE1) <- getEnterExitPair e1
  (enE2, exE2) <- getEnterExitPair e2

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
  (enWhile, exWhile) <- getEnterExitPair t
  loWhile <- getLoopEntry t

  (enExp, exExp) <- getEnterExitPair e
  (enBody, exBody) <- getEEPBlock b
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
  (enWhile, exWhile) <- getEnterExitPair t
  loWhile <- getLoopEntry t

  enExps <- getEnterNode (head es)
  exExps <- getExitNode (last es)

  (enBody, exBody) <- getEEPBlock b  
  assertEdges t [ (enWhile, loWhile)
                , (loWhile, enExps)
                , (exExps, enBody)
                , (exExps, exWhile)
                , (exBody, loWhile)
                ]
                [ enWhile, exWhile, loWhile
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
  (enDoWhile, exDoWhile) <- getEnterExitPair t
  loDoWhile <- getLoopEntry t

  (enBody, exBody) <- getEEPBlock b  
  (enExp, exExp) <- getEnterExitPair e

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
  case project' blk of
    Just b@(Block body r :&: _) -> case (extractF body, project' r) of
      ([], Just (LuaBlockEnd e :&: _)) -> (E blk, E blk)
      (_, Just (LuaBlockEnd e :&: _))  ->  case extractList <$> extractMaybe e of
        Nothing -> (E body, E body)
        Just [] -> (E body, E body)
        Just xs -> (E body, E (last xs))

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
  ) => BlockTermPairLab fs -> m (CfgNode fs, CfgNode fs)
getEEPBlock (bs, be) = do
  enBody <- getEnterNodeE bs
  exBody <- getExitNodeE be
  return (enBody, exBody)
