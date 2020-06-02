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


import           Cubix.Language.Info
import           Cubix.Language.Lua.Parametric.Common as LCommon
import qualified Cubix.Language.Lua.Parametric.Full as LFull
import           Cubix.Language.Parametric.Semantics.Cfg
import           Cubix.ParsePretty
import           Cubix.Sin.Compdata.Annotation ( getAnn )
import           Data.Comp.Multi ( E (..), project, stripA, (:&:) (..), Sum, All, caseCxt', para, (:*:), HFix, (:->), NatM, ffst, hfmap, ShowHF, HFunctor, (:*:) (..), inj', inject', EqHF, remA, (:-<:), proj, Cxt (..), inj )
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

instance AssertCfgWellFormed MLuaSig Stat where
  assertCfgWellFormed t@(Break :&: _) =
    assertCfgBreak (inject' t)
  assertCfgWellFormed t@(Repeat b e :&: _) =    
    assertCfgDoWhile (inject' t) (extractBlock b) e
  assertCfgWellFormed t@(While e b :&: _) =
    assertCfgWhile (inject' t) e (extractBlock b)
  assertCfgWellFormed t@(ForIn _ e b :&: _) = do
    assertCfgWhile (inject' t) e (extractBlock b)
  -- assertCfgWellFormed t@(Goto (n :*: _) :&: _) = HEnvM $ do
  --  assertCfgGoto t (nameString n)
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

assertCfgBreak :: (MonadTest m) => TermLab fs a -> m ()
assertCfgBreak _ = pure ()

assertCfgGoto :: (MonadTest m) => TermLab fs a -> m ()
assertCfgGoto _ = pure ()

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
  ) => TermLab fs a -> TermLab fs b -> TermLab fs c -> Maybe (TermLab fs d) -> TermLab fs e -> m ()
assertCfgForRange t init final optStep body = do
  (enForRange, exForRange) <- getEnterExitPair t
  (enInit, exInit) <- getEnterExitPair init
  (enFinal, exFinal) <- getEnterExitPair final
  optStepN <- maybe (pure Nothing) (fmap Just) (getEnterExitPair <$> optStep)
  (enBody, exBody) <- getEnterExitPair body

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
  ) => TermLab fs a -> [(TermLab fs e, TermLab fs b)] -> Maybe (TermLab fs b) -> m ()
assertCfgIfElse t cs optElse = do
  (enIf, exIf) <- getEnterExitPair t
  eepExps <- mapM (getEnterExitPair . fst) cs
  eepBodies <- mapM (getEnterExitPair . snd) cs
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
          (enElse, exElse) <- getEnterExitPair e
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
  ) => TermLab gs l -> TermLab gs i -> TermLab gs j -> m ()
assertCfgWhile t e b = do
  (enWhile, exWhile) <- getEnterExitPair t
  loWhile <- getLoopEntry t

  (enExp, exExp) <- getEnterExitPair e
  (enBody, exBody) <- getEnterExitPair b
  assertEdges t [ (enWhile, loWhile)
                , (loWhile, enExp)
                , (exExp, enBody)
                , (exExp, exWhile)
                , (exBody, loWhile)
                ]
                [ enWhile, exWhile, loWhile
                , enExp, exExp, enBody, exBody
                ]

assertCfgDoWhile ::
  ( MonadTest m
  , MonadReader (Cfg gs) m
  , All ShowHF gs
  , All HFunctor gs
  , All EqHF gs
  ) => TermLab gs l -> TermLab gs i -> TermLab gs j -> m ()
assertCfgDoWhile t b e = do
  (enDoWhile, exDoWhile) <- getEnterExitPair t
  loDoWhile <- getLoopEntry t

  (enBody, exBody) <- getEnterExitPair b
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

extractClauses :: TermLab MLuaSig [(ExpL, LFull.BlockL)] -> [(TermLab MLuaSig ExpL, TermLab MLuaSig [BlockItemL])]
extractClauses (project' -> Just (ConsF (project' -> Just (PairF e b :&: _)) xs :&: _)) = (inj e, extractBlock $ inj b) : extractClauses xs
extractClauses (project' -> Just (NilF :&: _))       = []

extractBlock :: TermLab MLuaSig LFull.BlockL -> TermLab MLuaSig [BlockItemL]
extractBlock (project' -> Just (BlockIsBlock blk :&: _)) =
  case project' blk of
    Just b@(Block body r :&: _) -> case (extractF body, project' r) of
      ([], Just (LuaBlockEnd e :&: _)) -> error "TODO: Fill in"
      _                                -> body

extractMaybe :: MLuaTermLab (Maybe l) -> Maybe (MLuaTermLab l)
extractMaybe (project' -> Just (JustF e :&: _)) = Just e        
extractMaybe _ = Nothing

