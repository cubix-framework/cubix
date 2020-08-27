{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |

module Main where

import Control.Lens ( (^.) )
import Control.Monad ( liftM, replicateM )
import Control.Monad.Identity ( runIdentity )
import Control.Monad.State ( evalState )

import qualified Data.Map as Map
import Data.Proxy ( Proxy(..) )
import Data.Text ( Text, append, pack)


import Language.Lua.Annotated ( parseFile, pprint )
import Language.Lua.Annotated.PrettyPrinter ( LPretty )

import Data.Comp.Multi ( Cxt(..), Term, E(..), K(..), DistAnn, project, project', inject, remA, stripA, (:&:)(..), subterms, inject' )
import Data.Comp.Multi.Strategic ( RewriteM, GRewrite, allbuR, promoteR, addFail )
import Data.Comp.Multi.Strategy.Classification ( DynCase, dynProj )


import Cubix.Language.Info
import Cubix.Language.Lua.Parametric.Common as C
import qualified Cubix.Language.Lua.Parametric.Full as F
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.ProgInfo
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Semantics.CfgInserter
import Cubix.Language.Parametric.Syntax.Functor

import Cubix.Sin.Compdata.Annotation ( DistAnn', MonadAnnotater, getAnn )

parse :: FilePath -> IO (Maybe (MLuaTerm LBlockL))
parse path = do
  res <- parseFile path
  case res of
    Left  e -> print e >> return Nothing
    Right b -> return $ Just $ C.translate $ F.translate b

prettyLua :: (LPretty (Targ l)) => MLuaTerm l -> String
prettyLua = show . pprint . F.untranslate . C.untranslate

pprintDisp :: forall l l'. (DynCase MLuaTerm l', LPretty (Targ l')) => Proxy l' -> (MLuaTerm l -> String) -> MLuaTerm l -> String
pprintDisp _ f t = case dynProj t :: Maybe (MLuaTerm l') of
  Just x  -> prettyLua x
  Nothing -> f t

type P a = Proxy a
p :: Proxy a
p = Proxy

prettyLuaUnk :: MLuaTerm l -> String
prettyLuaUnk = pprintDisp (p :: P LBlockL)
             $ pprintDisp (p :: P FunArgL)
             $ pprintDisp (p :: P FunCallL)
             $ pprintDisp (p :: P FunBodyL)
             $ pprintDisp (p :: P FunNameL)
             $ pprintDisp (p :: P TableFieldL)
             $ pprintDisp (p :: P PrefixExpL)
             $ pprintDisp (p :: P UnopL)
             $ pprintDisp (p :: P BinopL)
             $ pprintDisp (p :: P VarL)
             $ pprintDisp (p :: P ExpL)
             $ pprintDisp (p :: P StatL)
             $ (const "some other sort\n")

getCfgLab :: Cfg f -> TermLab f l -> [Label]
getCfgLab cfg t = map (^. cfg_node_lab) $ filter cmp (cfgNodes cfg)
  where
    cmp :: CfgNode f -> Bool
    cmp n = case n ^. cfg_node_term of
      E x -> getAnn x == getAnn t

putSubtree :: MLuaTermLab l -> Cfg MLuaSig ->  IO ()
putSubtree t cfg = do
 let cfgLab = getCfgLab cfg t
 if length cfgLab > 0 then do
   putStrLn ""
   putStrLn $ (show $ getAnn t) ++ "(cfg: " ++ show cfgLab ++ ")"
   putStrLn $ prettyLuaUnk $ stripA t
   putStrLn $ show $ stripA t
  else
   return ()

doubleAssign :: (MonadCfgInsertion m MLuaSig BlockItemL, MonadAnnotater Label m) => RewriteM m MLuaTermLab AssignL
doubleAssign t@(project' -> Just (Assign lhs _ _)) = do
  [l1, l2, l3, l4, l5] <- replicateM 5 nextLabel
  (t1 :: MLuaTermLab RhsL) <- labeledInjF $ inject' (ConsF (inject' $ Vararg :&: l2) (inject' $ NilF :&: l4) :&: l5)
  toInsert <- labeledInjF $ inject' $ (Assign lhs (inject' $ AssignOpEquals :&: l1) t1) :&: l3
  dominatingPrependFirst t (toInsert :: MLuaTermLab BlockItemL)
  return t

main = do
  Just tree <- parse "Foo.lua"
  gen <- mkConcurrentSupplyLabelGen
  let (treeLab :: MLuaTermLab _) = evalState (annotateLabel tree) gen
  putStrLn $ "\n\n"
  let progInfo = makeProgInfo treeLab
  --putStrLn $ show $ (cfg ^. cfg_ast_nodes)
  --putStrLn $ prettyCfg cfg
  --mapM (\(E t) -> putSubtree t cfg) $ subterms treeLab
  --putStrLn "\nBasic blocks: "
  --print $ map (^. cfg_node_lab) $ filter (startsBasicBlock cfg) $ cfgNodes cfg

  let t' = evalState (performCfgInsertions (Proxy :: Proxy BlockItemL) progInfo (allbuR $ promoteR doubleAssign) treeLab) gen

  putStrLn $ prettyLua $ stripA t'

  return ()

