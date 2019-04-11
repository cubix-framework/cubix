{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad ( when, liftM, (<=<), (=<<) )
import Control.Monad.Identity ( runIdentity )

import Data.Char  ( toLower )
import Data.List ( intercalate )
import Data.Map ( Map )
import Data.Maybe ( fromJust )
import System.Environment ( getArgs )
import System.IO ( hClose )

import Data.Comp.Multi ( Term, stripA, HFoldable, ShowHF )
import Data.Comp.Multi.Strategy.Classification ( dynProj )

import qualified Language.Dot.Pretty as Dot
import qualified Language.Dot.Syntax as Dot

import System.IO.Temp ( withSystemTempFile )

import Cubix.ParsePretty

import Cubix.Language.Info
import Cubix.Language.Parametric.Semantics.Cfg

import Cubix.Language.C.Parametric.Common
import Cubix.Language.Java.Parametric.Common
import Cubix.Language.JavaScript.Parametric.Common
import Cubix.Language.Lua.Parametric.Common
import Cubix.Language.Python.Parametric.Common as PCommon

import Cubix.Analysis.Call.Trivial

import Cubix.Transformations.Hoist
import Cubix.Transformations.Plumbing.IPT
import Cubix.Transformations.TAC
import Cubix.Transformations.TestCoverage

#ifdef ONLY_ONE_LANGUAGE
data LangProg = LuaProg (MLuaTerm LBlockL) | CfgDot Dot.Graph
  deriving ( Eq, Ord, Show )
#else
data LangProg = CProg      (MCTerm CTranslationUnitL)
              | JavaProg   (MJavaTerm CompilationUnitL)
              | JSProg     (MJSTerm JSASTL)
              | LuaProg    (MLuaTerm LBlockL)
              | PythonProg (MPythonTerm PCommon.ModuleL)
              | CfgDot     Dot.Graph
  deriving ( Eq, Ord, Show )
#endif

data LangProj = LuaProj    (Project MLuaSig)
#ifndef ONLY_ONE_LANGUAGE
              | CProj      (Project MCSig)
              | JavaProj   (Project MJavaSig)
              | JSProj     (Project MJSSig)
              | PythonProj (Project MPythonSig)
#endif


prettyProg :: LangProg -> String
#ifndef ONLY_ONE_LANGUAGE
prettyProg (CProg      p) = pretty p
prettyProg (JavaProg   p) = pretty p
prettyProg (JSProg     p) = pretty p
prettyProg (PythonProg p) = pretty p
#endif
prettyProg (LuaProg    p) = pretty p
prettyProg (CfgDot     p) = Dot.renderDot p

parseProg :: String -> String -> IO (Maybe LangProg)
#ifndef ONLY_ONE_LANGUAGE
parseProg "c"          = liftM (liftM CProg)      . parseFile
parseProg "java"       = liftM (liftM JavaProg)   . parseFile
parseProg "javascript" = liftM (liftM JSProg)     . parseFile
parseProg "python"     = liftM (liftM PythonProg) . parseFile
#endif
parseProg "lua"        = liftM (liftM LuaProg)    . parseFile
parseProg _            = error "Unrecognized language. Must be one of: c, java, javascript, lua, python"


parseProj :: LabelGen -> String -> [FilePath] -> IO (Maybe LangProj)
#ifndef ONLY_ONE_LANGUAGE
parseProj gen "c"          = (return . maybe Nothing (Just . CProj))      <=< parseProject gen parseFile
parseProj gen "java"       = (return . maybe Nothing (Just . JavaProj))   <=< parseProject gen parseFile
parseProj gen "javascript" = (return . maybe Nothing (Just . JSProj))     <=< parseProject gen parseFile
parseProj gen "python"     = (return . maybe Nothing (Just . PythonProj)) <=< parseProject gen parseFile
#endif
parseProj gen "lua"        = (return . maybe Nothing (Just . LuaProj))    <=< parseProject gen parseFile
parseProj _   _            = error "Unrecognized language. Must be one of: c, java, javascript, lua, python"

putProj :: LangProj -> IO ()
#ifndef ONLY_ONE_LANGUAGE
putProj (CProj      p) = putProject (prettyC          . fromJust . dynProj . stripA) p
putProj (JavaProj   p) = putProject (prettyJava       . fromJust . dynProj . stripA) p
putProj (JSProj     p) = putProject (prettyJavaScript . fromJust . dynProj . stripA) p
putProj (PythonProj p) = putProject (prettyPython     . fromJust . dynProj . stripA) p
#endif
putProj (LuaProj    p) = putProject (prettyLua        . fromJust . dynProj . stripA) p


debugTree' :: (ShowHF f, HFoldable f, CfgBuilder f) => Term f l -> IO ()
debugTree' t = do
  gen <- mkCSLabelGen
  let tLab = labelProg gen t
  debugCfg tLab (makeCfg tLab)
  print t


debugTree :: LangProg -> IO ()
#ifndef ONLY_ONE_LANGUAGE
debugTree (CProg      p) = debugTree' p
debugTree (JavaProg   p) = debugTree' p
debugTree (JSProg     p) = debugTree' p
debugTree (PythonProg p) = debugTree' p
#endif
debugTree (LuaProg    p) = debugTree' p


checkRoundTrip :: String -> LangProg -> IO LangProg
checkRoundTrip lang p = withSystemTempFile "roundtrip" $ \tmp h -> do
                            hClose h
                            writeFile tmp $ prettyProg p
                            pRes' <- parseProg lang tmp
                            case pRes' of
                              Nothing -> error "Failed re-parse"
                              Just p' -> if p == p' then
                                           return p'
                                         else
                                           error $  "Failed round trip: \n" ++ (show p) ++ "\n\n" ++ (show p')
                                                 ++ "\n\n" ++ (prettyProg p) ++ "\n\n" ++ (prettyProg p')

printCfgDot :: LangProg -> IO LangProg
printCfgDot t = do
  gen <- mkCSLabelGen
  case t of
#ifndef ONLY_ONE_LANGUAGE
    CProg      p -> return $ CfgDot $ renderCfgDot (labelProg gen p)
    JavaProg   p -> return $ CfgDot $ renderCfgDot (labelProg gen p)
    JSProg     p -> return $ CfgDot $ renderCfgDot (labelProg gen p)
    PythonProg p -> return $ CfgDot $ renderCfgDot (labelProg gen p)
#endif
    LuaProg    p -> return $ CfgDot $ renderCfgDot (labelProg gen p)
    _          -> error "Cannot render the CFG of that program"

runHoist :: LangProg -> LangProg
#ifndef ONLY_ONE_LANGUAGE
runHoist (CProg p)    = CProg    $ runIdentity $ hoistDeclarations p
runHoist (JavaProg p) = JavaProg $ runIdentity $ hoistDeclarations p
runHoist (JSProg p)   = JSProg   $ runIdentity $ hoistDeclarations p
#endif
runHoist (LuaProg p)  = LuaProg  $ runIdentity $ hoistDeclarations p
runHoist _            = error "Cannot run hoist on that program"

runElementaryHoist :: LangProg -> LangProg
#ifndef ONLY_ONE_LANGUAGE
runElementaryHoist (CProg p)    = CProg    $ elementaryHoist p
runElementaryHoist (JavaProg p) = JavaProg $ elementaryHoist p
runElementaryHoist (JSProg p)   = JSProg   $ elementaryHoist p
#endif
runElementaryHoist _            = error "Cannot run elementary hoist on that program"

runTAC :: LangProg -> IO LangProg
runTAC t = do
  gen <- mkCSLabelGen
  case t of
#ifndef ONLY_ONE_LANGUAGE
    JSProg     p -> liftM (JSProg     . stripA) $ toTAC (labelProg gen p)
    PythonProg p -> liftM (PythonProg . stripA) $ toTAC (labelProg gen p)
#endif
    LuaProg    p -> liftM (LuaProg    . stripA) $ toTAC (labelProg gen p)
    _          -> error "Cannot run TAC transform on that program"

runTestCov :: LangProg -> IO LangProg
runTestCov t = do
  gen <- mkCSLabelGen
  case t of
#ifndef ONLY_ONE_LANGUAGE
    CProg      p -> liftM (CProg      . stripA) $ instrumentTestCoverage (labelProg gen p)
    JavaProg   p -> liftM (JavaProg   . stripA) $ instrumentTestCoverage (labelProg gen p)
    JSProg     p -> liftM (JSProg     . stripA) $ instrumentTestCoverage (labelProg gen p)
    PythonProg p -> liftM (PythonProg . stripA) $ instrumentTestCoverage (labelProg gen p)
#endif
    LuaProg    p -> liftM (LuaProg    . stripA) $ instrumentTestCoverage (labelProg gen p)
    _          -> error "Cannot run testcov on that program"

runTrivCallAnalysis :: LangProj -> Map String [NodeIdx]
#ifndef ONLY_ONE_LANGUAGE
runTrivCallAnalysis (CProj      p) = callAnalysis p
runTrivCallAnalysis (JavaProj   p) = callAnalysis p
runTrivCallAnalysis (JSProj     p) = callAnalysis p
--runTrivCallAnalysis (PythonProj p) = callAnalysis p
#endif
runTrivCallAnalysis (LuaProj    p) = callAnalysis p

runIpt :: LabelGen -> LangProj -> IO ()
#ifndef ONLY_ONE_LANGUAGE
runIpt gen (CProj      p) = putProj =<< (CProj      <$> interproceduralPlumbingTransform gen p)
runIpt gen (JavaProj   p) = putProj =<< (JavaProj   <$> interproceduralPlumbingTransform gen p)
runIpt gen (JSProj     p) = putProj =<< (JSProj     <$> interproceduralPlumbingTransform gen p)
runIpt gen (PythonProj p) = putProj =<< (PythonProj <$> interproceduralPlumbingTransform gen p)
#endif
runIpt gen (LuaProj    p) = putProj =<< (LuaProj    <$> interproceduralPlumbingTransform gen p)

downcase :: String -> String
downcase = map toLower

tDebug, tId, tCfg, tElemHoist,tHoist, tTac, tTestcov :: String
tDebug     = "debug"
tId        = "id"
tCfg       = "cfg"
tElemHoist = "elementary-hoist"
tHoist     = "hoist"
tTac       = "tac"
tTestcov   = "testcov"

transformsList :: [String]
transformsList = [tDebug, tId, tCfg, tElemHoist, tHoist, tTac, tTestcov]

isTransform :: String -> Bool
isTransform a = elem a transformsList

aTrivCall :: String
aTrivCall = "anal-triv-call"

analsList :: [String]
analsList = [aTrivCall]

isAnalysis :: String -> Bool
isAnalysis a = elem a analsList

doAnal :: String -> String -> [FilePath] -> IO ()
doAnal lang anal fils = do
     gen <- mkCSLabelGen
     projRes <- parseProj gen (downcase lang) fils
     case projRes of
       Nothing   -> error "Parse failed"
       Just proj -> go proj
  where
    go proj
         | anal == aTrivCall = print $ runTrivCallAnalysis proj

doIpt :: String -> [FilePath] -> IO ()
doIpt lang fils = do
    gen <- mkCSLabelGen
    projRes <- parseProj gen (downcase lang) fils
    case projRes of
      Nothing   -> error "Parse failed"
      Just proj -> runIpt gen proj

doTransform :: String -> String -> FilePath -> IO ()
doTransform language transform file = do
    progRes <- parseProg (downcase language) file
    case progRes of
      Nothing -> error "Parse failed"
      Just prog -> putStrLn =<< (prettyProg <$> go (downcase transform) prog)
  where
    go t prog
      | t == tDebug     = (debugTree prog >> return prog)
      | t == tId        = checkRoundTrip language prog
      | t == tCfg       = printCfgDot prog
      | t == tElemHoist = return $ runElementaryHoist prog
      | t == tHoist     = return $ runHoist prog
      | t == tTac       = runTAC prog
      | t == tTestcov   = runTestCov prog

--FIXME: I got lazy and just made a separate branch for IPT. Should be merged
-- with other transformations

main = do
  args <- getArgs
  if length args < 3 then do
    when (length args == 0) $ putStrLn description
    putStrLn usage
   else
     let (language, cmd) = (downcase (args !! 0), downcase (args !! 1)) in
     if isTransform cmd then
       doTransform language cmd (args !! 2)
     else if isAnalysis cmd then
       doAnal language cmd (drop 2 args)
     else if cmd == "ipt" then
       doIpt language (drop 2 args)
     else
       putStrLn usage


description :: String
description = "Cubix 0.1.0.0\n"
           ++ "Cubix is a framework for language-parametric program "
           ++ "transformation. It currently supports C, Java, JavaScript, "
           ++ "Lua, and Python.\n"

usage :: String
usage =  "Usage:\n"
     ++ "examples-multi <language> <transform> <file>*\n"
     ++ "examples-multi <language> <analysis>  <file>*\n"
     ++ "Transforms available: " ++ intercalate "," transformsList ++ "\n"
     ++ "Analyses available: " ++ intercalate ", " analsList ++ "\n"
