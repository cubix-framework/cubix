{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.DeepSeq ( NFData(..) )
import Control.Monad ( (=<<) )
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import Control.Lens ( (^.) )
import Data.Comp.Multi.Annotation (ann)

import Criterion.Main

import qualified Language.C as CLib
import qualified Language.Java.Pretty as JLib
import qualified Language.Java.Syntax as JLib
import qualified Language.JavaScript.Parser as JSLib
import qualified Language.JavaScript.Pretty.Printer.Extended as JSLib
import qualified Language.Lua as LuaLib (pprint)
import qualified Language.Lua.Annotated.Lexer as LuaLib (SourceRange (..), SourcePos (..))
import qualified Language.Lua.Annotated as LuaLib
import qualified Language.Python.Common as PLib
import qualified Language.Python.Version3.Parser as PLib

import Data.Comp.Multi      ( HFunctor, ShowHF )

import Cubix.Language.Info
import Cubix.Language.Parametric.Semantics.Cfg

import Cubix.Language.C.Parametric.Common as CCommon
import qualified Cubix.Language.C.Parametric.Full as CFull
import qualified Cubix.Language.C.Parse as CParse
import qualified Cubix.Language.Java.Parse as JParse
import Cubix.Language.Java.Parametric.Common as JCommon
import qualified Cubix.Language.Java.Parametric.Full as JFull
import Cubix.Language.JavaScript.Parametric.Common as JSCommon
import qualified Cubix.Language.JavaScript.Parametric.Full as JSFull
import Cubix.Language.Lua.Parametric.Common as LCommon
import qualified Cubix.Language.Lua.Parametric.Full as LFull
import Cubix.Language.Python.Parametric.Common as PCommon
import qualified Cubix.Language.Python.Parametric.Full as PFull

import Cubix.Transformations.Hoist
import Cubix.Transformations.TAC
import Cubix.Transformations.TestCoverage

import Cubix.Sin.NFData ()

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left x) = error "Benchmarks.fromRight passed a Left"

instance NFData JLib.CompilationUnit where
  rnf = rnf . show

instance NFData JSLib.JSAST where
  rnf = rnf . show

instance NFData (PLib.Module ()) where
  rnf = rnf . show

-- Evaluating all but the term
instance NFData (CfgNode f) where
  rnf n =       (rnf $ n ^. cfg_node_prevs)
          `seq` (rnf $ n ^. cfg_node_succs)
          `seq` (rnf $ n ^. cfg_node_lab)
          `seq` (rnf $ n ^. cfg_node_type)
          `seq` ()

instance NFData (Cfg f) where
  rnf cfg =      (rnf $ cfg ^. cfg_nodes)
           `seq` (rnf $ cfg ^. cfg_ast_nodes)
           `seq` ()


#ifndef ONLY_ONE_LANGUAGE
getC :: FilePath -> IO ( CLib.CTranslUnit, CFull.CTerm CTranslationUnitL
                       , MCTerm CTranslationUnitL, MCTermLab CTranslationUnitL
                       {-, Cfg MCSig-})
getC path = do
  raw <- fromRight <$> CParse.parse path
  gen <- mkCSLabelGen -- OriginSynthetic
  let fullTree = CFull.translate $ fmap (const ()) raw
  let commTree = CCommon.translate fullTree
  let labTree  = labelProg gen commTree
  return (raw, fullTree, commTree, labTree{-, makeCfg labTree-})



getJava :: FilePath -> IO ( JLib.CompilationUnit, JFull.JavaTerm CompilationUnitL
                         , MJavaTerm CompilationUnitL, MJavaTermLab CompilationUnitL
                         {-, Cfg MJavaSig-})
getJava path = do
  raw <- fromRight <$> JParse.parse path
  gen <- mkCSLabelGen -- OriginSynthetic
  let fullTree = JFull.translate raw
  let commTree = JCommon.translate fullTree
  let labTree  = labelProg gen commTree
  return (raw, fullTree, commTree, labTree{-, makeCfg labTree-})


getJS :: FilePath -> IO ( JSLib.JSAST, JSFull.JSTerm JSASTL
                        , MJSTerm JSASTL, MJSTermLab JSASTL
                        {-, Cfg MJSSig-})
getJS path = do
  raw <- JSLib.parseFile path
  gen <- mkCSLabelGen -- OriginSynthetic
  let fullTree = JSFull.translate raw
  let commTree = JSCommon.translate fullTree
  let labTree  = labelProg gen commTree
  return (raw, fullTree, commTree, labTree{-, makeCfg labTree-})


getPython :: FilePath -> IO (PLib.Module (), PFull.PythonTerm ModuleL
                         , MPythonTerm ModuleL, MPythonTermLab ModuleL
                         {-, Cfg MPythonSig-})
getPython path = do
  contents <- readFile path
  let rawAnnot = fst $ fromRight $ PLib.parseModule contents path
  let raw = fmap (const ()) rawAnnot
  gen <- mkCSLabelGen -- OriginSynthetic
  let fullTree = PFull.translate raw
  let commTree = PCommon.translate fullTree
  let labTree  = labelProg gen commTree
  return (raw, fullTree, commTree, labTree{-, makeCfg labTree-})
#endif

getLua :: FilePath -> IO ( LuaLib.Block LuaLib.SourceRange, LFull.LuaTerm LBlockL
                         , MLuaTerm LBlockL, MLuaTermLab LBlockL
                         {-, Cfg MLuaSig-})
getLua path = do
  raw <- fromRight <$> LuaLib.parseFile path
  gen <- mkCSLabelGen -- OriginSynthetic
  let fullTree = LFull.translate (fmap toSourceSpan raw)
  let commTree = LCommon.translate fullTree
  let labTree  = labelProg gen commTree
  return (raw, fullTree, commTree, labTree {-, makeCfg labTree-})

-- NOTE: duplicated from Cubix.ParsePretty. Find a common home.
toSourceSpan :: LuaLib.SourceRange -> Maybe SourceSpan
toSourceSpan x = Just $ mkSourceSpan (T.unpack (LuaLib.sourceFile from))
                                         (LuaLib.sourceLine from, LuaLib.sourceColumn from)
                                         (LuaLib.sourceLine to,   LuaLib.sourceColumn to)
  where
    from = LuaLib.sourceFrom x
    to   = LuaLib.sourceTo   x


poorManNf :: (Show b) => (a -> b) -> a -> Benchmarkable
poorManNf f = nf (show . f)

poorManNfIO :: (Show b) => (a -> IO b) -> a -> Benchmarkable
poorManNfIO f x = nfIO (show <$> f x)

dummyNodeInfo :: CLib.NodeInfo
dummyNodeInfo = CLib.mkNodeInfoOnlyPos CLib.nopos

--labelProgIO :: Term f l -> IO (TermLab f l)
--labelProgIO x = labelProg <$> mkCSLabelGen <$> pure x

main = do
  gen <- mkCSLabelGen
  defaultMain [
#ifndef ONLY_ONE_LANGUAGE
      env (getC "input-files/c/Foo.c") $
            \ ~(lib, full, ips, lab{-, cfg-}) -> bgroup "c" [
                      bench "showOverheadLib" $ nf id lib
                    , bench "showOverheadMod" $ nf id full
                    , bench "showOverheadIps" $ nf id ips
                    -- , bench "showOverheadCfg" $ nf id cfg
                    , bench "removeAnnot"     $ nf (fmap (const ())) lib
                    , bench "pretty"          $ nf (show . CLib.pretty) lib

                    , bench "transMod"        $ nf (CFull.translate . fmap (const ())) lib
                    , bench "transIps"        $ nf CCommon.translate full
                    , bench "label"           $ nf (labelProg gen) ips
                    -- , bench "cfg"             $ nf makeCfg lab
                    , bench "hoist"           $ nf hoistDeclarations ips
                    -- , bench "testCov"         $ nfIO $ instrumentTestCoverage lab
                    , bench "untransIps"      $ nf CCommon.untranslate ips
                    , bench "untransMod"      $ nf CFull.untranslate full
                    ]

     , env (getJava "input-files/java/Foo.java") $
            \ ~(lib, full, ips, lab{-, cfg-}) -> bgroup "java" [
                      bench "showOverheadLib" $ nf id lib
                    , bench "showOverheadMod" $ nf id full
                    , bench "showOverheadIps" $ nf id ips
                    -- , bench "showOverheadCfg" $ nf id cfg
                    , bench "pretty"          $ nf (show . JLib.prettyPrint) lib

                    , bench "transMod"        $ nf JFull.translate lib
                    , bench "transIps"        $ nf JCommon.translate full
                    , bench "label"           $ nf (labelProg gen) ips
                    -- , bench "cfg"             $ nf makeCfg lab
                    , bench "hoist"           $ nf hoistDeclarations ips
                    -- , bench "testCov"         $ nfIO $ instrumentTestCoverage lab
                    , bench "untransIps"      $ nf JCommon.untranslate ips
                    , bench "untransMod"      $ nf JFull.untranslate full
                    ]

    , env (getJS "input-files/javascript/Foo.js") $
            \ ~(lib, full, ips, lab {-, cfg-}) -> bgroup "js" [
                      bench "showOverheadLib" $ nf id lib
                    , bench "showOverheadMod" $ nf id full
                    , bench "showOverheadIps" $ nf id ips
                    -- , bench "showOverheadCfg" $ nf id cfg
                    , bench "pretty"          $ nf (show . JSLib.prettyPrint) lib

                    , bench "transMod"        $ nf JSFull.translate lib
                    , bench "transIps"        $ nf JSCommon.translate full
                    , bench "label"           $ nf (labelProg gen) ips
                    -- , bench "cfg"             $ nf makeCfg lab
                    , bench "hoist"           $ nf hoistDeclarations ips
                    , bench "tac"             $ nfIO $ toTAC lab
                    -- , bench "testCov"         $ nfIO $ instrumentTestCoverage lab
                    , bench "untransIps"      $ nf JSCommon.untranslate ips
                    , bench "untransMod"      $ nf JSFull.untranslate full
                    ]

    , env (getPython "input-files/python/Foo.py") $
            \ ~(lib, full, ips, lab{-, cfg-}) -> bgroup "python" [
                      bench "showOverheadLib" $ nf id lib
                    , bench "showOverheadMod" $ nf id full
                    , bench "showOverheadIps" $ nf id ips
                    -- , bench "showOverheadCfg" $ nf id cfg
                    , bench "pretty"          $ nf (show . PLib.pretty) lib

                    , bench "transMod"        $ nf PFull.translate lib
                    , bench "transIps"        $ nf PCommon.translate full
                    , bench "label"           $ nf (labelProg gen) ips
                    -- , bench "cfg"             $ nf makeCfg lab
                    , bench "tac"             $ nfIO $ toTAC lab
                    -- , bench "testCov"         $ nfIO $ instrumentTestCoverage lab
                    , bench "untransIps"      $ nf PCommon.untranslate ips
                    , bench "untransMod"      $ nf PFull.untranslate full
                    ]

    ,
#endif
      env (getLua "input-files/lua/Foo.lua") $
            \ ~(lib, full, ips, lab{-, cfg-}) -> bgroup "lua" [
                      bench "showOverheadMod" $ nf id full
                    , bench "showOverheadIps" $ nf id ips
                    -- , bench "showOverheadCfg" $ nf id cfg
                    -- , bench "pretty"          $ nf (show . LuaLib.pprint) lib

                    , bench "transMod"        $ nf LFull.translate (fmap toSourceSpan lib)
                    , bench "transIps"        $ nf LCommon.translate full
                    , bench "label"           $ nf (labelProg gen) ips
                    -- , bench "cfg"             $ nf makeCfg lab
                    , bench "hoist"           $ nf hoistDeclarations ips
                    , bench "tac"             $ nfIO $ toTAC lab
                    -- , bench "testCov"         $ nfIO $ instrumentTestCoverage lab
                    , bench "untransIps"      $ nf LCommon.untranslate ips
                    , bench "untransMod"      $ nf LFull.untranslate (ann Nothing full)
                    ]
                ]

instance NFData SourceSpan where
  rnf = rnf . show
