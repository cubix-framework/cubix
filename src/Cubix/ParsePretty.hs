{-# LANGUAGE CPP #-}

module Cubix.ParsePretty (
    ParseFile(..)
  , Pretty(..)

  , parseLua
  , prettyLua

  , RootSort

#ifndef ONLY_ONE_LANGUAGE
  , parseC
  , parseJava
  , parseJavaScript
  , parsePython
  , parseSolidity

  , prettyC
  , prettyJava
  , prettyJavaScript
  , prettyPython
#endif
  ) where


import Control.Monad ( liftM, (>=>) )
import Control.Monad.Identity ( runIdentity )

import Data.Comp.Multi ( Term, stripA, ann )
import Data.Comp.Multi.Strategic ( RewriteM, allbuR, promoteR )
import Data.Comp.Multi.Strategy.Classification ( DynCase, fromDynProj )

import qualified Data.Text.IO as Text

import qualified Language.C as C
import qualified Language.Java.Pretty as Java
import qualified Language.JavaScript.Parser as JS
import qualified Language.JavaScript.Pretty.Printer.Extended as JS
import qualified Language.Lua.Annotated as Lua
import qualified Language.Lua.Annotated.Lexer as Lua
import qualified Language.Lua.PrettyPrinter as Lua
import qualified Language.Lua.Annotated.Simplify as Lua
import qualified Language.Python.Common as Python
import qualified Language.Python.Version3.Parser as Python

import           Solidity ( Solidity )
import qualified Solidity as Solidity

import Cubix.Language.Info

import           Cubix.Language.C.Parametric.Common          as CCommon
import qualified Cubix.Language.C.Parametric.Full            as CFull
import qualified Cubix.Language.C.Parse                      as CParse
import qualified Cubix.Language.Java.Parse                   as JParse
import           Cubix.Language.Java.Parametric.Common       as JCommon
import qualified Cubix.Language.Java.Parametric.Full         as JFull
import           Cubix.Language.JavaScript.Parametric.Common as JSCommon
import qualified Cubix.Language.JavaScript.Parametric.Full   as JSFull
import           Cubix.Language.Lua.Parametric.Common        as LCommon
import qualified Cubix.Language.Lua.Parametric.Full          as LFull
import           Cubix.Language.Python.Parametric.Common     as PCommon
import qualified Cubix.Language.Python.Parametric.Full       as PFull
import           Cubix.Language.Solidity.Parametric.Common   as SCommon
import qualified Cubix.Language.Solidity.Parametric.Full     as SFull

import qualified Data.Text as T (unpack)

---------------------------------------------------------------------------------------------

-------------------------------------------------------------------
--------------------- General interfaces --------------------------
-------------------------------------------------------------------

type family RootSort (fs :: [(* -> *) -> * -> *])

class ParseFile fs where
  -- | Parses a file with the appropriate parser for the language with signature @fs@.
  --
  -- Recommended to use with the @TypeApplications@ extension,
  -- e.g.: @parseFile \@MCSig "my_file.c"@.
  parseFile :: FilePath -> IO (Maybe (Term fs (RootSort fs)))

class Pretty fs where
  -- | Pretty-prints a term, using the appropriate pretty-printer for the language with
  -- signature @fs@.
  pretty :: Term fs (RootSort fs) -> String

  -- FIXME: The only reason this is needed is because Project forgets
  -- what sort its contents are
  prettyUnsafe :: Term fs l -> String
  default prettyUnsafe :: (DynCase (Term fs) (RootSort fs)) => Term fs l -> String
  prettyUnsafe = pretty . fromDynProj


-------------------------------------------------------------------
------------------------------ Lua --------------------------------
-------------------------------------------------------------------

-- | NOTE: This reflects the half-finished transition of Lua to annotated terms
parseLua :: FilePath -> IO (Maybe (MLuaTerm LBlockL))
parseLua path = do
    res <- Lua.parseFile path
    case res of
     Left errors -> print errors >> return Nothing
     Right tree  -> return $ Just $ LCommon.translate $ {-stripA $-} LFull.translate $ fmap toSourceSpan tree
  where
    toSourceSpan :: Lua.SourceRange -> Maybe SourceSpan
    toSourceSpan x = Just $ mkSourceSpan (T.unpack (Lua.sourceFile from))
                                         (Lua.sourceLine from, Lua.sourceColumn from)
                                         (Lua.sourceLine to,   Lua.sourceColumn to)
      where
        from = Lua.sourceFrom x
        to   = Lua.sourceTo   x

prettyLua :: MLuaTerm LBlockL -> String
prettyLua = show . Lua.pprint . Lua.sBlock . LFull.untranslate . ann Nothing . LCommon.untranslate

type instance RootSort MLuaSig = LBlockL
instance ParseFile MLuaSig where parseFile = parseLua
instance Pretty MLuaSig where pretty = prettyLua

#ifndef ONLY_ONE_LANGUAGE


-------------------------------------------------------------------
------------------------------- C ---------------------------------
-------------------------------------------------------------------

parseC :: FilePath -> IO (Maybe (MCTerm CTranslationUnitL))
parseC path = do
  res <- CParse.parse path
  case res of
    Left errors -> print errors >> return Nothing
    Right tree -> return $ Just $ CCommon.translate $ CFull.translate $ fmap (const ()) tree


dummyNodeInfo :: C.NodeInfo
dummyNodeInfo = C.mkNodeInfoOnlyPos C.nopos


prettyC :: MCTerm CTranslationUnitL -> String
prettyC = show . C.pretty . fmap (const dummyNodeInfo) . CFull.untranslate . CCommon.untranslate

type instance RootSort MCSig = CTranslationUnitL
instance ParseFile MCSig where parseFile = parseC
instance Pretty MCSig where pretty = prettyC


-------------------------------------------------------------------
----------------------------- Java --------------------------------
-------------------------------------------------------------------

parseJava :: FilePath -> IO (Maybe (MJavaTerm CompilationUnitL))
parseJava path = do
  res <- JParse.parse path
  case res of
    Left  x -> return Nothing
    Right p -> return $ Just $ JCommon.translate $ JFull.translate p


prettyJava :: MJavaTerm CompilationUnitL -> String
prettyJava = Java.prettyPrint . JFull.untranslate . JCommon.untranslate


type instance RootSort MJavaSig = CompilationUnitL
instance ParseFile MJavaSig where parseFile = parseJava
instance Pretty MJavaSig where pretty = prettyJava


-------------------------------------------------------------------
--------------------------- JavaScript ----------------------------
-------------------------------------------------------------------

parseJavaScript :: FilePath -> IO (Maybe (MJSTerm JSASTL))
parseJavaScript path = liftM (Just . JSCommon.translate . normalizeJS . JSFull.translate) $ JS.parseFile path
  where
    normalizeJS :: JSFull.JSTerm JSASTL -> JSFull.JSTerm JSASTL
    normalizeJS t = runIdentity $ allbuR (promoteR normalizeSemi >=> promoteR normalizeAnno) t

    normalizeSemi :: (Monad m) => RewriteM m JSFull.JSTerm JSSemiL
    normalizeSemi _ = return (iJSSemi iJSNoAnnot)

    normalizeAnno :: (Monad m) => RewriteM m JSFull.JSTerm JSAnnotL
    normalizeAnno _ = return iJSNoAnnot

prettyJavaScript :: MJSTerm JSASTL -> String
prettyJavaScript =  JS.prettyPrint . JSFull.untranslate . JSCommon.untranslate


type instance RootSort MJSSig = JSASTL
instance ParseFile MJSSig where parseFile = parseJavaScript
instance Pretty MJSSig where pretty = prettyJavaScript


-------------------------------------------------------------------
---------------------------- Python -------------------------------
-------------------------------------------------------------------

parsePython :: FilePath -> IO (Maybe (MPythonTerm PCommon.ModuleL))
parsePython path = do
  contents <- readFile path
  let res = Python.parseModule contents path
  case res of
    Left  e     -> print e >> return Nothing
    Right (m, _) -> return $ Just $ PCommon.translate $ PFull.translate $ fmap (const ()) m

prettyPython :: MPythonTerm PCommon.ModuleL -> String
prettyPython = show . Python.pretty . PFull.untranslate . PCommon.untranslate


type instance RootSort MPythonSig = PCommon.ModuleL
instance ParseFile MPythonSig where parseFile = parsePython
instance Pretty MPythonSig where pretty = prettyPython

-------------------------------------------------------------------
--------------------------- Solidity ------------------------------
-------------------------------------------------------------------

parseSolidity :: FilePath -> IO (Maybe (MSolidityTerm SolidityL))
parseSolidity path = do
  contents <- Text.readFile path
  let res = Solidity.parseFile path contents
  case res of
    Left  e -> print e >> return Nothing
    Right s -> return $ Just $ SCommon.translate $ SFull.translate s

type instance RootSort MSoliditySig = SCommon.SolidityL
instance ParseFile MSoliditySig where parseFile = parseSolidity

-- 2023.11.02: Initial Solidity library we're using has no pretty-printer.

#endif
