{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cubix.Transformations.Plumbing.IPT (
    interproceduralPlumbingTransform
  ) where


import Control.Monad ( (=<<), guard )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State ( MonadState, evalStateT )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Maybe ( runMaybeT )
import Control.Monad.Writer ( WriterT(..), tell )

import Data.List ( findIndex, intercalate, nub )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe, fromJust, maybeToList )
import Data.Proxy ( Proxy(..) )
import Data.Set ( Set )
import qualified Data.Set as Set

import System.IO ( hFlush, stdout )

import Control.Lens ( (&), (^.), makeClassy, use, at, (%%~), (%=) )

import Data.Comp.Multi ( Cxt(..), HFix, (:<:), isNode', project', HTraversable, ShowHF, EqHF, stripA, toCxt, E(..), runE )
import Data.Comp.Multi.Derive ( liftSum )
import Data.Comp.Multi.Strategic ( RewriteM, allbuR, promoteR )
import Data.Comp.Multi.Strategy.Classification ( DynCase, dynProj )

import qualified Language.Java.Syntax as JLib

import Cubix.Analysis.Call.Trivial

import Cubix.Language.C.Parametric.Common as C
import Cubix.Language.Java.Parametric.Common as J
import qualified Cubix.Language.Java.Parametric.Full as JFull
import Cubix.Language.Python.Parametric.Common as Py
import Cubix.ParsePretty

import Cubix.Language.Info
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Path
import Cubix.Language.Parametric.ProgInfo
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Syntax

-- Input: List of functions, and consumed/produced rels
----- Initial version will ignore control flow.
---------- I don't feel keen on diving into my CFGs and seeing how good they are.
---------- I think this means I can only handle consumes though....so can only handle
---------- the intermediate part of the threading
-- Algorithm:
---- While: (Unsatisfied functions, new parameter name) in unsatFun queue
----        Pop f from queue
----        Add parameter to f and decls
----        All callsites to check queue
------------- Collate callsites by containing function
----
---- While: Functions in check queue
----        Query user
----        If yes, modify all calls; add function to unsat queue



data IPTState f = IPTState { _ipt_gen       :: LabelGen
                           , _ipt_proginf   :: Map String (ProgInfo f)
                           , _ipt_calls     :: Map String [NodeIdx]
                           , _ipt_funcs     :: Map String [NodeIdx]
                           , _ipt_addedFunc :: Set NodeIdx
                           , _ipt_addedCall :: Set NodeIdx
                           }

makeClassy ''IPTState

type CanIPT f = ( ListF :<: f, ExtractF [] (HFixLab f)
                , Ident :<: f
                , FunctionDef :<: f, PositionalParameter :<: f
                , FunctionCall :<: f, FunctionArgumentList :<: f, PositionalArgument :<: f

                , DynCase (HFixLab f) FunctionDefL
                , InjF f IdentL PositionalArgExpL
--                , IsNode ReceiverArg f

                , HTraversable f
                , ShowHF f
                , EqHF f

                , CfgBuilder f
                , Pretty f
                , TrivialCallAnalysisConstraints f
                , TrivialFunctionAnalysisConstraints f

                , PatchFunctionDecls f
                , PromptParamAttrs f
                )

type HasFunctionDecls f = (FunctionDecl :<: f)

instance HasLabelGen (IPTState f) where
  labelGen = ipt_gen

class (MonadState (IPTState f) m) => MonadIPT f m | m -> f
instance (MonadState (IPTState f) m) => MonadIPT f m
class (MonadIPT f m, MonadIO m) => MonadIPTIO f m | m -> f
instance (MonadIPT f m, MonadIO m) => MonadIPTIO f m

patchFunctionDeclsReal :: (CanIPT f, HasFunctionDecls f, MonadIPT f m) => AddParamAction f -> Project f -> m (Project f)
patchFunctionDeclsReal _ = return

class PatchFunctionDecls f where
  patchFunctionDecls :: (MonadIPT f m) => AddParamAction f -> Project f -> m (Project f)

instance {-# OVERLAPPABLE #-} PatchFunctionDecls f where
  patchFunctionDecls _ = return

#ifndef ONLY_ONE_LANGUAGE
instance {-# OVERLAPPING #-} PatchFunctionDecls MCSig where
  patchFunctionDecls = patchFunctionDeclsReal
instance {-# OVERLAPPING #-} PatchFunctionDecls MJavaSig where
  patchFunctionDecls = patchFunctionDeclsReal
#endif

flush :: (MonadIO m) => m ()
flush = liftIO $ hFlush stdout

lookupUnsafe :: (Ord k) => k -> Map k v -> v
lookupUnsafe k m = fromJust $ Map.lookup k m

class PromptParamAttrs f where
  promptParamAttrs :: (MonadIO m) => m (HFix f ParameterAttrsL)

instance {-# OVERLAPPABLE #-} (EmptyParameterAttrs :<: f, HTraversable f) => PromptParamAttrs f where
  promptParamAttrs = return EmptyParameterAttrs'

#ifndef ONLY_ONE_LANGUAGE
instance PromptParamAttrs MPythonSig where
  promptParamAttrs = return $ PyParamAttrs' Nothing' Nothing'

instance PromptParamAttrs MCSig where
  promptParamAttrs = return $ iCFunParamAttrs (SingletonF' $ iCTypeSpec $ iCIntType iUnitF) NilF' Nothing' NilF'

instance PromptParamAttrs MJavaSig where
  promptParamAttrs = do liftIO $ putStrLn "Provide a Java type (e.g.: \"(PrimType IntT)\"): "
                        str <- liftIO getLine
                        return $ iJavaParamAttrs NilF' (J.translate $ JFull.translateType (read str :: JLib.Type)) 0

#endif

--isReceiverArg :: (CanIPT f) => HFix f l -> Bool
--isReceiverArg = isNode (Proxy :: Proxy ReceiverArg)

data Relation = Relation String [Int]
  deriving ( Eq, Ord, Show )

data AddParamAction f = AddParamAction { _apa_fnName     :: String
                                       , _apa_rel        :: Relation
                                       , _apa_paramName  :: String
                                       , _apa_paramAttrs :: HFix f ParameterAttrsL
                                       , _apa_funcPath   :: NodeIdx
                                       }
  deriving ( Eq, Ord, Show )

makeClassy ''AddParamAction

data CorrectCallsAction f = CorrectCallsAction { cca_fnName     :: String
                                               , cca_rel        :: Relation
                                               , cca_paramName  :: String
                                               , cca_paramAttrs :: HFix f ParameterAttrsL
                                               , cca_callPaths  :: [NodeIdx]
                                               }
  deriving ( Eq, Ord, Show )

getName :: (HTraversable f, Ident :<: f) => HFixLab f IdentL -> String
getName (stripA -> Ident' n) = n

getRange :: (Int, Int) -> [a] -> [a]
getRange (start, fromLast) l = reverse $ drop fromLast $ reverse $ drop start l

contextSize :: Int
contextSize = 2

strDiff :: Int -> String -> String -> (String, String)
strDiff context before after =
  let lines1 = lines before in
  let lines2 = lines after in
  let firstDiff = fromMaybe 0 $ findIndex (uncurry (/=)) $ zip lines1 lines2 in
  let lastDiff  = fromMaybe 0 $ findIndex (uncurry (/=)) $ zip (reverse lines1) (reverse lines2) in
  let extract = (\l -> intercalate "\n" $ getRange (max 0 (firstDiff - context), max 0 (lastDiff - context)) l) in
  (extract lines1, extract lines2)

promptChange :: (HTraversable f, Pretty f, MonadIO m) => FilePath -> HFixLab f i -> HFixLab f j -> m Bool
promptChange fil before after = do
  liftIO $ putStrLn $ "Proposing change to " ++ fil
  let str1 = prettyUnsafe $ stripA before
  let str2 = prettyUnsafe $ stripA after
  let (diff1, diff2) = strDiff contextSize str1 str2
  liftIO $ putStrLn diff1
  liftIO $ putStrLn "\n\n"
  liftIO $ putStrLn diff2
  liftIO $ putStrLn "\n\n"
  liftIO $ putStr $ "Accept changes? (y/N)"
  flush
  ans <- liftIO getLine
  return (ans == "y")

promptChangePrj :: (HTraversable f, Pretty f, MonadIO m) => FilePath -> Project f -> Project f -> m Bool
promptChangePrj fil before after = case (lookupUnsafe fil before, lookupUnsafe fil after) of
                                     (E x, E y) -> promptChange fil x y


addParam' :: (CanIPT f, MonadIPT f m) => AddParamAction f -> RewriteM m (HFixLab f) FunctionDefL
addParam' apa t@(project' -> Just (FunctionDef a n pars b)) =
    if (apa ^. apa_fnName) /= getName n then
      return t
    else do
      newPar <- annotateLabel $ PositionalParameter' (apa ^. apa_paramAttrs) (Ident' $ apa ^. apa_paramName)
      annotateLabelOuter $ FunctionDef' (Hole a) (Hole n) (insertFHole $ extractF pars ++ [newPar]) (Hole b)

addParam :: forall f m. (CanIPT f, MonadIPTIO f m) => AddParamAction f -> Project f -> m (Project f, [CorrectCallsAction f])
addParam apa@(AddParamAction fn rel paramNm attrs ni@(NodeIdx fil lab)) prj = do
    alreadyAdded <- use ipt_addedFunc
    if Set.member ni alreadyAdded then
      return (prj, [])
     else do
      prj' <- prj & at fil %%~ addParamToFile
      ipt_addedFunc %= Set.insert ni
      shouldDo <- promptChangePrj fil prj prj'
      ccas <- getCCAs
      if shouldDo then return (prj', ccas) else return (prj, [])
  where
    addParamToFile :: (Maybe (E (HFixLab f))) -> m (Maybe (E (HFixLab f)))
    addParamToFile Nothing = error "Expected file fil not found in Project map (apa)"
    addParamToFile (Just (E t)) = do path <- labToPath lab <$> lookupUnsafe fil <$> use ipt_proginf
                                     -- I have no clue why this type annotation is needed and I'm on a deadline
                                     (Just <$> E <$> rewriteAtPathM (promoteR $ addParam' apa) t path) :: (m (Maybe (E (HFixLab f))))

    getCCAs :: m [CorrectCallsAction f]
    getCCAs = do
      callMap <- use ipt_calls
      let calls = Map.findWithDefault [] fn callMap
      return $ coalesce $ map (CorrectCallsAction fn rel paramNm attrs) (map (\x -> [x]) calls)

getFnFromFunDef :: (CanIPT f) => HFixLab f l -> Maybe String
getFnFromFunDef (dynProj -> Just (stripA -> FunctionDef' _ (Ident' n) _ _)) = Just n
getFnFromFunDef _                                                           = Nothing

-- Left off: This is failing
getParentFn :: forall f l. (CanIPT f) => Path -> HFixLab f l -> Maybe String
getParentFn path t = do (E x) <- searchParent (isNode' (Proxy :: Proxy FunctionDef)) t path
                        getFnFromFunDef x


-- TODO: Handle multiple things properly
-- TODO: Check input relation
-- FIXME: I've let some local code ugliness creep in. Much duplication between the cca and apa functions;
-- justified because they're hacks anyway
correctCalls :: forall f m. (CanIPT f, MonadIPTIO f m) => CorrectCallsAction f -> Project f -> m (Project f, [AddParamAction f])
correctCalls cca@(CorrectCallsAction nm rel parNm attrs [ni@(NodeIdx fil lab)]) prj = do
    alreadyAdded <- use ipt_addedCall
    if Set.member ni alreadyAdded then
      return (prj, [])
     else do
      prj' <- prj & at fil %%~ addArgToFile
      ipt_addedCall %= Set.insert ni
      shouldDo <- promptChangePrj fil prj prj'
      if not shouldDo then return (prj, [])
       else do
        progInfs <- use ipt_proginf
        let path = labToPath lab $ lookupUnsafe fil progInfs
        funcMap <- use ipt_funcs
        let apa = do E t <- prj ^. at fil
                     newFnName <- getParentFn path t
                     let funcs = Map.findWithDefault [] newFnName funcMap
                     return $ map (AddParamAction newFnName rel parNm attrs) funcs
        return (prj', fromMaybe [] apa)
  where
    addArgToFile :: Maybe (E (HFixLab f)) -> m (Maybe (E (HFixLab f)))
    addArgToFile Nothing = error "Expected file fil not found in Project map"
    addArgToFile (Just (E t)) = do path <- labToPath lab <$> lookupUnsafe fil <$> use ipt_proginf
                                   Just <$> E <$> rewriteAtPathM addArg t path

    -- TODO: Check for non-positional arguments
    -- FIXME: What to do about clobbered labels? This is where mutable terms would help so much.
    addArg :: HFixLab f l -> m (HFixLab f l)
    addArg t@(project' -> Just (FunctionCall a f (project' -> Just (FunctionArgumentList args)))) = do
        newArg <- annotateLabel $ (PositionalArgument' (injF $ Ident' parNm))
        annotateLabelOuter $ FunctionCall' (Hole a) (Hole f) (FunctionArgumentList' $ insertFHole $ extractF args ++ [newArg])
    addArg t@(project' -> Just (FunctionCall _ _ _)) = do path <- labToPath lab <$> lookupUnsafe fil <$> use ipt_proginf
                                                          liftIO $ putStrLn $ "Handle manually: " ++ show (fil, path)
                                                          return t
    addArg _ = error "Something other than a function call found at a change site"

-- TODO: Merge like trees
coalesce :: [CorrectCallsAction f] -> [CorrectCallsAction f]
coalesce = id

printQueueSize :: (MonadIO m) => [AddParamAction f] -> [CorrectCallsAction f] -> m ()
printQueueSize apas ccas = do liftIO $ putStrLn ("APA queue has size " ++ show (length apas))
                              liftIO $ putStrLn ("CCA queue has size " ++ show (length ccas))

mainLoop :: (CanIPT f, MonadIPTIO f m) => [AddParamAction f] -> [CorrectCallsAction f] -> Project f -> m (Project f)
mainLoop []       []       prj = return prj
mainLoop apas     (c:ccas) prj = do (prj', apas') <- correctCalls c prj
                                    printQueueSize (apas++apas') ccas
                                    mainLoop (apas++apas') ccas prj'
mainLoop (a:apas) []       prj = do (prj', ccas) <- addParam a prj
                                    let apasReduced = nub apas
                                    printQueueSize apasReduced ccas
                                    mainLoop apasReduced ccas prj'

interproceduralPlumbingTransform :: (CanIPT f, MonadIO m) => LabelGen -> Project f -> m (Project f)
interproceduralPlumbingTransform gen prj = do
    liftIO $ putStr "Give a starting function: "
    flush
    fn <- liftIO getLine
    liftIO $ putStr "Give a parameter name: "
    flush
    paramNm <- liftIO getLine
    paramAttrs <- promptParamAttrs
    let progInfo = Map.mapWithKey (\k (E t) -> makeProgInfo t) prj
    let callMap = callAnalysis prj
    let funcMap = functionAnalysis prj
    let startState = IPTState gen progInfo callMap funcMap Set.empty Set.empty
    evalStateT (mainLoop (map (AddParamAction fn (Relation "" []) paramNm paramAttrs) (lookupUnsafe fn funcMap)) [] prj) startState
