{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Count occurrences of node pairs (parent-child type combinations) in a tree.
-- Node pairs are tracked at the constructor level, not the fragment level.
-- For example, a fragment @Stat@ with constructors @Assign@ and @Do@ will
-- generate pairs like @(Assign, Var)@ and @(Do, BlockIsBlock)@, not @(Stat, Var)@.
module Cubix.Analysis.NodePairs
  ( NodePair(..)
  , countNodePairs
  , countNodePairsWithPossible
  , countNodePairsInFolder
  , possibleNodePairs
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (filterM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)

import Control.DeepSeq (NFData(..), force)
import Control.Parallel.Strategies (parMap, rdeepseq)

import Language.Haskell.TH

import Data.Comp.Multi (All, ConstrNameHF(..), HFoldable(..), HFunctor, Term, caseCxt, unTerm)

import Cubix.Language.Parametric.Derive (sumToNames)

-- #######################################
-- Core Types
-- #######################################

-- | A node pair represents a parent-child relationship by constructor name.
-- If a node with constructor A has a child with constructor B, that's an (A, B) pair.
data NodePair = NodePair
  { parentType :: String
  , childType  :: String
  } deriving (Eq, Ord, Show)

instance NFData NodePair where
  rnf (NodePair p c) = rnf p `seq` rnf c

-- #######################################
-- File System Utilities
-- #######################################

-- | Get all files with a given extension from a directory (recursive)
getFilesWithExtension :: String -> FilePath -> IO [FilePath]
getFilesWithExtension ext dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  files <- filterM doesFileExist fullPaths
  dirs <- filterM doesDirectoryExist fullPaths
  let matchingFiles = filter (\f -> takeExtension f == ext) files
  subFiles <- concat <$> mapM (getFilesWithExtension ext) dirs
  return (matchingFiles ++ subFiles)

-- #######################################
-- Runtime Node Pair Analysis
-- #######################################

-- | Wrapper functor base names that should be treated as pass-through.
-- These don't create node pairs themselves; their children are treated
-- as direct children of the grandparent.
-- Matched by base name (after the last '.') since constructor names are now
-- fully qualified.
passThroughBaseNames :: Set String
passThroughBaseNames = Set.fromList
  [ "ListF", "ConsF", "NilF"
  , "MaybeF", "JustF", "NothingF"
  , "PairF", "TripleF"
  , "EitherF", "LeftF", "RightF"
  ]

-- | Check if a (possibly qualified) type name is a pass-through functor.
-- Extracts the base name (after last '.') and checks against known pass-throughs.
isPassThrough :: String -> Bool
isPassThrough name = Set.member (baseName name) passThroughBaseNames
  where
    baseName s = case break (== '.') s of
      (_, [])    -> s          -- No dots, already a base name
      (_, _:rest) -> baseName rest  -- Skip past the dot, recurse

-- | Count all node pairs in a term.
-- Returns a map from each node pair to its count in the tree.
-- Wrapper functors (ListF, MaybeF, PairF, etc.) are treated as pass-through:
-- their children are counted as direct children of the grandparent.
-- Parent names are constructor names (e.g., "Assign", "Do"), not fragment names.
countNodePairs :: forall fs l.
  ( All HFunctor fs
  , All HFoldable fs
  , All ConstrNameHF fs
  ) => Term fs l -> Map NodePair Int
countNodePairs = countWithParent Nothing
  where
    countWithParent :: Maybe String -> forall i. Term fs i -> Map NodePair Int
    countWithParent mparent t =
      let nodeName = caseCxt @ConstrNameHF constrNameHF (unTerm t)
      in if isPassThrough nodeName
         then -- Pass-through: don't create pair, pass parent through to children
              hfoldMap (countWithParent mparent) (unTerm t)
         else -- Regular node: create pair with parent (if any) and recurse with self as parent
              let pairMap = case mparent of
                    Nothing -> Map.empty
                    Just p  -> Map.singleton (NodePair p nodeName) 1
                  childMap = hfoldMap (countWithParent (Just nodeName)) (unTerm t)
              in Map.unionWith (+) pairMap childMap

-- | Like 'countNodePairs', but starts with all possible pairs mapped to 0.
-- Pairs that occur in the term get their actual counts; pairs that don't occur
-- remain at 0.
countNodePairsWithPossible :: forall fs l.
  ( All HFunctor fs
  , All HFoldable fs
  , All ConstrNameHF fs
  ) => Set NodePair -> Term fs l -> Map NodePair Int
countNodePairsWithPossible possiblePairs term =
  Map.unionWith (+) zeroCounts (countNodePairs term)
  where
    zeroCounts = Map.fromSet (const 0) possiblePairs

-- | Count node pairs across all files in a folder matching a given extension.
--
-- Takes:
--   * A set of all possible node pairs (to initialize counts to 0)
--   * A file extension to filter by (e.g., ".lua", ".move")
--   * A parse function that returns @Maybe (Term fs l)@
--   * A folder path
--
-- Returns a map with all possible pairs initialized to 0, then incremented
-- based on actual occurrences in the parsed files.
countNodePairsInFolder :: forall fs l.
  ( All HFunctor fs
  , All HFoldable fs
  , All ConstrNameHF fs
  ) => Set NodePair
    -> String
    -> (FilePath -> IO (Maybe (Term fs l)))
    -> FilePath
    -> IO (Map NodePair Int)
countNodePairsInFolder possiblePairs extension parseFile folder = do
  files <- getFilesWithExtension extension folder
  -- Parse all files and count node pairs per file (IO phase)
  perFileCounts <- mapM countFile files
  -- Evaluate per-file maps in parallel, one spark per file
  let sparkedCounts = parMap rdeepseq id perFileCounts
  -- Merge all maps
  evaluate $ force $ Map.unionsWith (+) (zeroCounts : sparkedCounts)
  where
    zeroCounts = Map.fromSet (const 0) possiblePairs

    countFile :: FilePath -> IO (Map NodePair Int)
    countFile filePath = do
      result <- try (parseFile filePath) :: IO (Either SomeException (Maybe (Term fs l)))
      case result of
        Left _ -> return Map.empty
        Right Nothing -> return Map.empty
        Right (Just term) -> return $ countNodePairs term

-- #######################################
-- Template Haskell: Static Node Pair Analysis
-- #######################################

-- | Extract all possible node pairs from a signature type synonym.
-- This uses Template Haskell to statically analyze the type definitions.
--
-- Usage: @$(possibleNodePairs ''MLuaSig)@
--
-- The signature type (e.g., @MLuaSig@) is a promoted type-level list like
-- @'[Fragment1, Fragment2, ...]@, created by @makeSumType@.
-- This function reifies the type synonym to extract the fragment names.
--
-- Returns a @Set NodePair@ containing pairs where:
--
--   * 'parentType' is the constructor name (e.g., "Assign", "Do", "Block")
--   * 'childType' is resolved to actual constructor names, unwrapping List/Maybe/etc.
--
-- For example, if Block has a child of type @[StatementL]@, and Statement produces
-- @StatementL@, then this generates a pair (Block, Statement).
-- Wrapper types (List, Maybe, Either, Pair) are treated as pass-through.
-- Sort labels are fully qualified to avoid collisions between identically-named
-- sorts from different modules (e.g., Lua's @BlockL@ vs parametric @BlockL@).
possibleNodePairs :: Name -> Q Exp
possibleNodePairs sigName = do
  fragNames <- sumToNames sigName
  -- Build a map from fully-qualified sort labels to constructor names that produce them
  sortToConstrs <- buildSortToConstrMap fragNames
  -- Get all pairs, resolving sorts to actual constructor names
  allPairs <- concat <$> mapM (getFragmentPairs sortToConstrs) fragNames
  let pairExps = map mkPairExp allPairs
  [| Set.fromList $(listE pairExps) |]

-- ##############################
-- Sort-to-Constructor Mapping
-- ##############################

-- | Build a map from fully-qualified sort labels (e.g., "Cubix...VarDecl.BlockL")
-- to constructor names that produce them.
-- Uses fully-qualified sort names to avoid collisions between sorts with the same
-- base name from different modules.
buildSortToConstrMap :: [Name] -> Q (Map String [String])
buildSortToConstrMap fragNames = do
  pairs <- concat <$> mapM getConstrSorts fragNames
  return $ Map.fromListWith (++) [(sort, [constr]) | (constr, sort) <- pairs]

-- | Get all (constructorName, qualifiedSortLabel) pairs for a fragment.
-- Each constructor of the fragment gets its own entry.
getConstrSorts :: Name -> Q [(String, String)]
getConstrSorts fragName = do
  info <- reify fragName
  case info of
    TyConI (DataD _ _ _ _ constrs _) ->
      return $ concatMap getConstrSort constrs
    TyConI (NewtypeD _ _ _ _ constr _) ->
      return $ getConstrSort constr
    _ -> return []

-- | Get the (constructorName, qualifiedSort) pair from a single constructor
getConstrSort :: Con -> [(String, String)]
getConstrSort con =
  let (conName, _, mRetSort) = normalCon' con
      conStr = qualifiedName conName
  in case mRetSort of
       Just sort -> [(conStr, sort)]
       Nothing -> []

-- ##############################
-- Fragment Analysis
-- ##############################

-- | Get all (parent, child) pairs for a single fragment, resolving sorts to constructors.
-- The parent name is the constructor name, not the fragment name.
getFragmentPairs :: Map String [String] -> Name -> Q [(String, String)]
getFragmentPairs sortToConstrs fragName = do
  info <- reify fragName
  case info of
    TyConI (DataD _ _ _ _ constrs _) ->
      concat <$> mapM (getConstrPairs sortToConstrs) constrs
    TyConI (NewtypeD _ _ _ _ constr _) ->
      getConstrPairs sortToConstrs constr
    _ -> do
      reportWarning $ "possibleNodePairs: " ++ show fragName ++ " is not a data type"
      return []

-- | Get pairs for a single constructor.
-- The parent name is the constructor name (e.g., "Assign"), not the fragment name (e.g., "Stat").
getConstrPairs :: Map String [String] -> Con -> Q [(String, String)]
getConstrPairs sortToConstrs con = do
  let (conName, argTypes, _) = normalCon' con
      parentName = qualifiedName conName
  childSorts <- concat <$> mapM extractChildSorts argTypes
  -- Resolve each sort to its possible constructor types
  let resolveSort sort = Map.findWithDefault [] sort sortToConstrs
      childConstrs = concatMap resolveSort childSorts
  return [(parentName, child) | child <- childConstrs]

-- ##############################
-- Type Extraction
-- ##############################

-- | Extract child sort names from a type, unwrapping List/Maybe/Either/Pair.
-- Returns fully-qualified sort names to avoid collisions.
extractChildSorts :: Type -> Q [String]
extractChildSorts ty = return $ go ty
  where
    go (AppT (VarT _) sortTy) = unwrapSort sortTy
    go (AppT t1 _) = go t1
    go _ = []

    -- Unwrap wrapper types to get the inner sort(s)
    -- Uses fully-qualified names via 'qualifiedName'
    unwrapSort :: Type -> [String]
    unwrapSort (ConT n) = [qualifiedName n]
    unwrapSort (VarT _) = []  -- Polymorphic sort - skip for now
    -- List types: [SortL] or [] SortL
    unwrapSort (AppT ListT inner) = unwrapSort inner
    unwrapSort (AppT (ConT n) inner)
      | nameBase n == "[]" = unwrapSort inner
      | nameBase n == "Maybe" = unwrapSort inner
      | nameBase n == "Either" = unwrapSort inner  -- Will get both via AppT
      | otherwise = [qualifiedName n]  -- Some other type constructor
    -- Tuple/pair types
    unwrapSort (AppT t1 t2) = unwrapSort t1 ++ unwrapSort t2
    unwrapSort _ = []

-- ##############################
-- TH Helpers
-- ##############################

-- | Get a fully-qualified name from a TH Name.
-- Falls back to 'nameBase' if the name has no module qualifier
-- (e.g., for locally-generated names).
qualifiedName :: Name -> String
qualifiedName n = case nameModule n of
  Just m  -> m ++ "." ++ nameBase n
  Nothing -> nameBase n

-- | Helper: normalize a constructor to get name, arg types, and return sort name.
-- For GADT constructors, the return sort is extracted from the explicit return type.
-- For ForallC with equality constraints (from createSortInclusionType),
-- we extract the return sort from the equality constraint (e.g., @i ~ NameL@).
-- Sort names are fully qualified.
normalCon' :: Con -> (Name, [Type], Maybe String)
normalCon' (NormalC constr args) = (constr, map snd args, Nothing)
normalCon' (RecC constr args) = (constr, map (\(_,_,t) -> t) args, Nothing)
normalCon' (InfixC a constr b) = (constr, [snd a, snd b], Nothing)
normalCon' (ForallC _ ctx constr) =
  let (cn, args, mRet) = normalCon' constr
      retSort = case mRet of
        Just _  -> mRet
        Nothing -> extractSortFromEqConstraints ctx
  in (cn, args, retSort)
normalCon' (GadtC (constr:_) args typ) = (constr, map snd args, extractSortFromReturnType typ)
normalCon' (GadtC [] _ _) = error "Empty GADT constructor list"
normalCon' (RecGadtC (constr:_) args typ) = (constr, map (\(_,_,t) -> t) args, extractSortFromReturnType typ)
normalCon' (RecGadtC [] _ _) = error "Empty RecGADT constructor list"

-- | Extract sort label from a GADT return type like @Foo e SortL@
-- Returns a fully-qualified sort name.
extractSortFromReturnType :: Type -> Maybe String
extractSortFromReturnType ty = go ty
  where
    go (AppT _ (ConT n)) = Just (qualifiedName n)
    go (AppT t _) = go t
    go _ = Nothing

-- | Extract a return sort from equality constraints in a ForallC.
-- deriveMultiComp and createSortInclusionType generate constructors like:
--   @ForallC [] [i ~ SortL] (NormalC ConName [...])@
-- GHC represents @~@ as either 'EqualityT' or @ConT ''(~)@ depending on version.
-- We handle both forms.
-- Returns a fully-qualified sort name.
extractSortFromEqConstraints :: [Type] -> Maybe String
extractSortFromEqConstraints [] = Nothing
extractSortFromEqConstraints (c : rest) = case getEqSort c of
  Just s  -> Just s
  Nothing -> extractSortFromEqConstraints rest
  where
    getEqSort (AppT (AppT EqualityT (VarT _)) (ConT n)) = Just (qualifiedName n)
    getEqSort (AppT (AppT (ConT eq)  (VarT _)) (ConT n))
      | nameBase eq == "~" = Just (qualifiedName n)
    getEqSort _ = Nothing

-- | Make a NodePair expression
mkPairExp :: (String, String) -> Q Exp
mkPairExp (parent, child) =
  [| NodePair $(litE (stringL parent)) $(litE (stringL child)) |]
