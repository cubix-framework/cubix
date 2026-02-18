-- | Example 2: Sort functions in a Sui Move module by name.
--
-- Uses allbuR with a sort-specific rewrite on [ModuleBodyInternal1L] lists
-- to reorder function definitions alphabetically within each module.
--
-- This example is Sui-Move-specific because it operates on
-- ModuleBodyInternal1 nodes, which are the top-level items in a Move module.
module Examples.SortFunctions (run) where

import Control.Monad (MonadPlus(..))
import Control.Monad.Identity (runIdentity)

import Data.List (sortBy)
import Data.Ord (comparing)

import Data.Comp.Multi (project)
import Data.Comp.Multi.Strategic (RewriteM, allbuR, promoteR)

import Cubix.Language.Parametric.InjF (InjF(..))
import Cubix.Language.Parametric.Syntax
  ( FunctionDefL, IdentL
  , pattern FunctionDef', pattern Ident'
  , InsertF(..), ExtractF(..)
  )

import Cubix.Language.SuiMove.IPS.Types (MSuiMoveTerm)
import Cubix.Language.SuiMove.Modularized
  ( ModuleBodyInternal1(..), ModuleBodyInternal1L
  , HiddenFunctionItem(..)
  )

import Examples.Shared

-- | Get the function name from a module body item, if it is a function definition.
getFunctionName :: MSuiMoveTerm ModuleBodyInternal1L -> Maybe String
getFunctionName t
  | Just (ModuleBodyInternal1FunctionItem hfi) <- project t
  , Just (HiddenFunctionItemFunctionDefinition fd) <- project hfi
  = case projF fd :: Maybe (MSuiMoveTerm FunctionDefL) of
      Just (FunctionDef' _ nameT _ _) -> case projF nameT :: Maybe (MSuiMoveTerm IdentL) of
        Just (Ident' s) -> Just s
        Nothing         -> Nothing
      Nothing -> Nothing
getFunctionName _ = Nothing

-- | Rewrite that sorts function definitions alphabetically within a list
-- of module body items. Non-function items keep their relative position.
sortFunctionsRewrite :: (MonadPlus m)
  => RewriteM m (MSuiMoveTerm) [ModuleBodyInternal1L]
sortFunctionsRewrite items = do
  let bodyItems = extractF items
      (nonFuncs, rest) = span (\t -> getFunctionName t == Nothing) bodyItems
      funcs = filter (\t -> getFunctionName t /= Nothing) rest
      nonFuncsRest = filter (\t -> getFunctionName t == Nothing) rest
  if null funcs
    then mzero  -- No functions to sort, skip
    else pure $ insertF $ nonFuncs ++ sortBy (comparing getFunctionName) funcs ++ nonFuncsRest

-- | Apply sort transformation to the whole tree.
sortFunctions :: MSuiMoveTerm l -> MSuiMoveTerm l
sortFunctions = runIdentity . allbuR (promoteR sortFunctionsRewrite)

run :: [String] -> IO ()
run [file] = do
  term <- parseSuiMoveOrDie file
  let sorted = sortFunctions term
  putStrLn $ prettySuiMove sorted
run _ = putStrLn "Usage: sui-move-examples sort-functions <file.move>"
