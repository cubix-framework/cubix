-- | Example 3: Count function calls across a project directory.
--
-- Uses crushtdT to collect all FunctionCall nodes and extract
-- the called function name. Prints results in a formatted table.
--
-- The core query uses crushtdT with promoteTF, a standard compstrat pattern
-- for generic queries.
--
-- Sui-Move-specific: handles ModuleAccess paths (e.g. module::function).
module Examples.CountCalls (run) where

import Control.Monad.Identity (runIdentity)

import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing, Down(..))

import Data.Comp.Multi (project)
import Data.Comp.Multi.Strategic (TranslateM, crushtdT, promoteTF, addFail)

import Cubix.Language.Parametric.InjF (InjF(..))
import Cubix.Language.Parametric.Syntax
  ( FunctionCallL, FunctionExpL, IdentL, pattern FunctionCall', pattern Ident'
  )

import Cubix.Language.SuiMove.IPS.Types
  ( MSuiMoveTerm
  , SuiMoveFunctionExp(..)
  )
import Cubix.Language.SuiMove.Modularized
  ( SourceFileL, ModuleAccessL
  , ModuleAccess(..), HiddenModuleIdentifier(..)
  )

import Examples.Shared

-- | Count function calls in a Sui Move term, extracting called names from
-- the SuiMoveFunctionExp and ModuleAccess nodes.
countCallsSuiMove :: MSuiMoveTerm SourceFileL -> Map String Int
countCallsSuiMove = runIdentity . crushtdT (promoteTF $ addFail getCalls)
  where
    getCalls :: (Monad m)
      => TranslateM m MSuiMoveTerm FunctionCallL (Map String Int)
    getCalls (FunctionCall' _ fexp _) =
      pure $ Map.singleton (extractCallName fexp) 1
    getCalls _ = pure Map.empty

    extractCallName :: MSuiMoveTerm FunctionExpL -> String
    extractCallName t
      | Just (SimpleFunctionExp _ ma) <- project t = extractMA ma
      | Just (MacroFunctionExp ma) <- project t    = extractMA ma ++ "!"
      | otherwise = "<unknown>"

    extractMA :: MSuiMoveTerm ModuleAccessL -> String
    extractMA t
      -- Simple identifier: foo(...)
      | Just (ModuleAccess4 ident _) <- project t
      , Just (Ident' s) <- projF ident :: Maybe (MSuiMoveTerm IdentL)
      = s
      -- Module::function: mod::foo(...)
      | Just (ModuleAccess5 modIdent _ _ funIdent) <- project t
      , Just (HiddenModuleIdentifier mi) <- project modIdent
      , Just (Ident' s_mod) <- projF mi :: Maybe (MSuiMoveTerm IdentL)
      , Just (Ident' s_fun) <- projF funIdent :: Maybe (MSuiMoveTerm IdentL)
      = s_mod ++ "::" ++ s_fun
      | otherwise = "<complex>"

run :: [String] -> IO ()
run [dir] = do
  files <- parseDirectory dir
  if null files
    then putStrLn $ "No .move files found in " ++ dir
    else do
      let allCounts = Map.unionsWith (+) $ map (countCallsSuiMove . snd) files
          sorted = sortBy (comparing (Down . snd)) (Map.toList allCounts)
          maxNameLen = if null sorted then 8 else maximum $ 8 : map (length . fst) sorted
      putStrLn $ "Function calls across " ++ show (length files) ++ " file(s):\n"
      putStrLn $ padRight maxNameLen "Function" ++ "  Calls"
      putStrLn $ replicate (maxNameLen + 8) '-'
      mapM_ (\(name, count) ->
        putStrLn $ padRight maxNameLen name ++ "  " ++ show count
        ) sorted
      putStrLn $ replicate (maxNameLen + 8) '-'
      putStrLn $ padRight maxNameLen "Total" ++ "  " ++ show (sum $ map snd sorted)
  where
    padRight n s = s ++ replicate (n - length s) ' '
run _ = putStrLn "Usage: sui-move-examples count-calls <directory>"
