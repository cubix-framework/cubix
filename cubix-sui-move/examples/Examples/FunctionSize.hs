-- | Example 6: Measure function size (number of block items per function).
--
-- Uses crushtdT to find all FunctionDef nodes and count the number
-- of block items in their bodies.
--
-- This example is conceptually generic (works on any language with
-- FunctionDef, Block, and Ident), but uses MSuiMoveTerm concretely
-- to avoid overlapping InjF instances from other language modules.
module Examples.FunctionSize (run) where

import Control.Monad.Identity (runIdentity)

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

import Data.Comp.Multi (project)
import Data.Comp.Multi.Strategic (TranslateM, crushtdT, promoteTF, addFail)

import Cubix.Language.Parametric.InjF (InjF(projF))
import Cubix.Language.Parametric.Syntax
  ( FunctionDefL, BlockItemL, IdentL
  , pattern FunctionDef', pattern Block', pattern Ident'
  , ExtractF(..)
  )

import Cubix.Language.SuiMove.IPS.Types (MSuiMoveTerm, BlockIsFunctionBody(..))
import Cubix.Language.SuiMove.Modularized (SourceFileL)
import Cubix.Language.Parametric.Syntax qualified as Parametric

import Examples.Shared

-- | Extract (function name, block item count) pairs from a term.
functionSizes :: MSuiMoveTerm SourceFileL -> [(String, Int)]
functionSizes = runIdentity . crushtdT (promoteTF $ addFail getSize)
  where
    getSize :: (Monad m) => TranslateM m MSuiMoveTerm FunctionDefL [(String, Int)]
    getSize (FunctionDef' _ nameT _ bodyT) = do
      let name = case projF nameT :: Maybe (MSuiMoveTerm IdentL) of
            Just (Ident' s) -> s
            Nothing         -> "<unnamed>"
          size = case project bodyT of
            Just (BlockIsFunctionBody block) ->
              case projF block :: Maybe (MSuiMoveTerm Parametric.BlockL) of
                Just (Block' items _) -> length (extractF items :: [MSuiMoveTerm BlockItemL])
                _ -> 0
            _ -> 0
      pure [(name, size)]
    getSize _ = pure mempty

run :: [String] -> IO ()
run [file] = do
  term <- parseSuiMoveOrDie file
  let sizes = sortBy (comparing (Down . snd)) $ functionSizes term
  if null sizes
    then putStrLn "No functions found."
    else do
      let maxNameLen = maximum $ 8 : map (length . fst) sizes
      putStrLn $ "Function sizes in " ++ file ++ ":\n"
      putStrLn $ padRight maxNameLen "Function" ++ "  Statements"
      putStrLn $ replicate (maxNameLen + 14) '-'
      mapM_ (\(name, size) ->
        putStrLn $ padRight maxNameLen name ++ "  " ++ show size
        ) sizes
  where
    padRight n s = s ++ replicate (max 0 (n - length s)) ' '
run _ = putStrLn "Usage: sui-move-examples function-size <file.move>"
