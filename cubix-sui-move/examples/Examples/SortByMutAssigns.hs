-- | Example 4: Sort files by the number of mutable assignments therein.
--
-- Counts two kinds of mutability indicators:
-- 1. Mutable function parameters (Mutable constructor in SuiMoveParameterAttrs)
-- 2. Assignment expressions (Assign nodes in the IPS)
--
-- The Assign counting is fully generic (works on any language with Assign).
-- The mutable parameter counting is Sui-Move-specific.
module Examples.SortByMutAssigns (run) where

import Control.Monad.Identity (runIdentity)

import Data.List (sortBy)
import Data.Monoid (Sum(..))
import Data.Ord (comparing, Down(..))

import Data.Comp.Multi (Term, All, HFoldable, HFunctor, project, (:-<:))
import Data.Comp.Multi.Strategic (TranslateM, crushtdT, promoteTF, addFail)
import Data.Comp.Multi.Strategy.Classification (DynCase)

import Cubix.Language.Parametric.Syntax
  ( Assign, AssignL, pattern Assign'
  , ParameterAttrsL
  )

import Cubix.Language.SuiMove.IPS.Types
  ( MSuiMoveTerm, SuiMoveParameterAttrs(..)
  )
import Cubix.Language.SuiMove.Modularized (SourceFileL)

import Examples.Shared

-- | Count Assign nodes in any term. Fully generic: works on any language
-- whose IPS includes the Assign node.
countAssigns :: forall fs l.
  ( Assign :-<: fs
  , DynCase (Term fs) AssignL
  , All HFunctor fs
  , All HFoldable fs
  ) => Term fs l -> Int
countAssigns = getSum . runIdentity . crushtdT (promoteTF $ addFail countAssign)
  where
    countAssign :: (Assign :-<: fs, Monad m)
      => TranslateM m (Term fs) AssignL (Sum Int)
    countAssign (Assign' _ _ _) = pure $ Sum 1
    countAssign _               = pure mempty

-- | Count mutable parameters. Sui-Move-specific.
countMutableParams :: MSuiMoveTerm SourceFileL -> Int
countMutableParams = getSum . runIdentity . crushtdT (promoteTF $ addFail countMut)
  where
    countMut :: (Monad m)
      => TranslateM m MSuiMoveTerm ParameterAttrsL (Sum Int)
    countMut t
      | Just (Mutable _) <- project t = pure $ Sum 1
      | otherwise = pure mempty

-- | Total mutability score: assignments + mutable parameters.
mutabilityScore :: MSuiMoveTerm SourceFileL -> Int
mutabilityScore t = countAssigns t + countMutableParams t

run :: [String] -> IO ()
run [dir] = do
  files <- parseDirectory dir
  if null files
    then putStrLn $ "No .move files found in " ++ dir
    else do
      let scored = map (\(path, term) -> (path, mutabilityScore term, countAssigns term, countMutableParams term)) files
          sorted = sortBy (comparing (\(_, s, _, _) -> Down s)) scored
          maxPathLen = maximum $ 4 : map (\(p, _, _, _) -> length p) sorted
      putStrLn $ padRight maxPathLen "File" ++ "  Total  Assigns  MutParams"
      putStrLn $ replicate (maxPathLen + 30) '-'
      mapM_ (\(path, total, assigns, mutParams) ->
        putStrLn $ padRight maxPathLen path
          ++ "  " ++ padRight 5 (show total)
          ++ "  " ++ padRight 7 (show assigns)
          ++ "  " ++ show mutParams
        ) sorted
  where
    padRight n s = s ++ replicate (max 0 (n - length s)) ' '
run _ = putStrLn "Usage: sui-move-examples sort-by-mut-assigns <directory>"
