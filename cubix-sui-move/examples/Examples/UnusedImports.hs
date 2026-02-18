-- | Example 5: Find unused imports in a Sui Move file.
--
-- Collects all UseDeclaration identifiers and compares them against
-- identifiers actually referenced in the code. Reports imports that
-- appear unused.
--
-- Sui-Move-specific: operates on UseDeclaration and ModuleAccess nodes.
module Examples.UnusedImports (run) where

import Control.Monad.Identity (runIdentity)

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Comp.Multi (project)
import Data.Comp.Multi.Strategic (TranslateM, crushtdT, promoteTF, addFail)

import Cubix.Language.Parametric.InjF (InjF(..))
import Cubix.Language.Parametric.Syntax (IdentL, pattern Ident')

import Cubix.Language.SuiMove.IPS.Types (MSuiMoveTerm)
import Cubix.Language.SuiMove.Modularized
  ( SourceFileL, UseModuleMemberL
  , UseModuleMember(..), UseMember(..)
  )

import Examples.Shared

-- | Extract names imported by use declarations.
collectImportedNames :: MSuiMoveTerm SourceFileL -> Set String
collectImportedNames = runIdentity . crushtdT (promoteTF $ addFail getImport)
  where
    getImport :: (Monad m)
      => TranslateM m MSuiMoveTerm UseModuleMemberL (Set String)
    getImport t
      | Just (UseModuleMember _ _ member) <- project t
      = pure $ extractMemberNames member
      | otherwise = pure mempty

    extractMemberNames :: MSuiMoveTerm l -> Set String
    extractMemberNames t
      | Just (UseMember1 ident _ _) <- project t
      , Just (Ident' s) <- projF ident :: Maybe (MSuiMoveTerm IdentL)
      = Set.singleton s
      | Just (UseMember2 ident _ _ _) <- project t
      , Just (Ident' s) <- projF ident :: Maybe (MSuiMoveTerm IdentL)
      = Set.singleton s
      | otherwise = Set.empty

-- | Collect all identifier references in the entire file.
collectAllRefs :: MSuiMoveTerm SourceFileL -> Set String
collectAllRefs = runIdentity . crushtdT (promoteTF $ addFail getIdent)
  where
    getIdent :: (Monad m)
      => TranslateM m MSuiMoveTerm IdentL (Set String)
    getIdent (Ident' s) = pure $ Set.singleton s
    getIdent _          = pure mempty

run :: [String] -> IO ()
run [file] = do
  term <- parseSuiMoveOrDie file
  let imported = collectImportedNames term
      allRefs  = collectAllRefs term
      -- An import is "unused" if its name never appears as an identifier
      -- elsewhere in the file. This is a heuristic: the imported name
      -- also appears in the use declaration itself, so we need at least
      -- 2 occurrences for it to be "used".
      unused   = Set.filter (\name -> not (Set.member name (Set.difference allRefs imported))) imported
  if Set.null imported
    then putStrLn "No use declarations found."
    else do
      putStrLn $ "Imported names: " ++ show (Set.toList imported)
      if Set.null unused
        then putStrLn "All imports appear to be used."
        else do
          putStrLn "Potentially unused imports:"
          mapM_ (\name -> putStrLn $ "  " ++ name) (Set.toList unused)
run _ = putStrLn "Usage: sui-move-examples unused-imports <file.move>"
