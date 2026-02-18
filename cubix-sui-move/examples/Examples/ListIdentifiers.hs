-- | Example 1: List all identifiers in a Sui Move file.
--
-- Uses crushtdT to generically collect all Ident nodes from the AST.
--
-- This example is fully generic and would work on any language
-- whose IPS includes Parametric.Ident (C, Java, JavaScript, Lua, Python, etc.).
module Examples.ListIdentifiers (run) where

import Control.Monad.Identity (runIdentity)

import Data.List (nub, sort)

import Data.Comp.Multi (Term, All, HFoldable, HFunctor, (:-<:))
import Data.Comp.Multi.Strategic (TranslateM, crushtdT, promoteTF, addFail)
import Data.Comp.Multi.Strategy.Classification (DynCase)

import Cubix.Language.Parametric.Syntax (Ident, IdentL, pattern Ident')

import Examples.Shared

-- | Extract all identifier names from any term whose signature includes Ident.
-- Fully generic: works on any language with Ident in its signature.
collectIdents :: forall fs l.
  ( Ident :-<: fs
  , DynCase (Term fs) IdentL
  , All HFunctor fs
  , All HFoldable fs
  ) => Term fs l -> [String]
collectIdents = runIdentity . crushtdT (promoteTF $ addFail getIdent)
  where
    getIdent :: (Ident :-<: fs, Monad m) => TranslateM m (Term fs) IdentL [String]
    getIdent (Ident' s) = pure [s]
    getIdent _          = pure mempty

run :: [String] -> IO ()
run [file] = do
  term <- parseSuiMoveOrDie file
  let idents = sort . nub $ collectIdents term
  putStrLn $ "Identifiers in " ++ file ++ " (" ++ show (length idents) ++ " unique):"
  mapM_ (\i -> putStrLn $ "  " ++ i) idents
run _ = putStrLn "Usage: sui-move-examples list-identifiers <file.move>"
