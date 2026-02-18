-- | Example 8: Rename an identifier throughout a Sui Move file.
--
-- Uses allbuR with a sort-specific rewrite on Ident nodes to rename
-- all occurrences of an identifier.
--
-- This example is fully generic and would work on any language
-- whose IPS includes Parametric.Ident (C, Java, JavaScript, Lua, Python, etc.).
module Examples.RenameIdent (run) where

import Control.Monad (MonadPlus(..))
import Control.Monad.Identity (runIdentity)

import Data.Comp.Multi (Term, All, HFoldable, HFunctor, HTraversable, (:-<:))
import Data.Comp.Multi.Strategic (RewriteM, allbuR, promoteR)
import Data.Comp.Multi.Strategy.Classification (DynCase)

import Cubix.Language.Parametric.Syntax (Ident, IdentL, pattern Ident', iIdent)

import Examples.Shared

-- | Rename all occurrences of an identifier.
-- Fully generic: works on any language with Ident in its signature.
renameIdent :: forall fs l.
  ( Ident :-<: fs
  , DynCase (Term fs) IdentL
  , All HFunctor fs
  , All HTraversable fs
  , All HFoldable fs
  ) => String -> String -> Term fs l -> Term fs l
renameIdent from to = runIdentity . allbuR (promoteR rename)
  where
    rename :: (MonadPlus m) => RewriteM m (Term fs) IdentL
    rename (Ident' s)
      | s == from = pure $ iIdent to
      | otherwise = mzero
    rename _ = mzero

run :: [String] -> IO ()
run [from, to, file] = do
  term <- parseSuiMoveOrDie file
  let renamed = renameIdent from to term
  putStrLn $ prettySuiMove renamed
run _ = putStrLn "Usage: sui-move-examples rename-ident <old-name> <new-name> <file.move>"
