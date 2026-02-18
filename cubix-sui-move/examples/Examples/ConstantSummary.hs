-- | Example 10: Summarize constants defined in a Sui Move module.
--
-- In Sui Move, constants are module-level bindings declared with `const`.
-- This analysis extracts all constant definitions and prints their names
-- and type annotations, providing a quick overview of a module's constants.
--
-- Sui-Move-specific: operates on Constant and related nodes.
module Examples.ConstantSummary (run) where

import Control.Monad.Identity (runIdentity)

import Data.Comp.Multi (project)
import Data.Comp.Multi.Strategic (TranslateM, crushtdT, promoteTF, addFail)

import Text.PrettyPrint.Leijen qualified as PP

import Cubix.Language.Parametric.InjF (InjF(..))
import Cubix.Language.Parametric.Syntax (IdentL, pattern Ident')

import Cubix.Language.SuiMove.IPS.Types (MSuiMoveTerm)
import Cubix.Language.SuiMove.IPS.Trans (untranslate)
import Cubix.Language.SuiMove.Modularized
  ( SourceFileL, ConstantL
  , Constant(..)
  )
import Cubix.Language.SuiMove.Pretty qualified as Pretty

import Examples.Shared

data ConstantInfo = ConstantInfo
  { constName :: String
  , constType :: String
  } deriving (Show)

-- | Extract constant definitions from the AST.
collectConstants :: MSuiMoveTerm SourceFileL -> [ConstantInfo]
collectConstants = runIdentity . crushtdT (promoteTF $ addFail getConst)
  where
    getConst :: (Monad m)
      => TranslateM m MSuiMoveTerm ConstantL [ConstantInfo]
    getConst t
      | Just (Constant ident typeT _) <- project t
      = do
          let name = case projF ident :: Maybe (MSuiMoveTerm IdentL) of
                Just (Ident' s) -> s
                Nothing         -> "<unnamed>"
              -- Pretty-print just the type annotation for display
              typStr = prettyType typeT
          pure [ConstantInfo name typStr]
      | otherwise = pure mempty

    -- Use the pretty printer to render just the type subterm.
    prettyType :: MSuiMoveTerm l -> String
    prettyType t =
      let doc = Pretty.prettyTerm (untranslate t)
      in stripWhitespace $ PP.displayS (PP.renderCompact doc) ""

    stripWhitespace :: String -> String
    stripWhitespace = unwords . words

run :: [String] -> IO ()
run [file] = do
  term <- parseSuiMoveOrDie file
  let consts = collectConstants term
  if null consts
    then putStrLn "No constants found."
    else do
      let maxNameLen = maximum $ 4 : map (length . constName) consts
      putStrLn $ "Constants in " ++ file ++ ":\n"
      putStrLn $ padRight maxNameLen "Name" ++ "  Type"
      putStrLn $ replicate (maxNameLen + 30) '-'
      mapM_ (\ci ->
        putStrLn $ padRight maxNameLen (constName ci) ++ "  " ++ constType ci
        ) consts
  where
    padRight n s = s ++ replicate (max 0 (n - length s)) ' '
run _ = putStrLn "Usage: sui-move-examples constant-summary <file.move>"
