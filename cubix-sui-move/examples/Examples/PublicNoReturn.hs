-- | Example 7: Find public functions with no explicit return type.
--
-- In Sui Move, public functions without explicit return types implicitly
-- return unit. This analysis finds them, which can help enforce style
-- guidelines that require explicit return type annotations.
--
-- Sui-Move-specific: operates on SuiMoveFunctionDefAttrs and Modifier nodes.
module Examples.PublicNoReturn (run) where

import Control.Monad.Identity (runIdentity)

import Data.Comp.Multi (project)
import Data.Comp.Multi.Strategic (TranslateM, crushtdT, promoteTF, addFail)

import Cubix.Language.Parametric.InjF (InjF(..))
import Cubix.Language.Parametric.Syntax
  ( FunctionDefL, IdentL
  , MaybeF(..)
  , pattern FunctionDef', pattern Ident'
  )

import Cubix.Language.SuiMove.IPS.Types
  ( MSuiMoveTerm, SuiMoveFunctionDefAttrs(..)
  )
import Cubix.Language.SuiMove.Modularized
  ( SourceFileL, Modifier(..)
  )

import Examples.Shared

-- | Find functions that are public but have no explicit return type.
findPublicNoReturn :: MSuiMoveTerm SourceFileL -> [String]
findPublicNoReturn = runIdentity . crushtdT (promoteTF $ addFail check)
  where
    check :: (Monad m)
      => TranslateM m MSuiMoveTerm FunctionDefL [String]
    check (FunctionDef' attrsT nameT _ _) = do
      let name = case projF nameT :: Maybe (MSuiMoveTerm IdentL) of
            Just (Ident' s) -> s
            Nothing         -> "<unnamed>"
      if isPublic attrsT && hasNoReturnType attrsT
        then pure [name]
        else pure mempty
    check _ = pure mempty

    isPublic :: MSuiMoveTerm l -> Bool
    isPublic t
      | Just (NormalFunctionDefAttrs vis _ _ _ _) <- project t
      = hasPublicModifier vis
      | Just (MacroFunctionDefAttrs vis _ _) <- project t
      = hasPublicModifier vis
      | otherwise = False

    hasPublicModifier :: MSuiMoveTerm l -> Bool
    hasPublicModifier t
      | Just (JustF inner) <- project t
      , Just (Modifier1 _ _) <- project inner
      = True
      | otherwise = False

    hasNoReturnType :: MSuiMoveTerm l -> Bool
    hasNoReturnType t
      | Just (NormalFunctionDefAttrs _ _ _ _ retT) <- project t
      = isNothingF retT
      | Just (MacroFunctionDefAttrs _ _ retT) <- project t
      = isNothingF retT
      | otherwise = True

    isNothingF :: MSuiMoveTerm l -> Bool
    isNothingF t
      | Just NothingF <- project t = True
      | otherwise = False

run :: [String] -> IO ()
run [file] = do
  term <- parseSuiMoveOrDie file
  let funcs = findPublicNoReturn term
  if null funcs
    then putStrLn "All public functions have explicit return types."
    else do
      putStrLn $ "Public functions without explicit return type in " ++ file ++ ":"
      mapM_ (\name -> putStrLn $ "  " ++ name) funcs
run _ = putStrLn "Usage: sui-move-examples public-no-return <file.move>"
