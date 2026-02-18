-- | Example 9: List struct definitions and their abilities.
--
-- In Sui Move, structs can have abilities (copy, drop, key, store) which
-- control how values of that type can be used. This analysis extracts
-- all struct definitions and reports their declared abilities.
--
-- Sui-Move-specific: operates on StructDefinition, Ability, and related nodes.
module Examples.StructAbilities (run) where

import Control.Monad.Identity (runIdentity)

import Data.Comp.Multi (project)
import Data.Comp.Multi.Strategic (TranslateM, crushtdT, promoteTF, addFail)

import Cubix.Language.Parametric.InjF (InjF(..))
import Cubix.Language.Parametric.Syntax
  ( IdentL, MaybeF(..), ExtractF(..)
  , pattern Ident'
  )

import Cubix.Language.SuiMove.IPS.Types (MSuiMoveTerm)
import Cubix.Language.SuiMove.Modularized
  ( SourceFileL, StructDefinitionL, AbilityL
  , StructDefinition(..), HiddenStructSignature(..)
  , HiddenStructIdentifier(..)
  , AbilityDecls(..), Ability(..)
  , PostfixAbilityDecls(..)
  )

import Examples.Shared

data StructInfo = StructInfo
  { structName      :: String
  , structAbilities :: [String]
  } deriving (Show)

-- | Extract struct definitions and their abilities.
collectStructs :: MSuiMoveTerm SourceFileL -> [StructInfo]
collectStructs = runIdentity . crushtdT (promoteTF $ addFail getStruct)
  where
    getStruct :: (Monad m)
      => TranslateM m MSuiMoveTerm StructDefinitionL [StructInfo]
    getStruct t
      | Just (StructDefinition _pub sig _fields postfixAbils) <- project t
      = do
          let name = extractStructName sig
              sigAbils = extractSigAbilities sig
              postAbils = extractPostfixAbilities postfixAbils
          pure [StructInfo name (sigAbils ++ postAbils)]
      | otherwise = pure mempty

    extractStructName :: MSuiMoveTerm l -> String
    extractStructName t
      | Just (HiddenStructSignature _ hiddenId _ _) <- project t
      , Just (HiddenStructIdentifier ident) <- project hiddenId
      , Just (Ident' s) <- projF ident :: Maybe (MSuiMoveTerm IdentL)
      = s
      | otherwise = "<unknown>"

    extractSigAbilities :: MSuiMoveTerm l -> [String]
    extractSigAbilities t
      | Just (HiddenStructSignature _ _ _ abilDecls) <- project t
      = extractAbilityDecls abilDecls
      | otherwise = []

    extractAbilityDecls :: MSuiMoveTerm l -> [String]
    extractAbilityDecls t
      | Just (JustF inner) <- project t
      , Just (AbilityDecls _ abils) <- project inner
      = map extractAbility (extractF abils)
      | otherwise = []

    extractPostfixAbilities :: MSuiMoveTerm l -> [String]
    extractPostfixAbilities t
      | Just (JustF inner) <- project t
      , Just (PostfixAbilityDecls abils) <- project inner
      = map extractAbility (extractF abils)
      | otherwise = []

    extractAbility :: MSuiMoveTerm AbilityL -> String
    extractAbility t
      | Just (AbilityCopy _) <- project t   = "copy"
      | Just (AbilityDrop _) <- project t   = "drop"
      | Just (AbilityStore _) <- project t  = "store"
      | Just (AbilityKey _) <- project t    = "key"
      | otherwise = "<unknown ability>"

run :: [String] -> IO ()
run [file] = do
  term <- parseSuiMoveOrDie file
  let structs = collectStructs term
  if null structs
    then putStrLn "No struct definitions found."
    else do
      putStrLn $ "Struct definitions in " ++ file ++ ":\n"
      mapM_ printStruct structs
  where
    printStruct si = do
      let abils = if null (structAbilities si)
                    then "(no abilities)"
                    else unwords (structAbilities si)
      putStrLn $ "  " ++ structName si ++ " has " ++ abils
run _ = putStrLn "Usage: sui-move-examples struct-abilities <file.move>"
