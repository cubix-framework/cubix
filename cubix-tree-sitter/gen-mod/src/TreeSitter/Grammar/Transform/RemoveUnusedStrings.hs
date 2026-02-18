{-# LANGUAGE RecordWildCards #-}

module TreeSitter.Grammar.Transform.RemoveUnusedStrings (
  removeUnusedStrings
) where

import Data.Map (Map)
import Data.Map qualified as Map (lookup)

import Data.Functor.Foldable (Corecursive (..), Recursive (..), fold)

import TreeSitter.Generate.Types
import TreeSitter.Grammar

removeUnusedStrings :: TokenMap -> Grammar -> Grammar
removeUnusedStrings preserved g@(Grammar {..}) = g { rules = removeUnusedStringsRules preserved rules }

removeUnusedStringsRules :: TokenMap -> Map RuleName Rule -> Map RuleName Rule
removeUnusedStringsRules preserved rules = flip fmap rules $ fold (removeUnusedStringsAlg preserved)

removeUnusedStringsAlg :: TokenMap -> RuleF Rule -> Rule
removeUnusedStringsAlg preserved = \case
  (StringRuleF string) -> case Map.lookup string preserved of
    Just _ -> StringRule string
    Nothing -> BlankRule
  r -> embed r
