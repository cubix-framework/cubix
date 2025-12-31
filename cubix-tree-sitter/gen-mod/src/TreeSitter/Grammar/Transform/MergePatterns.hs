{-# LANGUAGE RecordWildCards #-}

module TreeSitter.Grammar.Transform.MergePatterns (
  mergePatterns
) where

import Data.Functor.Foldable
import Data.Map (Map)
import Data.Vector qualified as Vector
import TreeSitter.Grammar

mergePatterns :: Grammar -> Grammar
mergePatterns g@(Grammar {..}) = g { rules = mergePatternsRules rules }

mergePatternsRules :: Map RuleName Rule -> Map RuleName Rule
mergePatternsRules rules = flip fmap rules $ fold mergePatternsAlg

isPattern :: Rule -> Bool
isPattern (PatternRule _ _) = True
isPattern _ = False

mergePatternsAlg :: RuleF Rule -> Rule
mergePatternsAlg (ChoiceRuleF members)
  | all isPattern members = Vector.head members
mergePatternsAlg r = embed r
