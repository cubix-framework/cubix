{-# LANGUAGE RecordWildCards #-}

module TreeSitter.Grammar.Transform.StripBlanks (
  stripBlanks
) where

import Data.Map (Map)
import Data.Vector qualified as Vector (elem, filter, length, null, unsafeHead)

import Data.Functor.Foldable (Corecursive (..), Recursive (..), fold)

import TreeSitter.Grammar

stripBlanks :: Grammar -> Grammar
stripBlanks g@(Grammar {..}) = g { rules = stripBlanksRules rules }

stripBlanksRules :: Map RuleName Rule -> Map RuleName Rule
stripBlanksRules rules = flip fmap rules $ fold stripBlanksAlg

-- Needs to process seq and choice rules as well, as those might
-- introduce more blanks
stripBlanksAlg :: RuleF Rule -> Rule
stripBlanksAlg (SeqRuleF (Vector.filter (/= BlankRule) -> members))
  | Vector.null members = BlankRule
  | Vector.length members == 1 = Vector.unsafeHead members
  | otherwise = SeqRule members

stripBlanksAlg (ChoiceRuleF members)
  | Vector.null members =
      BlankRule
  | Vector.elem BlankRule members =
      let members' = Vector.filter (/= BlankRule) members
      in if Vector.null members'
         then BlankRule
         else OptionalRule (ChoiceRule members')
  | otherwise =
      ChoiceRule members

stripBlanksAlg (AliasRuleF _ _ BlankRule) = BlankRule
stripBlanksAlg (RepeatRuleF BlankRule) = BlankRule
stripBlanksAlg (Repeat1RuleF BlankRule) = BlankRule
stripBlanksAlg (TokenRuleF BlankRule) = BlankRule
stripBlanksAlg (ImmediateTokenRuleF BlankRule) = BlankRule
stripBlanksAlg (FieldRuleF _ BlankRule) = BlankRule
stripBlanksAlg (PrecRuleF _ _ BlankRule) = BlankRule
stripBlanksAlg r = embed r
