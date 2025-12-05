{-# LANGUAGE RecordWildCards #-}

module TreeSitter.GenerateAst.Internal.Transform.SepBy (
  sepBy
) where

import Data.Functor.Foldable
import Data.Map (Map)
import Data.Vector qualified as Vector
import TreeSitter.GenerateAst.Internal.Grammar

sepBy :: Grammar -> Grammar
sepBy g@(Grammar {..}) = g { rules = sepByRules rules }

sepByRules :: Map RuleName Rule -> Map RuleName Rule
sepByRules rules = flip fmap rules $ fold sepByAlg

-- Needs to process seq and choice rules as well, as those might
-- introduce more blanks
sepByAlg :: RuleF Rule -> Rule
sepByAlg (SeqRuleF (Vector.filter (/= BlankRule) -> members))
  | Vector.null members = BlankRule
  | Vector.length members == 1 = Vector.unsafeHead members
  | otherwise = SeqRule members
    
sepByAlg (ChoiceRuleF (Vector.filter (/= BlankRule) -> members))
  | Vector.null members = BlankRule
  | Vector.length members == 1 = Vector.unsafeHead members
  | otherwise = ChoiceRule members

sepByAlg (AliasRuleF _ _ BlankRule) = BlankRule
sepByAlg (RepeatRuleF BlankRule) = BlankRule
sepByAlg (Repeat1RuleF BlankRule) = BlankRule
sepByAlg (TokenRuleF BlankRule) = BlankRule
sepByAlg (ImmediateTokenRuleF BlankRule) = BlankRule
sepByAlg (FieldRuleF _ BlankRule) = BlankRule
sepByAlg (PrecRuleF _ _ BlankRule) = BlankRule
sepByAlg r = embed r
