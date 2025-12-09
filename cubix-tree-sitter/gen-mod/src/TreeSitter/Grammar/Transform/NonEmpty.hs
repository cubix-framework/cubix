{-# LANGUAGE RecordWildCards #-}

module TreeSitter.Grammar.Transform.NonEmpty (
  nonEmpty
) where

import Data.Functor.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector qualified as Vector
import TreeSitter.Generate.Data (TokenMap)
import TreeSitter.Grammar

nonEmpty :: Grammar -> Grammar
nonEmpty g@(Grammar {..}) = g { rules = nonEmptyRules rules }

nonEmptyRules :: Map RuleName Rule -> Map RuleName Rule
nonEmptyRules rules = flip fmap rules $ fold nonEmptyAlg

isRepeatRule :: Rule -> Maybe Rule
isRepeatRule (RepeatRule r) = Just r
isRepeatRule _ = Nothing

nonEmptyAlg :: RuleF Rule -> Rule
-- This is too specific, we could split members arbitrarily and introduce nonempty
nonEmptyAlg (SeqRuleF members)
  | Just (a, as) <- Vector.uncons members
  , Just (b, _) <- Vector.uncons as =
      case isRepeatRule a of
        Just r -> if r == b then Repeat1Rule r else SeqRule members
        Nothing -> case isRepeatRule b of
          Just r -> if r == a then Repeat1Rule r else SeqRule members
          Nothing -> SeqRule members
  | otherwise = SeqRule members
nonEmptyAlg (ChoiceRuleF members)
  | (Just (m, ms)) <- Vector.uncons members
  , Vector.null ms = m
nonEmptyAlg r = embed r
