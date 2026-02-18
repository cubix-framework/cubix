{-# LANGUAGE RecordWildCards #-}

module TreeSitter.Grammar.Transform.SepBy (
  sepBy
) where

import Data.Map (Map)
import Data.Vector qualified as Vector (null, uncons)

import Data.Functor.Foldable (Corecursive (..), Recursive (..), fold)

import Data.Vector.Extra
import TreeSitter.Grammar

sepBy :: Grammar -> Grammar
sepBy g@(Grammar {..}) = g { rules = sepByRules rules }

sepByRules :: Map RuleName Rule -> Map RuleName Rule
sepByRules rules = flip fmap rules $ fold sepByAlg

-- run strip blanks first, as we need optionals first
sepByAlg :: RuleF Rule -> Rule
-- This is too specific, we could split members arbitrarily and introduce nonempty
sepByAlg (SeqRuleF members)
  | Just (RepeatRule (SeqRule sq), OptionalRule c, ms) <- uncons2 members
  , Just (c', s, rs) <- uncons2 sq
  , c == c' && Vector.null ms && Vector.null rs = SepByRule s c

  | Just (c, RepeatRule (SeqRule sq), OptionalRule s, ms) <- uncons3 members
  , Just (s', c', rs) <- uncons2 sq
  , c == c' && s == s' && Vector.null ms && Vector.null rs = SepBy1Rule s c

  | otherwise = SeqRule members
sepByAlg (ChoiceRuleF members)
  | (Just (m, ms)) <- Vector.uncons members
  , Vector.null ms = m
sepByAlg r = embed r

