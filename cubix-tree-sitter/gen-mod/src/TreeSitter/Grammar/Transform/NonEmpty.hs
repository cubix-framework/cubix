{-# LANGUAGE RecordWildCards #-}

module TreeSitter.Grammar.Transform.NonEmpty (
  nonEmpty
) where

import Data.Functor.Foldable
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import TreeSitter.Grammar

nonEmpty :: Grammar -> Grammar
nonEmpty g@(Grammar {..}) = g { rules = nonEmptyRules rules }

nonEmptyRules :: Map RuleName Rule -> Map RuleName Rule
nonEmptyRules rules = flip fmap rules $ fold nonEmptyAlg

-- run strip blanks first, as we need optionals first
nonEmptyAlg :: RuleF Rule -> Rule
-- This is too specific, we could split members arbitrarily and introduce nonempty
nonEmptyAlg (SeqRuleF members)
  | Just (RepeatRule (SeqRule sq), OptionalRule c, ms) <- uncons2 members
  , Just (c', s, rs) <- uncons2 sq
  , c == c' && Vector.null ms && Vector.null rs = SepByRule s c

  | Just (c, RepeatRule (SeqRule sq), OptionalRule s, ms) <- uncons3 members
  , Just (s', c', rs) <- uncons2 sq
  , c == c' && s == s' && Vector.null ms && Vector.null rs = SepBy1Rule s c

  | Just (StringRule open, ms) <- Vector.uncons members
  , Just (sq, StringRule close) <- Vector.unsnoc ms
  , not (Vector.null sq)
  = BetweenRule open (SeqRule sq) close

  | otherwise = SeqRule members
nonEmptyAlg (ChoiceRuleF members)
  | (Just (m, ms)) <- Vector.uncons members
  , Vector.null ms = m
nonEmptyAlg r = embed r

-- helpers for pattern matching on seq members
uncons2 :: Vector a -> Maybe (a, a, Vector a)
uncons2 as
  | Just (first, as') <- Vector.uncons as
  , Just (second, rest) <- Vector.uncons as'
  = Just (first, second, rest)
  | otherwise = Nothing

uncons3 :: Vector a -> Maybe (a, a, a, Vector a)
uncons3 as
  | Just (first, as') <- Vector.uncons as
  , Just (second, as'') <- Vector.uncons as'
  , Just (third, rest) <- Vector.uncons as''
  = Just (first, second, third, rest)
  | otherwise = Nothing

