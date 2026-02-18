{-# LANGUAGE RecordWildCards #-}

module TreeSitter.Grammar.Transform.Between (
  between
) where

import Data.Map (Map)
import Data.Vector (Vector)
import Data.Vector qualified as Vector (cons, length, tail, unsafeHead)

import Data.Functor.Foldable (Corecursive (..), Recursive (..), fold)

import Data.Vector.Extra
import TreeSitter.Grammar

between :: Grammar -> Grammar
between g@(Grammar {..}) = g { rules = betweenRules rules }

betweenRules :: Map RuleName Rule -> Map RuleName Rule
betweenRules rules = flip fmap rules $ fold betweenAlg

betweenAlg :: RuleF Rule -> Rule
betweenAlg (SeqRuleF members) =
  let updated = go members
  in if Vector.length updated == 1
        then Vector.unsafeHead updated
        else SeqRule updated
 where
  go :: Vector Rule -> Vector Rule
  go rules
    | Just (StringRule open, rule, StringRule close, rest) <- uncons3 rules =
        Vector.cons (BetweenRule open rule close) $ go rest
    | Just (a, b, c, rest) <- uncons3 rules =
        Vector.cons a $ go (Vector.tail rules)
    | otherwise = rules
betweenAlg r = embed r
