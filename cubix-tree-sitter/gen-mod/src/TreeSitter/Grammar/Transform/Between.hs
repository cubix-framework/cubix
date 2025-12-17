{-# LANGUAGE RecordWildCards #-}

module TreeSitter.Grammar.Transform.Between (
  between
) where

import Data.Functor.Foldable
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Extra qualified as Vector
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
    | Just (StringRule open, rule, StringRule close, rest) <- Vector.uncons3 rules =
        Vector.cons (BetweenRule open rule close) $ go rest
    | Just (a, b, c, rest) <- Vector.uncons3 rules =
        Vector.cons a $ go (Vector.tail rules)
    | otherwise = rules
betweenAlg r = embed r
