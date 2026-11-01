-- | Turn what tree-sitter lexes as a single token into opaque patterns,
-- i.e. leaves carrying text.
--
-- * Everything under @token(...)@ or @token.immediate(...)@ is one
--   token, so the structure inside it (Python's @integer@ is
--   @token(choice(seq(choice('0x', '0X'), ...), ...))@) never shows up
--   in the tree. Without this the generator would hoist the inner
--   choices into data types and expect tokens such as @0x@ as children.
--   @token("=>")@ (possibly under a precedence) is just the anonymous
--   @=>@ token and is left alone.
--
-- * A visible rule whose whole body is a string used nowhere else in the
--   grammar (Python's @true: 'True'@) is lexed as a token named after the
--   rule, so it is a leaf with no anonymous @True@ child. If the string
--   is used elsewhere (@wildcard_import: '*'@), tree-sitter keeps the
--   rule as a node wrapping the anonymous token, which is what the
--   generator already produces.
--
-- Hidden rules of that second kind are invisible terminals; they are
-- left alone for now, as are extras (comments): the generated parser
-- skips extras, so their rules only matter for the token set, which this
-- keeps stable for existing grammars.
module TreeSitter.Grammar.Transform.OpaqueTokens (
  makeTokensOpaque
) where

import Data.Map (Map)
import Data.Map qualified as Map (elems, fromListWith, lookup, mapWithKey)
import Data.Maybe (isJust)
import Data.Set qualified as Set (member)
import Data.Text (Text)

import Data.Functor.Foldable (Corecursive (..), fold, para)

import TreeSitter.Grammar

--------------------------------------------------------------------------------
-- Single-string tokens
--------------------------------------------------------------------------------

----------------------------------------
-- Recognizing them
----------------------------------------

-- | The string a rule consists of, if it is a single string token. A
-- precedence counts only inside @token(...)@: tree-sitter keeps Python's
-- @pass_statement: prec.left('pass')@ as a node wrapping @pass@.
tokenString :: Rule -> Maybe Text
tokenString = \case
  StringRule string -> Just string
  TokenRule content -> precString content
  ImmediateTokenRule content -> precString content
  _ -> Nothing

-- | The string a token's content consists of, possibly under
-- precedences (TypeScript's regex ends in @token.immediate(prec(1, '/'))@).
precString :: Rule -> Maybe Text
precString = \case
  PrecRule { content } -> precString content
  rule -> tokenString rule

isString :: Rule -> Bool
isString = isJust . precString

----------------------------------------
-- Counting their uses
----------------------------------------

-- | How often each string occurs as a token of its own, i.e. not as part
-- of a larger @token(...)@.
stringUses :: [Rule] -> Map Text Int
stringUses rules = Map.fromListWith (+) [ (string, 1) | string <- concatMap (para alg) rules ]
  where
    alg :: RuleF (Rule, [Text]) -> [Text]
    alg = \case
      StringRuleF string -> [string]

      TokenRuleF (content, strings)
        | isString content -> strings
        | otherwise -> []

      ImmediateTokenRuleF (content, strings)
        | isString content -> strings
        | otherwise -> []

      r -> concatMap snd r

--------------------------------------------------------------------------------
-- Collapsing tokens into opaque patterns
--------------------------------------------------------------------------------

----------------------------------------
-- The opaque pattern
----------------------------------------

opaque :: Rule
opaque = PatternRule { value = "<token>", flags = Nothing }

----------------------------------------
-- Structured token(...) bodies
----------------------------------------

makeTokensOpaqueAlg :: RuleF Rule -> Rule
makeTokensOpaqueAlg = \case
  TokenRuleF content | not (isString content) -> opaque
  ImmediateTokenRuleF content | not (isString content) -> opaque
  r -> embed r

----------------------------------------
-- Entry point
----------------------------------------

makeTokensOpaque :: Grammar -> Grammar
makeTokensOpaque g@(Grammar { rules }) = g { rules = Map.mapWithKey collapse rules }
  where
    uses = stringUses (Map.elems rules)

    collapse ruleName rule
      | ruleName `Set.member` extraRuleNames g = rule
      | not (isHidden ruleName)
      , Just string <- tokenString rule
      , Map.lookup string uses == Just 1 = opaque
      | otherwise = fold makeTokensOpaqueAlg rule
