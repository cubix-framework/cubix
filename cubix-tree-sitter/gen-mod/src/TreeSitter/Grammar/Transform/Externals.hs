-- | Resolve references to external tokens: tokens produced by a
-- grammar's hand-written C scanner (@scanner.c@) and declared under
-- @externals@ in @grammar.json@. Rules refer to them as ordinary
-- 'SymbolRule's, but they have no entry in @rules@, so every later
-- pass would otherwise fail on the first reference.
--
-- * Externals that share a name with a rule (an internal fallback
--   lexer, e.g. TypeScript's @regex_pattern@) are left alone: the rule
--   describes the token.
-- * String externals (Python's @]@ or @except@) are the very token the
--   rules already reference as a 'StringRule', so there is nothing to do.
-- * Hidden externals (@_indent@, @_automatic_semicolon@, Haskell's
--   layout tokens) never appear in tree-sitter's output tree, and
--   neither do externals that are extras (comments). References to
--   them are replaced by 'BlankRule', so they contribute nothing to
--   the syntax.
-- * Some hidden externals carry text that is nowhere else in the tree:
--   Python's @_string_content@ (the literal text of a string), Haskell's
--   @_varsym@ (the whole of an @operator@ node). Whether a scanner token
--   is zero-width (layout) or carries text cannot be read off the
--   grammar, so the caller lists these. They are blanked too, but every
--   rule referring to one gets its own source text as a leading field.
-- * Visible externals (@string_start@, @quasiquote_body@) appear in the
--   tree as leaves carrying text. Each gets a synthesized top-level
--   rule that is an opaque 'PatternRule', so it is generated exactly
--   like a regex-defined token such as @identifier@.
module TreeSitter.Grammar.Transform.Externals (
  resolveExternals
) where

import Data.Map (Map)
import Data.Map qualified as Map (fromList, member, union)
import Data.Set (Set)
import Data.Set qualified as Set (fromList, member)
import Data.Vector qualified as Vector (fromList, toList)

import Data.Functor.Foldable (Corecursive (..), cata, fold)

import TreeSitter.Grammar

--------------------------------------------------------------------------------
-- Building blocks
--------------------------------------------------------------------------------

----------------------------------------
-- Placeholder patterns
----------------------------------------

-- | The source text of the node being parsed.
nodeText :: Rule
nodeText = PatternRule { value = "<node text>", flags = Nothing }

-- | Stands in for the scanner's lexer. Only its being a pattern
-- matters to later passes; the pattern text itself is never matched.
scannerToken :: Rule
scannerToken = PatternRule { value = "<external scanner token>", flags = Nothing }

----------------------------------------
-- Rule traversals
----------------------------------------

symbols :: Rule -> [RuleName]
symbols = cata $ \case
  SymbolRuleF symbol -> [symbol]
  r -> concat r

blankInvisible :: Set RuleName -> RuleF Rule -> Rule
blankInvisible invisible = \case
  SymbolRuleF symbol | symbol `Set.member` invisible -> BlankRule
  r -> embed r

--------------------------------------------------------------------------------
-- Resolving externals
--------------------------------------------------------------------------------

resolveExternals
  :: Set RuleName -- ^ Hidden externals whose text should be kept
  -> Grammar
  -> Grammar
resolveExternals keepText g@(Grammar { rules, externals }) =
  g { rules = fmap (fold (blankInvisible invisible) . withText) rules `Map.union` visibleRules }
  where

    ------ Classifying externals

    scannerOnly :: [RuleName]
    scannerOnly =
      [ external
      | SymbolRule external <- maybe [] Vector.toList externals
      , not (external `Map.member` rules)
      ]

    isInvisible :: RuleName -> Bool
    isInvisible external = isHidden external || external `Set.member` extraRuleNames g

    ------ Invisible externals

    invisible :: Set RuleName
    invisible = Set.fromList (filter isInvisible scannerOnly)

    withText :: Rule -> Rule
    withText rule
      | any (`Set.member` keepText) (symbols rule) = SeqRule (Vector.fromList [nodeText, rule])
      | otherwise = rule

    ------ Visible externals

    visibleRules :: Map RuleName Rule
    visibleRules = Map.fromList
      [ (external, scannerToken) | external <- scannerOnly, not (isInvisible external) ]
