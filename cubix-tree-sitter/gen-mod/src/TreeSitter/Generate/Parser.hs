{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module TreeSitter.Generate.Parser where

import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import TreeSitter.Grammar
import TreeSitter.Generate.Types

data Parser
  = Symbol Name
  -- | like symbol but has mangled name and we want to discard it in ast
  | Tok Text
  -- | Like symbol but without parsing tree-sitter symbol first
  | Inline Name
  | Seq (NonEmpty Parser)
  | Optional Parser
  | Pair Parser Parser
  -- 0 or more
  | Many Parser
  -- 1 or more
  | Some Parser
  | Skip
  -- get text content
  | Extract
  -- recovered parsers
  | SepBy Parser Parser
  | SepBy1 Parser Parser
  | Between Parser Parser Parser
  deriving (Show, Generic)

mkParser :: Rule -> Parser
mkParser (SeqRule members) = case Vector.uncons (cata alg <$> members) of
  Nothing -> Skip
  Just (h, ms) -> if Vector.null ms
    then h
    else Seq (h :| Vector.toList ms) 
mkParser r = cata alg r

alg :: RuleF Parser -> Parser
alg = \case
  BlankRuleF -> Skip
  StringRuleF {..} -> Tok valueF
  PatternRuleF {} -> Extract
  SymbolRuleF {..} -> Symbol (Name nameF)
  SeqRuleF {..} -> case Vector.uncons membersF of
    Nothing -> Skip
    Just (h, ms) -> if Vector.null ms
      then h
      else foldl Pair h ms
  ChoiceRuleF {} -> error "ChoiceRule should not appear after preprocessing"
  AliasRuleF {..} -> contentF
  RepeatRuleF {..} -> Many contentF
  Repeat1RuleF {..} -> Some contentF
  TokenRuleF {..} -> contentF
  ImmediateTokenRuleF {..} -> contentF
  FieldRuleF {..} -> contentF
  PrecRuleF {..} -> contentF
  RefRuleF {..} -> Inline (Name nameF)
  OptionalRuleF {..} -> Optional contentF
  SepByRuleF {..} -> SepBy separatorF contentF
  SepBy1RuleF {..} -> SepBy1 separatorF contentF
  BetweenRuleF {..} -> Between
    (Symbol (Name $ openF  <> "_tok"))
    (Symbol (Name $ closeF <> "_tok"))
    contentF
