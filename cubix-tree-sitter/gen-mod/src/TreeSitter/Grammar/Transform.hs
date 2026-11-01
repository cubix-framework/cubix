module TreeSitter.Grammar.Transform (
  TransformOptions (..),
  transform
) where

import Data.Set (Set)

import TreeSitter.Grammar
import TreeSitter.Grammar.Transform.Between
import TreeSitter.Grammar.Transform.Externals
import TreeSitter.Grammar.Transform.HoistDefinitions
import TreeSitter.Grammar.Transform.MergePatterns
import TreeSitter.Grammar.Transform.OpaqueTokens
import TreeSitter.Grammar.Transform.SepBy
import TreeSitter.Grammar.Transform.StripBlanks

data TransformOptions = TransformOptions
  { keepTextExternals :: Set RuleName -- ^ Hidden external tokens whose text should be kept
  }

transform :: TransformOptions -> Grammar -> Grammar
transform TransformOptions { keepTextExternals } =
    hoistDefinitions
  . mergePatterns
  . between
  . sepBy
  . stripBlanks
  . makeTokensOpaque
  . resolveExternals keepTextExternals
