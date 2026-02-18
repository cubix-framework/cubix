module TreeSitter.Grammar.Transform (transform) where

import TreeSitter.Grammar
import TreeSitter.Grammar.Transform.Between
import TreeSitter.Grammar.Transform.HoistDefinitions
import TreeSitter.Grammar.Transform.MergePatterns
import TreeSitter.Grammar.Transform.SepBy
import TreeSitter.Grammar.Transform.StripBlanks

transform :: Grammar -> Grammar
transform = hoistDefinitions
          . mergePatterns
          . between
          . sepBy
          . stripBlanks
