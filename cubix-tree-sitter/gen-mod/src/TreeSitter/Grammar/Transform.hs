module TreeSitter.Grammar.Transform (transform) where

import TreeSitter.Grammar
import TreeSitter.Grammar.Transform.Between (between)
import TreeSitter.Grammar.Transform.HoistDefinitions (hoistDefinitions)
import TreeSitter.Grammar.Transform.MergePatterns (mergePatterns)
import TreeSitter.Grammar.Transform.SepBy (sepBy)
import TreeSitter.Grammar.Transform.StripBlanks (stripBlanks)

transform :: Grammar -> Grammar
transform = hoistDefinitions
          . mergePatterns
          . between
          . sepBy
          . stripBlanks
