module TreeSitter.Grammar.Transform (transform) where

import TreeSitter.Generate.Data
import TreeSitter.Grammar
import TreeSitter.Grammar.Transform.Between (between)
import TreeSitter.Grammar.Transform.HoistDefinitions (hoistDefinitions)
import TreeSitter.Grammar.Transform.MergePatterns (mergePatterns)
import TreeSitter.Grammar.Transform.SepBy (sepBy)
import TreeSitter.Grammar.Transform.StripBlanks (stripBlanks)

transform :: TokenMap -> Grammar -> Grammar
transform preserved = hoistDefinitions preserved
                    . mergePatterns
                    . between
                    . sepBy
                    . stripBlanks
