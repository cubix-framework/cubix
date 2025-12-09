module TreeSitter.Grammar.Transform (transform) where

import TreeSitter.Generate.Data
import TreeSitter.Grammar
import TreeSitter.Grammar.Transform.HoistDefinitions (hoistDefinitions)
import TreeSitter.Grammar.Transform.NonEmpty (nonEmpty)
import TreeSitter.Grammar.Transform.RemoveUnusedStrings (removeUnusedStrings)
import TreeSitter.Grammar.Transform.StripBlanks (stripBlanks)

transform :: TokenMap -> Grammar -> Grammar
transform preserved = hoistDefinitions preserved . nonEmpty . stripBlanks . removeUnusedStrings preserved
