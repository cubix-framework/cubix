{-# LANGUAGE RecordWildCards #-}

module TreeSitter.GenerateAst.Internal.Transform (transform) where

import TreeSitter.GenerateAst.Internal.Data
import TreeSitter.GenerateAst.Internal.Grammar
import TreeSitter.GenerateAst.Internal.Transform.HoistDefinitions (hoistDefinitions)
import TreeSitter.GenerateAst.Internal.Transform.NonEmpty (nonEmpty)
import TreeSitter.GenerateAst.Internal.Transform.RemoveUnusedStrings (removeUnusedStrings)
import TreeSitter.GenerateAst.Internal.Transform.StripBlanks (stripBlanks)

transform :: TokenMap -> Grammar -> Grammar
transform preserved = hoistDefinitions preserved . nonEmpty . stripBlanks . removeUnusedStrings preserved
