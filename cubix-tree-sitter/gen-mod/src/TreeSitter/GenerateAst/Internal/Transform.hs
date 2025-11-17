{-# LANGUAGE RecordWildCards #-}

module TreeSitter.GenerateAst.Internal.Transform (transform) where

import TreeSitter.GenerateAst.Internal.Grammar
import TreeSitter.GenerateAst.Internal.Transform.HoistChoices (hoistChoices)

transform :: Grammar -> Grammar
transform = hoistChoices
