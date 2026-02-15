-- | ParseFile and Pretty instances for Sui Move IPS
--
-- This module provides the interface for parsing Sui Move files directly
-- into the IPS (Incremental Parametric Syntax) representation.
module Cubix.Language.SuiMove.ParsePretty (
    parseSuiMove
  , prettySuiMove
  ) where

import TreeSitter.SuiMove (tree_sitter_sui_move)

import Cubix.ParsePretty (ParseFile(..), Pretty(..), RootSort)

import Cubix.Language.SuiMove.IPS.Types (MSuiMoveSig, MSuiMoveTerm)
import Cubix.Language.SuiMove.IPS.Trans (translate, untranslate)
import Cubix.Language.SuiMove.Modularized (SourceFileL)
import Cubix.Language.SuiMove.RawParse qualified as RawParse
import Cubix.Language.SuiMove.Pretty qualified as Pretty

-- | Parse a Sui Move file into IPS representation
parseSuiMove :: FilePath -> IO (Maybe (MSuiMoveTerm (RootSort MSuiMoveSig)))
parseSuiMove path = do
  mast <- RawParse.parse path tree_sitter_sui_move
  return $ fmap translate mast

-- | Pretty-print a Sui Move IPS term back to source code
prettySuiMove :: MSuiMoveTerm SourceFileL -> String
prettySuiMove = Pretty.pretty . untranslate

instance ParseFile MSuiMoveSig where
  parseFile = parseSuiMove

instance Pretty MSuiMoveSig where
  pretty = prettySuiMove
