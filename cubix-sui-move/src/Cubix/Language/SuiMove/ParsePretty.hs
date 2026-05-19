-- | ParseFile and Pretty instances for Sui Move IPS
--
-- This module provides the interface for parsing Sui Move files directly
-- into the IPS (Incremental Parametric Syntax) representation.
module Cubix.Language.SuiMove.ParsePretty (
    parseSuiMove
  , parseSuiMoveTrackSources
  , prettySuiMove
  ) where

import Data.Comp.Multi (stripA)

import TreeSitter.SuiMove (tree_sitter_sui_move)

import Cubix.Language.Info (SourceSpan)
import Cubix.ParsePretty (ParseFile(..), Pretty(..), RootSort)

import Cubix.Language.SuiMove.IPS.Types (MSuiMoveSig, MSuiMoveTerm)
import Cubix.Language.SuiMove.IPS.Trans (translate, untranslate)
import Cubix.Language.SuiMove.Modularized (MoveSig, SourceFileL)
import Cubix.Language.SuiMove.RawParse qualified as RawParse
import Cubix.Language.SuiMove.Pretty qualified as Pretty

-- | Parse a Sui Move file into IPS representation
parseSuiMove :: FilePath -> IO (Maybe (MSuiMoveTerm (RootSort MSuiMoveSig)))
parseSuiMove = fmap (fmap (translate . stripA)) . parseSuiMoveTrackSources

-- | Parse a Sui Move file into a source-span-annotated Modularized AST.
--
-- Every Cubix layer carries a @Maybe SourceSpan@ annotation attached at
-- construction time by 'RawParse.parse'. Mirrors @JParse.parseFile@'s
-- behaviour for Java; the IPS-layer 'parseSuiMove' strips annotations
-- with 'stripA' and then runs 'translate'.
parseSuiMoveTrackSources
  :: FilePath
  -> IO (Maybe (RawParse.MoveTermAnn (Maybe SourceSpan) (RootSort MoveSig)))
parseSuiMoveTrackSources path = RawParse.parse path tree_sitter_sui_move

-- | Pretty-print a Sui Move IPS term back to source code
prettySuiMove :: MSuiMoveTerm SourceFileL -> String
prettySuiMove = Pretty.pretty . untranslate

instance ParseFile MSuiMoveSig where
  parseFile = parseSuiMove

instance Pretty MSuiMoveSig where
  pretty = prettySuiMove
