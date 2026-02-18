-- | Shared utilities for Sui Move examples.
module Examples.Shared
  ( parseSuiMove
  , parseSuiMoveOrDie
  , prettySuiMove
  , parseDirectory
  ) where

import Control.Monad (forM)

import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)

import Cubix.ParsePretty (ParseFile(..))

import Cubix.Language.SuiMove.IPS.Types (MSuiMoveSig, MSuiMoveTerm)
import Cubix.Language.SuiMove.IPS.Trans (untranslate)
import Cubix.Language.SuiMove.Modularized (SourceFileL)
import Cubix.Language.SuiMove.ParsePretty ()
import Cubix.Language.SuiMove.Pretty qualified as Pretty

parseSuiMove :: FilePath -> IO (Maybe (MSuiMoveTerm SourceFileL))
parseSuiMove = parseFile @MSuiMoveSig

parseSuiMoveOrDie :: FilePath -> IO (MSuiMoveTerm SourceFileL)
parseSuiMoveOrDie path = do
  result <- parseSuiMove path
  case result of
    Nothing -> error $ "Failed to parse: " ++ path
    Just t  -> pure t

prettySuiMove :: MSuiMoveTerm SourceFileL -> String
prettySuiMove = Pretty.pretty . untranslate

parseDirectory :: FilePath -> IO [(FilePath, MSuiMoveTerm SourceFileL)]
parseDirectory dir = do
  entries <- listDirectory dir
  let moveFiles = filter (\f -> takeExtension f == ".move") entries
  results <- forM moveFiles $ \f -> do
    let path = dir </> f
    isFile <- doesFileExist path
    if isFile
      then do
        mt <- parseSuiMove path
        pure $ case mt of
          Just t  -> [(path, t)]
          Nothing -> []
      else pure []
  pure (concat results)
