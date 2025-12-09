{-# LANGUAGE RecordWildCards #-}
module Main where

import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info, metavar, progDesc, strArgument, (<**>))
import Text.Pretty.Simple
-- import GHC.Debug.Stub (withGhcDebug)

import Cubix.Language.SuiMove.ParsePretty qualified as SuiMove

data Options = Options
  { inputFile :: FilePath }

parserOptions :: Parser Options
parserOptions =
  Options
    <$> strArgument
      ( metavar "FILE"
          <> help "Input Sui Move file."
      )

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (parserOptions <**> helper)
    (fullDesc <> progDesc "Sui Move helper.")

main :: IO ()
-- Jakub 2025.10.23: uncomment for ghc-debug use
main = -- withGhcDebug $
  do
    Options{..} <- execParser optionsInfo
    mast <-
      SuiMove.parse inputFile
    case mast of
      Just ast -> pPrintLightBg ast
      Nothing -> putStrLn "something went wrong"

    pure ()
