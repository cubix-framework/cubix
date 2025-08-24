{-# LANGUAGE RecordWildCards #-}
module Main where

import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info, metavar, progDesc, strArgument, (<**>))

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
main = do
  Options{..} <- execParser optionsInfo
  mast <- SuiMove.parse inputFile
  case mast of
    Just ast -> print ast
    Nothing -> putStrLn "something wont wrong"

  pure ()
