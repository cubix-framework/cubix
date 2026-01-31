{-# LANGUAGE RecordWildCards #-}
module Main where

import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info, metavar, progDesc, strArgument, (<**>))
import Text.Pretty.Simple
-- import GHC.Debug.Stub (withGhcDebug)
import Cubix.Language.SuiMove.ParsePretty qualified as SuiMove
import Cubix.Language.SuiMove.IPS (translate, untranslate)
import TreeSitter.SuiMove (tree_sitter_sui_move)

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
    mast <- SuiMove.parse inputFile tree_sitter_sui_move
    ast <- case mast of
      Nothing -> error "Couldn't parse top level sort"
      Just a -> pure a
    putStrLn "Original:"
    pPrintLightBg ast
    let ips = translate ast

    putStrLn "Incremental Parametric:"
    pPrintLightBg ips

    putStrLn "Un-translated:"
    pPrintLightBg (untranslate ips)
    -- Nothing -> putStrLn "something went wrong"

    pure ()
