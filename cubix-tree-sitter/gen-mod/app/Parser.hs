{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Applicative (Alternative (..))
import Data.Aeson (eitherDecodeFileStrict)
import Data.FileEmbed (embedFileRelative)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Options.Applicative (Parser, ParserInfo, execParser, flag', fullDesc, help, helper, info, long, metavar, optional, progDesc, short, strArgument, strOption, (<**>))
import TreeSitter.GenerateAst.Internal.CodeGen (Metadata (..), generateAst)
-- import Text.Pretty.Simple (pPrint)

template :: Text
template = T.decodeUtf8 $(embedFileRelative "gen-mod/data/ParsePretty.hs.template")

data Options = Options
  { inputFile :: FilePath
  , outputFile :: Maybe FilePath
  , tokenMapFile :: Maybe FilePath
  , metadata :: Metadata
  }

parserMetadata :: Parser Metadata
parserMetadata =
  Metadata
    <$> strOption
      ( short 's'
          <> long "start-rule-name"
          <> metavar "RULE_NAME"
          <> help "The start rule name."
      )
    <*> optional
      ( strOption
          ( short 'm'
              <> long "module-name"
              <> metavar "MODULE_NAME"
              <> help "Haskell module name."
          )
      )
    <*> parserPretty

parserPretty :: Parser Bool
parserPretty =
  flag'
    True
    ( long "pretty"
        <> help "Generate instances of Pretty."
    )
    <|> flag'
      False
      ( long "no-pretty"
          <> help "Do not generate instances of Pretty."
      )
    <|> pure True

parserOptions :: Parser Options
parserOptions =
  Options
    <$> strArgument
      ( metavar "GRAMMAR_FILE"
          <> help "Input file (grammar.json)."
      )
    <*> optional
      ( strOption
          ( short 'o'
              <> long "output"
              <> metavar "HASKELL_FILE"
              <> help "Output file."
          )
      )
    <*> optional
      ( strOption
          ( short 't'
              <> long "token-map"
              <> metavar "JSON_FILE"
              <> help "Hint which token to preserve in the syntax."
          )
      )
    <*> parserMetadata

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (parserOptions <**> helper)
    (fullDesc <> progDesc "Generate a Haskell module with an Ast for a tree-sitter grammar.")

main :: IO ()
main = do
  Options{..} <- execParser optionsInfo
  grammar <- either fail pure =<< eitherDecodeFileStrict inputFile
  tokenMap <- either fail pure =<< maybe (pure $ Right Map.empty) eitherDecodeFileStrict tokenMapFile
  -- pPrint (grammar :: Grammar)
  result <- either fail pure (generateAst metadata grammar "Ast.hs.template" template tokenMap)
  maybe T.putStrLn T.writeFile outputFile result

