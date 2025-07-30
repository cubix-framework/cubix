{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Applicative (Alternative (..))
import Data.Aeson (eitherDecodeFileStrict)
import Data.FileEmbed (embedFileRelative)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Options.Applicative (Parser, ParserInfo, execParser, flag', fullDesc, help, helper, info, long, metavar, optional, progDesc, short, strArgument, strOption, (<**>))
import TreeSitter.GenerateAst.Internal.CodeGen (Metadata (..), generateAst)

template :: Text
template = T.decodeUtf8 $(embedFileRelative "gen-ast/data/Ast.hs.template")

data Options = Options
  { inputFile :: FilePath
  , outputFile :: Maybe FilePath
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
  result <- either fail pure (generateAst metadata grammar "Ast.hs.template" template)
  maybe T.putStrLn T.writeFile outputFile result

