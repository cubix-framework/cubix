{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Applicative (Alternative (..))
import Data.Set qualified as Set (fromList)
import Data.Text (Text)
import Data.Text.Encoding qualified as T (decodeUtf8)
import Data.Text.IO qualified as T (putStrLn, writeFile)

import Data.Aeson (eitherDecodeFileStrict)
import Data.FileEmbed (embedFileRelative)
import Options.Applicative (Parser, ParserInfo, execParser, flag', fullDesc, help, helper, info, long, metavar, optional, progDesc, short, strArgument, strOption, (<**>))

import TreeSitter.Generate.Render
import TreeSitter.Grammar
import TreeSitter.Grammar.Transform

template :: Text
template = T.decodeUtf8 $(embedFileRelative "gen-mod/data/Modularized.hs.template")

data Options = Options
  { inputFile :: FilePath
  , outputFile :: Maybe FilePath
  , tokenMapFile :: Maybe FilePath
  , keepText :: [Text]
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
    <*> many
      ( strOption
          ( long "keep-text"
              <> metavar "EXTERNAL"
              <> help "A hidden external token that carries text (e.g. Python's _string_content). Rules using it keep their source text. Repeatable."
          )
      )
    <*> parserMetadata

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (parserOptions <**> helper)
    (fullDesc <> progDesc "Generate a Haskell module with Cubix Modularized Syntax for a tree-sitter grammar.")

main :: IO ()
main = do
  Options { inputFile, outputFile, keepText, metadata } <- execParser optionsInfo
  (grammar :: Grammar) <- either fail pure =<< eitherDecodeFileStrict inputFile
  let grammar' = transform TransformOptions { keepTextExternals = Set.fromList keepText } grammar
      -- nodes = Map.mapWithKey (topRuleToNode grammar'.rules) grammar'.rules

  -- pPrintStringLightBg "==== RULES ===="
  -- pPrintLightBg grammar'.rules
  -- pPrintStringLightBg "==== NODES ===="
  -- pPrintLightBg nodes

  result <- either fail pure (renderSyntax metadata grammar' "Modularized.hs.template" template)
  maybe T.putStrLn T.writeFile outputFile result

