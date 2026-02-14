{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.Identity ( runIdentity )

import Data.Char ( toLower )
import Data.List ( intercalate )

import Options.Applicative
  ( Parser, ParserInfo, execParser, fullDesc, help, helper, info, metavar
  , progDesc, strArgument, (<**>)
  )
import Text.Pretty.Simple ( pPrintLightBg )

import TreeSitter.SuiMove ( tree_sitter_sui_move )

import Cubix.Transformations.Hoist ( hoistDeclarations )

import Cubix.Language.SuiMove.IPS ( translate, untranslate )
import Cubix.Language.SuiMove.IPS.Hoist ()
import Cubix.Language.SuiMove.ParsePretty qualified as SuiMove

data Options = Options
  { action    :: String
  , inputFile :: FilePath
  }

parserOptions :: Parser Options
parserOptions =
  Options
    <$> strArgument
      ( metavar "ACTION"
          <> help ("Action to perform: " ++ intercalate ", " actionsList)
      )
    <*> strArgument
      ( metavar "FILE"
          <> help "Input Sui Move file."
      )

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (parserOptions <**> helper)
    (fullDesc <> progDesc description)

-- | Available actions
aPrintAst, aPrintIps, aHoist :: String
aPrintAst = "print-ast"
aPrintIps = "print-ips"
aHoist    = "hoist"

actionsList :: [String]
actionsList = [aPrintAst, aPrintIps, aHoist]

description :: String
description = "Sui Move parser and transformation tool for Cubix. Note: No pretty-printer available yet, outputs are AST representations."

downcase :: String -> String
downcase = map toLower

main :: IO ()
main = do
  Options{..} <- execParser optionsInfo
  mast <- SuiMove.parse inputFile tree_sitter_sui_move
  ast <- case mast of
    Nothing -> error "Couldn't parse file"
    Just a -> pure a

  let act = downcase action

  case act of
    _ | act == aPrintAst -> do
      -- Print the modularized AST
      putStrLn "Modularized AST:"
      pPrintLightBg ast

    _ | act == aPrintIps -> do
      -- Print the IPS representation
      let ips = translate ast
      putStrLn "Incremental Parametric Syntax:"
      pPrintLightBg ips

    _ | act == aHoist -> do
      -- Apply hoist transformation
      let ips = translate ast
      let hoisted = runIdentity $ hoistDeclarations ips
      let result = untranslate hoisted
      putStrLn "Hoisted (Modularized AST):"
      pPrintLightBg result

    _ -> do
      putStrLn $ "Unknown action: " ++ action
      putStrLn $ "Available actions: " ++ intercalate ", " actionsList
