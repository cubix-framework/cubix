{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.Identity ( runIdentity )

import Data.Char ( toLower )
import Data.List ( intercalate )

import Options.Applicative
  ( Parser, ParserInfo, execParser, fullDesc, help, helper, info, metavar
  , progDesc, strArgument, (<**>)
  )
import System.IO ( hClose )
import System.IO.Temp ( withSystemTempFile )
import Text.Pretty.Simple ( pPrintLightBg )

import TreeSitter.SuiMove ( tree_sitter_sui_move )

import Cubix.ParsePretty ( ParseFile(..) )
import Cubix.Transformations.Hoist ( hoistDeclarations )

import Cubix.Language.SuiMove.IPS ( untranslate, MSuiMoveSig )
import Cubix.Language.SuiMove.IPS.Hoist ()
import Cubix.Language.SuiMove.ParsePretty ()  -- Import for ParseFile instance
import Cubix.Language.SuiMove.Pretty qualified as Pretty
import Cubix.Language.SuiMove.RawParse qualified as RawParse

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
aPrintAst, aPrintIps, aHoist, aPretty, aRoundTrip :: String
aPrintAst  = "print-ast"
aPrintIps  = "print-ips"
aHoist     = "hoist"
aPretty    = "pretty"
aRoundTrip = "round-trip"

actionsList :: [String]
actionsList = [aPrintAst, aPrintIps, aHoist, aPretty, aRoundTrip]

description :: String
description = "Sui Move parser and transformation tool for Cubix."

downcase :: String -> String
downcase = map toLower

main :: IO ()
main = do
  Options{..} <- execParser optionsInfo

  let act = downcase action

  case act of
    _ | act == aPrintAst -> do
      -- Parse using tree-sitter directly and print the modularized AST
      mast <- RawParse.parse inputFile tree_sitter_sui_move
      ast <- case mast of
        Nothing -> error "Couldn't parse file"
        Just a -> pure a
      putStrLn "Modularized AST:"
      pPrintLightBg ast

    _ | act == aPrintIps -> do
      -- Use ParseFile typeclass to get IPS representation directly
      mips <- parseFile @MSuiMoveSig inputFile
      ips <- case mips of
        Nothing -> error "Couldn't parse file"
        Just a -> pure a
      putStrLn "Incremental Parametric Syntax:"
      pPrintLightBg ips

    _ | act == aHoist -> do
      -- Use ParseFile typeclass for parsing, then apply hoist transformation
      mips <- parseFile @MSuiMoveSig inputFile
      ips <- case mips of
        Nothing -> error "Couldn't parse file"
        Just a -> pure a
      let hoisted = runIdentity $ hoistDeclarations ips
      let result = untranslate hoisted
      putStrLn (Pretty.pretty result)

    _ | act == aPretty -> do
      -- Parse and pretty-print the Modularized AST
      mast <- RawParse.parse inputFile tree_sitter_sui_move
      ast <- case mast of
        Nothing -> error "Couldn't parse file"
        Just a -> pure a
      putStrLn (Pretty.pretty ast)

    _ | act == aRoundTrip -> do
      -- Test round-trip property: parse(pretty(parse(x))) == parse(x)
      mast <- RawParse.parse inputFile tree_sitter_sui_move
      ast <- case mast of
        Nothing -> error "Couldn't parse file"
        Just a -> pure a
      let prettyPrinted = Pretty.pretty ast
      -- Write to temp file and re-parse
      withSystemTempFile "roundtrip.move" $ \tmpPath tmpHandle -> do
        hClose tmpHandle
        writeFile tmpPath prettyPrinted
        mast2 <- RawParse.parse tmpPath tree_sitter_sui_move
        case mast2 of
          Nothing -> do
            putStrLn "Round-trip FAILED: Could not re-parse pretty-printed output"
            putStrLn "Pretty-printed output:"
            putStrLn prettyPrinted
          Just ast2 -> do
            if ast == ast2
              then putStrLn "Round-trip SUCCESS"
              else do
                putStrLn "Round-trip FAILED: ASTs differ"
                putStrLn "Pretty-printed output:"
                putStrLn prettyPrinted

    _ -> do
      putStrLn $ "Unknown action: " ++ action
      putStrLn $ "Available actions: " ++ intercalate ", " actionsList
