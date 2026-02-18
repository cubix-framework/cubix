module Main where

import Data.Char (toLower)
import Data.List (intercalate)

import Options.Applicative
  ( Parser, ParserInfo, execParser, fullDesc, help, helper, info, metavar
  , many, progDesc, strArgument, (<**>)
  )

import Examples.ListIdentifiers qualified as Ex1
import Examples.SortFunctions qualified as Ex2
import Examples.CountCalls qualified as Ex3
import Examples.SortByMutAssigns qualified as Ex4
import Examples.UnusedImports qualified as Ex5
import Examples.FunctionSize qualified as Ex6
import Examples.PublicNoReturn qualified as Ex7
import Examples.RenameIdent qualified as Ex8
import Examples.StructAbilities qualified as Ex9
import Examples.ConstantSummary qualified as Ex10

data Options = Options
  { exampleName :: String
  , inputArgs   :: [String]
  }

parserOptions :: Parser Options
parserOptions =
  Options
    <$> strArgument
      ( metavar "EXAMPLE"
          <> help ("Example to run: " ++ intercalate ", " exampleNames)
      )
    <*> many (strArgument
      ( metavar "ARGS..."
          <> help "Arguments for the example (typically file paths)"
      ))

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (parserOptions <**> helper)
    (fullDesc <> progDesc description)

exampleNames :: [String]
exampleNames =
  [ "list-identifiers"
  , "sort-functions"
  , "count-calls"
  , "sort-by-mut-assigns"
  , "unused-imports"
  , "function-size"
  , "public-no-return"
  , "rename-ident"
  , "struct-abilities"
  , "constant-summary"
  ]

description :: String
description = "Cubix Sui Move examples: 10 small programs demonstrating "
           ++ "analysis and transformation of Sui Move programs using compstrat."

downcase :: String -> String
downcase = map toLower

main :: IO ()
main = do
  Options name args <- execParser optionsInfo
  case downcase name of
    "list-identifiers"    -> Ex1.run args
    "sort-functions"      -> Ex2.run args
    "count-calls"         -> Ex3.run args
    "sort-by-mut-assigns" -> Ex4.run args
    "unused-imports"      -> Ex5.run args
    "function-size"       -> Ex6.run args
    "public-no-return"    -> Ex7.run args
    "rename-ident"        -> Ex8.run args
    "struct-abilities"    -> Ex9.run args
    "constant-summary"    -> Ex10.run args
    _ -> do
      putStrLn $ "Unknown example: " ++ name
      putStrLn $ "Available: " ++ intercalate ", " exampleNames
