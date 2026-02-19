{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Comp.Multi ( Term, unTerm, HFix(..), (:&:) )
import Text.Pretty.Simple ( pShowLightBg )
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL

import TreeSitter.SuiMove ( tree_sitter_sui_move )

import Cubix.Language.SuiMove.IPS ( translate, untranslate )
import Cubix.Language.SuiMove.Modularized ( MoveSig, MoveTerm, SourceFileL )
import Cubix.Language.SuiMove.RawParse qualified as RawParse

-- | Compare two terms and show where they differ
-- We pretty-print both and do a line-by-line comparison
compareTrees :: MoveTerm SourceFileL -> MoveTerm SourceFileL -> IO ()
compareTrees orig roundtripped = do
  let origStr = TL.unpack $ pShowLightBg orig
      rtStr   = TL.unpack $ pShowLightBg roundtripped
      origLines = lines origStr
      rtLines   = lines rtStr

  putStrLn $ "Original has " ++ show (length origLines) ++ " lines"
  putStrLn $ "Roundtripped has " ++ show (length rtLines) ++ " lines"

  -- Find first difference
  let pairs = zip3 [1::Int ..] origLines rtLines
      diffs = filter (\(_, a, b) -> a /= b) pairs

  case diffs of
    [] -> if length origLines == length rtLines
          then putStrLn "Trees are IDENTICAL"
          else putStrLn $ "Trees have same content but different line counts: "
                       ++ show (length origLines) ++ " vs " ++ show (length rtLines)
    ((lineNum, origLine, rtLine):_) -> do
      putStrLn $ "\nFirst difference at pretty-printed line " ++ show lineNum ++ ":"
      putStrLn $ "  ORIGINAL:     " ++ origLine
      putStrLn $ "  ROUNDTRIPPED: " ++ rtLine

      -- Show context: 3 lines before and after
      putStrLn "\n--- Context (original) ---"
      let startCtx = max 0 (lineNum - 4)
          endCtx = min (length origLines - 1) (lineNum + 2)
      mapM_ (\i -> putStrLn $ show (i+1) ++ ": " ++ (origLines !! i)) [startCtx..endCtx]

      putStrLn "\n--- Context (roundtripped) ---"
      let endCtxRt = min (length rtLines - 1) (lineNum + 2)
      mapM_ (\i -> putStrLn $ show (i+1) ++ ": " ++ (rtLines !! i)) [startCtx..endCtxRt]

      putStrLn $ "\nTotal differences: " ++ show (length diffs)

main :: IO ()
main = do
  let filepath = "../sui-move-test-corpus/deepbookv3/packages/margin_liquidation/sources/liquidation_vault.move"
  putStrLn $ "Parsing: " ++ filepath
  parsed <- RawParse.parse filepath tree_sitter_sui_move
  case parsed of
    Nothing -> putStrLn "Failed to parse file"
    Just orig -> do
      putStrLn "Parse succeeded"
      let roundtripped = untranslate . translate $ orig
      if orig == roundtripped
        then putStrLn "Roundtrip SUCCESS - trees are equal"
        else do
          putStrLn "Roundtrip FAILED - trees differ"
          putStrLn ""
          compareTrees orig roundtripped
