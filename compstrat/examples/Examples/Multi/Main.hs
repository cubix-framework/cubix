{-# LANGUAGE OverlappingInstances #-}

module Main where

import Examples.Multi.Syntax
import Examples.Multi.Strat

import Control.DeepSeq

prog1 :: ProgLab ExpL
prog1 = iAPair 2 (iAConst 0 2) (iAConst 1 3)

expProg :: Int -> ProgLab ExpL
expProg 0 = prog1
expProg n = iAPair 0 prev prev
  where
    prev = expProg (n-1)


main = do
  --print $ extractLabels prog1
  print (rnf $ countConsts $ expProg 25)
  putStrLn "Done"
