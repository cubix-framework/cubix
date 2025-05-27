module Main where

import Data.Map ( Map )

import Cubix.Language.Info

import Examples.Syntax
import Examples.Strat

prog1 :: Prog ExpL
prog1 = iPair (iConst 2) (iConst 3)

expProg :: Int -> Prog ExpL
expProg 0 = prog1
expProg n = iPair prev prev
  where
    prev = expProg (n-1)


main = do
  gen <- mkConcurrentSupplyLabelGen
  let prog1Labeled = labelProg gen prog1 :: ProgLab ExpL
  print $ (extractLabels prog1Labeled :: Map Label (ProgLab ExpL))
  print (countConsts $ labelProg gen $ expProg 25)
  putStrLn "Done"
