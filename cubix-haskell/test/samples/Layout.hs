-- Layout (zero-width external tokens), let/where/do and sections.
module Layout where

import Data.List (sortBy)

main :: IO ()
main = do
  let xs = [3, 1, 2]
      ys = map (* 2) xs
  print (sortBy compare ys)
  mapM_ print [x | x <- ys, x > 2]
  where
    unused = (`div` 2) <$> Just 4

-- let/in: the layout ends before "in", not at the end of the expression.
area :: Int -> Int
area w =
  let h = w + 1
      d = 2
  in w * h `div` d
