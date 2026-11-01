module Data.Vector.Extra where

import Data.Vector (Vector)
import Data.Vector qualified as Vector (ifilter, notElem, take, uncons)

uncons2 :: Vector a -> Maybe (a, a, Vector a)
uncons2 as
  | Just (first, as') <- Vector.uncons as
  , Just (second, rest) <- Vector.uncons as'
  = Just (first, second, rest)
  | otherwise = Nothing

uncons3 :: Vector a -> Maybe (a, a, a, Vector a)
uncons3 as
  | Just (first, as') <- Vector.uncons as
  , Just (second, as'') <- Vector.uncons as'
  , Just (third, rest) <- Vector.uncons as''
  = Just (first, second, third, rest)
  | otherwise = Nothing

-- | Removes duplicates, keeping the first occurrence of each element.
-- Like 'Data.List.nub', quadratic, but needs only 'Eq'.
nub :: Eq a => Vector a -> Vector a
nub as = Vector.ifilter (\i a -> a `Vector.notElem` Vector.take i as) as
