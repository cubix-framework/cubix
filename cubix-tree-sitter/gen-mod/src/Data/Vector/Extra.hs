module Data.Vector.Extra where

import Data.Vector (Vector)
import Data.Vector qualified as Vector (uncons)

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
