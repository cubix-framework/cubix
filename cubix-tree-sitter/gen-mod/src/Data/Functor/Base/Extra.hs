{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Functor.Base.Extra where

import Data.Vector (Vector)
import Data.Vector qualified as Vector (cons, empty, foldr, null, unfoldr, unsafeHead, unsafeTail)
import Prelude hiding (head, tail)

import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))

type instance Base (Vector a) = ListF a

instance Recursive (Vector a) where
  project v
    | Vector.null v = Nil
    | otherwise = Cons (Vector.unsafeHead v) (Vector.unsafeTail v)

  cata f = Vector.foldr (\x acc -> f (Cons x acc)) (f Nil)

  para f = go
    where
      go v
        | Vector.null v = f Nil
        | otherwise =
            let x = Vector.unsafeHead v
                xs = Vector.unsafeTail v
            in f (Cons x (xs, go xs))

instance Corecursive (Vector a) where
  embed Nil = Vector.empty
  embed (Cons x xs) = Vector.cons x xs
  
  ana coalg = Vector.unfoldr $ \seed ->
    case coalg seed of
      Nil -> Nothing
      Cons x seed' -> Just (x, seed')

  apo coalg = go
    where
      go seed = case coalg seed of
        Nil -> Vector.empty
        Cons x (Left v) -> Vector.cons x v
        Cons x (Right seed') -> Vector.cons x (go seed')
