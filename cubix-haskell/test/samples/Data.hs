-- Data declarations, constructor patterns, records and operators.
module Data (Tree (..), insert, size) where

data Tree = Leaf | Node Tree Int Tree
  deriving (Show, Eq)

data Account = Account { owner :: String, balance :: Int }
  deriving Show

insert :: Int -> Tree -> Tree
insert x Leaf = Node Leaf x Leaf
insert x t@(Node l v r)
  | x < v = Node (insert x l) v r
  | x > v = Node l v (insert x r)
  | otherwise = t

size :: Tree -> Int
size t = case t of
  Leaf -> 0
  Node l _ r -> size l + 1 + size r

deposit :: Int -> Account -> Account
deposit n acc = acc { balance = balance acc + n }
