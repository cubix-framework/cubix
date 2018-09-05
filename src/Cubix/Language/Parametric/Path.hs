{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Cubix.Language.Parametric.Path
  (
    Path
  , emptyPath
  , isEmptyPath
  , parentPath
  , pathDistance
  , getChild
  , rewriteChild
  , followPath
  , rewriteAtPathM
  , getAncestors
  , getPaths
  , searchParent
  ) where

import Control.Monad ( (=<<) )

import Data.Comp.Multi ( Cxt(..), Term, Alg, cata, K(..), E(..), runE, (:&:), hfoldMap, HTraversable(..) )
import Data.Comp.Multi.Mapping ( Numbered(..), number )
import Data.Comp.Multi.Strategy.Classification ( DynCase, caseE )

import Data.List ( find )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Monoid ( First(..) )

import Cubix.Language.Info

import Cubix.Sin.Compdata.Annotation ( getAnn' )

--------------------------------------------------------------------------------

newtype Path = Path {getPath :: [Int]}
 deriving (Eq, Show)

instance Ord Path where
  compare (Path p1) (Path p2) = reverse p1 `compare` reverse p2

emptyPath :: Path
emptyPath = Path []

isEmptyPath :: Path -> Bool
isEmptyPath = (==[]) . getPath

parentPath :: Path -> Path
parentPath (Path [])    = error "Cannot take parent of empty path"
parentPath (Path (_:p)) = Path p

-- A crappy but easy-to-implement metric for the distance between two nodes,
-- as represented by their paths
--
-- Note that lists of statements are represented as cons cells, so this will be
-- approximately linear in the line distance for things drawn from the same block
revPathDistance :: [Int] -> [Int] -> Int
revPathDistance []     ys      = length ys
revPathDistance xs     []      = length xs
revPathDistance (x:xs) (y:ys)
                   | x == y    = revPathDistance xs ys
                   | otherwise = length (x:xs) + length (y:ys)

pathDistance :: Path -> Path -> Int
pathDistance p1 p2 = revPathDistance (reverse $ getPath p1) (reverse $ getPath p2)

getChild :: (HTraversable f) => Term f i -> Int -> Maybe (E (Term f))
getChild (Term t) i = getFirst (hfoldMap eqP (number t))
  where
    eqP (Numbered j x)
          | i == j    = First (Just (E x))
          | otherwise = First Nothing


rewriteChild :: (HTraversable f, Applicative m) => Int -> Term f i -> (forall j. Term f j -> m (Term f j)) -> m (Term f i)
rewriteChild i (Term t) f = Term <$> htraverse rw (number t)
  where
    rw (Numbered j x)
          | i == j    = f x
          | otherwise = pure x

followRevPath :: (HTraversable f) => [Int] -> Term f i -> Maybe (E (Term f))
followRevPath []     t = Just (E t)
followRevPath (i:is) t = runE (followRevPath is) =<< getChild t i

getAncestors :: (HTraversable f) => Path -> Term f i -> [E (Term f)]
getAncestors (Path p) t = go (reverse p) t
  where
    go :: (HTraversable f) => [Int] -> Term f i -> [E (Term f)]
    go []     t = [E t]
    go (i:is) t = case getChild t i of
                    Just (E x) -> (E t) : go is x
                    Nothing    -> [E t]

followPath :: (HTraversable f) => Path -> Term f i -> Maybe (E (Term f))
followPath p t = followRevPath (reverse $ getPath p) t

rewriteAtPathM :: (HTraversable f, Applicative m) => (forall j. Term f j -> m (Term f j)) -> Term f i -> Path -> m (Term f i)
rewriteAtPathM f t (Path p) = go f (reverse p) t
  where
    go :: (HTraversable f, Applicative m) =>  (forall j. Term f j -> m (Term f j)) -> [Int] -> Term f i -> m (Term f i)
    go f []     x = f x
    go f (i:is) x = rewriteChild i x (go f is)

pathAlg :: forall f f'. (HTraversable f) => Alg (f :&: Label) (K ([Int] -> Map Label Path))
pathAlg t = K $ \path -> Map.insert lab (Path path) (childPaths path)
  where
    lab = getAnn' t
    tInd = number t
    childPaths path = hfoldMap (\(Numbered i kf) -> (unK kf) (i:path)) tInd

getPaths :: (HTraversable f) => TermLab f i -> Map Label Path
getPaths t = unK (cata pathAlg t) []

searchParent :: (HTraversable f) => (forall i. Term f i -> Bool) -> Term f l -> Path -> Maybe (E (Term f))
searchParent f prog path = find (runE f) (reverse $ getAncestors path prog)