-- | See comment in Bijective.hs for important note on lenses
--
-- This file implements associative lenses, a very important kind of lens, which, shockingly,
-- I have not found in the literature. An associative lens represents an associative operation
-- and an "inverse"

module Cubix.Rewriting.Lenses.Associative (
    -- * Associative data structure
    AssociativeDag
  , makeDag
  , dagFromList
  , dagFromAllSublists
  , dagSize
  , dagLookup
  , dagGetAllPaths

    -- * Associative lens definition
  , AssociativeLens(..)

    -- * Specific lenses
  , concatLens
  , extractFLens
  , concatTermLens
  ) where



-----------------------------------------------------------------------------

import           Data.Vector ( Vector, (!) )
import qualified Data.Vector as Vector

import Type.Reflection ( Typeable )

import Data.Comp.Multi ( (:-<:), TreeLike )

import Cubix.Language.Parametric.Syntax

import Cubix.Rewriting.Var

-------------------------------------------------------
---------------- Data structure -----------------------
-------------------------------------------------------


data AssociativeDag a = AssociativeDag { assocDagContents :: Vector (Vector [a])

                                       -- If DAG size is n, then there are edges from i to j for 0 <= i < j <= n
                                       -- (Note: That's inclusive)
                                       , assocDagSize     :: Int
                                       }

makeDag :: Int -> (Int -> Int -> [a]) -> AssociativeDag a
makeDag n f = AssociativeDag { assocDagContents = Vector.generate (n+1) (\i -> Vector.generate i (\j -> f i j))
                             , assocDagSize = n
                             }

dagFromList :: [a] -> AssociativeDag a
dagFromList l = makeDag len (\i j -> if i == 0 && j == len then l else [])
  where len = length l

dagFromAllSublists :: [a] -> AssociativeDag [a]
dagFromAllSublists l = makeDag (length l) (\i j -> [sublist l i j])
  where
    -- | Given a list xs = x_0, ..., x_n, `sublist xs a b` returns x_a,...,x_{b-1}
    sublist :: [a] -> Int -> Int -> [a]
    sublist xs a b = take (b-a) (drop a xs)

dagSize :: AssociativeDag a -> Int
dagSize = assocDagSize

dagLookup :: AssociativeDag a -> Int -> Int -> [a]
dagLookup d i j
  | i >= j        = error "Associative.dagLookup: Must have i < j"
  | i < 0         = error "Associative.dagLookup: Must have i <= 0"
  | j > dagSize d = error "Associative.dagLookup: Must have j <= DAG size"
  | otherwise     = (assocDagContents d) ! i ! j

mapDag :: (a -> b) -> AssociativeDag a -> AssociativeDag b
mapDag f d = d { assocDagContents = Vector.map (Vector.map (map f)) (assocDagContents d) }

-- | Gets the concatenation of elements on all paths in the DAG,
--   i.e.: the set of all things that it represents.
--
-- Exponential-time. Avoid.
dagGetAllPaths :: forall a. AssociativeDag a -> [[a]]
dagGetAllPaths d = go 0
  where
    len = dagSize d

    go :: Int -> [[a]]
    go n
      | n == len = [[]]
    go i = concat [cartesianAppend (map (:[]) $ dagLookup d i j) (go j) | j <- [(i+1)..len]]

    cartesianAppend :: (Monoid t) => [t] -> [t] -> [t]
    cartesianAppend xs ys = [x `mappend` y | x <- xs, y <- ys]

-------------------------------------------------------
------------------------- Lens ------------------------
-------------------------------------------------------

-- |
-- For some associative operation f, let b = f(a1, ..., aN)
-- An associative lens represents both f (recompose), and a way to
-- get all possible a1,...,aN such that the above equation holds (decompose).
--
-- Law: exists n. map recompose (dagGetAllPaths (decompose x)) == replicate n x
--
-- I.e.: For all decompositions, the recomposition is the original
--
-- TODO: Ask a lens expert if this already exists in the literature
data AssociativeLens a b = AssociativeLens { decompose :: b -> AssociativeDag a
                                           , recompose :: [a] -> b
                                           }


concatLens :: AssociativeLens [a] [a]
concatLens = AssociativeLens { decompose = dagFromAllSublists
                             , recompose = concat
                             }


extractFLens :: forall fs l. (ListF :-<: fs, ExtractF [] (RewritingTerm fs), TreeLike fs, Typeable l)
             => AssociativeLens (RewritingTerm fs l) (RewritingTerm fs [l])
extractFLens = AssociativeLens { decompose = dagFromList . extractF, recompose = insertF }

concatTermLens :: forall fs l. (ListF :-<: fs, ExtractF [] (RewritingTerm fs), TreeLike fs, Typeable l)
               => AssociativeLens (RewritingTerm fs [l]) (RewritingTerm fs [l])
concatTermLens = AssociativeLens { decompose = mapDag insertF . dagFromAllSublists . extractF
                                 , recompose = insertF . concat . map extractF
                                 }

