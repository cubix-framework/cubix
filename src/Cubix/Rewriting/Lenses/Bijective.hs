-- | Bijective Lenses
--  
-- * A general note on lenses in this package
--
-- THESE ARE NOT LIKE NORMAL HASKELL LENSES
-- 
-- "lens" in Haskell has come to mean a getter and a setter, usually for the field of a structure
-- But that is but a very special (and easy) case of the defenition of a lens, and not what
-- specialized lens researchers have in mind.
--
-- Lenses allow for keeping two different representations of data in synch.
-- And these representations can be very different.
--
-- So, when thinking of lenses in general, think of the following as a better canonical example:
-- getting and editing the min of a list
--  
--
--     > get minLens [4, 1, 3, 5] -- 1
--     > put minLens [4, 1, 3, 5] 2 -- [4, 2, 3, 5]
--     > put minLens [4, 1, 3, 5] 4 -- [4, 4, 4, 5]
--
-- * This package
--
-- This package features bijections and partial bijections, a very simple case of lenses requiring
-- no complement.

module Cubix.Rewriting.Lenses.Bijective (
    PartialBijection(..)
  , invert
  , listTerm
  ) where

import Type.Reflection ( Typeable )

import Data.Comp.Multi ( CxtS, TreeLike, (:-<:) )

import Cubix.Language.Parametric.Syntax

------------------------------------------------------------

data PartialBijection a b = PartialBijection { putr :: a -> Maybe b
                                             , putl :: b -> Maybe a
                                             }

invert :: PartialBijection a b -> PartialBijection b a
invert (PartialBijection {putr, putl}) = PartialBijection {putr = putl, putl = putr}

listTerm :: (ListF :-<: fs, ExtractF [] (CxtS h fs a), TreeLike fs, Typeable l)
         => PartialBijection (CxtS h fs a [l]) [CxtS h fs a l]
listTerm = PartialBijection { putr = Just . extractF, putl = Just . insertF}
