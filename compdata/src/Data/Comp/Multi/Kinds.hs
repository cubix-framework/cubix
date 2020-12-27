-- |
-- Module      :  Data.Comp.Kinds
-- Copyright   : (c) 2020 James Koppel
-- License     :  BSD3
--
-- This module provides kind-synonyms for the common kinds in this package.
-- All such kinds should be treated opaquely.
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Kinds
    ( Sort
    , Family
    , Node
    , Fragment
    , Signature
    ) where

type Sort      = *
type Family    = Sort -> *
type Node      = Family -> Family
type Fragment  = Family -> Family
type Signature = [Fragment]