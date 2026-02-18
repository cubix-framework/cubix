{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.ConstrName
-- Copyright   :  (c) 2026 James Koppel
-- License     :  BSD3
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module lifts 'ConstrNameHF' to sums of higher-order functors.
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.ConstrName
    ( ConstrNameHF(..)
    ) where

import Data.Comp.Multi.Derive

$(derive [liftSum] [''ConstrNameHF])
