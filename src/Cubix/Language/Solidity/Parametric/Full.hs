{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Solidity.Parametric.Full () where
#else
module Cubix.Language.Solidity.Parametric.Full
  (
    module Cubix.Language.Solidity.Parametric.Full.Types
  , module Cubix.Language.Solidity.Parametric.Full.Trans
  ) where

import Cubix.Language.Solidity.Parametric.Full.Types
import Cubix.Language.Solidity.Parametric.Full.Trans
#endif