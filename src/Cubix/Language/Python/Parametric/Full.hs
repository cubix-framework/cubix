{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Python.Parametric.Full () where
#else

module Cubix.Language.Python.Parametric.Full
  (
    module Cubix.Language.Python.Parametric.Full.Types
  , module Cubix.Language.Python.Parametric.Full.Trans
  ) where

import Cubix.Language.Python.Parametric.Full.Types
import Cubix.Language.Python.Parametric.Full.Trans

#endif