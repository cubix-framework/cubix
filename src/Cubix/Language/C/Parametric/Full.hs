{-# LANGUAGE CPP #-}
#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.C.Parametric.Full () where
#else

module Cubix.Language.C.Parametric.Full
  (
    module Cubix.Language.C.Parametric.Full.Types
  , module Cubix.Language.C.Parametric.Full.Trans
  ) where

import Cubix.Language.C.Parametric.Full.Types
import Cubix.Language.C.Parametric.Full.Trans
#endif