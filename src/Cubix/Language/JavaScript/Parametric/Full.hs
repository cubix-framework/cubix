{-# LANGUAGE CPP #-}
#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.JavaScript.Parametric.Full () where
#else

module Cubix.Language.JavaScript.Parametric.Full
  (
    module Cubix.Language.JavaScript.Parametric.Full.Types
  , module Cubix.Language.JavaScript.Parametric.Full.Trans
  ) where

import Cubix.Language.JavaScript.Parametric.Full.Types
import Cubix.Language.JavaScript.Parametric.Full.Trans

#endif