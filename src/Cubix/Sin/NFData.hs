module Cubix.Sin.NFData where

import Control.DeepSeq ( NFData(..) )

import Data.Comp.Multi ( Cxt )
import Data.Comp.Multi.Generic ( size )
import Data.Comp.Multi.HFoldable ( HFoldable(..) )



instance HFoldable f => NFData (Cxt h f a l) where
  rnf = rnf . size
