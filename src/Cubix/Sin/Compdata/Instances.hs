{-|
  Provides instances of EqHF and OrdHF for 'Data.Comp.Multi.:&:'.

  The reason we have not yet suggested these be merged into compdata is that it is unclear
  how annotations should in general be treated. In particular, for efficiency, we may at one point
  wish to use annotations and ignore terms for the EqHF and OrdHF instances.
 -}

module Cubix.Sin.Compdata.Instances (
  ) where

import Data.Comp.Multi.Equality ( EqHF(..) )
import Data.Comp.Multi.Ops ( (:&:)(..) )
import Data.Comp.Multi.Ordering ( OrdHF(..) )

instance (EqHF f, Eq a) => EqHF (f :&: a) where
  (v1 :&: c1) `eqHF` (v2 :&: c2) = (v1 `eqHF` v2) && (c1 == c2)

instance (OrdHF f, Ord a) => OrdHF (f :&: a) where
  (v1 :&: c1) `compareHF` (v2 :&: c2) = case v1 `compareHF` v2 of
                                          EQ -> c1 `compare` c2
                                          x -> x
                                      