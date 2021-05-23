

module Cubix.Rewriting.Rules.Matchable (
    Matchable(..)
  ) where


import Data.Comp.Multi ( Cxt )

-------------------------------------------------------------------



class Matchable a
instance Matchable (Cxt h f a i)

instance (Matchable a, Matchable b) => Matchable (a, b)