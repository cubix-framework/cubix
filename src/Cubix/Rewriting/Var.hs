{-# LANGUAGE UndecidableInstances #-}

module Cubix.Rewriting.Var (
    RewritingTerm

  , withRawVars

  , withUniSigVars

  , withVars1
  , withVars2
  , withVars3
  , withVars4
  , withVars5
  , withVars6
  , withVars7
  ) where

import Control.Arrow ( (***) )

import Data.Comp.Multi ( Cxt(Hole), ContextS )

---------------------------------------------------------------------

data VarId = VarId { varName :: String }
  deriving (Eq, Ord, Show)

data TermVar l = TermVar { termVar_var :: VarId }
  deriving ( Eq, Ord, Show )

-- | A `CtxVar i j` is a context of sort `j` all of whose holes have sort `i`
--
--   Implementor's note: Keep this i parameter hidden; this type signature
--   will need to change to support contexts with multiple heterogeneous holes
data CtxVar i j = CtxVar VarId
  deriving (Eq, Ord, Show)

data Ctx i j = OneHoledCtx  (CtxVar i j)
             | ManyHoledCtx (CtxVar i j)

data VarHole fs l where
  TermVarHole :: TermVar l                        -> VarHole fs l
  PlugCtx     :: CtxVar k l -> RewritingTerm fs k -> VarHole fs l


mkVar :: String -> VarHole fs l
mkVar s = TermVarHole $ TermVar $ VarId s

type RewritingTerm fs = ContextS fs (VarHole fs)

---------------------------------------------------------------------
-------------------------- withVars ---------------------------------
---------------------------------------------------------------------

------------------
---- ConsTuple
------------------

-- | This section is some type-level programming to support WithRawVars


class ConsTuple a b c | a b -> c, c -> a b where
  consTuple   :: a -> b -> c
  unconsTuple :: c -> (a, b)

instance ConsTuple a (b, c) (a, b, c) where
  consTuple a (b, c)    = (a, b, c)
  unconsTuple (a, b, c) = (a, (b, c))

instance ConsTuple a (b, c, d) (a, b, c, d) where
  consTuple a (b, c, d)    = (a, b, c, d)
  unconsTuple (a, b, c, d) = (a, (b, c, d))

instance ConsTuple a (b, c, d, e) (a, b, c, d, e) where
  consTuple a (b, c, d, e)    = (a, b, c, d, e)
  unconsTuple (a, b, c, d, e) = (a, (b, c, d, e))

instance ConsTuple a (b, c, d, e, f) (a, b, c, d, e, f) where
  consTuple a (b, c, d, e, f)    = (a, b, c, d, e, f)
  unconsTuple (a, b, c, d, e, f) = (a, (b, c, d, e, f))

------------------
---- withRawVars
------------------

class WithRawVars a x where
  mkRawVars :: a -> x

withRawVars :: (WithRawVars a x) => a -> (x -> b) -> b
withRawVars a f = f (mkRawVars a)

instance {-# OVERLAPPING #-} WithRawVars String (VarHole fs l) where
  mkRawVars a = mkVar a

instance {-# OVERLAPPING #-} WithRawVars (String, String) (VarHole fs l1, VarHole fs l2) where
  mkRawVars (a, b) = (mkVar a, mkVar b)

instance {-# OVERLAPPABLE #-} (WithRawVars a x, ConsTuple String a a', ConsTuple (VarHole fs l) x x') => WithRawVars a' x' where
  mkRawVars = uncurry consTuple . (mkVar *** mkRawVars) . unconsTuple

------------------
---- withUniSigVars
------------------

-- | TODO: Given a proper name, can actually be merged with WithRawVars. Should it?

class WithUniSigVars a x where
  mkUniSigVars :: a -> x

withUniSigVars :: (WithUniSigVars a x) => a -> (x -> b) -> b
withUniSigVars a f = f (mkUniSigVars a)

instance {-# OVERLAPPING #-} WithUniSigVars String (RewritingTerm fs l) where
  mkUniSigVars a = Hole $ mkVar a


instance {-# OVERLAPPING #-} WithUniSigVars (String, String) (RewritingTerm fs l1, RewritingTerm gs l2) where
  mkUniSigVars (a, b) = (Hole $ mkVar a, Hole $ mkVar b)

instance {-# OVERLAPPABLE #-} (WithUniSigVars a x, ConsTuple String a a', ConsTuple (RewritingTerm fs l) x x') => WithUniSigVars a' x' where
  mkUniSigVars = uncurry consTuple . ((Hole . mkVar) *** mkUniSigVars) . unconsTuple

------------------
---- withVars
------------------

-- | 5/23/2021: I believe that the lack of impredicative polymorphism
--   makes it genuinely impossible to generalize withVars. Doing so would
--   require a type variable (either in a function signature of typeclass signature)
--   which gets expanded to a tuple/function containing several distinct quantified variables.
--
--   Using a newtype wrapper defeats the purpose (convenience/brevity)
--   of having this compared to using withRawVars.


withVars1 :: String -> ((forall fs. RewritingTerm fs l1) -> b) -> b
withVars1 (a) f = f (Hole $ mkVar a)

withVars2 :: (String, String) -> ((forall fs. RewritingTerm fs l1) -> (forall fs. RewritingTerm fs l2) -> b) -> b
withVars2 (a, b) f = f (Hole $ mkVar a) (Hole $ mkVar b)

withVars3 :: (String, String, String) -> ((forall fs. RewritingTerm fs l1) -> (forall fs. RewritingTerm fs l2) -> (forall fs. RewritingTerm fs l3) -> b) -> b
withVars3 (a, b, c) f = f (Hole $ mkVar a) (Hole $ mkVar b) (Hole $ mkVar c)

withVars4 :: (String, String, String, String) -> ((forall fs. RewritingTerm fs l1) -> (forall fs. RewritingTerm fs l2) -> (forall fs. RewritingTerm fs l3) -> (forall fs. RewritingTerm fs l4) -> b) -> b
withVars4 (a, b, c, d) f = f (Hole $ mkVar a) (Hole $ mkVar b) (Hole $ mkVar c) (Hole $ mkVar d)

withVars5 :: (String, String, String, String, String) -> ((forall fs. RewritingTerm fs l1) -> (forall fs. RewritingTerm fs l2) -> (forall fs. RewritingTerm fs l3) -> (forall fs. RewritingTerm fs l4) -> (forall fs. RewritingTerm fs l5) -> b) -> b
withVars5 (a, b, c, d, e) f = f (Hole $ mkVar a) (Hole $ mkVar b) (Hole $ mkVar c) (Hole $ mkVar d) (Hole $ mkVar e)

withVars6 :: (String, String, String, String, String, String) -> ((forall fs. RewritingTerm fs l1) -> (forall fs. RewritingTerm fs l2) -> (forall fs. RewritingTerm fs l3) -> (forall fs. RewritingTerm fs l4) -> (forall fs. RewritingTerm fs l5) -> (forall fs. RewritingTerm fs l6) -> b) -> b
withVars6 (a, b, c, d, e, g) f = f (Hole $ mkVar a) (Hole $ mkVar b) (Hole $ mkVar c) (Hole $ mkVar d) (Hole $ mkVar e) (Hole $ mkVar g)

withVars7 :: (String, String, String, String, String, String, String) -> ((forall fs. RewritingTerm fs l1) -> (forall fs. RewritingTerm fs l2) -> (forall fs. RewritingTerm fs l3) -> (forall fs. RewritingTerm fs l4) -> (forall fs. RewritingTerm fs l5) -> (forall fs. RewritingTerm fs l6) -> (forall fs. RewritingTerm fs l7) -> b) -> b
withVars7 (a, b, c, d, e, g, h) f = f (Hole $ mkVar a) (Hole $ mkVar b) (Hole $ mkVar c) (Hole $ mkVar d) (Hole $ mkVar e) (Hole $ mkVar g) (Hole $ mkVar h)
