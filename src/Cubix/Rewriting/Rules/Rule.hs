{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE UndecidableInstances #-}


module Cubix.Rewriting.Rules.Rule (
    -- * Relations and Relata
    Relation
  , RewritingTerm

    -- * Rules
    -- ** Lenses
  , PartialBijection(..)
  , AssociativeLens(..)

    -- ** Rewrites
  , Rewrite
  , TermRewrite

    -- ** Rules
  , Premise
  , EPremise(..)
  , Rule

    -- ** Indices
  , RuleIndex(..)
  ) where

import Control.Arrow ( (***) )
import Control.Monad ( MonadPlus )
import Data.Map ( Map )
import Type.Reflection ( Typeable, TypeRep, typeRep)

import Data.Comp.Multi ( Cxt(Hole), ContextS, Term, E(..), (:-<:) )

import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax.Functor
import Cubix.Language.Parametric.Syntax.VarDecl
import Cubix.Rewriting.Rules.Matchable
import Cubix.Rewriting.Rules.Var

----------------------------------------------------------------------

data VarHole l
mkVar :: String -> VarHole l
mkVar = undefined

-----------------------------------------------------------
------------------------ Relations ------------------------
-----------------------------------------------------------

data Relation a b = Relation { relation_longName  :: String
                             , relation_shortName :: String

                             , relation_leftType  :: TypeRep a
                             , relation_rightType :: TypeRep b
                             }

data ERelation where
  ERelation :: (Matchable a, Matchable b) => Relation a b -> ERelation


type RewritingTerm  fs = ContextS fs VarHole
type ERewritingTerm fs = E (RewritingTerm fs)

type TermRelation fs gs l = Relation (RewritingTerm fs l) (RewritingTerm gs l)

-----------------------------------------------------------
-------------------------- Rules --------------------------
-----------------------------------------------------------

--------------------------
-------- Lenses
--------------------------


data PartialBijection a b = PartialBijection { putr :: a -> Maybe b
                                             , putl :: b -> Maybe a
                                             }


-- |
-- For some associative operation f, let a1, ..., aM = f(b1, ..., bN)
-- An associative lens represents both f (recompose), and a way to
-- get all possible a1, ..., aM such that the above equation holds.
--
-- Law: (decompose x >>= (return.recompose) >>= \r -> r == x) == return True
-- I.e.: For all decompositions, the recomposition is the original
--
-- Can be composed with a PartialBijection to get lenses between associative things
-- that are not lists
--
-- TODO: DUDE this time signature is way too general. Impossible to implement optimized matching like this
-- TODO: Ask a lens expert if this already exists in the literature
data AssociativeLens a b = AssociativeLens { decompose :: forall m. (MonadPlus m) => [a] -> m [b]
                                           , recompose :: [b] -> [a]
                                           }

--------------------------
-------- Rewrites
--------------------------

data Rewrite a b = Rewrite a (Relation a b) b

type TermRewrite fs gs l = Rewrite (RewritingTerm fs l) (RewritingTerm gs l)

--------------------------
-------- Rules
--------------------------

data Premise a b where
  RewritePremise     :: Rewrite a b                    -> Premise a b
  LensPremise        :: a -> PartialBijection a b -> b -> Premise a b

  AssociativePremise :: (Matchable b, Matchable c) => AssociativeLens a b -> [Premise b c] -> AssociativeLens d c -> Premise a d


data EPremise where
  EPremise :: (Matchable a, Matchable b) => Premise a b -> EPremise

-- Extra invariant: Rules are L-attributed. That is, all meta-variables
-- in the LHS of a premise must be in the LHS of a previous premise or of the conclusion
data Rule a b = (Rewrite a b) :- [EPremise]

type TermRewriteRule fs gs l = Rule (RewritingTerm fs l) (RewritingTerm gs l)

data ARule where
  ARule :: Rule a b -> ARule


--------------------------
-------- Indices
--------------------------

data RuleIndex = RuleIndex { lookupRulesRight :: forall a b. a -> Relation a b -> [Rule a b] -- Returns a list of rules that *might* match
                           , lookupRulesLeft  :: forall a b. Relation a b -> b -> [Rule a b] -- Returns a list of rules that *might* match
                           }


-------------------------------------------------------------
------------------- Desugaring relation ---------------------
-------------------------------------------------------------

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

class WithVars a x where
  mkVars :: a -> x

withVars :: (WithVars a x) => a -> (x -> b) -> b
withVars a f = f (mkVars a)

instance {-# OVERLAPPING #-} WithVars String (VarHole l) where
  mkVars a = mkVar a

instance {-# OVERLAPPING #-} WithVars (String, String) (VarHole l1, VarHole l2) where
  mkVars (a, b) = (mkVar a, mkVar b)

instance {-# OVERLAPPABLE #-} (WithVars a x, ConsTuple String a a', ConsTuple (VarHole l) x x') => WithVars a' x' where
  mkVars = uncurry consTuple . (mkVar *** mkVars) . unconsTuple

desugar :: (Typeable fs, Typeable gs, Typeable l) => TermRelation fs gs l
desugar = Relation { relation_longName  = "desugars_to"
                   , relation_shortName = ":~>"
                   , relation_leftType  = typeRep
                   , relation_rightType = typeRep
                   }


(~>) :: (Typeable fs, Typeable gs, Typeable l) => RewritingTerm fs l -> RewritingTerm gs l -> TermRewrite fs gs l
(~>) a b = Rewrite a desugar b

multidecRule1 :: ( MultiLocalVarDecl :-<: fs, SingleLocalVarDecl :-<: fs, SingleLocalVarDecl :-<: gs, ListF :-<: fs,
                   InjF fs MultiLocalVarDeclL l, InjF gs SingleLocalVarDeclL l,
                   Typeable fs, Typeable gs, Typeable l )
              => PartialBijection (RewritingTerm fs MultiLocalVarDeclCommonAttrsL, RewritingTerm fs LocalVarDeclAttrsL) (RewritingTerm gs LocalVarDeclAttrsL)
              -> TermRewriteRule fs gs l
multidecRule1 joinAttrs = withVars ("attrs1", "attrs2", "binder", "init", "attrsJoined") $ \(attrs1, attrs2, binder, init, attrsJoined) ->
      (iMultiLocalVarDecl (Hole attrs1) (SingletonF' (SingleLocalVarDecl' (Hole attrs2) (Hole binder) (Hole init))) ~> iSingleLocalVarDecl (Hole attrsJoined) (Hole binder) (Hole init))
      :-
      [ EPremise $ LensPremise (Hole attrs1, Hole attrs2) joinAttrs (Hole attrsJoined)
      ]
