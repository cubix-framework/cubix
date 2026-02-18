--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi
-- Copyright   :  Originals (c) 2011 Patrick Bahr; modifications (c) 2017 Jmase koppel
-- License     :  BSD3
--
-- This module defines the infrastructure necessary to use
-- /Generalised Compositional Data Types/. Generalised Compositional Data Types
-- is an extension of Compositional Data Types with mutually recursive
-- data types, and more generally GADTs. Examples of usage are bundled with the
-- package in the library @examples\/Examples\/Multi@.
--
-- This is a fork of Patrick Bahrs @compdata@ package. The chief differences
-- are that the representation of sums has been replaced with a version that uses constant memory,
-- and the portions of the library for single-sorted terms have been removed to speed compilation times.
-- There are several minor changes and additions to the utility functions of this library.
--
--------------------------------------------------------------------------------
module Data.Comp.Multi (
    module Data.Comp.Multi.Algebra
  , module Data.Comp.Multi.Annotation
  , module Data.Comp.Multi.Equality
  , module Data.Comp.Multi.Generic
  , module Data.Comp.Multi.HFoldable
  , module Data.Comp.Multi.HFunctor
  , module Data.Comp.Multi.HTraversable
  , module Data.Comp.Multi.Kinds
  , module Data.Comp.Multi.Ops
  , module Data.Comp.Multi.Ordering
  , module Data.Comp.Multi.Show
  , module Data.Comp.Multi.Sum
  , module Data.Comp.Multi.Term
  , module Data.Comp.Dict
    ) where

import Data.Comp.Multi.Algebra
import Data.Comp.Multi.Annotation
import Data.Comp.Multi.Equality
import Data.Comp.Multi.Generic
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.Kinds
import Data.Comp.Multi.Ops
import Data.Comp.Multi.Ordering
import Data.Comp.Multi.Show
import Data.Comp.Multi.Sum
import Data.Comp.Multi.Term
import Data.Comp.Dict

{- $ex1
The example illustrates how to use generalised compositional data types
to implement a small expression language, with a sub language of values, and
an evaluation function mapping expressions to values.

The following language extensions are needed in order to run the example:
@TemplateHaskell@, @TypeOperators@, @MultiParamTypeClasses@,
@FlexibleInstances@, @FlexibleContexts@, and @UndecidableInstances@,
@GADTs@. Besides, GCH 7 is required.

> import Data.Comp.Multi
> import Data.Comp.Multi.Show ()
> import Data.Comp.Multi.Derive
>
> -- Signature for values and operators
> data Value e l where
>   Const  ::        Int -> Value e Int
>   Pair   :: e s -> e t -> Value e (s,t)
> data Op e l where
>   Add, Mult  :: e Int -> e Int   -> Op e Int
>   Fst        ::          e (s,t) -> Op e s
>   Snd        ::          e (s,t) -> Op e t
>
> -- Signature for the simple expression language
> type Sig = Op :+: Value
>
> -- Derive boilerplate code using Template Haskell (GHC 7 needed)
> $(derive [makeHFunctor, makeHShowF, makeHEqF, smartConstructors]
>          [''Value, ''Op])
>
> -- Term evaluation algebra
> class Eval f v where
>   evalAlg :: Alg f (Term v)
>
> instance (Eval f v, Eval g v) => Eval (f :+: g) v where
>   evalAlg (Inl x) = evalAlg x
>   evalAlg (Inr x) = evalAlg x
>
> -- Lift the evaluation algebra to a catamorphism
> eval :: (HFunctor f, Eval f v) => Term f :-> Term v
> eval = cata evalAlg
>
> instance (Value :<: v) => Eval Value v where
>   evalAlg = inject
>
> instance (Value :<: v) => Eval Op v where
>   evalAlg (Add x y)  = iConst $ (projC x) + (projC y)
>   evalAlg (Mult x y) = iConst $ (projC x) * (projC y)
>   evalAlg (Fst x)    = fst $ projP x
>   evalAlg (Snd x)    = snd $ projP x
>
> projC :: (Value :<: v) => Term v Int -> Int
> projC v = case project v of Just (Const n) -> n
>
> projP :: (Value :<: v) => Term v (s,t) -> (Term v s, Term v t)
> projP v = case project v of Just (Pair x y) -> (x,y)
>
> -- Example: evalEx = iConst 2
> evalEx :: Term Value Int
> evalEx = eval (iFst $ iPair (iConst 2) (iConst 1) :: Term Sig Int)
-}

{- $ex2
The example illustrates how to use generalised compositional data types to
implement a small expression language, with a sub language of values, and a
monadic evaluation function mapping expressions to values.

The following language extensions are needed in order to run the example:
@TemplateHaskell@, @TypeOperators@, @MultiParamTypeClasses@,
@FlexibleInstances@, @FlexibleContexts@, and @UndecidableInstances@,
@GADTs@. Besides, GCH 7 is required.

> import Data.Comp.Multi
> import Data.Comp.Multi.Show ()
> import Data.Comp.Multi.Derive
> import Control.Monad (liftM)
>
> -- Signature for values and operators
> data Value e l where
>   Const  ::        Int -> Value e Int
>   Pair   :: e s -> e t -> Value e (s,t)
> data Op e l where
>   Add, Mult  :: e Int -> e Int   -> Op e Int
>   Fst        ::          e (s,t) -> Op e s
>   Snd        ::          e (s,t) -> Op e t
>
> -- Signature for the simple expression language
> type Sig = Op :+: Value
>
> -- Derive boilerplate code using Template Haskell (GHC 7 needed)
> $(derive [makeHFunctor, makeHTraversable, makeHFoldable,
>           makeHEqF, makeHShowF, smartConstructors]
>          [''Value, ''Op])
>
> -- Monadic term evaluation algebra
> class EvalM f v where
>   evalAlgM :: AlgM Maybe f (Term v)
>
> instance (EvalM f v, EvalM g v) => EvalM (f :+: g) v where
>   evalAlgM (Inl x) = evalAlgM x
>   evalAlgM (Inr x) = evalAlgM x
>
> evalM :: (HTraversable f, EvalM f v) => Term f l -> Maybe (Term v l)
> evalM = cataM evalAlgM
>
> instance (Value :<: v) => EvalM Value v where
>   evalAlgM = return . inject
>
> instance (Value :<: v) => EvalM Op v where
>   evalAlgM (Add x y)  = do n1 <- projC x
>                            n2 <- projC y
>                            return $ iConst $ n1 + n2
>   evalAlgM (Mult x y) = do n1 <- projC x
>                            n2 <- projC y
>                            return $ iConst $ n1 * n2
>   evalAlgM (Fst v)    = liftM fst $ projP v
>   evalAlgM (Snd v)    = liftM snd $ projP v
>
> projC :: (Value :<: v) => Term v Int -> Maybe Int
> projC v = case project v of
>             Just (Const n) -> return n; _ -> Nothing
>
> projP :: (Value :<: v) => Term v (a,b) -> Maybe (Term v a, Term v b)
> projP v = case project v of
>             Just (Pair x y) -> return (x,y); _ -> Nothing
>
> -- Example: evalMEx = Just (iConst 5)
> evalMEx :: Maybe (Term Value Int)
> evalMEx = evalM ((iConst 1) `iAdd`
>                  (iConst 2 `iMult` iConst 2) :: Term Sig Int)
-}

{- $ex3
The example illustrates how to use generalised compositional data types
to implement a small expression language, and  an evaluation function mapping
intrinsically typed expressions to values.

The following language extensions are needed in order to run the example:
@TemplateHaskell@, @TypeOperators@, @MultiParamTypeClasses@,
@FlexibleInstances@, @FlexibleContexts@, and @UndecidableInstances@,
@GADTs@. Besides, GCH 7 is required.

> import Data.Comp.Multi
> import Data.Comp.Multi.Show ()
> import Data.Comp.Multi.Derive
>
> -- Signature for values and operators
> data Value e l where
>   Const  ::        Int -> Value e Int
>   Pair   :: e s -> e t -> Value e (s,t)
> data Op e l where
>   Add, Mult  :: e Int -> e Int   -> Op e Int
>   Fst        ::          e (s,t) -> Op e s
>   Snd        ::          e (s,t) -> Op e t
>
> -- Signature for the simple expression language
> type Sig = Op :+: Value
>
> -- Derive boilerplate code using Template Haskell (GHC 7 needed)
> $(derive [makeHFunctor, makeHShowF, makeHEqF, smartConstructors]
>          [''Value, ''Op])
>
> -- Term evaluation algebra
> class EvalI f where
>   evalAlgI :: Alg f I
>
> instance (EvalI f, EvalI g) => EvalI (f :+: g) where
>   evalAlgI (Inl x) = evalAlgI x
>   evalAlgI (Inr x) = evalAlgI x
>
> -- Lift the evaluation algebra to a catamorphism
> evalI :: (HFunctor f, EvalI f) => Term f i -> i
> evalI = unI . cata evalAlgI
>
> instance EvalI Value where
>   evalAlgI (Const n) = I n
>   evalAlgI (Pair (I x) (I y)) = I (x,y)
>
> instance EvalI Op where
>   evalAlgI (Add (I x) (I y))  = I (x + y)
>   evalAlgI (Mult (I x) (I y)) = I (x * y)
>   evalAlgI (Fst (I (x,_)))    = I x
>   evalAlgI (Snd (I (_,y)))    = I y
>
> -- Example: evalEx = 2
> evalIEx :: Int
> evalIEx = evalI (iFst $ iPair (iConst 2) (iConst 1) :: Term Sig Int)
-}

{- $ex4
The example illustrates how to compose a term homomorphism and an algebra,
exemplified via a desugaring term homomorphism and an evaluation algebra.

The following language extensions are needed in order to run the example:
@TemplateHaskell@, @TypeOperators@, @MultiParamTypeClasses@,
@FlexibleInstances@, @FlexibleContexts@, and @UndecidableInstances@,
@GADTs@. Besides, GCH 7 is required.

> import Data.Comp.Multi
> import Data.Comp.Multi.Show ()
> import Data.Comp.Multi.Derive
>
> -- Signature for values, operators, and syntactic sugar
> data Value e l where
>   Const  ::        Int -> Value e Int
>   Pair   :: e s -> e t -> Value e (s,t)
> data Op e l where
>   Add, Mult  :: e Int -> e Int   -> Op e Int
>   Fst        ::          e (s,t) -> Op e s
>   Snd        ::          e (s,t) -> Op e t
> data Sugar e l where
>   Neg   :: e Int   -> Sugar e Int
>   Swap  :: e (s,t) -> Sugar e (t,s)
>
> -- Source position information (line number, column number)
> data Pos = Pos Int Int
>            deriving Show
>
> -- Signature for the simple expression language
> type Sig = Op :+: Value
> type SigP = Op :&: Pos :+: Value :&: Pos
>
> -- Signature for the simple expression language, extended with syntactic sugar
> type Sig' = Sugar :+: Op :+: Value
> type SigP' = Sugar :&: Pos :+: Op :&: Pos :+: Value :&: Pos
>
> -- Derive boilerplate code using Template Haskell (GHC 7 needed)
> $(derive [makeHFunctor, makeHTraversable, makeHFoldable,
>           makeHEqF, makeHShowF, smartConstructors]
>          [''Value, ''Op, ''Sugar])
>
> -- Term homomorphism for desugaring of terms
> class (HFunctor f, HFunctor g) => Desugar f g where
>   desugHom :: Hom f g
>   desugHom = desugHom' . hfmap Hole
>   desugHom' :: Alg f (Context g a)
>   desugHom' x = appCxt (desugHom x)
>
> instance (Desugar f h, Desugar g h) => Desugar (f :+: g) h where
>   desugHom (Inl x) = desugHom x
>   desugHom (Inr x) = desugHom x
>   desugHom' (Inl x) = desugHom' x
>   desugHom' (Inr x) = desugHom' x
>
> instance (Value :<: v, HFunctor v) => Desugar Value v where
>   desugHom = simpCxt . inj
>
> instance (Op :<: v, HFunctor v) => Desugar Op v where
>   desugHom = simpCxt . inj
>
> instance (Op :<: v, Value :<: v, HFunctor v) => Desugar Sugar v where
>   desugHom' (Neg x)  = iConst (-1) `iMult` x
>   desugHom' (Swap x) = iSnd x `iPair` iFst x
>
> -- Term evaluation algebra
> class Eval f v where
>   evalAlg :: Alg f (Term v)
>
> instance (Eval f v, Eval g v) => Eval (f :+: g) v where
>   evalAlg (Inl x) = evalAlg x
>   evalAlg (Inr x) = evalAlg x
>
> instance (Value :<: v) => Eval Value v where
>   evalAlg = inject
>
> instance (Value :<: v) => Eval Op v where
>   evalAlg (Add x y)  = iConst $ (projC x) + (projC y)
>   evalAlg (Mult x y) = iConst $ (projC x) * (projC y)
>   evalAlg (Fst x)    = fst $ projP x
>   evalAlg (Snd x)    = snd $ projP x
>
> projC :: (Value :<: v) => Term v Int -> Int
> projC v = case project v of Just (Const n) -> n
>
> projP :: (Value :<: v) => Term v (s,t) -> (Term v s, Term v t)
> projP v = case project v of Just (Pair x y) -> (x,y)
>
> -- Compose the evaluation algebra and the desugaring homomorphism to an
> -- algebra
> eval :: Term Sig' :-> Term Value
> eval = cata (evalAlg `compAlg` (desugHom :: Hom Sig' Sig))
>
> -- Example: evalEx = iPair (iConst 2) (iConst 1)
> evalEx :: Term Value (Int,Int)
> evalEx = eval $ iSwap $ iPair (iConst 1) (iConst 2)
-}

{- $ex5
The example illustrates how to lift a term homomorphism to annotations,
exemplified via a desugaring term homomorphism lifted to terms annotated with
source position information.

The following language extensions are needed in order to run the example:
@TemplateHaskell@, @TypeOperators@, @MultiParamTypeClasses@,
@FlexibleInstances@, @FlexibleContexts@, and @UndecidableInstances@,
@GADTs@. Besides, GCH 7 is required.

> import Data.Comp.Multi
> import Data.Comp.Multi.Show ()
> import Data.Comp.Multi.Derive
>
> -- Signature for values, operators, and syntactic sugar
> data Value e l where
>   Const  ::        Int -> Value e Int
>   Pair   :: e s -> e t -> Value e (s,t)
> data Op e l where
>   Add, Mult  :: e Int -> e Int   -> Op e Int
>   Fst        ::          e (s,t) -> Op e s
>   Snd        ::          e (s,t) -> Op e t
> data Sugar e l where
>   Neg   :: e Int   -> Sugar e Int
>   Swap  :: e (s,t) -> Sugar e (t,s)
>
> -- Source position information (line number, column number)
> data Pos = Pos Int Int
>            deriving (Show, Eq)
>
> -- Signature for the simple expression language
> type Sig = Op :+: Value
> type SigP = Op :&: Pos :+: Value :&: Pos
>
> -- Signature for the simple expression language, extended with syntactic sugar
> type Sig' = Sugar :+: Op :+: Value
> type SigP' = Sugar :&: Pos :+: Op :&: Pos :+: Value :&: Pos
>
> -- Derive boilerplate code using Template Haskell (GHC 7 needed)
> $(derive [makeHFunctor, makeHTraversable, makeHFoldable,
>           makeHEqF, makeHShowF, smartConstructors]
>          [''Value, ''Op, ''Sugar])
>
> -- Term homomorphism for desugaring of terms
> class (HFunctor f, HFunctor g) => Desugar f g where
>   desugHom :: Hom f g
>   desugHom = desugHom' . hfmap Hole
>   desugHom' :: Alg f (Context g a)
>   desugHom' x = appCxt (desugHom x)
>
> instance (Desugar f h, Desugar g h) => Desugar (f :+: g) h where
>   desugHom (Inl x) = desugHom x
>   desugHom (Inr x) = desugHom x
>   desugHom' (Inl x) = desugHom' x
>   desugHom' (Inr x) = desugHom' x
>
> instance (Value :<: v, HFunctor v) => Desugar Value v where
>   desugHom = simpCxt . inj
>
> instance (Op :<: v, HFunctor v) => Desugar Op v where
>   desugHom = simpCxt . inj
>
> instance (Op :<: v, Value :<: v, HFunctor v) => Desugar Sugar v where
>   desugHom' (Neg x)  = iConst (-1) `iMult` x
>   desugHom' (Swap x) = iSnd x `iPair` iFst x
>
> -- Lift the desugaring term homomorphism to a catamorphism
> desug :: Term Sig' :-> Term Sig
> desug = appHom desugHom
>
> -- Example: desugEx = iPair (iConst 2) (iConst 1)
> desugEx :: Term Sig (Int,Int)
> desugEx = desug $ iSwap $ iPair (iConst 1) (iConst 2)
>
> -- Lift desugaring to terms annotated with source positions
> desugP :: Term SigP' :-> Term SigP
> desugP = appHom (propAnn desugHom)
>
> iSwapP :: (DistAnn f p f', Sugar :<: f) => p -> Term f' (a,b) -> Term f' (b,a)
> iSwapP p x = Term (injectA p $ inj $ Swap x)
>
> iConstP :: (DistAnn f p f', Value :<: f) => p -> Int -> Term f' Int
> iConstP p x = Term (injectA p $ inj $ Const x)
>
> iPairP :: (DistAnn f p f', Value :<: f) => p -> Term f' a -> Term f' b -> Term f' (a,b)
> iPairP p x y = Term (injectA p $ inj $ Pair x y)
>
> iFstP :: (DistAnn f p f', Op :<: f) => p -> Term f' (a,b) -> Term f' a
> iFstP p x = Term (injectA p $ inj $ Fst x)
>
> iSndP :: (DistAnn f p f', Op :<: f) => p -> Term f' (a,b) -> Term f' b
> iSndP p x = Term (injectA p $ inj $ Snd x)
>
> -- Example: desugPEx = iPairP (Pos 1 0)
> --                            (iSndP (Pos 1 0) (iPairP (Pos 1 1)
> --                                                     (iConstP (Pos 1 2) 1)
> --                                                     (iConstP (Pos 1 3) 2)))
> --                            (iFstP (Pos 1 0) (iPairP (Pos 1 1)
> --                                                     (iConstP (Pos 1 2) 1)
> --                                                     (iConstP (Pos 1 3) 2)))
> desugPEx :: Term SigP (Int,Int)
> desugPEx = desugP $ iSwapP (Pos 1 0) (iPairP (Pos 1 1) (iConstP (Pos 1 2) 1)
>                                                        (iConstP (Pos 1 3) 2))
-}
