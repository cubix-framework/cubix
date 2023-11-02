{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Dict
-- Copyright   :  (c) 2020 James Koppel
-- License     :  BSD3
--
-- The goal of this package is to define the `All` typeclass, where, if @fs :: [k]@ is a type-level
-- list of types or type constructors, then @All c fs@ holds if @c f@ holds for each @f@ in @fs@.
--
--------------------------------------------------------------------------------

module Data.Comp.Dict
       ( Dict (..)
       , All
       , withDict
       , (\\)
       , dictFor
       , mapAll
       ) where

import GHC.Exts ( Constraint, Proxy#, proxy# )
import Data.Proxy ( Proxy(..) )
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Unsafe.Coerce as U

import Data.Comp.Elem

-- | Existential combinator. See `dictFor` function for a usage.
--   Like the definition of @E@ in @Data.Comp.Multi.HFunctor@, but PolyKinded.
data E (f :: k1 -> *) where
  E :: f e -> E f

-- | A reified `Constraint`. Similar in spirit to @Dict@ from ekmett's @constraints@ package,
--   but that definition has a kind of @Constraint -> */
data Dict (c :: k -> Constraint) (a :: k) where
  Dict :: c a => Dict c a

-- | Puts the typeclass instance of a `Dict` back into the context.
withDict :: Dict c a -> (c a => r) -> r
withDict Dict x = x

infixl 1 \\

-- | Inline version of `withDict`
(\\) :: (c a => r) -> Dict c a -> r
(\\) x Dict = x

-- |
-- An instance of @All c fs@ holds if @c f@ holds for all @f@ in @fs@.
--
-- Example: @All `HFunctor` '[Add, Mul]@ holds if there are `HFunctor` instances for signatures
-- @Add@ and @Mul@.
--
-- The primary way to consume an `All` instance is with the `caseCxt` function. E.g.:
--
-- @
-- class Pretty f where
--   pretty :: f e l -> String
--
-- instance (All Pretty fs) => Pretty (Sum fs) where
--   pretty x = caseCxt @Pretty pretty x
-- @
class All (c :: k -> Constraint) (fs :: [k]) where
  -- | Primitive which returns list of dictionaries for each @f@ in @fs@
  dicts :: Proxy# fs -> [E (Dict c)]

instance All c '[] where
  {-# INLINE dicts #-}
  dicts _ = []

instance (All c fs, c f) => All c (f ': fs) where
  {-# INLINE dicts #-}
  dicts p = E (Dict :: Dict c f) : (dicts (reproxy @fs p))

reproxy :: forall b a. Proxy# a -> Proxy# b
reproxy _ = proxy#

-- | If @All c fs@ holds, and @e@ is type-level proof that @f@ is in @fs@,
--   then @dictFor e@ is the instance of `c` for `f`
--
--   This uses `unsafeCoerce` under the hood, but is safe if @e@ is constructed by the public API.
dictFor :: forall c f fs. (All c fs) => Elem f fs -> Dict c f
dictFor (Elem v) =
  let ds = V.fromList (dicts (proxy# :: Proxy# fs)) :: Vector (E (Dict c))
  in case ds V.! v of
       E d -> U.unsafeCoerce d
{-# INLINE dictFor #-}

-- | Lifts a mapping from type to value (k -> *) -> a into
--   a mapping from list of types to list of values [(k -> *)] -> [a]
--
-- Example usage:
--
-- @
-- class TypeNum f where
--  typeNum :: Proxy f -> Int
--
-- instance TypeNum [] where
--   typeNum _ = 0
--
-- instance TypeNum Maybe where
--   typeNum _ = 1
--
-- test = mapAll @TypeNum @'[[], Maybe] typeNum
-- @
--
-- For commentary on why Proxy is needed in the polymorphic argument,
-- see https://stackoverflow.com/questions/65488453/preventing-premature-monomorphization-of-constrained-polymorphic-values
mapAll :: forall cxt fs a. (All cxt fs) => (forall f. cxt f => Proxy f -> a) -> [a]
mapAll v = [useDict d | E d <- dicts (proxy# :: Proxy# fs)]
  where
    useDict :: forall g. Dict cxt g -> a
    useDict d = withDict d (v $ Proxy @g)
