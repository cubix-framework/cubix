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
       , All (..)
       , withDict
       , (\\)
       , dictFor
       ) where

import GHC.Exts
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

-- | An instance of @All c fs@ holds if @c f@ holds for all @f@ in @fs@.
--   `dictFor` is the main way to use
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


