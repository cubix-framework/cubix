module AST.Token
  ( Token(..)
  ) where

import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..), showsUnaryWith)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (KnownSymbol, Nat, Symbol, symbolVal)

-- | An AST node representing a token, indexed by its name and numeric value.
--
-- For convenience, token types are typically used via type synonyms, e.g.:
--
-- @
-- type AnonymousPlus = Token "+" 123
-- @
newtype Token (symName :: Symbol) (symVal :: Nat) (f :: Type -> Type) a = Token { ann :: a }
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Traversable)

instance (KnownSymbol n, Show a) => Show (Token n v f a) where
  show (Token a) = "Token: " <> symbolVal (Proxy @n) <> " " <> show a

instance Eq1 (Token n v f) where
  liftEq eq (Token a) (Token b) = eq a b

instance Ord1 (Token n v f) where
  liftCompare cmp (Token a) (Token b) = cmp a b

instance KnownSymbol n => Show1 (Token n v f) where
  liftShowsPrec sp _ d (Token a) = showsUnaryWith sp ("Token: " <> symbolVal (Proxy @n)) d a
