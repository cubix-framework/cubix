module TreeSitter.Generate.Types where

import Data.Map (Map)
import Data.String (IsString (..))
import Data.Text (Text)

newtype Name = Name { getName :: Text }
  deriving newtype (Eq, Ord, Show, IsString)

-- | Preserves tokens used as keys, and renames them to value
type TokenMap = Map Text Text


