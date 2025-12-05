module TreeSitter.GenerateAst.Internal.Parser where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB

import TreeSitter.GenerateAst.Internal.Data

data Parser
  = Symbol Name
  -- | Like symbol but without parsing tree-sitter symbol first
  | Inline Name
  | Alt Parser Parser
  -- | Choice (NonEmpty Parser)
  | Optional Parser
  -- 0 or more
  | Many Parser
  -- 1 or more
  | Some Parser
  | Pair Parser Parser
  | Skip
  -- | Bind Name Parser
  | Extract

mkParser :: Type -> Parser
mkParser = \case
  Node name -> Symbol name
  Ref name -> Inline name
  List a -> Many (mkParser a)
  NonEmpty a -> Some (mkParser a)
  Unit -> Skip
  Tuple a b -> Pair (mkParser a) (mkParser b)
  Token name -> Symbol (Name name)
  Either a b -> Alt (mkParser a) (mkParser b)
  Maybe a -> Optional (mkParser a)
  Content -> Extract
      -- Bind n p -> _
      
      -- Node name -> TLB.fromText (snakeToCase Upper (prefixedName name) <> "L")
      -- List a -> "[" <> t2t False a <> "]"
      -- NonEmpty a -> "NonEmpty" <> " " <> t2t True a
      -- Token t -> TLB.fromText (snakeToCase Upper t) <> "TokL"
      -- Unit -> mempty
      -- Tuple a b -> par True (t2t False a <> ", " <> t2t False b)
      -- Either a b -> par p ("Either" <> " " <> t2t True a <> " " <> t2t True b)
      -- Maybe a -> par p ("Maybe" <> " " <> t2t True a)
