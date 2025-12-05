module TreeSitter.Generate.Parser where

import TreeSitter.Generate.Data

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
  -- Node name -> Symbol name
  Ref name -> Inline name
  List a -> Many (mkParser a)
  NonEmpty a -> Some (mkParser a)
  Unit -> Skip
  Tuple a b -> Pair (mkParser a) (mkParser b)
  Token name -> Symbol (Name name)
  -- Either a b -> Alt (mkParser a) (mkParser b)
  Maybe a -> Optional (mkParser a)
  Content -> Extract
