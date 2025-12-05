{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module TreeSitter.GenerateAst.Internal.Data (
  Name (..),
  unName,
  prefixedName,
  Type (..),
  Constr (..),
  Data (..),
  Field (..),
  fieldName,
  isHidden,
  isInternal,
  GrammarError (..),
  toDataTypes,
  type TokenMap
) where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception, throw)
import Data.Foldable (Foldable (..))
import Data.Graph.Inductive (Gr)
import Data.Graph.Inductive qualified as Gr
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as V
import TreeSitter.GenerateAst.Internal.Grammar (Grammar (..), Rule (..), RuleName)

import Debug.Trace

-- | Preserves tokens used as keys, and renames them to value
type TokenMap = Map Text (Maybe Text)

newtype Name = Name {getName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

-- | If we have rules named: "rule" and "_rule" it will produce a
-- conflict
unName :: Name -> Text
unName (Name n) = n

isHidden :: Name -> Bool
isHidden (Name n) = Text.head n == '_'

isInternal :: Name -> Bool
isInternal (Name n) = let segments = Text.split (== '_') n
  in any (Text.isPrefixOf "internal") segments

prefixedName :: Name -> Text
prefixedName n
  | isHidden n = "hidden" <> unName n
  | otherwise  = unName n

data Type
  = Node Name
  | Ref Name
  | List Type
  | NonEmpty Type
  | Unit
  | Token Text
  | Tuple Type Type
  | Either Type Type
  | Maybe Type
  | Content
  deriving (Eq, Show)

mkTuple :: Type -> Type -> Type
mkTuple Unit Unit = Unit
mkTuple a Unit = a
mkTuple Unit b = b
mkTuple a b = Tuple a b

mkEither :: Type -> Type -> Type
mkEither Unit Unit = Unit
mkEither a Unit = Maybe a
mkEither Unit b = Maybe b
mkEither a b = Either a b

data Constr = Constr {name :: Name, fields :: [Field]}
  deriving (Show)

data Data = Data {name :: Name, constrs :: [Constr]}
  deriving (Show)

data Field
  = Named {name :: Name, type_ :: Type}
  | Unnamed {type_ :: Type}
  deriving (Show)

fieldName :: Field -> Maybe Name
fieldName Named{..} = Just name
fieldName _ = Nothing

newtype GrammarError
  = GrammarErrorUnknownRule RuleName
  deriving (Show)

instance Exception GrammarError

toDataTypes :: RuleName -> Grammar -> TokenMap -> [Data]
toDataTypes start grammar tokenMap = dataTypes `reachableFrom` startName
 where
  startName :: Name
  startName = Name start

  -- aliases = foldr (\r a -> a <> gatherAliases r) M.empty grammar.rules

  dataTypes :: [Data]
  dataTypes = ruleNameToData <$> M.keys (grammar.rules <> externals)

  externals :: Map RuleName Rule
  externals = foldMap (foldMap ruleToRuleMap . toList) grammar.externals
   where
    ruleToRuleMap = \case
      SymbolRule ruleName -> M.singleton ruleName BlankRule
      _ -> mempty

  rule :: RuleName -> Rule
  rule ruleName = fromMaybe failUnknownRule (tryRules <|> tryExternals) -- <|> tryAliases)
   where
    tryRules = M.lookup ruleName grammar.rules
    tryExternals = M.lookup ruleName externals
    -- tryAliases = M.lookup ruleName aliases
    failUnknownRule = throw $ GrammarErrorUnknownRule ruleName

  -- gatherAliases :: Rule -> Map RuleName Rule
  -- gatherAliases = go M.empty
  --   where go :: Map RuleName Rule -> Rule -> Map RuleName Rule
  --         go acc = \case
  --           AliasRule n _ r -> case r of
  --             SymbolRule sr -> M.insert n (rule sr) acc
  --             RefRule sr -> M.insert n (rule sr) acc
  --             _ -> go (M.insert n r acc) r
  --           SeqRule rs -> foldr (flip go) acc rs
  --           ChoiceRule rs -> foldr (flip go) acc rs

  --           RepeatRule r -> go acc r
  --           Repeat1Rule r -> go acc r
  --           TokenRule r -> go acc r
  --           ImmediateTokenRule r -> go acc r
  --           FieldRule _ r -> go acc r
  --           PrecRule _ _ r -> go acc r
  --           Optional r -> go acc r

  --           BlankRule -> acc
  --           StringRule _ -> acc
  --           PatternRule _ _ -> acc
  --           SymbolRule _ -> acc
  --           RefRule _ -> acc

  ruleNameToData :: RuleName -> Data
  ruleNameToData s = ruleToData (rule s)
   where
    ruleToData :: Rule -> Data
    ruleToData = \case
      -- ... if the rule is a choice rule, it becomes a sum type:
      ChoiceRule rs ->
        let constrs = rulesToConstrs s rs
        in Data (Name s) $ renameAnonymousConstr s constrs
      -- ... if the rule is a seq rule, it becomes a product type:
      SeqRule rs ->
        Data (Name s) [Constr (Name s) [f | r <- V.toList rs, let f = ruleToField r, f.type_ /= Unit]]
      -- ignored rules
      PrecRule _ _ r -> ruleToData r
      -- ... otherwise, it becomes a newtype wrapper:
      (ruleToField -> f) -> Data (Name s) [Constr (Name s) [f | f.type_ /= Unit]]

  renameAnonymousConstr :: Text -> [Constr] -> [Constr]
  renameAnonymousConstr typeName constrs = go <$> zip constrs [1..]
    where go :: (Constr, Int) -> Constr
          go (ctor@(Constr (Name name) fields), ord) = if name == typeName
            then Constr (Name $ name <> Text.show ord) fields
            else ctor

  rulesToConstrs :: RuleName -> Vector Rule -> [Constr]
  rulesToConstrs s = catMaybes . go . V.toList
   where
    suffix name = Name $ name <> "_" <> s
    go :: [Rule] -> [Maybe Constr]
    go [] = []
    go (r : rest) = case r of
      SymbolRule sr -> Just (Constr (suffix sr) [f | let f = ruleToField r, f.type_ /= Unit]) : go rest
      RefRule sr -> Just (Constr (suffix sr) [f | let f = ruleToField r, f.type_ /= Unit]) : go rest
      ChoiceRule rs -> go (V.toList rs <> rest)
      BlankRule -> go rest
      StringRule str -> case M.lookup str tokenMap of
        Just (fromMaybe str -> tok) -> Just (Constr (suffix tok) [ Unnamed (Token tok) ]) : go rest
        Nothing  -> go rest
      AliasRule n _ (ruleToField -> f) ->
        Just (Constr (Name n) [f | f.type_ /= Unit]) : go rest
      TokenRule tr -> go (tr : rest)
      ImmediateTokenRule tr -> go (tr : rest)
      FieldRule n (ruleToField -> f) -> Just (Constr (suffix n) [f | f.type_ /= Unit]) : go rest
      PrecRule _ _ pr -> go (pr : rest)
      Optional opt -> go (opt : rest)
      SeqRule rs ->
        Just (Constr (Name s) [ f
                              | item <- V.toList rs
                              , let f = ruleToField item
                              , f.type_ /= Unit ]) : go rest
      PatternRule{} -> Just (Constr (Name s) [ f | let f = ruleToField r, f.type_ /= Unit ]) : go rest
      -- The following constructors constitute faillure:
      RepeatRule{} -> []
      Repeat1Rule{} -> []


  ruleToType :: Rule -> Type
  ruleToType = \case
    BlankRule -> Unit
    StringRule s -> maybe Unit (Token . fromMaybe s) (M.lookup s tokenMap)
    PatternRule{} -> Content
    SymbolRule (Name -> s) ->  if isHidden s
      then trace ("looking up: " <> Prelude.show s) $ ruleToType (rule $ unName s) -- Node (Name s)
      else Node s
    RefRule s -> Ref (Name s)
    SeqRule rs -> foldl1Or mkTuple Unit (ruleToType <$> rs)
    ChoiceRule rs -> foldl1Or mkEither Unit (ruleToType <$> rs)
    RepeatRule r -> List (ruleToType r)
    Repeat1Rule r -> NonEmpty (ruleToType r)
    AliasRule n True _ -> Node (Name n)
    AliasRule _ False r -> ruleToType r
    -- The following constructors are transparent:
    TokenRule r -> ruleToType r
    ImmediateTokenRule r -> ruleToType r
    FieldRule _ r -> ruleToType r
    PrecRule _ _ r -> ruleToType r
    Optional r -> Maybe (ruleToType r)

  ruleToField :: Rule -> Field
  ruleToField = \case
    -- Nested field rules are ignored:
    FieldRule n r -> Named (Name n) (ruleToType r)
    AliasRule n True  _ -> Named (Name n) (Node (Name n))
    AliasRule _ False r -> ruleToField r
    -- The following constructors are transparent:
    TokenRule r -> ruleToField r
    ImmediateTokenRule r -> ruleToField r
    PrecRule _ _ r -> ruleToField r
    -- All remaining constructors default to ruleToType:
    r -> Unnamed (ruleToType r)

depsOfData :: Data -> [Name]
depsOfData (Data _ cs) = concatMap depsOfConstr cs

depsOfConstr :: Constr -> [Name]
depsOfConstr (Constr _ fs) = concatMap (depsOfType . (.type_)) fs

depsOfType :: Type -> [Name]
depsOfType = \case
  Node sort -> [sort]
  Ref sort -> [sort]
  List t -> depsOfType t
  NonEmpty t -> depsOfType t
  Unit -> []
  Token _ -> []
  Tuple t1 t2 -> depsOfType t1 <> depsOfType t2
  Either t1 t2 -> depsOfType t1 <> depsOfType t2
  Maybe t -> depsOfType t
  Content -> []

reachableFrom :: [Data] -> Name -> [Data]
reachableFrom dataTypes start = usedData
 where
  usedData :: [Data]
  usedData =
    [ fromMaybe (error "impossible") (Gr.lab depGraph node)
    | node <- Gr.reachable (nameToNode start) depGraph
    ]

  depGraph :: Gr Data ()
  depGraph = Gr.mkGraph nodesAndData edges

  nodesAndData :: [(Gr.Node, Data)]
  nodesAndData = zip [0 ..] dataTypes

  edges :: [(Gr.Node, Gr.Node, ())]
  edges =
    [ (node, nameToNode depName, ())
    | (node, dataType) <- nodesAndData
    , depName <- depsOfData dataType
    ]

  nameToNode :: Name -> Gr.Node
  nameToNode name = fromMaybe (throw . GrammarErrorUnknownRule . unName $ name) (M.lookup name nameNodeMap)

  nameNodeMap :: Map Name Gr.Node
  nameNodeMap = M.fromList [(name, node) | (node, Data name _) <- nodesAndData]

-- | @`foldl1Or` f e xs@ returns @e@ if @xs@ is empty and @`foldl1` f xs@ otherwise.
foldl1Or :: (a -> a -> a) -> a -> Vector a -> a
foldl1Or f e v
  | null v = e
  | otherwise = V.foldl1' f v
