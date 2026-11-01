{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-------------------------------------------------------------------------------
-- Template Parser and Renderer
--------------------------------------------------------------------------------

module TreeSitter.Generate.Render where

import Data.Bifunctor (second)
import Data.Char (isAlphaNum, isDigit, toLower, toUpper)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.List (find, intercalate, sortOn, uncons)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Map qualified as Map (elems, fromList, fromListWith, mapWithKey, singleton, toList)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set (fromList, insert, map, notMember, singleton, size, toList)
import Data.String (IsString (..))
import Data.Traversable (mapAccumL)
import Data.Text (Text)
import Data.Text qualified as Text (breakOnEnd, length, lines, null, pack, replicate, show, span, unlines, unpack)
import Data.Text.Lazy qualified as Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder (fromText, toLazyText)
import Data.Vector qualified as Vector (toList)

import Text.DocLayout (Doc, render)
import Text.DocLayout qualified as Doc (Doc (..))
import Text.DocTemplates (Context (..), ToContext (..), Val (..), applyTemplate)
import Unicode.Char.General.Names qualified as Unicode (nameOrAlias)

import TreeSitter.Generate.Data
import TreeSitter.Generate.Parser
import TreeSitter.Generate.Types
import TreeSitter.Grammar

data Metadata = Metadata
  { startRuleName :: RuleName
  , moduleName :: Maybe Text
  , pretty :: Bool
  }

data RenderState = InText | InTemplate [Text]

-- | Haskell constructors share one namespace, but names derived from
-- different rules can coincide once camel-cased (TypeScript's
-- @literal_type@ has alternatives @_number@ and @number@, and
-- @for_statement@ has three alternatives that are all the field
-- @initializer@). Number the later ones. The first argument is the
-- constructors that are already taken: those of 'Token'.
uniqueConstructorNames :: Set Text -> [Node] -> [Node]
uniqueConstructorNames taken = snd . mapAccumL renameNode taken
  where
    renameNode used node =
      second (\ctors -> node { nCtrs = ctors }) (mapAccumL renameCtor used (nCtrs node))

    renameCtor used ctor =
      let candidates = cName ctor : [ cName ctor <> Text.show i | i <- [2 :: Int ..] ]
          isFree c = camelName (Name c) `Set.notMember` used
          name = fromMaybe (cName ctor) (find isFree candidates)
      in (Set.insert (camelName (Name name)) used, ctor { cName = name })

renderSyntax :: Metadata -> Grammar -> FilePath -> Text -> Either String Text
renderSyntax Metadata{..} grammar templateFile template =
  {- unsafePerformIO (pPrintLightBg (rules grammar)) `seq` -} errorOrModule
 where
  defaultModuleName = snakeToCase Upper grammar.name <> "Ast"

  -- The parser imports the Modularized syntax from its sibling module,
  -- following the @Cubix.Language.<Lang>.{Modularized,RawParse}@ layout.
  modularizedModuleName = case moduleName of
    Just name | (parent, _) <- Text.breakOnEnd "." name, not (Text.null parent) ->
      parent <> "Modularized"
    _ -> defaultModuleName

  grammarName = snakeToCase Upper grammar.name

  metadataContext =
    Context . Map.fromList $
      [ ("startRuleName", textToVal startRuleName)
      , ("grammarName", textToVal grammarName)
      , ("moduleName", textToVal $ fromMaybe defaultModuleName moduleName)
      , ("modularizedModuleName", textToVal modularizedModuleName)
      , ("pretty", BoolVal pretty)
      ]
      <> (second textToVal <$> templateTypes grammarName)

  nodes  = Map.elems $ Map.mapWithKey (topRuleToNode grammar.rules) grammar.rules
  nodes' = uniqueConstructorNames
    (Set.map (camelName . tokenName) tokens)
    (nodes `reachableFrom` Name startRuleName)

  -- Collect all tokens from grammar rules by traversing them with catamorphism
  tokens = mconcat $ tokensOfRule <$> Map.elems grammar.rules
  tokensCtx = Context . Map.singleton "tokens" . toVal $ Set.toList tokens <&> (\k ->
    MapVal . Context . Map.fromList $
      [ ("name", toVal (tokenName k))
      , ("symbol", textToVal (Text.show k)) -- as a Haskell string literal
      ])

  nodesCtx = Context . Map.fromList $
    [ ("data", toVal nodes')
    , ("startSort", maybe NullVal (\(Node _ sort _, _) -> toVal (Name sort)) (uncons nodes'))
    ]
  -- every top level rule forms a label
  labels = Set.toList . Set.fromList $ nSort <$> nodes
  labelsCtx = Context . Map.singleton "labels" . ListVal $ toVal . Name <$> labels

  context :: Context Text
  context = metadataContext <> nodesCtx <> labelsCtx <> tokensCtx
  errorOrModule = case (visibleSupertypes, clashes) of
    ([], []) -> renderTemplate templateFile template context
    (names@(_ : _), _) -> Left $ unlines
      [ "Grammar declares supertypes that are not hidden: " <> show names
      , "Tree-sitter never emits nodes for supertypes, but the generated parser"
      , "would expect them. Remove them from grammar.json first (jq/supertypes.jq)."
      ]
    (_, _) -> Left . unlines $
      "Grammar names that map to the same Haskell identifier:"
      : [ "  " <> Text.unpack ident <> ": " <> intercalate ", " (Text.unpack <$> sources)
        | (ident, sources) <- clashes
        ]

  visibleSupertypes = filter (not . isHidden) (foldMap Vector.toList grammar.supertypes)

  -- Generated names can only clash with each other: the templates bring
  -- nothing else into their namespace (see their imports). Constructors
  -- are numbered apart ('uniqueConstructorNames'); types and parsers,
  -- which other code refers to by name, are checked.
  clashes = nameClashes types <> nameClashes parsers
  types =
    [ (camelName (Name n.nName), "rule " <> n.nName) | n <- nodes' ]
    <> [ (sortName (Name l), "sort of rule " <> l) | l <- labels ]
    <> [ (sortName (tokenName t), "sort of token " <> Text.show t) | t <- Set.toList tokens ]
    <> [ (ty, "the Modularized template") | (_, ty) <- templateTypes grammarName ]
  parsers =
    [ ("p" <> camelName (Name n.nName), "rule " <> n.nName) | n <- nodes' ]
    <> [ ("p" <> camelName (tokenName t), "token " <> Text.show t) | t <- Set.toList tokens ]

-- | The types the Modularized template declares besides the generated
-- ones, by template variable. Their names are chosen here so that
-- 'renderSyntax' can check them against the generated names.
templateTypes :: Text -> [(Text, Text)]
templateTypes grammarName =
  [ ("tokenType", tokenType)
  , ("sigType", grammarName <> "Sig")
  , ("termType", grammarName <> "Term")
  , ("termLabType", grammarName <> "TermLab")
  ]

-- | The data type of all tokens.
tokenType :: Text
tokenType = "Token"

-- | Identifiers that more than one grammar name maps to, with those
-- names. Camel-casing is not injective: @foo_bar@ and @fooBar@, @_foo@
-- and @hidden_foo@, or the rule @foo_l@ and the sort of rule @foo@
-- give the same identifier.
nameClashes :: [(Text, Text)] -> [(Text, [Text])]
nameClashes named =
  [ (ident, Set.toList sources)
  | (ident, sources) <- Map.toList (Map.fromListWith (<>) (second Set.singleton <$> named))
  , Set.size sources > 1
  ]

renderTemplate :: (ToContext Text b) => FilePath -> Text -> b -> Either String Text
renderTemplate templateFile templateText context =
  fmap (Text.Lazy.toStrict . Builder.toLazyText) . go InText . zip [1 ..] . Text.lines $ templateText
 where
  go :: RenderState -> [(Int, Text)] -> Either String Builder
  go InText [] = pure mempty
  go InText ((_n, ln) : lns)
    | ln == "{-+}" = go (InTemplate mempty) lns
    | otherwise = (line ln <>) <$> go InText lns
  go (InTemplate acc) [] = template acc
  go (InTemplate acc) ((_n, ln) : lns)
    | ln == "{+-}" = (<>) <$> template acc <*> go InText lns
    | otherwise = go (InTemplate (ln : acc)) lns

  line :: Text -> Builder
  line ln = Builder.fromText (ln <> "\n")

  template :: [Text] -> Either String Builder
  template acc = do
    let tpl = Text.unlines . reverse $ acc
    (doc :: Doc Text) <- runIdentity (applyTemplate templateFile tpl context)
    pure . Builder.fromText . render Nothing $ doc

textToVal :: Text -> Val Text
textToVal text = SimpleVal (Doc.Text (Text.length text) text)

instance ToContext Text Name where
  toVal :: Name -> Val Text
  toVal name =
    MapVal . Context . Map.fromList $
      [ ("text", textToVal (getName name))
      , ("sort", textToVal (sortName name))
      , ("camelCase", textToVal (camelName name))
      ]

instance ToContext Text Node where
  toVal :: Node -> Val Text
  toVal = MapVal . toContext
  toContext :: Node -> Context Text
  toContext Node {..} =
    Context . Map.fromList $
      [ ("name", toVal (Name nName))
      , ("sort", toVal (sortName $ Name nSort))
      , ("hasSymbol", toVal hasSymbol)
      , ("sum", toVal (length nCtrs /= 1))
      , ("constructors", ListVal (toVal . (Name nSort,) <$> orderedCtrs))
      ]
    where
      -- if the node exists in ts token stream
      hasSymbol :: Bool
      hasSymbol = not $ isHidden nSort || isInternal nName

      -- | Order constructors so longer (more-field) alternatives come
      -- first. The generated parser picks the alternative that consumes
      -- the most children ('Megaparsec.Cubix.pLongest'), so this order
      -- only decides between alternatives that consume equally many;
      -- it used to be what kept a shorter alternative (e.g.
      -- @identifier@ vs @identifier "::" identifier@) from succeeding
      -- prematurely. Sorting is stable, by descending field count, then
      -- by required field count; remaining ties keep grammar order.
      orderedCtrs :: [Constructor]
      orderedCtrs = sortOn specificity nCtrs

      specificity :: Constructor -> (Down Int, Down Int)
      specificity ctor =
        ( Down (length ctor.cFields)
        , Down (length (filter isRequiredField ctor.cFields))
        )

      isRequiredField :: Field -> Bool
      isRequiredField field = case field.fType of
        Maybe _ -> False
        _       -> True

instance ToContext Text (Name, Constructor) where
  toVal :: (Name, Constructor) -> Val Text
  toVal = MapVal . toContext
  toContext :: (Name, Constructor) -> Context Text
  toContext (sort, Constructor name fields cParser) =
    Context . Map.fromList $
      [ ("name", toVal $ Name name)
      , ("type", toVal sort)
      , ("fields", ListVal (toVal <$> zip [(0 :: Int) ..] fields))
      , ("hasChildren", toVal hasChildren)
      , ("parser", toVal cParser)
      ]
   where
    hasChildren :: Bool
    hasChildren = any (isNodeLike . fType) fields

instance ToContext Text (Int, Field) where
  toVal :: (Int, Field) -> Val Text
  toVal = MapVal . toContext
  toContext :: (Int, Field) -> Context Text
  toContext (index, field) =
    Context . Map.fromList $
      [ ("name", maybe NullVal toVal (fieldName field))
      , ("type", toVal (fType field))
      , ("index", SimpleVal . fromString . show $ index)
      ]

isNodeLike :: Type -> Bool
isNodeLike = \case
  Ref _name -> True
  List a -> isNodeLike a
  NonEmpty a -> isNodeLike a
  Unit -> False
  Tuple a b -> isNodeLike a && isNodeLike b
  Token _ -> True
  Maybe a -> isNodeLike a
  Content -> True

instance ToContext Text Type where
  toVal :: Type -> Val Text
  toVal ty = textToVal . Text.Lazy.toStrict . Builder.toLazyText . label ty $ t2t True ty
   where
    label = \case
      Content -> id
      _ -> ("e " <>)
    par b t = if b then "(" <> t <> ")" else t
    t2t p = \case
      -- Node name -> TLB.fromText (snakeToCase Upper (prefixedName name) <> "L")
      Ref name -> Builder.fromText $ sortName name -- (snakeToCase Upper (prefixedName name) <> "L")
      List a -> "[" <> t2t False a <> "]"
      NonEmpty a -> "[" <> t2t False a <> "]"
        -- TODO: CUBIX_NON_EMPTY
        -- discuss cubix non empty support first
        -- par p ("NonEmpty" <> " " <> t2t True a)
      Token t -> Builder.fromText (sortName (tokenName t))
      Unit -> mempty
      Tuple a b -> par True (t2t False a <> ", " <> t2t False b)
      -- The Modularized template imports these qualified
      Maybe a -> par p ("Prelude.Maybe" <> " " <> t2t True a)
      Content -> "Text.Text"

instance ToContext Text Parser where
  toVal :: Parser -> Val Text
  toVal = textToVal . Text.Lazy.toStrict . Builder.toLazyText . p2t False
   where
    par b t = if b then "(" <> t <> ")" else t
    hasSymbol :: Name -> Bool
    hasSymbol (getName -> n) = not $ isHidden n || isInternal n
    p2t p = \case
      Symbol name -> par p $ name2psort (camelName name) name
      Inline name -> par p $ name2psort (camelName name) name
      Tok t -> par p $ name2psort tokenType (tokenName t)
      Seq ps -> mconcat $ intersperseBy (p2t False) inspect (NonEmpty.toList ps)
        -- mconcat (p2t False <$> NonEmpty.toList ps)
      Pair a b -> par p ("pairP "<> p2t True a <> " " <> p2t True b)
      Optional a -> par p ("maybeP " <> p2t True a)
      Many ps -> par p ("manyP " <> p2t True ps)
      Some ps -> par p ("someP " <> p2t True ps)
      -- Nothing to parse, e.g. a hidden rule made of invisible tokens
      -- (Haskell's @_layout_end@, which comes before @in@ in @let_in@).
      -- Not 'eof': a node's parser is checked for 'eof' anyway, and such a
      -- rule need not end its node.
      Skip -> Builder.fromText "pure ()"
      Extract -> "contentP"
      SepBy sep content -> par p ("sepByP " <> p2t True content <> " " <> p2t True sep)
      SepBy1 sep content -> par p ("sepBy1P " <> p2t True content <> " " <> p2t True sep)
      Between open close content -> par p ("betweenP " <> p2t True open <> " " <> p2t True close <> " " <> p2t True content)
    inspect :: Parser -> Parser -> Builder
    -- future
    -- inspect (Tok _) _ = Builder.fromText " *> "
    -- inspect _ (Tok _) = Builder.fromText " <* "
    inspect _ _ = Builder.fromText " <*> "

    -- A child of the node's own data type or, for a token, of 'Token'.
    -- The parser imports the Modularized syntax qualified as @M@.
    name2psort fragment name
      | hasSymbol name =
          "fragmentSortP @M." <>
          Builder.fromText fragment <>
          " @M." <>
          Builder.fromText (sortName name) <>
          " " <>
          Builder.fromText (Text.show (getName name))
      | otherwise = "p" <> Builder.fromText (camelName name)

    intersperseBy :: Monoid b => (a -> b) -> (a -> a -> b) -> [a] -> [b]
    intersperseBy _ _ []     = mempty
    intersperseBy g _ [x]    = [g x]
    intersperseBy g f (x:y:xs) = g x : f x y : intersperseBy g f (y:xs)

hiddenName :: Name -> Text
hiddenName (getName -> n)
  | isHidden n = "hidden" <> n
  | otherwise  = n

sortName :: Name -> Text
sortName n = snakeToCase Upper (hiddenName n <> "_l")

-- | Haskell identifier for the data type, constructor or parser
-- (@p<Name>@) of a rule or token. The generated modules import
-- everything else qualified, or in a shape no such identifier has (see
-- the templates), so it cannot clash with an imported name.
camelName :: Name -> Text
camelName = snakeToCase Upper . hiddenName

-- | The rule-like name a token is declared under. Tokens are named after
-- their text, not after a rule, so a leading underscore (Python's @_@
-- wildcard pattern) must not make them look hidden: it is spelled out.
tokenName :: Text -> Name
tokenName t = Name (Text.replicate (Text.length underscores) "low_line_" <> rest <> "_tok")
  where
    (underscores, rest) = Text.span (== '_') t

--------------------------------------------------------------------------------
-- Helper functions for case conversion
--------------------------------------------------------------------------------

data Case = Upper | Lower | Keep

-- | @`snakeToCase` c txt@ converts a string from snake or kebab case to camel case.
--
-- Characters that cannot appear in an identifier are spelled out by
-- their Unicode name (or alias, for control characters), so tokens such
-- as @"=>"@ or @"\\n"@ get distinct names. So is a leading digit
-- (@"0x"@), which cannot start a Haskell identifier.
snakeToCase :: Case -> Text -> Text
snakeToCase b = Text.pack . start . Text.unpack
 where
  start (c : cs) | isDigit c = go b (spell c ++ cs)
  start cs = go b cs

  go _ [] = []
  go a (c : cs) =
    if | c == '_' -> go Upper cs
       | isAlphaNum c -> c `to` a : go Keep cs
       | otherwise -> go Upper (spell c ++ cs)

  -- Unicode names separate words with spaces, and some with '-'
  spell c = maybe "" (fmap toLower . replaceChar ' ' '_' . replaceChar '-' '_') (Unicode.nameOrAlias c) ++ "_"

  to c Upper = toUpper c
  to c Lower = toLower c
  to c Keep = c

replaceChar :: Char -> Char -> String -> String
replaceChar needle new haystack = haystack <&> \c ->
  if c == needle then new else c
