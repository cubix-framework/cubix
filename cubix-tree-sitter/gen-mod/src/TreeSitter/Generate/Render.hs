{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-------------------------------------------------------------------------------
-- Template Parser and Renderer
--------------------------------------------------------------------------------

module TreeSitter.Generate.Render where

import Data.Char (isAlphaNum, toLower, toUpper)
import Data.Functor.Identity (Identity (..))
import Data.List (uncons)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Unicode.Char.General.Names qualified as Unicode
import Text.DocLayout (Doc, render)
import Text.DocLayout qualified as Doc (Doc (..))
import Text.DocTemplates (Context (..), ToContext (..), Val (..), applyTemplate)

import TreeSitter.Generate.Data
import TreeSitter.Generate.Parser (Parser (..), mkParser)
import TreeSitter.Grammar (Grammar (..), RuleName)

import Text.Pretty.Simple
import System.IO.Unsafe (unsafePerformIO)

data Metadata = Metadata
  { startRuleName :: RuleName
  , moduleName :: Maybe Text
  , pretty :: Bool
  }

data RenderState = InText | InTemplate [Text]

renderSyntax :: Metadata -> Grammar -> FilePath -> Text -> TokenMap -> Either String Text
renderSyntax Metadata{..} grammar templateFile template tokenMap =
  unsafePerformIO (pPrintLightBg (rules grammar)) `seq` errorOrModule
 where
  defaultModuleName = snakeToCase Upper grammar.name <> "Ast"
  metadataContext =
    Context . Map.fromList $
      [ ("startRuleName", textToVal startRuleName)
      , ("grammarName", textToVal $ snakeToCase Upper grammar.name)
      , ("moduleName", textToVal $ fromMaybe defaultModuleName moduleName)
      , ("pretty", BoolVal pretty)
      ]
  tokens = Map.mapWithKey
    (\k v -> MapVal . Context . Map.fromList $
      [ ("name", toVal . Name $ fromMaybe k v)
      , ("symbol", textToVal k)])
    tokenMap
  tokensCtx = Context . Map.singleton "tokens" . toVal
    $ Map.elems tokens
  nodes = Map.elems $ Map.mapWithKey (topRuleToNode tokenMap grammar.rules) grammar.rules
  nodes' = {- unsafePerformIO (pPrintLightBg nodes) `seq` -} nodes `reachableFrom` Name startRuleName

  nodesCtx = Context . Map.fromList $
    [ ("data", toVal nodes')
    , ("startSort", maybe NullVal (\(Node _ sort _, _) -> toVal (Name sort)) (uncons nodes'))
    ]
  -- every top level rule forms a label
  labels = Set.toList . Set.fromList $ nSort <$> nodes
  labelsCtx = Context . Map.singleton "labels" . ListVal $ toVal . Name <$> labels

  context :: Context Text
  context = metadataContext <> nodesCtx <> labelsCtx <> tokensCtx
  errorOrModule = renderTemplate templateFile template context

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

  template :: [Text] -> Either String Builder.Builder
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
      , ("camelCase", textToVal (snakeToCase Upper (hiddenName name)))
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
      , ("constructors", ListVal (toVal . (Name nSort,) <$> nCtrs))
      ]
    where
      -- if the node exists in ts token stream
      hasSymbol :: Bool
      hasSymbol = not $ isHidden nSort || isInternal nName

instance ToContext Text (Name, Constructor) where
  toVal :: (Name, Constructor) -> Val Text
  toVal = MapVal . toContext
  toContext :: (Name, Constructor) -> Context Text
  toContext (sort, Constructor name fields) =
    Context . Map.fromList $
      [ ("name", toVal $ Name name)
      , ("type", toVal sort)
      , ("fields", ListVal (toVal <$> zip [(0 :: Int) ..] fields))
      , ("hasChildren", toVal hasChildren)
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
      , ("parser", toVal (mkParser (fType field)))
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
      Token t -> Builder.fromText (snakeToCase Upper t) <> "TokL"
      Unit -> mempty
      Tuple a b -> par True (t2t False a <> ", " <> t2t False b)
      Maybe a -> par p ("Maybe" <> " " <> t2t True a)
      Content -> "Text"

instance ToContext Text Parser where
  toVal :: Parser -> Val Text
  toVal = textToVal . Text.Lazy.toStrict . Builder.toLazyText . p2t False
   where
    par b t = if b then "(" <> t <> ")" else t
    p2t p = \case
      Symbol name -> Builder.fromText ("p" <> snakeToCase Upper (hiddenName name))
      Inline name -> Builder.fromText ("p" <> snakeToCase Upper (hiddenName name))
      Alt a b -> par p ("pEither " <> p2t True a <> " " <> p2t True b)
      -- Choice ps -> "
      Optional a -> par p ("pMaybe " <> p2t True a)
      Many ps -> par p ("pMany " <> p2t True ps)
      Some ps -> par p ("pSome " <> p2t True ps)
      Pair a b -> par p ("pPair " <> p2t True a <> " " <> p2t True b)
      Skip -> Builder.fromText "pure ()"
      Extract -> "pText"

hiddenName :: Name -> Text
hiddenName (getName -> n)
  | isHidden n = "hidden" <> n
  | otherwise  = n

sortName :: Name -> Text
sortName n = snakeToCase Upper (hiddenName n <> "_l")

--------------------------------------------------------------------------------
-- Helper functions for case conversion
--------------------------------------------------------------------------------

data Case = Upper | Lower | Keep

-- | @`snakeToCase` c txt@ converts a string from snake or kebab case to camel case.
snakeToCase :: Case -> Text -> Text
snakeToCase b = Text.pack . go b . Text.unpack
 where
  go _ [] = []
  go a (c : cs) =
    if | c == '_' -> go Upper cs
       | isAlphaNum c -> c `to` a : go Keep cs
       | Just n <- Unicode.name c -> go a (toLower <$> n ++ cs)
       | otherwise -> go Upper cs

  to c Upper = toUpper c
  to c Lower = toLower c
  to c Keep = c
