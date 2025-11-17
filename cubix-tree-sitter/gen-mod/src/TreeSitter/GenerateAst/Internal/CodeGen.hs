{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TreeSitter.GenerateAst.Internal.CodeGen (
  Metadata (..),
  generateAst,
) where

import Data.Char (isAlphaNum, toLower, toUpper)
import Data.Functor.Identity (Identity (..))
import Data.List (uncons)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Text.DocLayout (Doc, render)
import Text.DocLayout qualified as Doc (Doc (..))
import Text.DocTemplates (Context (..), ToContext (..), Val (..), applyTemplate)
import TreeSitter.GenerateAst.Internal.Data (Constr (..), Data (..), Field (..), Name (..), Type (..), type TokenMap, fieldName, isHidden, toDataTypes, unName, prefixedName)
import TreeSitter.GenerateAst.Internal.Grammar (Grammar (..), RuleName)
import TreeSitter.GenerateAst.Internal.Parser (Parser (..), mkParser)
import TreeSitter.GenerateAst.Internal.Transform (transform)

import Debug.Trace
import Text.Pretty.Simple
import System.IO.Unsafe

-------------------------------------------------------------------------------
-- Template Parser and Renderer
--------------------------------------------------------------------------------

data Metadata = Metadata
  { startRuleName :: RuleName
  , moduleName :: Maybe Text
  , pretty :: Bool
  }

generateAst :: Metadata -> Grammar -> FilePath -> Text -> TokenMap -> Either String Text
generateAst Metadata{..} (transform -> grammar) templateFile template tokenMap =
  {- unsafePerformIO (pPrintLightBg (rules grammar)) `seq` -} errorOrModule
 where
  defaultModuleName = snakeToCase Upper grammar.name <> "Ast"
  metadataContext =
    Context . M.fromList $
      [ ("startRuleName", textToVal startRuleName)
      , ("grammarName", textToVal $ snakeToCase Upper grammar.name)
      , ("moduleName", textToVal $ fromMaybe defaultModuleName moduleName)
      , ("pretty", BoolVal pretty)
      ]
  datatypes = toDataTypes startRuleName grammar tokenMap
  tokens = M.mapWithKey
    (\k v -> MapVal . Context . M.fromList $
      [ ("name", toVal . Name $ fromMaybe k v)
      , ("symbol", textToVal k)])
    tokenMap
  tokensContext = Context . M.singleton "tokens" . toVal $
    M.elems tokens
  dataTypesContext =
    Context . M.fromList $
      [ ("dataTypes", toVal datatypes)
      , ("startSort", maybe NullVal (\(Data name _, _) -> toVal name) (uncons datatypes))
      ]
  context = metadataContext <> dataTypesContext <> tokensContext
  errorOrModule = renderTemplate templateFile template context

data RenderState = InText | InTemplate [Text]

renderTemplate :: (ToContext Text b) => FilePath -> Text -> b -> Either String Text
renderTemplate templateFile templateText context =
  fmap (TL.toStrict . TLB.toLazyText) . go InText . zip [1 ..] . T.lines $ templateText
 where
  go :: RenderState -> [(Int, Text)] -> Either String TLB.Builder
  go InText [] = pure mempty
  go InText ((_n, ln) : lns)
    | ln == "{-+}" = go (InTemplate mempty) lns
    | otherwise = (line ln <>) <$> go InText lns
  go (InTemplate acc) [] = template acc
  go (InTemplate acc) ((_n, ln) : lns)
    | ln == "{+-}" = (<>) <$> template acc <*> go InText lns
    | otherwise = go (InTemplate (ln : acc)) lns

  line :: Text -> TLB.Builder
  line ln = TLB.fromText (ln <> "\n")

  template :: [Text] -> Either String TLB.Builder
  template acc = do
    let tpl = T.unlines . reverse $ acc
    (doc :: Doc Text) <- runIdentity (applyTemplate templateFile tpl context)
    pure . TLB.fromText . render Nothing $ doc

textToVal :: Text -> Val Text
textToVal text = SimpleVal (Doc.Text (T.length text) text)

isNodeLike :: Type -> Bool
isNodeLike = \case
  Node _name -> True
  List a -> isNodeLike a
  NonEmpty a -> isNodeLike a
  Unit -> False
  Tuple a b -> isNodeLike a && isNodeLike b
  Token _ -> True
  Either a b -> isNodeLike a && isNodeLike b
  Maybe a -> isNodeLike a

instance ToContext Text Data where
  toVal :: Data -> Val Text
  toVal = MapVal . toContext
  toContext :: Data -> Context Text
  toContext Data{..} =
    Context . M.fromList $
      [ ("name", toVal name)
      , ("constrs", ListVal (toVal . (name,) <$> constrs))
      , ("isSum", toVal (length constrs /= 1))
      , ("hidden", toVal $ isHidden name)
      ]

instance ToContext Text (Name, Constr) where
  toVal :: (Name, Constr) -> Val Text
  toVal = MapVal . toContext
  toContext :: (Name, Constr) -> Context Text
  toContext (sortName, Constr{..}) =
    Context . M.fromList $
      [ ("sort", toVal sortName)
      , ("name", toVal name)
      , ("hasChildren", toVal hasChildren)
      , ("fields", ListVal (toVal <$> zip [(0 :: Int) ..] fields))
      ]
   where
    hasChildren :: Bool
    hasChildren = any (isNodeLike . type_) fields

instance ToContext Text (Int, Field) where
  toVal :: (Int, Field) -> Val Text
  toVal = MapVal . toContext
  toContext :: (Int, Field) -> Context Text
  toContext (index, field) =
    Context . M.fromList $
      [ ("name", maybe NullVal toVal (fieldName field))
      , ("type", toVal (type_ field))
      , ("parser", toVal (mkParser (type_ field)))
      , ("index", SimpleVal . fromString . show $ index)
      ]

instance ToContext Text Type where
  toVal :: Type -> Val Text
  toVal = textToVal . TL.toStrict . TLB.toLazyText . ("e " <>) . t2t True
   where
    par b t = if b then "(" <> t <> ")" else t
    t2t p = \case
      Node name -> TLB.fromText (snakeToCase Upper (prefixedName name) <> "L")
      List a -> "[" <> t2t False a <> "]"
      NonEmpty a -> "NonEmpty" <> " " <> t2t True a
      Token t -> TLB.fromText (snakeToCase Upper t) <> "TokL"
      Unit -> mempty
      Tuple a b -> par True (t2t False a <> ", " <> t2t False b)
      Either a b -> par p ("Either" <> " " <> t2t True a <> " " <> t2t True b)
      Maybe a -> par p ("Maybe" <> " " <> t2t True a)

instance ToContext Text Name where
  toVal :: Name -> Val Text
  toVal name =
    MapVal . Context . M.fromList $
      [ ("text", textToVal (unName name))
      , ("camelCase", textToVal (snakeToCase Upper (prefixedName name)))
      ]

instance ToContext Text Parser where
  toVal :: Parser -> Val Text
  toVal = textToVal . TL.toStrict . TLB.toLazyText . p2t False
   where
    par b t = if b then "(" <> t <> ")" else t
    p2t p = \case
      Symbol name -> TLB.fromText ("p" <> snakeToCase Upper (prefixedName name))
      Alt a b -> par p ("pEither " <> p2t True a <> " " <> p2t True b)
      -- Choice ps -> "
      Optional a -> par p ("pMaybe " <> p2t True a)
      Many ps -> par p ("pMany " <> p2t True ps)
      Some ps -> par p ("pSome " <> p2t True ps)
      Pair a b -> par p ("pPair " <> p2t True a <> " " <> p2t True b)
      Skip -> TLB.fromText "pure ()"

--------------------------------------------------------------------------------
-- Helper functions for case conversion
--------------------------------------------------------------------------------

data Case = Upper | Lower | Keep

-- | @`snakeToCase` c txt@ converts a string from snake or kebab case to camel case.
snakeToCase :: Case -> Text -> Text
snakeToCase = \b -> T.pack . go b . T.unpack
 where
  go _ [] = []
  go b (c : cs) = if isAlphaNum c then c `to` b : go Keep cs else go Upper cs

  to c Upper = toUpper c
  to c Lower = toLower c
  to c Keep = c
