{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module TreeSitter.Grammar where

import Control.Applicative (Alternative (..))
import Data.Aeson.Types (FromJSON (..), Parser, Value (..), defaultOptions, genericParseJSON, typeMismatch, withObject, (.:), (.:?))
import Data.Functor.Base.Extra ()
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

type GrammarName = Text

type Pattern = Text

type PrecName = Text

type PrecLevel = Int

type RuleName = Text

type FieldName = Text

data Grammar = Grammar
  { name :: !GrammarName
  , inherits :: !(Maybe GrammarName)
  , rules :: !(Map RuleName Rule)
  , extras :: !(Vector Rule)
  , precedences :: !(Maybe (Vector (Vector PrecSpec)))
  , reserved :: !(Maybe (Map RuleName Rule))
  , externals :: !(Maybe (Vector Rule))
  , inline :: !(Maybe (Vector RuleName))
  , conflicts :: !(Maybe (Vector (Vector RuleName)))
  , word :: !(Maybe Pattern)
  , supertypes :: !(Maybe (Vector RuleName))
  }
  deriving (Show, Generic)

instance FromJSON Grammar where
  parseJSON :: Value -> Parser Grammar
  parseJSON = genericParseJSON defaultOptions

data Rule
  = BlankRule
  | StringRule
      { value :: !Text
      }
  | PatternRule
      { value :: !Pattern
      , flags :: !(Maybe Text)
      }
  | SymbolRule
      { name :: !RuleName
      }
  | SeqRule
      { members :: !(Vector Rule)
      }
  | ChoiceRule
      { members :: !(Vector Rule)
      }
  | AliasRule
      { value :: !RuleName
      , named :: {-# UNPACK #-} !Bool
      , content :: !Rule
      }
  | RepeatRule
      { content :: !Rule
      }
  | Repeat1Rule
      { content :: !Rule
      }
  | TokenRule
      { content :: !Rule
      }
  | ImmediateTokenRule
      { content :: !Rule
      }
  | FieldRule
      { name :: !FieldName
      , content :: !Rule
      }
  | PrecRule
      { type_ :: PrecType
      , -- NOTE: the JSON key that corresponds to `prec` is called `value`
        prec :: !Prec
      , content :: !Rule
      }
  -- | Artificially introduced rule that will pop in our transformations:
  -- reference another rule, without parsing additional tree-sitter symbol
  | RefRule
      { name :: !RuleName
      }
  | Optional
      { content :: !Rule
      }
  deriving (Eq, Show, Generic)

data PrecType = Prec | PrecLeft | PrecRight | PrecDynamic
  deriving (Eq, Show, Generic)

data Prec = PrecLevel PrecLevel | PrecName PrecName | PrecSymbol RuleName
  deriving (Eq, Show, Generic)

instance FromJSON Prec where
  parseJSON :: Value -> Parser Prec
  parseJSON v = (PrecName <$> parseJSON v) <|> (PrecLevel <$> parseJSON v)

newtype PrecSpec = PrecSpec Prec
  deriving (Show, Generic)

instance FromJSON PrecSpec where
  parseJSON :: Value -> Parser PrecSpec
  parseJSON = withObject "Prec" $ \o -> do
    (type_ :: Text) <- o .: "type"
    case type_ of
      "NUMBER" -> PrecSpec . PrecLevel <$> o .: "value"
      "STRING" -> PrecSpec . PrecName <$> o .: "value"
      "SYMBOL" -> PrecSpec . PrecSymbol <$> o .: "name"
      _otherwise -> typeMismatch "Prec" (Object o)

safeReadPrecType :: Text -> Maybe PrecType
safeReadPrecType = \case
  "PREC" -> Just Prec
  "PREC_LEFT" -> Just PrecLeft
  "PREC_RIGHT" -> Just PrecRight
  "PREC_DYNAMIC" -> Just PrecDynamic
  _otherwise -> Nothing

instance FromJSON Rule where
  parseJSON :: Value -> Parser Rule
  parseJSON = withObject "Rule" $ \o -> do
    (type_ :: Text) <- o .: "type"
    case type_ of
      "BLANK" -> pure BlankRule
      "STRING" -> StringRule <$> o .: "value"
      "PATTERN" -> PatternRule <$> o .: "value" <*> o .:? "flags"
      "SYMBOL" -> SymbolRule <$> o .: "name"
      "SEQ" -> SeqRule <$> o .: "members"
      "CHOICE" -> ChoiceRule <$> o .: "members"
      "ALIAS" -> AliasRule <$> o .: "value" <*> o .: "named" <*> o .: "content"
      "REPEAT" -> RepeatRule <$> o .: "content"
      "REPEAT1" -> Repeat1Rule <$> o .: "content"
      "TOKEN" -> TokenRule <$> o .: "content"
      "IMMEDIATE_TOKEN" -> ImmediateTokenRule <$> o .: "content"
      "FIELD" -> FieldRule <$> o .: "name" <*> o .: "content"
      (safeReadPrecType -> Just precType) -> PrecRule precType <$> o .: "value" <*> o .: "content"
      _otherwise -> typeMismatch "Rule" (Object o)

makeBaseFunctor ''Rule
