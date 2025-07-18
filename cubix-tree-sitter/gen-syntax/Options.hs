{-# OPTIONS_GHC -fno-warn-orphans #-}

module Options where

import Control.Exception
       (displayException)
import Data.Text
       (Text)
import GHC.Generics
       (Generic)
import Options.Applicative qualified as OP
import Options.Generic
       ( ParseField (..)
       , ParseFields (..)
       , ParseRecord (..)
       , Unwrapped
       , Wrapped
       , getOnly
       , type (<?>)
       , type (:::)
       )
import Path qualified

data Options w = Options
  { language :: w ::: Text <?> "Name of the language in use"
  , nodes :: w ::: Path.SomeBase Path.File <?> "Path to node-types.json file from tree-sitter output"
  -- , grammar :: w ::: Path.SomeBase Path.File <?> "Path to grammar.json file from tree-sitter output"
  , destination :: w ::: Maybe (Path.SomeBase Path.File) <?> "Where to put generated output"
  } deriving stock Generic

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = \case
  Left a -> Left $ f a
  Right b -> Right b

file :: OP.ReadM (Path.SomeBase Path.File)
file = OP.eitherReader $ mapLeft displayException . Path.parseSomeFile

dir :: OP.ReadM (Path.SomeBase Path.Dir)
dir = OP.eitherReader $ mapLeft displayException . Path.parseSomeDir

fileArgument
  :: OP.Mod OP.ArgumentFields (Path.SomeBase Path.File)
  -> OP.Parser (Path.SomeBase Path.File)
fileArgument = OP.argument file

dirArgument
  :: OP.Mod OP.ArgumentFields (Path.SomeBase Path.Dir)
  -> OP.Parser (Path.SomeBase Path.Dir)
dirArgument = OP.argument dir

fileOption
  :: OP.Mod OP.OptionFields (Path.SomeBase Path.File)
  -> OP.Parser (Path.SomeBase Path.File)
fileOption = OP.option file

dirOption
  :: OP.Mod OP.OptionFields (Path.SomeBase Path.Dir)
  -> OP.Parser (Path.SomeBase Path.Dir)
dirOption = OP.option dir

instance ParseField (Path.SomeBase Path.File) where
  readField :: OP.ReadM (Path.SomeBase Path.File)
  readField = file

  metavar _ = "FILE"

instance ParseField (Path.SomeBase Path.Dir) where
  readField :: OP.ReadM (Path.SomeBase Path.Dir)
  readField = dir

  metavar _ = "DIR"

instance ParseFields (Path.SomeBase Path.File) where
  parseFields = parseField
instance ParseRecord (Path.SomeBase Path.File) where
  parseRecord = fmap getOnly parseRecord

instance ParseFields (Path.SomeBase Path.Dir) where
  parseFields = parseField
instance ParseRecord (Path.SomeBase Path.Dir) where
  parseRecord = fmap getOnly parseRecord
