{-# LANGUAGE TemplateHaskell #-}
module AST.GenerateSyntax where

-- import Data.Aeson qualified as Aeson
import Foreign
       ( Ptr )
import Foreign.C.String
       ( peekCString )
import Language.Haskell.TH
-- import Language.Haskell.TH.Syntax as TH
-- import Path (Path, Rel, File, (</>))
import Path
       ( File )
import Path qualified
import Path.IO qualified
import TreeSitter.Language qualified as TS
import TreeSitter.Symbol
       ( TSSymbol ) -- , toHaskellCamelCaseIdentifier)

import AST.Deserialize
       ( Named (..) )

astDeclarationsIO :: Ptr TS.Language -> Path.SomeBase File -> IO [Dec]
astDeclarationsIO lang p = do
  cwd <-
    Path.IO.getCurrentDir

  file <-
    case p of
      Path.Abs absfile -> Path.stripProperPrefix cwd absfile
      Path.Rel relfile -> pure relfile

  runQ (astDeclarationsRelative (const (pure Nothing)) lang (Path.fromRelFile file))

astDeclarationsRelative :: (String -> Q (Maybe Name)) -> Ptr TS.Language -> FilePath -> Q [Dec]
astDeclarationsRelative _lookupNameM language _invocationRelativePath = do
  -- input <- runIO (Aeson.eitherDecodeFileStrict' invocationRelativePath) >>= either fail pure
  allSymbols <- runIO (getAllSymbols language)
  debugSymbolNames <- [d|
    debugSymbolNames :: [String]
    debugSymbolNames = $(listE (map (litE . stringL . debugPrefix) allSymbols))
    |]
  pure debugSymbolNames
  -- stockTC <- TH.Stock.sumtypeTypeClasses
  -- showTC <- TH.Show.sumtypeTypeClasses
  -- mappend (debugSymbolNames <> stockTC <> showTC) . concat @[] <$> traverse (syntaxDatatype lookupNameM language) input

-- Build a list of all symbols
getAllSymbols :: Ptr TS.Language -> IO [(String, Named)]
getAllSymbols language = do
  count <- TS.ts_language_symbol_count language
  traverse getSymbol [(0 :: TSSymbol) .. fromIntegral (pred count)]
  where
    getSymbol i = do
      cname <- TS.ts_language_symbol_name language i
      n <- peekCString cname
      t <- TS.ts_language_symbol_type language i
      let named = if t == 0 then Named else Anonymous
      pure (n, named)

-- | Prefix symbol names for debugging to disambiguate between Named and Anonymous nodes.
debugPrefix :: (String, Named) -> String
debugPrefix (name, Named)     = name
debugPrefix (name, Anonymous) = "_" <> name
