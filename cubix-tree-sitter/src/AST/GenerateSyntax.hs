{-# LANGUAGE TemplateHaskell #-}
module AST.GenerateSyntax where

import Data.Aeson qualified as Aeson
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Foreign (Ptr)
import Foreign.C.String (peekCString, withCStringLen)
import GHC.Generics hiding (Constructor, Datatype)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as TH
-- import Path (Path, Rel, File, (</>))
import Path (File)
import Path qualified
import Path.IO qualified
import TreeSitter.Language qualified as TS
-- import TreeSitter.Node
import TreeSitter.Symbol
       ( TSSymbol
       , toHaskellCamelCaseIdentifier
       , toHaskellPascalCaseIdentifier
       )

import AST.Deserialize
import AST.Token

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
astDeclarationsRelative lookupNameM language invocationRelativePath = do
  input <- runIO (Aeson.eitherDecodeFileStrict' invocationRelativePath) >>= either fail pure
  allSymbols <- runIO (getAllSymbols language)
  debugSymbolNames <- [d|
    debugSymbolNames :: [String]
    debugSymbolNames = $(listE (map (litE . stringL . debugPrefix) allSymbols))
    |]
  -- stockTC <- TH.Stock.sumtypeTypeClasses
  -- showTC <- TH.Show.sumtypeTypeClasses
  mappend debugSymbolNames {- <> stockTC <> showTC -} . concat @[]
    <$> traverse (syntaxDatatype lookupNameM language allSymbols) input

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

annParameterName :: Name
annParameterName = mkName "a"

hktParameterName :: Name
hktParameterName = mkName "f"

syntaxDatatype :: (String -> Q (Maybe Name)) -> Ptr TS.Language -> [(String, Named)] -> Datatype -> Q [Dec]
syntaxDatatype lookupType language _allSymbols datatype = skipDefined $ do
  let -- traversalInstances = mappend <$> makeStandaloneDerivings (conT name) <*> makeTraversalInstances (conT name)
      -- glue a b c = a : b <> c
      name = mkName nameStr
      generatedDatatype cons = dataD (cxt []) name [plainTV annParameterName] Nothing cons [{- deriveStockClause, deriveAnyClassClause -}]
      -- deriveStockClause = derivClause (Just StockStrategy) [conT ''Generic, conT ''Generic1]
      -- deriveAnyClassClause = derivClause (Just AnyclassStrategy) [ [t| (forall a. Traversable1 a) |] ]
      -- deriveGN = derivClause (Just NewtypeStrategy) [conT ''TS.SymbolMatching]
  case datatype of
    SumType (DatatypeName _) _ subtypes ->
      let types' = fieldTypesToNestedSum subtypes
          fieldName = mkName ("get" <> nameStr)
          con = recC name [varBangType fieldName (bangType strictness (types' `appT` varT annParameterName))]
          -- hasFieldInstance = makeHasFieldInstance (conT name) (varE fieldName)
          newType = newtypeD (cxt []) name [plainTV annParameterName] Nothing con [{- deriveGN, deriveStockClause, deriveAnyClassClause -}]
      in pure <$> newType -- glue <$> newType <*> hasFieldInstance <*> traversalInstances
    ProductType datatypeName' _named children fields ->
      let con = ctorForProductType datatypeName' children fields
          -- symbols = symbolMatchingInstance allSymbols name named datatypeName
      in pure <$> generatedDatatype [con] -- glue <$> generatedDatatype [con] <*> symbols <*> traversalInstances
      -- Anonymous leaf types are defined as synonyms for the `Token` datatype
    LeafType (DatatypeName datatypeName') Anonymous -> do
      let tsSymbol = runIO $ withCStringLen datatypeName' (\(s, len) -> TS.ts_language_symbol_for_name language s len False)
      fmap (pure @[]) (tySynD name [] (conT ''Token `appT` litT (strTyLit datatypeName') `appT` litT (tsSymbol >>= numTyLit . fromIntegral)))
    LeafType datatypeName' Named ->
      let con = ctorForLeafType datatypeName' annParameterName
          -- symbols = symbolMatchingInstance allSymbols name Named datatypeName
      in pure <$> generatedDatatype [con] -- glue <$> generatedDatatype [con] <*> symbols <*> traversalInstances
  where
    -- Skip generating datatypes that have already been defined (overridden) in the module where the splice is running.
    skipDefined m = do
      isLocal <- lookupType nameStr >>= maybe (pure False) isLocalName
      if isLocal then pure [] else m
    nameStr = toNameString (datatypeNameStatus datatype) (getDatatypeName (AST.Deserialize.datatypeName datatype))

-- | Build Q Constructor for product types (nodes with fields)
ctorForProductType :: DatatypeName -> Maybe Children -> [(String, Field)] -> Q Con
ctorForProductType constructorName children fields = ctorForTypes constructorName lists where
  lists = annotation : fieldList <> childList
  annotation = ("ann", varT annParameterName)
  fieldList = map (fmap toType) fields
  childList = toList $ fmap toTypeChild children

  inject t = conT hktParameterName `appT` t

  toType :: Field -> TypeQ
  toType (MkField required fieldTypes mult) =
    let ftypes = inject (fieldTypesToNestedSum fieldTypes `appT` varT annParameterName)
    in case (required, mult) of
      (Required, Multiple) -> appT (conT ''NonEmpty) ftypes
      (Required, Single)   -> ftypes
      (Optional, Multiple) -> appT listT ftypes
      (Optional, Single)   -> appT (conT ''Maybe) ftypes

  toTypeChild (MkChildren field) = ("extra_children", toType field)

-- | Build Q Constructor for leaf types (nodes with no fields or subtypes)
ctorForLeafType :: DatatypeName -> Name -> Q Con
ctorForLeafType name annName = ctorForTypes name
  [ ("ann",  varT annName) -- ann :: a
  , ("text", conT ''Text)            -- text :: Text
  ]

-- | Build Q Constructor for records
ctorForTypes :: DatatypeName -> [(String, Q TH.Type)] -> Q Con
ctorForTypes (DatatypeName constructorName) types = recC (toName Named constructorName) recordFields
  where
    recordFields = map (uncurry toVarBangType) types
    toVarBangType str type' = varBangType (mkName . toHaskellCamelCaseIdentifier $ str) (bangType strictness type')

-- | Convert field types to Q types
fieldTypesToNestedSum :: NonEmpty AST.Deserialize.Type -> Q TH.Type
fieldTypesToNestedSum xs = go (toList xs)
  where
    combine lhs = uInfixT lhs ''(:+:) -- (((((a :+: b) :+: c) :+: d)) :+: e)   ((a :+: b) :+: (c :+: d))
    -- convertToQType' (MkType (DatatypeName n) named) = conT (toName named n)
    go [x] = convertToQType x
    go xs'  = let (l,r) = splitAt (length xs' `div` 2) xs' in combine (go l) (go r)

convertToQType :: AST.Deserialize.Type -> TypeQ
convertToQType (MkType (DatatypeName n) named) = conT (toName named n)

-- | Create bang required to build records
strictness :: BangQ
strictness = bang noSourceUnpackedness noSourceStrictness

-- | Prepend "Anonymous" to named node when false, otherwise use regular toName
toName :: Named -> String -> Name
toName named str = mkName (toNameString named str)

toNameString :: Named -> String -> String
toNameString named str = prefix named <> toHaskellPascalCaseIdentifier str
  where
    prefix Anonymous = "Anonymous"
    prefix Named     = ""

-- | Get the 'Module', if any, for a given 'Name'.
moduleForName :: Name -> Maybe Module
moduleForName n = Module . PkgName <$> namePackage n <*> (ModName <$> nameModule n)

-- | Test whether the name is defined in the module where the splice is executed.
isLocalName :: Name -> Q Bool
isLocalName n = (moduleForName n ==) . Just <$> thisModule
