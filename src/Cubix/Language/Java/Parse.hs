{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A JVM-free Java parser for Cubix, built on top of a vendored
-- tree-sitter-java grammar.
--
-- This module walks the tree-sitter concrete syntax tree and produces an
-- annotated cubix IPS Full-layer term ('JTerm' 'CompilationUnitL') whose
-- structure exactly matches what the legacy @javaparser-to-hs.jar@ used
-- to produce. Every layer carries a 'Maybe SourceSpan' annotation
-- attached at construction time.
--
-- Shape correctness against the JAR is verified by
-- @cubix-examples/java-ts-validate/@.
--
-- The converter intentionally rejects most modern-Java constructs
-- (records, sealed classes, switch expressions, text blocks, lambdas,
-- pattern matching, @var@ declarations, @yield@) with a clear error.
-- These can be lowered to existing 'Language.Java.Syntax' shapes in a
-- follow-up; for now the corpus we care about (K-Java, Java 1.4-ish) does
-- not exercise them.
module Cubix.Language.Java.Parse
  ( parse
  , parseFile
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (Exception, throwIO, try)
import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Bits (shiftL)
import Data.Char (chr, digitToInt, isDigit, isHexDigit, ord)
import Data.Foldable (foldlM)
import Data.Function (on)
import Data.IntMap.Strict qualified as IM
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Text.Read (readMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.TypeNats (KnownNat, natVal)
import System.IO.Unsafe (unsafePerformIO)

import Language.Java.Syntax qualified as J
import TreeSitter qualified as TS

import Data.Comp.Multi (AnnTerm, stripA)
import Data.Comp.Multi.Annotation ((:&:) (..))
import Data.Comp.Multi.Ops ((:-<:), inj)
import Data.Comp.Multi.Term (Cxt (Term))

import Cubix.Language.Info (SourceSpan, mkSourceSpan)
import Cubix.Language.Java.Parametric.Full
  ( JavaSig
  , CompilationUnit (CompilationUnit)
  , PackageDecl (PackageDecl)
  , ImportDecl (ImportDecl)
  , TypeDecl (ClassTypeDecl, InterfaceTypeDecl)
  , ClassDecl (ClassDecl, EnumDecl)
  , ClassBody (ClassBody)
  , EnumBody (EnumBody), EnumConstant (EnumConstant)
  , InterfaceDecl (InterfaceDecl), InterfaceBody (InterfaceBody)
  , InterfaceKind (InterfaceNormal)
  , Decl (MemberDecl, InitDecl)
  , MemberDecl
      ( FieldDecl, MethodDecl, ConstructorDecl
      , MemberClassDecl, MemberInterfaceDecl
      )
  , VarDecl (VarDecl), VarDeclId (VarId, VarDeclArray)
  , VarInit (InitExp, InitArray)
  , FormalParam (FormalParam)
  , MethodBody (MethodBody), ConstructorBody (ConstructorBody)
  , ExplConstrInv (ThisInvoke, SuperInvoke, PrimarySuperInvoke)
  , Modifier
      ( Public, Private, Protected, Abstract, Final, Static
      , StrictFP, Transient, Volatile, Native, Synchronized_
      )
  , Annotation
      ( NormalAnnotation, SingleElementAnnotation, MarkerAnnotation
      )
  , ElementValue (EVVal, EVAnn)
  , Block (Block), BlockStmt (BlockStmt, LocalClass, LocalVars)
  , Stmt
      ( StmtBlock, IfThen, IfThenElse, While, BasicFor, EnhancedFor
      , Empty, ExpStmt, Assert, Switch, Do
      , Break, Continue, Return, Synchronized
      , Throw, Try, Labeled
      )
  , Catch (Catch), SwitchBlock (SwitchBlock)
  , SwitchLabel (SwitchCase, Default)
  , ForInit (ForLocalVars, ForInitExps)
  , Exp
      ( Lit, ClassLit, This, ThisClass
      , InstanceCreation, QualInstanceCreation
      , ArrayCreate, ArrayCreateInit
      , FieldAccess, MethodInv, ArrayAccess, ExpName
      , PostIncrement, PostDecrement, PreIncrement, PreDecrement
      , PrePlus, PreMinus, PreBitCompl, PreNot
      , Cast, BinOp, InstanceOf, Cond, Assign
      )
  , Lhs (NameLhs, FieldLhs, ArrayLhs)
  , ArrayIndex (ArrayIndex)
  , FieldAccess (PrimaryFieldAccess, SuperFieldAccess, ClassFieldAccess)
  , ArrayInit (ArrayInit)
  , MethodInvocation
      ( MethodCall, PrimaryMethodCall, SuperMethodCall, ClassMethodCall
      , TypeMethodCall
      )
  , Literal
  , Op
      ( Mult, Div, Rem, Add, Sub, LShift, RShift, RRShift
      , LThan, GThan, LThanE, GThanE, Equal, NotEq
      , And, Or, Xor, CAnd, COr
      )
  , AssignOp
      ( EqualA, MultA, DivA, RemA, AddA, SubA
      , LShiftA, RShiftA, RRShiftA, AndA, XorA, OrA
      )
  , Type (PrimType, RefType)
  , PrimType
      ( BooleanT, ByteT, ShortT, IntT, LongT, CharT, FloatT, DoubleT
      )
  , RefType (ClassRefType, ArrayType)
  , ClassType (ClassType)
  , TypeArgument (Wildcard, ActualType)
  , WildcardBound (ExtendsBound, SuperBound)
  , TypeDeclSpecifier
      ( TypeDeclSpecifier, TypeDeclSpecifierWithDiamond
      , TypeDeclSpecifierUnqualifiedWithDiamond
      )
  , Diamond (Diamond)
  , TypeParam (TypeParam)
  , Ident (Ident), Name (Name)
  , CompilationUnitL, PackageDeclL, ImportDeclL, TypeDeclL
  , ClassDeclL, ClassBodyL, EnumBodyL, EnumConstantL
  , InterfaceDeclL, InterfaceBodyL, InterfaceKindL
  , DeclL, MemberDeclL
  , VarDeclL, VarDeclIdL, VarInitL, FormalParamL
  , ConstructorBodyL, ExplConstrInvL
  , ModifierL, AnnotationL, ElementValueL
  , BlockL, BlockStmtL, StmtL, CatchL, SwitchLabelL
  , ExpL, LhsL, ArrayIndexL, FieldAccessL, ArrayInitL
  , MethodInvocationL, LiteralL
  , OpL, AssignOpL
  , TypeL, PrimTypeL, RefTypeL, ClassTypeL
  , TypeArgumentL
  , TypeParamL, TypeDeclSpecifierL, IdentL, NameL
  )
import Cubix.Language.Java.Parametric.Full qualified as JF
import Cubix.Language.Parametric.Syntax.Functor
  ( ListF (NilF, ConsF), MaybeF (NothingF, JustF), PairF (PairF)
  )
import TreeSitter.Java (tree_sitter_java)

--------------------------------------------------------------------------------
-- Annotated term type and core helpers

type JTerm = AnnTerm (Maybe SourceSpan) JavaSig

-- | Build an annotated layer by injecting a fragment value into 'JavaSig'
-- and attaching a 'SourceSpan'.
layer :: (g :-<: JavaSig) => g JTerm l -> Maybe SourceSpan -> JTerm l
layer g sp = Term (inj g :&: sp)

-- | Wrap a list of annotated terms into a single annotated list term.
--
-- The list/option/pair wrappers are IPS encoding artifacts, not real
-- Java AST nodes, so they carry no source span — matching the
-- convention used by the Python/Lua/C parsers, where the @:<:@ instance
-- through @:&:@ supplies 'Nothing' via the 'Default' class.
annList :: forall l. Typeable l => [JTerm l] -> JTerm [l]
annList []     = layer (NilF :: ListF JTerm [l]) Nothing
annList (x:xs) = layer (ConsF x (annList xs)) Nothing

-- | Wrap an optional annotated term. See 'annList' on why the wrapper
-- itself is unannotated.
annMaybe :: forall l. Typeable l => Maybe (JTerm l) -> JTerm (Maybe l)
annMaybe Nothing  = layer (NothingF :: MaybeF JTerm (Maybe l)) Nothing
annMaybe (Just x) = layer (JustF x) Nothing

-- | Wrap a pair of annotated terms. See 'annList' on why the wrapper
-- itself is unannotated.
annPair :: (Typeable a, Typeable b) => JTerm a -> JTerm b -> JTerm (a, b)
annPair a b = layer (PairF a b) Nothing

--------------------------------------------------------------------------------
-- Top-level entry points

-- | Parse a file as a Java 'J.CompilationUnit'. @Left@ if tree-sitter
-- rejected the source, or if the converter encountered an unsupported
-- construct.
parse :: FilePath -> IO (Either String J.CompilationUnit)
parse path = try (parseFile path) >>= \case
  Left (ConverterError msg) -> pure (Left msg)
  Right Nothing             -> pure (Left $ "tree-sitter-java failed to parse " <> path)
  Right (Just t)            -> pure (Right (JF.untranslate (stripA t)))

-- | Parse a file to an annotated cubix Full-layer term, throwing on
-- converter errors and returning 'Nothing' on tree-sitter parse failure.
parseFile :: FilePath -> IO (Maybe (JTerm CompilationUnitL))
parseFile path = do
  -- Java Unicode escapes are a pre-lexical source transformation: they can
  -- spell punctuation, line terminators, and identifier characters outside
  -- string/char literal bodies. Apply them before tree-sitter tokenizes, then
  -- normalise line endings to match javac/the legacy JAR backend.
  (src, srcMap) <- decodeJavaSource <$> BS.readFile path
  lang <- javaLanguage
  parser <- TS.parserNew
  ok <- TS.parserSetLanguage parser lang
  if not ok
    then error "tree-sitter-java: parserSetLanguage returned False (ABI mismatch?)"
    else do
      mtree <- TS.parserParseByteString parser Nothing src
      case mtree of
        Nothing -> pure Nothing
        Just tree -> do
          -- A tree-sitter @TSNode@ is a value-typed handle that contains a
          -- raw pointer back into its tree's arena. The 'TS.Tree' itself
          -- is the owner; if it is GC'd while we are mid-walk, every
          -- subsequent node access dereferences freed memory and we
          -- segfault. 'TS.Node' carries no Haskell-level reference to
          -- its tree, so we keep the tree (and the parser, which owns
          -- the tree) alive in the converter context.
          root <- TS.treeRootNode tree
          hasError <- TS.nodeHasError root
          if hasError
            then pure Nothing
            else do
              term <- runReaderT (convertProgram root) (Ctx path src srcMap tree parser)
              pure (Just term)

-- | Load the Java grammar as a 'TS.Language', performing an ABI
-- compatibility check.
--
-- The vendored @parser.c@ is built against a specific tree-sitter ABI
-- (currently 14). The runtime supplied by @hs-tree-sitter@ supports a
-- closed range of ABIs (currently @[13, 14]@). If the vendored
-- grammar's ABI is outside this range, the runtime would refuse to
-- load it later with an opaque \"failed to set parser language\"
-- error; we instead fail loudly here with both versions named.
--
-- The pointer returned by @tree_sitter_java()@ refers to a @static@
-- @TSLanguage@ struct compiled into the binary, but
-- @hs-tree-sitter@'s 'TS.unsafeToLanguage' attaches a foreign-pointer
-- finalizer that calls @ts_language_delete@ — running that on a
-- static struct corrupts it, so a second parse later in the program
-- segfaults. We sidestep this by initializing the wrapped 'TS.Language'
-- under a lock and retaining it in a top-level CAF: as long as the cache
-- holds the value, the finalizer never runs.
javaLanguage :: IO TS.Language
javaLanguage =
  modifyMVar javaLanguageCache $ \case
    Just l  -> pure (Just l, l)
    Nothing -> do
      l <- TS.unsafeToLanguage =<< tree_sitter_java
      checkJavaLanguageAbi l
      pure (Just l, l)

{-# NOINLINE javaLanguageCache #-}
javaLanguageCache :: MVar (Maybe TS.Language)
javaLanguageCache = unsafePerformIO (newMVar Nothing)

-- | Verify the Java grammar's ABI version is within the runtime's
-- supported range. Throws 'error' on mismatch.
checkJavaLanguageAbi :: TS.Language -> IO ()
checkJavaLanguageAbi lang = do
  grammarAbi <- TS.languageVersion lang
  let runtimeAbi    = natValW (Proxy @TS.TREE_SITTER_LANGUAGE_VERSION)
      runtimeMinAbi = natValW (Proxy @TS.TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION)
  unless (grammarAbi >= runtimeMinAbi && grammarAbi <= runtimeAbi) $
    error $
      "Cubix.Language.Java.Parse: tree-sitter-java grammar ABI "
        <> show grammarAbi
        <> " is incompatible with hs-tree-sitter runtime ABI range ["
        <> show runtimeMinAbi
        <> ", "
        <> show runtimeAbi
        <> "]. Re-vendor parser.c at a compatible upstream tag (see cbits/README.md)."
  where
    natValW :: KnownNat n => Proxy n -> Word32
    natValW = fromIntegral . natVal

-- | Decode Java Unicode escape sequences (a backslash followed by one or
-- more @u@ characters and four hex digits) before lexical analysis.
decodeJavaUnicodeEscapes :: ByteString -> ByteString
decodeJavaUnicodeEscapes = fst . decodeJavaSource

data OrigPos = OrigPos
  { origRow :: !Int
  , origCol :: !Int
  }

data SourceMap = SourceMap
  { sourceMapStarts :: !(IM.IntMap OrigPos)
  , sourceMapEnds   :: !(IM.IntMap OrigPos)
  }

data DecodedChar = DecodedChar
  { decodedChar      :: !Char
  , decodedStart     :: !OrigPos
  , decodedEnd       :: !OrigPos
  , decodedInclusive :: !OrigPos
  }

decodeJavaSource :: ByteString -> (ByteString, SourceMap)
decodeJavaSource src =
  buildSourceMap (normaliseDecodedChars (decodeChars 0 (OrigPos 1 1) (T.unpack (TE.decodeUtf8With lenientDecode src))))

decodeChars :: Int -> OrigPos -> String -> [DecodedChar]
decodeChars _ _ [] = []
decodeChars slashRun pos (c : rest)
  | c /= '\\' =
      let pos' = advancePos pos c
      in DecodedChar c pos pos' pos : decodeChars 0 pos' rest
  | odd slashRun =
      let pos' = advancePos pos c
      in DecodedChar c pos pos' pos : decodeChars (slashRun + 1) pos' rest
  | otherwise =
      case span (== 'u') rest of
        ([], _) ->
          let pos' = advancePos pos c
          in DecodedChar c pos pos' pos : decodeChars (slashRun + 1) pos' rest
        (us, h1 : h2 : h3 : h4 : rest')
          | all isHexDigit [h1, h2, h3, h4] ->
              let consumed = '\\' : us ++ [h1, h2, h3, h4]
                  pos'     = advanceString pos consumed
                  cp       = hexValueC h1 h2 h3 h4
                  ch       = chr cp
                  slashRun' = if ch == '\\' then slashRun + 1 else 0
              in DecodedChar ch pos pos' (lastOrigPos pos consumed) : decodeChars slashRun' pos' rest'
        _ ->
          let pos' = advancePos pos c
          in DecodedChar c pos pos' pos : decodeChars (slashRun + 1) pos' rest
  where
    hexValueC h1 h2 h3 h4 =
      digitToInt h1 * 4096
        + digitToInt h2 * 256
        + digitToInt h3 * 16
        + digitToInt h4

normaliseDecodedChars :: [DecodedChar] -> [DecodedChar]
normaliseDecodedChars (a : b : rest)
  | decodedChar a == '\r' && decodedChar b == '\n' =
      DecodedChar '\n' (decodedStart a) (decodedEnd b) (decodedInclusive b)
        : normaliseDecodedChars rest
normaliseDecodedChars (a : rest)
  | decodedChar a == '\r' =
      a { decodedChar = '\n' } : normaliseDecodedChars rest
  | otherwise = a : normaliseDecodedChars rest
normaliseDecodedChars [] = []

buildSourceMap :: [DecodedChar] -> (ByteString, SourceMap)
buildSourceMap chars =
  let (chunks, starts, ends, finalOffset, finalPos) = foldl step ([], IM.empty, IM.empty, 0, OrigPos 1 1) chars
      starts' = IM.insert finalOffset finalPos starts
      ends'   = IM.insertWith (\_ old -> old) finalOffset finalPos ends
  in (BS.concat (reverse chunks), SourceMap starts' ends')
  where
    step (chunks, starts, ends, offset, _) ch =
      let bytes     = TE.encodeUtf8 (T.singleton (decodedChar ch))
          len       = BS.length bytes
          starts'   = foldl (\m i -> IM.insert (offset + i) (decodedStart ch) m) starts [0 .. len - 1]
          ends'     = IM.insert (offset + len) (decodedInclusive ch) ends
          finalPos  = decodedEnd ch
      in (bytes : chunks, starts', ends', offset + len, finalPos)

advanceString :: OrigPos -> String -> OrigPos
advanceString = foldl advancePos

lastOrigPos :: OrigPos -> String -> OrigPos
lastOrigPos pos [] = pos
lastOrigPos pos [c] = pos
lastOrigPos pos (c : rest) = lastOrigPos (advancePos pos c) rest

advancePos :: OrigPos -> Char -> OrigPos
advancePos (OrigPos row col) '\r' = OrigPos (row + 1) 1
advancePos (OrigPos row col) '\n' = OrigPos (row + 1) 1
advancePos (OrigPos row col) '\t' = OrigPos row (((col - 1) `div` sourceTabWidth + 1) * sourceTabWidth + 1)
advancePos (OrigPos row col) _    = OrigPos row (col + 1)

sourceTabWidth :: Int
sourceTabWidth = 8

--------------------------------------------------------------------------------
-- Converter monad

data Ctx = Ctx
  { ctxPath   :: FilePath
  , ctxSource :: ByteString
  , ctxSourceMap :: SourceMap
  , _ctxTree   :: TS.Tree    -- ^ kept alive for the duration of conversion
  , _ctxParser :: TS.Parser  -- ^ kept alive for the duration of conversion
  }
type Convert = ReaderT Ctx IO

-- | Thrown by the converter when it encounters something it cannot lower
-- (modern-Java feature, malformed tree, or unimplemented rule).
newtype ConverterError = ConverterError String
  deriving (Show)
instance Exception ConverterError

bail :: String -> Convert a
bail = liftIO . throwIO . ConverterError

--------------------------------------------------------------------------------
-- Tree-sitter node helpers

-- | The byte slice underlying a node, as the original source text. Used for
-- identifier names, literal lexemes, etc.
nodeText :: TS.Node -> Convert ByteString
nodeText n = do
  src <- asks ctxSource
  s <- liftIO (TS.nodeStartByte n)
  e <- liftIO (TS.nodeEndByte n)
  pure (BS.take (fromIntegral (e - s)) (BS.drop (fromIntegral s) src))

-- | The byte slice underlying a node, decoded as UTF-8 ('String'). The Java
-- source is read verbatim as bytes, so any non-ASCII identifier or string
-- literal lands here as a multi-byte UTF-8 sequence that must be decoded
-- before downstream code (which treats 'String' as a sequence of code points)
-- sees it.
nodeTextS :: TS.Node -> Convert String
nodeTextS = fmap (T.unpack . TE.decodeUtf8With lenientDecode) . nodeText

-- | The tree-sitter rule name of a node (e.g. @"class_declaration"@,
-- @"identifier"@).
nodeKind :: TS.Node -> Convert ByteString
nodeKind = liftIO . TS.nodeType

-- | Number of named children (i.e. excluding anonymous tokens like
-- punctuation, keywords).
namedChildCount :: TS.Node -> Convert Word32
namedChildCount = liftIO . TS.nodeNamedChildCount

-- | All named children of a node, in source order, /excluding/
-- tree-sitter's @line_comment@ and @block_comment@.
--
-- Tree-sitter exposes comments as *named* children, but they are never
-- syntactically meaningful in the Java AST — they're trivia. Filtering at
-- this single chokepoint keeps every downstream arity check, positional
-- index, and @traverse convertX@ robust against awkwardly-placed inline
-- comments anywhere a Java construct allows whitespace.
--
-- Use 'namedChildrenAll' if you need the raw stream including comments.
namedChildren :: TS.Node -> Convert [TS.Node]
namedChildren n = do
  raw <- namedChildrenAll n
  filterM (fmap (not . isCommentKind) . nodeKind) raw
  where
    isCommentKind k = k == "line_comment" || k == "block_comment"

-- | All named children of a node, in source order, *including* tree-sitter
-- comments. Reserved for code that deliberately walks the comment stream
-- (none in this module today; kept as an escape hatch).
namedChildrenAll :: TS.Node -> Convert [TS.Node]
namedChildrenAll n = do
  k <- namedChildCount n
  if k == 0 then pure []
            else traverse (liftIO . TS.nodeNamedChild n) [0 .. k - 1]

-- | All children (named and anonymous) of a node, in source order.
allChildren :: TS.Node -> Convert [TS.Node]
allChildren n = do
  k <- liftIO (TS.nodeChildCount n)
  if k == 0 then pure []
            else traverse (liftIO . TS.nodeChild n) [0 .. k - 1]

-- | Look up a named field on a node. Returns 'Nothing' if the field is
-- absent.
field :: ByteString -> TS.Node -> Convert (Maybe TS.Node)
field name n = do
  child <- liftIO (TS.nodeChildByFieldName n (TS.WrapTSFieldName name))
  isNull <- liftIO (TS.nodeIsNull child)
  pure (if isNull then Nothing else Just child)

-- | Look up a named field; fail if absent.
requireField :: ByteString -> TS.Node -> Convert TS.Node
requireField name n = field name n >>= \case
  Just c  -> pure c
  Nothing -> do
    k <- nodeKind n
    bail $ "missing required field " <> BS8.unpack name <> " on " <> BS8.unpack k

-- | Look up a named field and run a converter on its value, if present.
optField :: ByteString -> (TS.Node -> Convert a) -> TS.Node -> Convert (Maybe a)
optField name f n = field name n >>= traverse f

-- | All children belonging to a repeated named field. tree-sitter
-- represents repeated fields by attaching the same field name to several
-- children; we walk the parent's children with a 'TS.TreeCursor' so we
-- can read each child's optional field name safely.
--
-- Note: 'TS.nodeFieldNameForChild' is not safe to call when the child
-- has no field — its hs-tree-sitter wrapper unconditionally unwraps a
-- @NULL@ @const char *@ via @unsafePackCString@ and segfaults. The
-- cursor API returns @Maybe ByteString@, which is why we use it here.
fieldAll :: ByteString -> TS.Node -> Convert [TS.Node]
fieldAll name parent = liftIO $ do
  cursor <- TS.treeCursorNew parent
  hasFirst <- TS.treeCursorGotoFirstChild cursor
  if not hasFirst
    then pure []
    else collect cursor []
  where
    collect cursor acc = do
      mfname <- TS.treeCursorCurrentFieldName cursor
      acc' <- case mfname of
        Just fname | fname == name -> do
          c <- TS.treeCursorCurrentNode cursor
          pure (c : acc)
        _ -> pure acc
      hasNext <- TS.treeCursorGotoNextSibling cursor
      if hasNext
        then collect cursor acc'
        else pure (reverse acc')

-- | The first named child of @n@ whose 'nodeKind' equals the given kind,
-- or 'Nothing'. Useful for finding children that don't have a field name
-- (e.g. @modifiers@, @throws@, @catch_formal_parameter@ in
-- tree-sitter-java).
findChildOfKind :: ByteString -> TS.Node -> Convert (Maybe TS.Node)
findChildOfKind kind n = do
  cs <- namedChildren n
  go cs
  where
    go [] = pure Nothing
    go (c:rest) = do
      k <- nodeKind c
      if k == kind then pure (Just c) else go rest

-- | All named children of @n@ matching the given kind, in source order.
findChildrenOfKind :: ByteString -> TS.Node -> Convert [TS.Node]
findChildrenOfKind kind n = do
  cs <- namedChildren n
  goFilter cs []
  where
    goFilter [] acc = pure (reverse acc)
    goFilter (c:rest) acc = do
      k <- nodeKind c
      goFilter rest (if k == kind then c : acc else acc)

-- | Reject an unsupported tree-sitter rule (modern-Java, etc.).
unsupported :: String -> TS.Node -> Convert a
unsupported what n = do
  k <- nodeKind n
  bail $ "unsupported Java construct: " <> what
       <> " (tree-sitter rule \"" <> BS8.unpack k <> "\")"

unknownKind :: String -> TS.Node -> Convert a
unknownKind sort n = do
  k <- nodeKind n
  bail $ "unrecognised " <> sort <> ": \"" <> BS8.unpack k <> "\""

-- | Tree-sitter node → 'SourceSpan'.
--
-- Tree-sitter reports byte offsets in the decoded Java input. Source spans
-- must point into the original file, so we translate those offsets through
-- the source map built while applying Unicode escapes and EOL normalisation.
spanOf :: TS.Node -> Convert (Maybe SourceSpan)
spanOf n = do
  path  <- asks ctxPath
  srcMap <- asks ctxSourceMap
  start <- fromIntegral <$> liftIO (TS.nodeStartByte n)
  end   <- fromIntegral <$> liftIO (TS.nodeEndByte n)
  let OrigPos startRow startCol = sourceStartPos srcMap start
      OrigPos endRow endCol     = sourceEndPos srcMap end
  pure $ Just $ mkSourceSpan path
                  ( startRow, startCol )
                  ( endRow, endCol )

sourceStartPos :: SourceMap -> Int -> OrigPos
sourceStartPos (SourceMap starts _) off =
  fromMaybe (OrigPos 1 1) (IM.lookup off starts)

sourceEndPos :: SourceMap -> Int -> OrigPos
sourceEndPos (SourceMap _ ends) off =
  fromMaybe (OrigPos 1 1) (IM.lookup off ends)

--------------------------------------------------------------------------------
-- program → CompilationUnit

-- | Entry: walks a @program@ node into a 'JTerm' 'CompilationUnitL'.
convertProgram :: TS.Node -> Convert (JTerm CompilationUnitL)
convertProgram n = do
  sp <- spanOf n
  kids <- dropComments =<< namedChildren n
  (pkg, kids1) <- case kids of
    (c : rest) -> nodeKind c >>= \case
      "package_declaration" -> do
        p <- convertPackageDecl c
        rest' <- dropComments rest
        pure (Just p, rest')
      _ -> pure (Nothing, kids)
    _ -> pure (Nothing, kids)
  (imports, types) <- partitionImportsAndTypes kids1
  pure $ layer
    (CompilationUnit (annMaybe pkg)
                     (annList imports)
                     (annList types))
    sp
  where
    dropComments = filterM (\c -> nodeKind c >>= \k -> pure (k /= "line_comment" && k /= "block_comment"))
    partitionImportsAndTypes []     = pure ([], [])
    partitionImportsAndTypes (c:cs) = do
      k <- nodeKind c
      case k of
        "import_declaration" -> do
          i <- convertImportDecl c
          (is, ts) <- partitionImportsAndTypes cs
          pure (i:is, ts)
        "line_comment"  -> partitionImportsAndTypes cs
        "block_comment" -> partitionImportsAndTypes cs
        _ -> do
          ts <- mapMaybeM tryTypeDecl (c:cs)
          pure ([], ts)
    tryTypeDecl c = nodeKind c >>= \case
      "line_comment"  -> pure Nothing
      "block_comment" -> pure Nothing
      _               -> Just <$> convertTypeDecl c

--------------------------------------------------------------------------------
-- package / import declarations

convertPackageDecl :: TS.Node -> Convert (JTerm PackageDeclL)
convertPackageDecl n = do
  sp <- spanOf n
  -- package_declaration: "package" name ";"
  -- The dotted name follows the "package" keyword; pick the last named child.
  kids <- namedChildren n
  case reverse kids of
    (nm : _) -> do
      nameTerm <- convertName nm
      pure (layer (PackageDecl nameTerm) sp)
    _        -> bail "empty package_declaration"

convertImportDecl :: TS.Node -> Convert (JTerm ImportDeclL)
convertImportDecl n = do
  sp <- spanOf n
  -- import_declaration children (per the tree-sitter-java grammar):
  --   "import" "static"? (identifier | scoped_identifier) ("." asterisk)? ";"
  -- The trailing @asterisk@ marks a wildcard import. Both the name
  -- nodes and the @*@ are exposed as named children, so we filter the
  -- name out and check for asterisk by kind.
  named <- namedChildren n
  isStatic <- anyChildText "static" n
  taggedKinds <- traverse (\c -> (\k -> (c, k)) <$> nodeKind c) named
  let nameNodes = [c | (c, k) <- taggedKinds, k /= "asterisk"]
      isStar    = any ((== "asterisk") . snd) taggedKinds
  case nameNodes of
    [] -> bail "empty import_declaration"
    _  -> do
      nameTerm <- convertName (last nameNodes)
      pure (layer (ImportDecl isStatic nameTerm isStar) sp)

-- | True if any direct child token has the given source text.
anyChildText :: ByteString -> TS.Node -> Convert Bool
anyChildText tok n = do
  cs <- allChildren n
  go cs
  where
    go []     = pure False
    go (c:cs) = do
      isN <- liftIO (TS.nodeIsNamed c)
      if isN
        then go cs
        else do
          t <- nodeText c
          if t == tok then pure True else go cs

--------------------------------------------------------------------------------
-- Names and identifiers

-- | A tree-sitter @identifier@ or @type_identifier@ node → 'JTerm' 'IdentL'.
convertIdent :: TS.Node -> Convert (JTerm IdentL)
convertIdent n = do
  sp <- spanOf n
  s  <- nodeTextS n
  pure (layer (Ident s) sp)

-- | A dotted name (e.g. @java.util.List@). In tree-sitter-java, scoped
-- names show up as nested @scoped_identifier { scope: …, name: … }@
-- nodes. A bare identifier is a single-segment name.
convertName :: TS.Node -> Convert (JTerm NameL)
convertName n = do
  sp <- spanOf n
  segs <- nameSegments n
  pure (layer (Name (annList segs)) sp)

-- | Try to interpret a tree-sitter primary-expression node as a
-- dotted-identifier 'Name'. Returns @Nothing@ if any component isn't a
-- plain identifier (e.g. method call, parens, @this@/@super@), and so
-- the qualifier really is a runtime expression rather than a type name.
--
-- Tree-sitter parses dotted names in /expression/ position as nested
-- @field_access@ rather than @scoped_identifier@ (the latter is reserved
-- for import/declaration contexts), so we have to flatten both shapes.
nodeAsName :: TS.Node -> Convert (Maybe (JTerm NameL))
nodeAsName n = do
  msegs <- nameSegs n
  case msegs of
    Nothing   -> pure Nothing
    Just segs -> do
      sp <- spanOf n
      pure (Just (layer (Name (annList segs)) sp))
  where
    nameSegs node = nodeKind node >>= \case
      "identifier"        -> Just . (:[]) <$> convertIdent node
      "scoped_identifier" -> do
        scope <- requireField "scope" node
        nm    <- requireField "name"  node
        ms    <- nameSegs scope
        case ms of
          Nothing   -> pure Nothing
          Just segs -> do
            i <- convertIdent nm
            pure (Just (segs ++ [i]))
      "field_access" -> do
        fld <- requireField "field" node
        fldKind <- nodeKind fld
        if fldKind /= "identifier" then pure Nothing
        else do
          obj <- requireField "object" node
          ms  <- nameSegs obj
          case ms of
            Nothing   -> pure Nothing
            Just segs -> do
              i <- convertIdent fld
              pure (Just (segs ++ [i]))
      _ -> pure Nothing

-- | Flatten a scoped/bare identifier tree into its segments in source order.
nameSegments :: TS.Node -> Convert [JTerm IdentL]
nameSegments n = nodeKind n >>= \case
  "identifier"        -> singleton <$> convertIdent n
  "type_identifier"   -> singleton <$> convertIdent n
  "this"              -> bail "name segment was 'this' — caller should handle 'this' before reaching nameSegments"
  "scoped_identifier" -> do
    scope <- requireField "scope" n
    name  <- requireField "name"  n
    (++) <$> nameSegments scope <*> (singleton <$> convertIdent name)
  k -> bail $ "expected identifier or scoped_identifier, got " <> BS8.unpack k
  where
    singleton x = [x]

--------------------------------------------------------------------------------
-- Type declarations

convertTypeDecl :: TS.Node -> Convert (JTerm TypeDeclL)
convertTypeDecl n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "class_declaration"     -> do
      cd <- convertClassDecl n
      pure (layer (ClassTypeDecl cd) sp)
    "interface_declaration" -> do
      idl <- convertInterfaceDecl n
      pure (layer (InterfaceTypeDecl idl) sp)
    "enum_declaration"      -> do
      cd <- convertEnumDecl n
      pure (layer (ClassTypeDecl cd) sp)
    "annotation_type_declaration" ->
      unsupported "annotation type declaration" n
    "record_declaration" ->
      unsupported "Java 14+ record" n
    ";" -> bail "unexpected ';' as top-level type declaration"
    _   -> unknownKind "type declaration" n

convertClassDecl :: TS.Node -> Convert (JTerm ClassDeclL)
convertClassDecl n = do
  sp        <- spanOf n
  modifiers <- modifiersOf n
  name      <- requireField "name" n >>= convertIdent
  tparams   <- optField "type_parameters" convertTypeParameters n >>= pure . fromMaybe []
  super     <- optField "superclass" convertSuperclass n
  ifaces    <- optField "interfaces" convertInterfaces n >>= pure . fromMaybe []
  -- @permits@ is the Java 17+ @sealed class S permits T, U {}@ clause.
  -- The legacy AST has no slot for it, and 'convertKeywordModifier'
  -- already rejects @sealed@/@non-sealed@; reject @permits@ too so we
  -- never silently drop the clause if tree-sitter recovers from a
  -- malformed source.
  field "permits" n >>= \case
    Nothing -> pure ()
    Just p  -> unsupported "Java 17+ 'permits' clause on sealed class" p
  body      <- requireField "body" n >>= convertClassBody
  pure $ layer
    (ClassDecl
       modifiers
       name
       (annList tparams)
       (annMaybe super)
       (annList ifaces)
       body)
    sp

-- | tree-sitter @superclass@: \"extends\" type
convertSuperclass :: TS.Node -> Convert (JTerm RefTypeL)
convertSuperclass n = do
  named <- namedChildren n
  case named of
    [t] -> convertRefType t
    _   -> bail "expected single named child in superclass"

-- | tree-sitter @super_interfaces@/@extends_interfaces@: \"implements\"/\"extends\" type_list
convertInterfaces :: TS.Node -> Convert [JTerm RefTypeL]
convertInterfaces n = do
  named <- namedChildren n
  case named of
    [tl] -> nodeKind tl >>= \case
      "type_list" -> traverse convertRefType =<< namedChildren tl
      _           -> traverse convertRefType named
    _    -> traverse convertRefType named

convertClassBody :: TS.Node -> Convert (JTerm ClassBodyL)
convertClassBody n = do
  sp    <- spanOf n
  named <- namedChildren n
  decls <- traverse convertDecl named
  pure (layer (ClassBody (annList (concat decls))) sp)

-- | Some tree-sitter members map to zero or more 'JTerm DeclL' values (e.g.
-- @field_declaration@ with multiple variable declarators becomes one
-- 'FieldDecl' but is wrapped in 'MemberDecl' once).
convertDecl :: TS.Node -> Convert [JTerm DeclL]
convertDecl n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "field_declaration"       -> do
      fd <- convertFieldDecl n
      pure [layer (MemberDecl fd) sp]
    "method_declaration"      -> do
      md <- convertMethodDecl n
      pure [layer (MemberDecl md) sp]
    "constructor_declaration" -> do
      cd <- convertConstructorDecl n
      pure [layer (MemberDecl cd) sp]
    "class_declaration"       -> do
      cd  <- convertClassDecl n
      let inner = layer (MemberClassDecl cd) sp
      pure [layer (MemberDecl inner) sp]
    "interface_declaration"   -> do
      idl   <- convertInterfaceDecl n
      let inner = layer (MemberInterfaceDecl idl) sp
      pure [layer (MemberDecl inner) sp]
    "enum_declaration"        -> do
      cd  <- convertEnumDecl n
      let inner = layer (MemberClassDecl cd) sp
      pure [layer (MemberDecl inner) sp]
    "static_initializer"      -> do
      -- @static_initializer@'s required child is a @block@, with no field
      -- name. Find it by kind.
      blk <- findChildOfKind "block" n >>= \case
               Just c  -> pure c
               Nothing -> bail "static_initializer has no block child"
      b   <- convertBlock blk
      pure [layer (InitDecl True b) sp]
    "block"                   -> do
      b <- convertBlock n
      pure [layer (InitDecl False b) sp]
    ";"                       -> pure []
    "line_comment"            -> pure []
    "block_comment"           -> pure []
    _                         -> unknownKind "class member" n

convertInterfaceDecl :: TS.Node -> Convert (JTerm InterfaceDeclL)
convertInterfaceDecl n = do
  sp        <- spanOf n
  modifiers <- modifiersOf n
  name      <- requireField "name" n >>= convertIdent
  tparams   <- optField "type_parameters" convertTypeParameters n >>= pure . fromMaybe []
  -- @extends_interfaces@ is a regular named child (no field name) on
  -- @interface_declaration@; it wraps a @type_list@.
  exts      <- findChildOfKind "extends_interfaces" n >>= \case
                 Nothing -> pure []
                 Just c  -> convertInterfaces c
  -- Sealed interface @permits@ clause — same story as 'convertClassDecl'.
  field "permits" n >>= \case
    Nothing -> pure ()
    Just p  -> unsupported "Java 17+ 'permits' clause on sealed interface" p
  body      <- requireField "body" n >>= convertInterfaceBody
  let kindTerm = layer (InterfaceNormal :: InterfaceKind JTerm InterfaceKindL) sp
  pure $ layer
    (InterfaceDecl
       kindTerm
       modifiers
       name
       (annList tparams)
       (annList exts)
       body)
    sp

convertInterfaceBody :: TS.Node -> Convert (JTerm InterfaceBodyL)
convertInterfaceBody n = do
  sp      <- spanOf n
  named   <- namedChildren n
  members <- concat <$> traverse interfaceMember named
  pure (layer (InterfaceBody (annList members)) sp)
  where
    interfaceMember c = nodeKind c >>= \case
      "field_declaration"           -> do { fd <- convertFieldDecl c; pure [fd] }
      "constant_declaration"        -> do { fd <- convertFieldDecl c; pure [fd] }
      "method_declaration"          -> do { md <- convertMethodDecl c; pure [md] }
      "class_declaration"           -> do
        spc <- spanOf c
        cd  <- convertClassDecl c
        pure [layer (MemberClassDecl cd) spc]
      "interface_declaration"       -> do
        spc <- spanOf c
        idl <- convertInterfaceDecl c
        pure [layer (MemberInterfaceDecl idl) spc]
      ";"                           -> pure []
      "line_comment"                -> pure []
      "block_comment"               -> pure []
      _                             -> unknownKind "interface member" c

convertEnumDecl :: TS.Node -> Convert (JTerm ClassDeclL)
convertEnumDecl n = do
  sp        <- spanOf n
  modifiers <- modifiersOf n
  name      <- requireField "name"  n >>= convertIdent
  ifaces    <- optField "interfaces" convertInterfaces n >>= pure . fromMaybe []
  body      <- requireField "body"  n >>= convertEnumBody
  pure $ layer
    (EnumDecl modifiers name (annList ifaces) body)
    sp

convertEnumBody :: TS.Node -> Convert (JTerm EnumBodyL)
convertEnumBody n = do
  sp     <- spanOf n
  named  <- namedChildren n
  consts <- collectConstants named
  decls  <- collectDecls     named
  pure (layer (EnumBody (annList consts) (annList decls)) sp)
  where
    collectConstants = fmap concat . traverse take1
      where
        take1 c = nodeKind c >>= \case
          "enum_constant" -> (:[]) <$> convertEnumConstant c
          _               -> pure []
    collectDecls = fmap concat . traverse take1
      where
        take1 c = nodeKind c >>= \case
          "enum_body_declarations" -> do
            inner <- namedChildren c
            concat <$> traverse convertDecl inner
          _ -> pure []

convertEnumConstant :: TS.Node -> Convert (JTerm EnumConstantL)
convertEnumConstant n = do
  sp   <- spanOf n
  -- enum_constant: modifiers? name=identifier arguments? body=class_body?
  -- The legacy 'EnumConstant' has no 'modifiers' slot, so annotations or
  -- keywords on a constant (e.g. @\@Deprecated FOO@) would be silently
  -- dropped. Reject explicitly instead.
  findChildOfKind "modifiers" n >>= \case
    Nothing -> pure ()
    Just ms -> unsupported "modifiers/annotations on enum constants" ms
  name <- requireField "name" n >>= convertIdent
  args <- optField "arguments" convertArguments n >>= pure . fromMaybe []
  body <- optField "body" convertClassBody n
  pure $ layer
    (EnumConstant name (annList args) (annMaybe body))
    sp

--------------------------------------------------------------------------------
-- Modifiers and annotations

-- | Convenience: looks up the (optional) @modifiers@ child of a
-- declaration node and returns the resulting annotated list. Returns
-- the empty list when absent.
--
-- Modifiers are sorted by the @show@ form of their unannotated
-- 'J.Modifier' projection so the resulting list matches what the JAR
-- oracle emits (the JAR canonicalises modifier order this way
-- regardless of the source order).
modifiersOf :: TS.Node -> Convert (JTerm [ModifierL])
modifiersOf n = do
  sp <- spanOf n
  ms <- findChildOfKind "modifiers" n >>= maybe (pure []) convertModifiers
  let key m = show (JF.untranslate (stripA m) :: J.Modifier)
  pure (annList (sortBy (compare `on` key) ms))

convertModifiers :: TS.Node -> Convert [JTerm ModifierL]
convertModifiers n = do
  -- The @modifiers@ node mixes named children (annotations) with
  -- anonymous keyword tokens (\"public\", \"static\", …). Use 'allChildren'
  -- so the keywords aren't dropped.
  cs <- allChildren n
  catSomeMods cs
  where
    catSomeMods [] = pure []
    catSomeMods (c:rest) = do
      isN <- liftIO (TS.nodeIsNamed c)
      m <- if isN
             then convertNamedModifier c
             else convertKeywordModifier c
      ms <- catSomeMods rest
      pure (case m of Nothing -> ms; Just x -> x : ms)

    convertNamedModifier c = nodeKind c >>= \case
      "marker_annotation" -> do
        spc <- spanOf c
        ann <- convertAnnotation c
        pure (Just (layer (JF.Annotation ann) spc))
      "annotation"        -> do
        spc <- spanOf c
        ann <- convertAnnotation c
        pure (Just (layer (JF.Annotation ann) spc))
      "line_comment"      -> pure Nothing
      "block_comment"     -> pure Nothing
      _                   -> Just <$> convertModifier c

    convertKeywordModifier c = do
      spc <- spanOf c
      t   <- nodeText c
      case t of
        "public"       -> pure $ Just (layer Public spc)
        "private"      -> pure $ Just (layer Private spc)
        "protected"    -> pure $ Just (layer Protected spc)
        "abstract"     -> pure $ Just (layer Abstract spc)
        "final"        -> pure $ Just (layer Final spc)
        "static"       -> pure $ Just (layer Static spc)
        "strictfp"     -> pure $ Just (layer StrictFP spc)
        "transient"    -> pure $ Just (layer Transient spc)
        "volatile"     -> pure $ Just (layer Volatile spc)
        "native"       -> pure $ Just (layer Native spc)
        "synchronized" -> pure $ Just (layer Synchronized_ spc)
        -- Per the tree-sitter-java @modifiers@ grammar these are the
        -- *only* anonymous tokens that can appear here. The legacy AST
        -- can't represent them, so reject like every other unsupported
        -- modern construct rather than silently dropping the keyword.
        "default"      -> bail "unsupported modifier: default (Java 8 default method)"
        "sealed"       -> bail "unsupported modifier: sealed (Java 17+)"
        "non-sealed"   -> bail "unsupported modifier: non-sealed (Java 17+)"
        _              -> bail $ "unknown modifier keyword: " <> BS8.unpack t

convertModifier :: TS.Node -> Convert (JTerm ModifierL)
convertModifier n = do
  sp <- spanOf n
  k  <- nodeKind n
  case k of
    "marker_annotation" -> do
      ann <- convertAnnotation n
      pure (layer (JF.Annotation ann) sp)
    "annotation"        -> do
      ann <- convertAnnotation n
      pure (layer (JF.Annotation ann) sp)
    _ -> do
      t <- nodeText n
      case t of
        "public"       -> pure (layer Public sp)
        "private"      -> pure (layer Private sp)
        "protected"    -> pure (layer Protected sp)
        "abstract"     -> pure (layer Abstract sp)
        "final"        -> pure (layer Final sp)
        "static"       -> pure (layer Static sp)
        "strictfp"     -> pure (layer StrictFP sp)
        "transient"    -> pure (layer Transient sp)
        "volatile"     -> pure (layer Volatile sp)
        "native"       -> pure (layer Native sp)
        "synchronized" -> pure (layer Synchronized_ sp)
        "default"      -> bail "unsupported modifier: default (Java 8 default method)"
        "sealed"       -> bail "unsupported modifier: sealed (Java 17+)"
        "non-sealed"   -> bail "unsupported modifier: non-sealed (Java 17+)"
        _              -> bail $ "unknown modifier: " <> BS8.unpack t

convertAnnotation :: TS.Node -> Convert (JTerm AnnotationL)
convertAnnotation n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "marker_annotation" -> do
      name <- requireField "name" n >>= convertName
      pure (layer (MarkerAnnotation name) sp)
    "annotation" -> do
      name <- requireField "name" n >>= convertName
      args <- requireField "arguments" n
      convertAnnotationArgs sp name args
    _ -> unknownKind "annotation" n

convertAnnotationArgs
  :: Maybe SourceSpan -> JTerm NameL -> TS.Node -> Convert (JTerm AnnotationL)
convertAnnotationArgs sp name args = do
  spArgs <- spanOf args
  named  <- namedChildren args
  case named of
    [single] -> do
      k <- nodeKind single
      if k == "element_value_pair"
        then do
          kvs <- traverse convertElementValuePair named
          pure $ layer
            (NormalAnnotation name (annList kvs))
            sp
        else do
          v <- convertElementValue single
          pure (layer (SingleElementAnnotation name v) sp)
    _ -> do
      kvs <- traverse convertElementValuePair named
      pure (layer (NormalAnnotation name (annList kvs)) sp)

convertElementValuePair
  :: TS.Node -> Convert (JTerm (IdentL, ElementValueL))
convertElementValuePair n = do
  sp  <- spanOf n
  key <- requireField "key" n >>= convertIdent
  val <- requireField "value" n >>= convertElementValue
  pure (annPair key val)

convertElementValue :: TS.Node -> Convert (JTerm ElementValueL)
convertElementValue n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "element_value_array_initializer" -> do
      ai <- convertElementValueArray n
      pure (layer (EVVal (layer (InitArray ai) sp)) sp)
    "marker_annotation" -> do
      ann <- convertAnnotation n
      pure (layer (EVAnn ann) sp)
    "annotation"        -> do
      ann <- convertAnnotation n
      pure (layer (EVAnn ann) sp)
    _ -> do
      e <- convertExp n
      let initT = layer (InitExp e) sp
      pure (layer (EVVal initT) sp)

-- | Tree-sitter's @element_value_array_initializer@ (the @{...}@ inside
-- annotations like @\@Foo({"a", "b"})@) maps to language-java's
-- 'ArrayInit'. Its elements are themselves element values, but
-- language-java's 'ArrayInit' is a list of 'VarInit's and cannot
-- represent a sub-annotation, so any embedded annotation is rejected.
convertElementValueArray :: TS.Node -> Convert (JTerm ArrayInitL)
convertElementValueArray n = do
  sp <- spanOf n
  vs <- namedChildren n
  inits <- traverse elementValueAsVarInit vs
  pure (layer (ArrayInit (annList inits)) sp)

elementValueAsVarInit :: TS.Node -> Convert (JTerm VarInitL)
elementValueAsVarInit n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "element_value_array_initializer" -> do
      ai <- convertElementValueArray n
      pure (layer (InitArray ai) sp)
    "marker_annotation" ->
      unsupported "annotation as annotation-array element" n
    "annotation"        ->
      unsupported "annotation as annotation-array element" n
    _ -> do
      e <- convertExp n
      pure (layer (InitExp e) sp)

--------------------------------------------------------------------------------
-- Type parameters and types

convertTypeParameters :: TS.Node -> Convert [JTerm TypeParamL]
convertTypeParameters n = do
  named <- namedChildren n
  traverse convertTypeParam named

convertTypeParam :: TS.Node -> Convert (JTerm TypeParamL)
convertTypeParam n = do
  sp    <- spanOf n
  named <- namedChildren n
  -- Java 8+ JSR-308 lets you annotate a type parameter, e.g. @\<\@A T>@.
  -- Tree-sitter exposes the annotation as the first named child of
  -- @type_parameter@, so without an explicit reject the unchecked path
  -- would 'convertIdent' the annotation node (yielding e.g. @Ident
  -- \"\@A\"@) and demote the real identifier @T@ into the bounds list.
  -- The legacy AST has no slot for type-parameter annotations, and we
  -- already reject other type-use annotations, so fail explicitly here.
  forM_ named $ \c -> nodeKind c >>= \case
    "annotation"        -> unsupported "Java 8+ annotation on type parameter" c
    "marker_annotation" -> unsupported "Java 8+ annotation on type parameter" c
    _                   -> pure ()
  case named of
    []  -> bail "empty type_parameter"
    (i : bounds) -> do
      ident <- convertIdent i
      tys   <- traverse extractBound bounds
      pure (layer (TypeParam ident (annList (concat tys))) sp)
  where
    extractBound c = nodeKind c >>= \case
      "type_bound" -> traverse convertRefType =<< namedChildren c
      _            -> singleton <$> convertRefType c
    singleton x = [x]

-- | Tree-sitter @_type@ → 'JTerm TypeL'.
convertType :: TS.Node -> Convert (JTerm TypeL)
convertType n = do
  sp <- spanOf n
  nodeKind n >>= \case
    -- Java 8+ JSR-308 type-use annotations (@Nullable String). The
    -- legacy AST has no slot for them, and the legacy parser doesn't
    -- accept them either, so reject rather than silently stripping them
    -- from the type.
    "annotated_type" ->
      unsupported "Java 8+ type-use annotation (JSR 308)" n
    "void_type" -> bail "void_type used where a value type was expected"
    "boolean_type"          -> do
      let pt = layer BooleanT sp
      pure (layer (PrimType pt) sp)
    "integral_type"         -> do
      kind <- integralKind n
      let pt = layer kind sp
      pure (layer (PrimType pt) sp)
    "floating_point_type"   -> do
      kind <- floatingKind n
      let pt = layer kind sp
      pure (layer (PrimType pt) sp)
    _ -> do
      rt <- convertRefType n
      pure (layer (RefType rt) sp)

integralKind :: TS.Node -> Convert (PrimType JTerm PrimTypeL)
integralKind n = do
  -- integral_type has a single anonymous token: "byte" | "short" | "int" | "long" | "char"
  t <- nodeText n
  case t of
    "byte"  -> pure ByteT
    "short" -> pure ShortT
    "int"   -> pure IntT
    "long"  -> pure LongT
    "char"  -> pure CharT
    _       -> bail $ "unknown integral_type: " <> BS8.unpack t

floatingKind :: TS.Node -> Convert (PrimType JTerm PrimTypeL)
floatingKind n = do
  t <- nodeText n
  case t of
    "float"  -> pure FloatT
    "double" -> pure DoubleT
    _        -> bail $ "unknown floating_point_type: " <> BS8.unpack t

convertRefType :: TS.Node -> Convert (JTerm RefTypeL)
convertRefType n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "type_identifier" -> do
      i <- convertIdent n
      let segs = annList [annPair i (annList [])]
          ct   = layer (ClassType segs) sp
      pure (layer (ClassRefType ct) sp)
    "scoped_type_identifier" -> do
      ct <- convertClassType n
      pure (layer (ClassRefType ct) sp)
    "generic_type"           -> do
      ct <- convertClassType n
      pure (layer (ClassRefType ct) sp)
    "array_type" -> do
      -- A tree-sitter @array_type@ collapses all of @int[][]@ into a
      -- single node: @element@ is the base type, @dimensions@ is the
      -- run of trailing brackets. language-java models each @[]@ as a
      -- separate 'ArrayType', so we wrap the element once per pair.
      elt  <- requireField "element"    n >>= convertType
      dims <- requireField "dimensions" n >>= countBracketPairs
      wrapArray sp dims elt
    "annotated_type" ->
      unsupported "Java 8+ type-use annotation (JSR 308)" n
    k -> bail $ "expected reference type, got " <> BS8.unpack k
  where
    -- Stays in 'Convert' so a tree-sitter invariant slip (zero @[]@ pairs
    -- on an @array_type@) raises a parse error rather than crashing the
    -- runtime via 'error'. Unreachable today per the grammar.
    wrapArray _  0 _ = bail "array_type with zero dimensions"
    wrapArray sp 1 t = pure (layer (ArrayType t) sp)
    wrapArray sp k t = do
      inner <- wrapArray sp (k - 1) t
      let inT = layer (RefType inner) sp
      pure (layer (ArrayType inT) sp)

-- | Count the @[]@ pairs in a @dimensions@ node by inspecting its
-- anonymous child tokens.
countBracketPairs :: TS.Node -> Convert Int
countBracketPairs d = do
  cs <- allChildren d
  ks <- traverse nodeText cs
  pure (length [() | k <- ks, k == "["])

-- | Apply a function @n@ times to a value (right fold).
applyTimes :: Int -> (a -> a) -> a -> a
applyTimes 0 _ x = x
applyTimes n f x = applyTimes (n - 1) f (f x)

-- | @scoped_type_identifier@ or @generic_type@ → 'JTerm ClassTypeL'.
convertClassType :: TS.Node -> Convert (JTerm ClassTypeL)
convertClassType n = do
  sp   <- spanOf n
  segs <- convertClassTypeSegments n
  pure (layer (ClassType (annList [annPair i ts | (i, ts) <- segs])) sp)

-- | Flatten a (possibly generic, possibly scoped) class-type node into
-- a list of (identifier, type-arguments) pairs, leftmost first. The
-- type-arguments are annotated lists, possibly empty.
convertClassTypeSegments
  :: TS.Node -> Convert [(JTerm IdentL, JTerm [TypeArgumentL])]
convertClassTypeSegments n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "type_identifier" -> do
      i <- convertIdent n
      pure [(i, annList [])]
    "generic_type" -> do
      named <- namedChildren n
      case named of
        [base, args] -> do
          baseSegs <- convertClassTypeSegments base
          targs    <- convertTypeArguments args
          spArgs   <- spanOf args
          case reverse baseSegs of
            ((ident, _) : rest) ->
              pure (reverse ((ident, annList targs) : rest))
            []                  ->
              pure [(layer (Ident "_anon") sp, annList targs)]
        _ -> bail "malformed generic_type"
    -- A @scoped_type_identifier@ child can be a plain @type_identifier@,
    -- another @scoped_type_identifier@, *or* a @generic_type@ when a
    -- qualifier carries its own type arguments (e.g. @Outer<String>.Inner@,
    -- @Outer<String>.Inner<Integer>@). Recurse so each child contributes
    -- its own segment(s) with the right type arguments attached.
    "scoped_type_identifier" -> do
      named <- namedChildren n
      concat <$> traverse classTypeSegment named
    k -> bail $ "expected class type, got " <> BS8.unpack k
  where
    -- Java 8+ JSR-308 lets you annotate a qualified segment, e.g.
    -- @Outer.\@A Inner@. The legacy AST has no slot for type-use
    -- annotations (the analogous 'convertType'/'convertRefType' paths
    -- already reject @annotated_type@), so reject here too rather than
    -- silently dropping the annotation.
    classTypeSegment c = nodeKind c >>= \case
      "annotation"        ->
        unsupported "Java 8+ type-use annotation on qualified type segment" c
      "marker_annotation" ->
        unsupported "Java 8+ type-use annotation on qualified type segment" c
      _                   -> convertClassTypeSegments c

convertTypeArguments :: TS.Node -> Convert [JTerm TypeArgumentL]
convertTypeArguments n = do
  named <- namedChildren n
  traverse convertTypeArgument named

-- | Like 'convertClassTypeSegments', but also reports whether the outer
-- shell is a Java 7+ diamond (a @generic_type@ whose @type_arguments@
-- has no named children). Callers can use the flag to choose between
-- 'TypeDeclSpecifier' and 'TypeDeclSpecifier{Unqualified}WithDiamond'.
-- | True if any segment of a class-type node carries explicit type
-- arguments (i.e. nests a @generic_type@). Used to detect cases like
-- @Outer.Inner\<T\>@ where the inner class is parameterised — a shape
-- some legacy AST constructors (e.g. 'QualInstanceCreation', which only
-- holds an 'Ident' for the inner name) cannot represent.
hasGenericTypeSegment :: TS.Node -> Convert Bool
hasGenericTypeSegment n = nodeKind n >>= \case
  "generic_type"           -> pure True
  "scoped_type_identifier" -> do
    kids <- namedChildren n
    or <$> traverse hasGenericTypeSegment kids
  _                        -> pure False

classTypeSegmentsAllowingDiamond
  :: TS.Node
  -> Convert ([(JTerm IdentL, JTerm [TypeArgumentL])], Bool)
classTypeSegmentsAllowingDiamond n = nodeKind n >>= \case
  "generic_type" -> do
    named <- namedChildren n
    case named of
      [base, args] -> do
        kids <- namedChildren args
        if null kids
          then do
            baseSegs <- convertClassTypeSegments base
            pure (baseSegs, True)
          else do
            segs <- convertClassTypeSegments n
            pure (segs, False)
      _ -> bail "malformed generic_type"
  _ -> do
    segs <- convertClassTypeSegments n
    pure (segs, False)

convertTypeArgument :: TS.Node -> Convert (JTerm TypeArgumentL)
convertTypeArgument n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "wildcard" -> do
      mb <- wildcardBound n
      pure (layer (Wildcard (annMaybe mb)) sp)
    _          -> do
      rt <- convertRefType n
      pure (layer (ActualType rt) sp)
  where
    wildcardBound w = do
      kids <- namedChildren w
      case kids of
        [] -> pure Nothing
        bs -> do
          let bnd = last bs
          isSuper <- anyChildText "super" w
          ty <- convertRefType bnd
          spB <- spanOf w
          pure $ Just $
            if isSuper
              then layer (SuperBound ty)   spB
              else layer (ExtendsBound ty) spB

--------------------------------------------------------------------------------
-- Methods, constructors, fields

convertFieldDecl :: TS.Node -> Convert (JTerm MemberDeclL)
convertFieldDecl n = do
  sp        <- spanOf n
  modifiers <- modifiersOf n
  ty        <- requireField "type" n >>= convertType
  decls     <- collectVarDeclarators "declarator" n
  pure (layer (FieldDecl modifiers ty (annList decls)) sp)

convertMethodDecl :: TS.Node -> Convert (JTerm MemberDeclL)
convertMethodDecl n = do
  sp        <- spanOf n
  modifiers <- modifiersOf n
  tparams   <- optField "type_parameters" convertTypeParameters n >>= pure . fromMaybe []
  mRetTy    <- requireField "type" n >>= methodReturnTypeJust
  -- C-style trailing dimensions, e.g. @int f()[]@, are surfaced by
  -- tree-sitter on a separate @dimensions@ field on @method_declaration@.
  -- They are equivalent to wrapping the return type in that many array
  -- layers, so fold them into the return type rather than dropping them.
  dimsN     <- fromMaybe 0 <$> optField "dimensions" countDimensionPairs n
  let retT = annMaybe (fmap (wrapArrayDims sp dimsN) mRetTy)
  name      <- requireField "name" n >>= convertIdent
  ps        <- requireField "parameters" n >>= convertFormalParameters
  throws    <- methodThrows n
  body      <- methodBody n sp
  pure $ layer
    (MethodDecl
       modifiers
       (annList tparams)
       retT
       name
       (annList ps)
       (annList throws)
       (annMaybe Nothing)
       body)
    sp
  where
    methodReturnTypeJust t = nodeKind t >>= \case
      "void_type" -> pure Nothing
      _           -> Just <$> convertType t
    methodThrows nn = do
      mt <- findChildOfKind "throws" nn
      case mt of
        Nothing -> pure []
        Just t  -> namedChildren t >>= traverse convertRefType
    methodBody nn sp0 = do
      mb <- optField "body" convertBlock nn
      pure (layer (MethodBody (annMaybe mb)) sp0)

-- | Count the @[]@ pairs in a tree-sitter @dimensions@ node. The pairs are
-- anonymous children (the only named children are annotations), so we look
-- at the raw @[@ tokens directly.
countDimensionPairs :: TS.Node -> Convert Int
countDimensionPairs n = do
  kids <- allChildren n
  ks   <- traverse nodeKind kids
  pure $ length [() | k <- ks, k == "["]

-- | Wrap a type in @n@ array layers — used to fold trailing dimensions
-- from declarators (@int x[]@, @int f()[]@) into the canonical
-- array-of-T return\/variable type.
wrapArrayDims :: Maybe SourceSpan -> Int -> JTerm TypeL -> JTerm TypeL
wrapArrayDims _  0 t = t
wrapArrayDims sp k t =
  let arr = layer (ArrayType t) sp
      ty  = layer (RefType arr) sp
  in wrapArrayDims sp (k - 1) ty

convertConstructorDecl :: TS.Node -> Convert (JTerm MemberDeclL)
convertConstructorDecl n = do
  sp        <- spanOf n
  modifiers <- modifiersOf n
  tparams   <- optField "type_parameters" convertTypeParameters n >>= pure . fromMaybe []
  name      <- requireField "name" n >>= convertIdent
  ps        <- requireField "parameters" n >>= convertFormalParameters
  throws    <- do
                 mt <- findChildOfKind "throws" n
                 case mt of
                   Nothing -> pure []
                   Just t  -> namedChildren t >>= traverse convertRefType
  body      <- requireField "body" n >>= convertConstructorBody
  pure $ layer
    (ConstructorDecl
       modifiers
       (annList tparams)
       name
       (annList ps)
       (annList throws)
       body)
    sp

convertConstructorBody :: TS.Node -> Convert (JTerm ConstructorBodyL)
convertConstructorBody n = do
  sp <- spanOf n
  -- Same shape as 'convertBlock': we have to walk all children so we
  -- catch standalone-";" empty statements that the named-children view
  -- hides. We also have to extract the optional leading
  -- 'explicit_constructor_invocation' separately because language-java
  -- gives it its own slot in 'ConstructorBody'.
  cs <- allChildren n
  (eci, rest) <- splitEci cs
  stmts <- concat <$> traverse blockChild rest
  pure (layer (ConstructorBody (annMaybe eci) (annList stmts)) sp)
  where
    splitEci [] = pure (Nothing, [])
    splitEci (c:rest) = do
      isN <- liftIO (TS.nodeIsNamed c)
      k <- nodeKind c
      case (isN, k) of
        (False, "{") -> splitEci rest
        -- Comments are *named* tree-sitter children, so they sit between
        -- the opening brace and a leading this()/super(). Skip past them
        -- (blockChild's convertBlockStmt drops them too) so the ECI still
        -- attaches to ConstructorBody instead of falling through to
        -- convertBlockStmt, which rejects explicit_constructor_invocation.
        (True, "line_comment")  -> splitEci rest
        (True, "block_comment") -> splitEci rest
        (True, "explicit_constructor_invocation") -> do
          e <- convertExplConstrInv c
          pure (Just e, rest)
        _ -> pure (Nothing, c : rest)
    blockChild c = do
      isN <- liftIO (TS.nodeIsNamed c)
      k <- nodeKind c
      if isN
        then convertBlockStmt c
        else case k of
               ";" -> do
                 spc <- spanOf c
                 let emptyStmt = layer (Empty :: Stmt JTerm StmtL) spc
                 pure [layer (BlockStmt emptyStmt) spc]
               _   -> pure []

convertExplConstrInv :: TS.Node -> Convert (JTerm ExplConstrInvL)
convertExplConstrInv n = do
  sp <- spanOf n
  -- explicit_constructor_invocation:
  --     ( primary "." )? type_arguments? constructor=("this"|"super") arguments
  -- The "constructor" field is a 'this' or 'super' named node; the
  -- leading primary (when present) qualifies a super() call (only).
  ctorKind <- requireField "constructor" n >>= nodeKind
  primary  <- optField "object"         convertExp         n
  targs    <- optField "type_arguments" convertTypeArgRefs n >>= pure . fromMaybe []
  args     <- requireField "arguments" n >>= convertArguments
  case (primary, ctorKind) of
    (Nothing, "this")  ->
      pure (layer (ThisInvoke  (annList targs) (annList args)) sp)
    (Nothing, "super") ->
      pure (layer (SuperInvoke (annList targs) (annList args)) sp)
    (Just e,  "super") ->
      pure (layer (PrimarySuperInvoke e (annList targs) (annList args)) sp)
    (Just _,  "this")  ->
      bail "explicit_constructor_invocation: this() cannot have a primary qualifier"
    (_, k)             ->
      bail $ "explicit_constructor_invocation: unexpected constructor kind " <> BS8.unpack k

convertTypeArgRefs :: TS.Node -> Convert [JTerm RefTypeL]
convertTypeArgRefs n = do
  named <- namedChildren n
  -- Drop wildcard arguments — only @ActualType@ is meaningful for the
  -- explicit-constructor-invocation type-arg slot in language-java.
  let pickRefs = mapMaybeM $ \c -> nodeKind c >>= \case
        "wildcard" -> pure Nothing
        _          -> Just <$> convertRefType c
  pickRefs named

convertFormalParameters :: TS.Node -> Convert [JTerm FormalParamL]
convertFormalParameters n = do
  named <- namedChildren n
  traverse convertFormalParam named

convertFormalParam :: TS.Node -> Convert (JTerm FormalParamL)
convertFormalParam n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "formal_parameter" -> do
      modifiers <- modifiersOf n
      ty        <- requireField "type" n >>= convertType
      -- C-style trailing brackets in a parameter list (e.g. @int x[]@)
      -- show up in the optional @dimensions@ field. The JAR keeps these
      -- on the variable-declarator side ('VarDeclArray') rather than
      -- folding them into the type, so do the same here.
      extra     <- optField "dimensions" countBracketPairs n
      baseNm    <- requireField "name" n >>= convertVarDeclId
      let nm = applyTimes (fromMaybe 0 extra)
                          (\inner -> layer (VarDeclArray inner) sp)
                          baseNm
      pure (layer (FormalParam modifiers ty False nm) sp)
    "spread_parameter" -> do
      modifiers <- modifiersOf n
      named <- namedChildren n
      tagged <- traverse (\c -> (\k -> (c, k)) <$> nodeKind c) named
      let typeNodes =
            [ c
            | (c, k) <- tagged
            , k /= "modifiers"
            , k /= "annotation"
            , k /= "marker_annotation"
            , k /= "variable_declarator"
            ]
          declNodes = [ c | (c, k) <- tagged, k == "variable_declarator" ]
      case (typeNodes, declNodes) of
        ([ty], [vd]) -> do
          t  <- convertType ty
          vd' <- convertVarDeclator vd
          vid <- extractVarId vd'
          pure (layer (FormalParam modifiers t True vid) sp)
        ([ty], []) -> do
          -- A spread_parameter with only the type — pretty rare
          t <- convertType ty
          let ident     = layer (Ident "_") sp
              vid       = layer (VarId ident) sp
          pure (layer (FormalParam modifiers t True vid) sp)
        _ -> bail "malformed spread_parameter"
    k -> bail $ "expected formal_parameter, got " <> BS8.unpack k

-- | Pull the 'VarDeclId' out of a 'VarDecl' annotated term, by reading the
-- corresponding J.VarDecl. This is used only on the rare
-- 'spread_parameter' path.
extractVarId :: JTerm VarDeclL -> Convert (JTerm VarDeclIdL)
extractVarId vd = do
  -- A 'VarDecl' has a single 'VarDeclId' child (and an optional
  -- initialiser); we already constructed it from the tree-sitter
  -- variable_declarator, so it's safe to re-walk here.
  case JF.untranslate (stripA vd) of
    J.VarDecl _ _ ->
      -- We can't recover the JTerm easily, so we just produce a fresh
      -- VarId with the same name. Spread parameters are rare and
      -- typically simple, so a single-segment rebuild is fine.
      pure (rebuildVarId (JF.untranslate (stripA vd)))
  where
    rebuildVarId :: J.VarDecl -> JTerm VarDeclIdL
    rebuildVarId (J.VarDecl vid _) = goId vid
    goId (J.VarId (J.Ident s))     =
      layer (VarId (layer (Ident s) Nothing)) Nothing
    goId (J.VarDeclArray inner)    =
      layer (VarDeclArray (goId inner)) Nothing

-- | tree-sitter variable_declarator → 'JTerm VarDeclL'.
convertVarDeclator :: TS.Node -> Convert (JTerm VarDeclL)
convertVarDeclator n = do
  sp       <- spanOf n
  baseName <- requireField "name" n >>= convertVarDeclId
  extra    <- optField "dimensions" countBracketPairs n
  let nm = applyTimes (fromMaybe 0 extra)
                      (\inner -> layer (VarDeclArray inner) sp)
                      baseName
  init <- optField "value" convertVarInit n
  pure (layer (VarDecl nm (annMaybe init)) sp)

convertVarInit :: TS.Node -> Convert (JTerm VarInitL)
convertVarInit n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "array_initializer" -> do
      ai <- convertArrayInit n
      pure (layer (InitArray ai) sp)
    _                   -> do
      e <- convertExp n
      pure (layer (InitExp e) sp)

convertArrayInit :: TS.Node -> Convert (JTerm ArrayInitL)
convertArrayInit n = do
  sp <- spanOf n
  named <- namedChildren n
  inits <- traverse convertVarInit named
  pure (layer (ArrayInit (annList inits)) sp)

convertVarDeclId :: TS.Node -> Convert (JTerm VarDeclIdL)
convertVarDeclId n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "identifier" -> do
      i <- convertIdent n
      pure (layer (VarId i) sp)
    -- Old-style array syntax: name in formal_parameter followed by dimensions.
    -- tree-sitter folds `int[] x` into array_type, but `int x[]` is a
    -- distinct case attached at the declarator level. We approximate.
    k -> bail $ "expected identifier as variable name, got " <> BS8.unpack k

collectVarDeclarators :: ByteString -> TS.Node -> Convert [JTerm VarDeclL]
collectVarDeclarators fname n = do
  declNodes <- fieldAll fname n
  traverse convertVarDeclator declNodes

--------------------------------------------------------------------------------
-- Statements and blocks

convertBlock :: TS.Node -> Convert (JTerm BlockL)
convertBlock n = do
  sp <- spanOf n
  -- Walk all children so we don't drop standalone empty statements
  -- (the ";" token is anonymous in tree-sitter-java's grammar but is
  -- a real 'Empty' statement in language-java's AST).
  cs <- allChildren n
  stmts <- concat <$> traverse blockChild cs
  pure (layer (Block (annList stmts)) sp)
  where
    blockChild c = do
      isN <- liftIO (TS.nodeIsNamed c)
      k <- nodeKind c
      if isN
        then convertBlockStmt c
        else case k of
               ";" -> do
                 spc <- spanOf c
                 let emptyStmt = layer (Empty :: Stmt JTerm StmtL) spc
                 pure [layer (BlockStmt emptyStmt) spc]
               _   -> pure []  -- "{" "}" and other punctuation

convertBlockStmt :: TS.Node -> Convert [JTerm BlockStmtL]
convertBlockStmt n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "local_variable_declaration" -> do
      modifiers <- modifiersOf n
      ty        <- requireField "type" n >>= convertType
      decls     <- collectVarDeclarators "declarator" n
      pure [layer (LocalVars modifiers ty (annList decls)) sp]
    "class_declaration" -> do
      c <- convertClassDecl n
      pure [layer (LocalClass c) sp]
    "line_comment"  -> pure []
    "block_comment" -> pure []
    _ -> do
      s <- convertStmt n
      pure [layer (BlockStmt s) sp]

convertStmt :: TS.Node -> Convert (JTerm StmtL)
convertStmt n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "block" -> do
      b <- convertBlock n
      pure (layer (StmtBlock b) sp)
    "expression_statement" -> do
      named <- namedChildren n
      case named of
        [e] -> do
          ex <- convertExp e
          pure (layer (ExpStmt ex) sp)
        _   -> bail "expression_statement: expected a single expression child"
    "if_statement" -> do
      cond     <- requireField "condition"   n >>= convertParenExp
      conseq   <- requireField "consequence" n >>= convertStmt
      alt      <- optField    "alternative"   convertStmt n
      case alt of
        Nothing -> pure (layer (IfThen cond conseq) sp)
        Just a  -> pure (layer (IfThenElse cond conseq a) sp)
    "while_statement" -> do
      cond <- requireField "condition" n >>= convertParenExp
      body <- requireField "body"      n >>= convertStmt
      pure (layer (While cond body) sp)
    "do_statement" -> do
      body <- requireField "body"      n >>= convertStmt
      cond <- requireField "condition" n >>= convertParenExp
      pure (layer (Do body cond) sp)
    "for_statement" -> convertForStmt n
    "enhanced_for_statement" -> convertEnhancedForStmt n
    "return_statement" -> do
      named <- namedChildren n
      case named of
        []  -> pure (layer (Return (annMaybe Nothing)) sp)
        [e] -> do
          ex <- convertExp e
          pure (layer (Return (annMaybe (Just ex))) sp)
        _   -> bail "malformed return_statement"
    "break_statement" -> do
      named <- namedChildren n
      case named of
        []  -> pure (layer (Break (annMaybe Nothing)) sp)
        [i] -> do
          ident <- convertIdent i
          pure (layer (Break (annMaybe (Just ident))) sp)
        _   -> bail "malformed break_statement"
    "continue_statement" -> do
      named <- namedChildren n
      case named of
        []  -> pure (layer (Continue (annMaybe Nothing)) sp)
        [i] -> do
          ident <- convertIdent i
          pure (layer (Continue (annMaybe (Just ident))) sp)
        _   -> bail "malformed continue_statement"
    "throw_statement" -> do
      named <- namedChildren n
      case named of
        [e] -> do
          ex <- convertExp e
          pure (layer (Throw ex) sp)
        _   -> bail "malformed throw_statement"
    "try_statement"            -> convertTryStmt n
    "try_with_resources_statement" -> unsupported "try-with-resources" n
    -- tree-sitter-java unifies the traditional switch statement and the
    -- Java 14+ switch expression under "switch_expression". When it
    -- appears as a statement, lower it to 'Switch' (the legacy form);
    -- the expression form is rejected in 'convertExp'.
    "switch_expression"        -> convertSwitchStmt n
    "switch_statement"         -> convertSwitchStmt n
    "synchronized_statement"   -> do
      paren <- findChildOfKind "parenthesized_expression" n >>= \case
                Just c  -> pure c
                Nothing -> bail "synchronized_statement has no parenthesized_expression"
      e   <- convertParenExp paren
      blk <- requireField "body" n >>= convertBlock
      pure (layer (Synchronized e blk) sp)
    "labeled_statement" -> do
      named <- namedChildren n
      case named of
        [i, s] -> do
          ident <- convertIdent i
          stm   <- convertStmt s
          pure (layer (Labeled ident stm) sp)
        _      -> bail "malformed labeled_statement"
    "assert_statement" -> do
      named <- namedChildren n
      case named of
        [e]    -> do
          ex <- convertExp e
          pure (layer (Assert ex (annMaybe Nothing)) sp)
        [e, m] -> do
          ex <- convertExp e
          mx <- convertExp m
          pure (layer (Assert ex (annMaybe (Just mx))) sp)
        _      -> bail "malformed assert_statement"
    ";" -> pure (layer (Empty :: Stmt JTerm StmtL) sp)
    _   -> unknownKind "statement" n

convertParenExp :: TS.Node -> Convert (JTerm ExpL)
convertParenExp n = nodeKind n >>= \case
  "parenthesized_expression" -> do
    named <- namedChildren n
    case named of
      [e] -> convertExp e
      _   -> bail "malformed parenthesized_expression"
  _ -> convertExp n

convertForStmt :: TS.Node -> Convert (JTerm StmtL)
convertForStmt n = do
  sp          <- spanOf n
  initNodes   <- fieldAll "init" n
  finit       <- buildForInit sp initNodes
  fcond       <- optField "condition" convertExp n
  updateNodes <- fieldAll "update" n
  fupdExps    <- traverse convertExp updateNodes
  let fupd =
        if null updateNodes
          then annMaybe Nothing
          else annMaybe (Just (annList fupdExps))
  body     <- requireField "body" n >>= convertStmt
  pure (layer (BasicFor (annMaybe finit) (annMaybe fcond) fupd body) sp)
  where
    buildForInit _  []     = pure Nothing
    buildForInit sp [c]    = nodeKind c >>= \case
      "local_variable_declaration" -> Just <$> lvdAsInit sp c
      _                            -> do
        e <- convertExp c
        let asLayer = layer (ForInitExps (annList [e])) sp
        pure (Just asLayer)
    buildForInit sp cs     = do
      -- Multiple init nodes can only be expressions (a single
      -- local_variable_declaration covers multiple declarators).
      es <- traverse convertExp cs
      pure (Just (layer (ForInitExps (annList es)) sp))
    lvdAsInit sp c = do
      spc       <- spanOf c
      modifiers <- modifiersOf c
      ty        <- requireField "type" c >>= convertType
      decls     <- collectVarDeclarators "declarator" c
      pure (layer (ForLocalVars modifiers ty (annList decls)) sp)

convertEnhancedForStmt :: TS.Node -> Convert (JTerm StmtL)
convertEnhancedForStmt n = do
  sp        <- spanOf n
  modifiers <- modifiersOf n
  ty        <- requireField "type"  n >>= convertType
  nm        <- requireField "name"  n >>= convertIdent
  iter      <- requireField "value" n >>= convertExp
  body      <- requireField "body"  n >>= convertStmt
  pure (layer (EnhancedFor modifiers ty nm iter body) sp)

convertTryStmt :: TS.Node -> Convert (JTerm StmtL)
convertTryStmt n = do
  sp      <- spanOf n
  blk     <- requireField "body" n >>= convertBlock
  named   <- namedChildren n
  -- named[0] is the body; subsequent named children are catch clauses
  -- and an optional finally_clause.
  let rest = drop 1 named
  catches <- collectCatches rest
  fin     <- collectFinally  rest
  pure (layer (Try blk (annList catches) (annMaybe fin)) sp)
  where
    collectCatches = foldlM step []
      where
        step acc c = nodeKind c >>= \case
          "catch_clause" -> do
            ct <- convertCatchClause c
            pure (acc ++ [ct])
          _              -> pure acc
    collectFinally cs = do
      hits <- traverse pick cs
      case [x | Just x <- hits] of
        []    -> pure Nothing
        (b:_) -> pure (Just b)
      where
        pick c = nodeKind c >>= \case
          "finally_clause" -> do
            named <- namedChildren c
            case named of
              [b] -> Just <$> convertBlock b
              _   -> bail "malformed finally_clause"
          _ -> pure Nothing

convertCatchClause :: TS.Node -> Convert (JTerm CatchL)
convertCatchClause n = do
  sp     <- spanOf n
  -- @catch_formal_parameter@ is a regular named child of @catch_clause@,
  -- not a field. The block body is a field.
  cfp    <- findChildOfKind "catch_formal_parameter" n >>= \case
              Just c  -> pure c
              Nothing -> bail "catch_clause has no catch_formal_parameter"
  formal <- convertCatchFormal cfp
  body   <- requireField "body" n >>= convertBlock
  pure (layer (Catch formal body) sp)
  where
    convertCatchFormal cf = do
      spCf <- spanOf cf
      nodeKind cf >>= \case
        "catch_formal_parameter" -> do
          modifiers <- modifiersOf cf
          cty       <- findChildOfKind "catch_type" cf >>= \case
                         Just c  -> pure c
                         Nothing -> bail "catch_formal_parameter has no catch_type"
          ty        <- convertCatchType cty
          nm        <- requireField "name" cf >>= convertVarDeclId
          pure (layer (FormalParam modifiers ty False nm) spCf)
        k -> bail $ "expected catch_formal_parameter, got " <> BS8.unpack k
    -- multi-catch types: @t1 | t2 | t3@ — language-java doesn't have this.
    convertCatchType cf = nodeKind cf >>= \case
      "catch_type" -> do
        named <- namedChildren cf
        case named of
          [single] -> convertType single
          _        -> unsupported "multi-catch (union type)" cf
      _ -> convertType cf

convertSwitchStmt :: TS.Node -> Convert (JTerm StmtL)
convertSwitchStmt n = do
  sp     <- spanOf n
  scr    <- requireField "condition" n >>= convertParenExp
  body   <- requireField "body"      n
  named  <- namedChildren body
  blocks <- concat <$> traverse collectBlocks named
  pure (layer (Switch scr (annList blocks)) sp)
  where
    collectBlocks c = nodeKind c >>= \case
      "switch_block_statement_group" -> convertSwitchBlocks c
      -- Java 14+ arrow form @case X -> ...@. The legacy AST has no
      -- representation for it, so reject explicitly rather than silently
      -- dropping every label and statement.
      "switch_rule" -> unsupported "Java 14+ arrow switch rule" c
      k             -> unknownKind ("switch body child (" <> BS8.unpack k <> ")") c
    convertSwitchBlocks c = do
      spc <- spanOf c
      gn  <- namedChildren c
      (labels, stmts) <- splitGroups gn
      ss <- concat <$> traverse convertBlockStmt stmts
      case labels of
        []      -> bail "switch group with no label"
        [lbl]   -> pure [layer (SwitchBlock lbl (annList ss)) spc]
        _       ->
          let emptyBlocks = [ layer (SwitchBlock lbl (annList [])) spc | lbl <- init labels ]
              finalBlock  = layer (SwitchBlock (last labels) (annList ss)) spc
          in pure (emptyBlocks <> [finalBlock])
    splitGroups [] = pure ([], [])
    splitGroups (c:cs) = do
      k <- nodeKind c
      case k of
        "switch_label" -> do
          ls <- convertSwitchLabels c
          (restLabels, restStmts) <- splitGroups cs
          pure (ls <> restLabels, restStmts)
        _ -> do
          (restLabels, restStmts) <- splitGroups cs
          pure (restLabels, c : restStmts)
    convertSwitchLabels c = do
      sp' <- spanOf c
      named <- namedChildren c
      kinds <- traverse nodeKind named
      if any (`elem` ["guard", "pattern"]) kinds
        then unsupported "guarded or pattern switch label" c
        else case named of
          [] -> pure [layer (Default :: SwitchLabel JTerm SwitchLabelL) sp']
          es -> traverse (\e -> layer . SwitchCase <$> convertExp e <*> pure sp') es

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap (foldr step []) . traverse f
  where step Nothing  acc = acc
        step (Just x) acc = x : acc

--------------------------------------------------------------------------------
-- Expressions

convertArguments :: TS.Node -> Convert [JTerm ExpL]
convertArguments n = do
  named <- namedChildren n
  traverse convertExp named

-- | Convert any expression-bearing tree-sitter node to 'JTerm ExpL'.
convertExp :: TS.Node -> Convert (JTerm ExpL)
convertExp n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "parenthesized_expression" -> do
      named <- namedChildren n
      case named of
        [e] -> convertExp e
        _   -> bail "malformed parenthesized_expression"

    -- Literals
    "decimal_integer_literal"        -> do
      lit <- parseIntLit  <$> nodeTextS n
      pure (layer (Lit (layer lit sp)) sp)
    "hex_integer_literal"            -> do
      lit <- parseIntLit  <$> nodeTextS n
      pure (layer (Lit (layer lit sp)) sp)
    "octal_integer_literal"          -> do
      raw <- nodeTextS n
      when (hasNonJavaOctalPrefix raw) $
        bail ("malformed octal_integer_literal " <> show raw <> ": Java does not support 0o/0O prefixes")
      let lit = parseIntLit raw
      pure (layer (Lit (layer lit sp)) sp)
    "binary_integer_literal"         -> do
      lit <- parseIntLit  <$> nodeTextS n
      pure (layer (Lit (layer lit sp)) sp)
    "decimal_floating_point_literal" -> do
      raw <- nodeTextS n
      lit <- case parseFloatLit raw of
        Right l  -> pure l
        Left err -> bail ("malformed decimal_floating_point_literal " <> show raw <> ": " <> err)
      pure (layer (Lit (layer lit sp)) sp)
    "hex_floating_point_literal"     -> do
      raw <- nodeTextS n
      lit <- case parseFloatLit raw of
        Right l  -> pure l
        Left err -> bail ("malformed hex_floating_point_literal " <> show raw <> ": " <> err)
      pure (layer (Lit (layer lit sp)) sp)
    "character_literal"              -> do
      raw <- nodeTextS n
      lit <- case parseCharLit raw of
        Right l  -> pure l
        Left err -> bail ("malformed character_literal " <> show raw <> ": " <> err)
      pure (layer (Lit (layer lit sp)) sp)
    "string_literal"                 -> do
      s <- stringLiteralText n
      pure (layer (Lit (layer (JF.String s) sp)) sp)
    "true"                           -> pure (layer (Lit (layer (JF.Boolean True)  sp)) sp)
    "false"                          -> pure (layer (Lit (layer (JF.Boolean False) sp)) sp)
    "null_literal"                   -> pure (layer (Lit (layer JF.Null sp)) sp)

    -- Names and primaries
    "identifier"      -> do
      i <- convertIdent n
      let nm = layer (Name (annList [i])) sp
      pure (layer (ExpName nm) sp)
    "scoped_identifier" -> do
      nm <- convertName n
      pure (layer (ExpName nm) sp)
    "this"            -> pure (layer This sp)
    "super"           -> bail "bare 'super' is not a valid expression"

    -- Field access (with the @X.this@ special case lifted to 'ThisClass')
    "field_access" -> do
      fld <- requireField "field" n
      nodeKind fld >>= \case
        "this" -> do
          obj <- requireField "object" n
          nm  <- convertName obj
          pure (layer (ThisClass nm) sp)
        _ -> do
          fa <- convertFieldAccess n
          pure (layer (FieldAccess fa) sp)

    -- Method invocation
    "method_invocation" -> do
      mi <- convertMethodInvocation n
      pure (layer (MethodInv mi) sp)

    -- Array access
    "array_access" -> do
      ai <- convertArrayAccess n
      pure (layer (ArrayAccess ai) sp)

    -- Object creation
    "object_creation_expression" -> convertObjectCreation n
    "array_creation_expression"  -> convertArrayCreation  n

    -- Assignments
    "assignment_expression" -> convertAssignExp n

    -- Binary / unary / update / cast / ternary / instanceof
    "binary_expression"     -> convertBinExp n
    "unary_expression"      -> convertUnaryExp n
    "update_expression"     -> convertUpdateExp n
    "cast_expression"       -> convertCastExp n
    "ternary_expression"    -> convertTernaryExp n
    "instanceof_expression" -> convertInstanceofExp n

    -- Class literals: e.g. `Foo.class`
    "class_literal" -> convertClassLiteral n

    -- Unsupported modern Java
    "lambda_expression"       -> unsupported "lambda expression" n
    "method_reference"        -> unsupported "method reference" n
    "switch_expression"       -> unsupported "switch expression" n
    "text_block"              -> unsupported "text block" n
    "yield_statement"         -> unsupported "yield statement" n

    k -> unknownKind ("expression (kind " <> BS8.unpack k <> ")") n

convertFieldAccess :: TS.Node -> Convert (JTerm FieldAccessL)
convertFieldAccess n = do
  sp    <- spanOf n
  obj   <- requireField "object" n
  fld   <- requireField "field"  n >>= convertIdent
  objKind <- nodeKind obj
  hasSuperQual <- hasMiddleSuper n
  case objKind of
    "super" -> pure (layer (SuperFieldAccess fld) sp)
    "this"  -> do
      spObj <- spanOf obj
      let thisE = layer This spObj
      pure (layer (PrimaryFieldAccess thisE fld) sp)
    _ | hasSuperQual -> do
          nm <- convertName obj
          pure (layer (ClassFieldAccess nm fld) sp)
      | otherwise -> do
          e <- convertExp obj
          pure (layer (PrimaryFieldAccess e fld) sp)
  where
    hasMiddleSuper p = do
      objNode <- requireField "object" p
      objStart <- liftIO (TS.nodeStartByte objNode)
      cs <- namedChildren p
      go objStart cs
      where
        go _ [] = pure False
        go objStart (c:rest) = do
          k <- nodeKind c
          if k == "super"
            then do
              cStart <- liftIO (TS.nodeStartByte c)
              if cStart /= objStart
                then pure True
                else go objStart rest
            else go objStart rest

convertArrayAccess :: TS.Node -> Convert (JTerm ArrayIndexL)
convertArrayAccess n = do
  sp  <- spanOf n
  arr <- requireField "array" n >>= convertExp
  idx <- requireField "index" n >>= convertExp
  pure (layer (ArrayIndex arr (annList [idx])) sp)

-- | Core shape rule: a method invocation builds either 'MethodCall' (no
-- qualifier, single-ident name) or one of 'PrimaryMethodCall' /
-- 'SuperMethodCall' / 'ClassMethodCall' (qualified). The trailing
-- identifier is always the method name and goes in its own slot —
-- never folded back into a multi-segment 'Name'.
convertMethodInvocation :: TS.Node -> Convert (JTerm MethodInvocationL)
convertMethodInvocation n = do
  sp     <- spanOf n
  mObj   <- field "object" n
  mTArgs <- optField "type_arguments" convertTypeArgRefs n >>= pure . fromMaybe []
  name   <- requireField "name" n >>= convertIdent
  args   <- requireField "arguments" n >>= convertArguments
  case mObj of
    Nothing -> do
      -- Java requires a qualifier (Type/Expr/this/super) when explicit
      -- method type arguments are present (JLS §15.12); tree-sitter's
      -- current grammar reflects that. Defensively reject if a future
      -- grammar update ever exposes @<T>m()@ as @method_invocation@
      -- with no @object@ — 'MethodCall' has no slot for type arguments
      -- so silently dropping them would be a lossy parse.
      unless (null mTArgs) $
        bail "unqualified method invocation cannot carry explicit type arguments"
      let nm = layer (Name (annList [name])) sp
      pure (layer (MethodCall nm (annList args)) sp)
    Just obj -> do
      objKind <- nodeKind obj
      hasSuperQual <- hasMiddleSuper n obj
      mQualName <- if not (null mTArgs) then nodeAsName obj else pure Nothing
      case objKind of
        "super" -> pure (layer (SuperMethodCall (annList mTArgs) name (annList args)) sp)
        _ | hasSuperQual -> do
              nm <- convertName obj
              pure (layer (ClassMethodCall nm (annList mTArgs) name (annList args)) sp)
          -- @A.<T>m()@ / @pkg.A.<T>m()@: the qualifier is a dotted-name and
          -- explicit type arguments are present. language-java represents
          -- this with the dedicated 'TypeMethodCall', distinct from a
          -- 'PrimaryMethodCall' on a runtime expression. Without this case
          -- consumers can't tell the two apart, even for valid source the
          -- legacy parser distinguishes.
          | Just qual <- mQualName ->
              pure (layer (TypeMethodCall qual (annList mTArgs) name (annList args)) sp)
          | otherwise -> do
              e <- convertExp obj
              pure (layer (PrimaryMethodCall e (annList mTArgs) name (annList args)) sp)
  where
    hasMiddleSuper p objNode = do
      objStart <- liftIO (TS.nodeStartByte objNode)
      cs <- namedChildren p
      go objStart cs
      where
        go _ [] = pure False
        go objStart (c:rest) = do
          k <- nodeKind c
          if k == "super"
            then do
              cStart <- liftIO (TS.nodeStartByte c)
              if cStart /= objStart
                then pure True
                else go objStart rest
            else go objStart rest

convertObjectCreation :: TS.Node -> Convert (JTerm ExpL)
convertObjectCreation n = do
  sp      <- spanOf n
  ty      <- requireField "type" n
  tyStart <- liftIO (TS.nodeStartByte ty)
  named   <- namedChildren n
  qual    <- case named of
               []      -> pure Nothing
               (c : _) -> do
                 cStart <- liftIO (TS.nodeStartByte c)
                 pure $ if cStart < tyStart then Just c else Nothing
  mQual   <- traverse convertExp qual
  -- The Java 7+ diamond @<>@ surfaces here as a @generic_type@ whose
  -- @type_arguments@ has no named children. It's part of the @type@
  -- field, *not* the outer @type_arguments@ field (which is reserved for
  -- explicit prefix type args, e.g. @new \<T\>Foo()@).
  (segs, isDiamond) <- classTypeSegmentsAllowingDiamond ty
  targs   <- fromMaybe [] <$> optField "type_arguments" convertTypeArguments n
  let classTy = layer (ClassType (annList [annPair i ts | (i, ts) <- segs])) sp
  args    <- requireField "arguments" n >>= convertArguments
  body    <- findChildOfKind "class_body" n >>= traverse convertClassBody
  case mQual of
    Nothing -> do
      tyDeclSpec <- if isDiamond
        then diamondTypeDeclSpec sp segs
        else pure (layer (TypeDeclSpecifier classTy) sp)
      pure $ layer
        (InstanceCreation
           (annList targs)
           tyDeclSpec
           (annList args)
           (annMaybe body))
        sp
    Just o  -> do
      -- @QualInstanceCreation@ in language-java has no diamond form, so we
      -- reject @obj.new Inner<>()@ explicitly rather than silently dropping
      -- the diamond marker.
      when isDiamond $
        unsupported "diamond <> on qualified object creation" n
      -- @QualInstanceCreation@ also takes only an 'Ident' (no type args)
      -- for the inner class name, so it can't carry @<String>@ in
      -- @obj.new Inner<String>()@. Reject rather than silently dropping
      -- the inner type arguments.
      innerHasGenerics <- hasGenericTypeSegment ty
      when innerHasGenerics $
        unsupported "type arguments on inner class in qualified object creation" n
      -- For qualified instance creation, extract the trailing ident as
      -- the inner-class name, not the full type-decl spec. We pull the
      -- ident name out of the J.ClassType projection.
      let jct = JF.untranslate (stripA classTy) :: J.ClassType
      case jct of
        J.ClassType ((J.Ident s, _) : _) -> do
          let identTerm = layer (Ident s) sp
          pure $ layer
            (QualInstanceCreation
               o
               (annList targs)
               identTerm
               (annList args)
               (annMaybe body))
            sp
        _ -> bail "malformed qualified object_creation_expression"

-- | Build a 'TypeDeclSpecifier' for a diamond-typed @new Foo<>()@ /
-- @new Outer.Inner<>()@. The unqualified form uses the dedicated
-- single-ident constructor; nested types split off the trailing ident as
-- the inner name and keep the rest as the qualifier 'ClassType'.
diamondTypeDeclSpec
  :: Maybe SourceSpan
  -> [(JTerm IdentL, JTerm [TypeArgumentL])]
  -> Convert (JTerm TypeDeclSpecifierL)
diamondTypeDeclSpec sp segs = case reverse segs of
  [] -> bail "object_creation_expression: diamond <> with no type segments"
  [(i, _)] ->
    pure $ layer
      (TypeDeclSpecifierUnqualifiedWithDiamond i (layer Diamond sp))
      sp
  ((i, _) : restRev) ->
    let qual = layer
                 (ClassType (annList [annPair j ts | (j, ts) <- reverse restRev]))
                 sp
    in pure $ layer (TypeDeclSpecifierWithDiamond qual i (layer Diamond sp)) sp

convertArrayCreation :: TS.Node -> Convert (JTerm ExpL)
convertArrayCreation n = do
  sp       <- spanOf n
  ty       <- requireField "type" n >>= convertType
  dimNodes <- fieldAll "dimensions" n
  vals     <- optField "value" convertArrayInit n
  parts <- traverse classify dimNodes
  let sized = [ e | DimSized e <- parts ]
      empty = sum   [ k | DimEmpty k <- parts ]
  case vals of
    Just ai -> pure (layer (ArrayCreateInit ty empty ai) sp)
    Nothing -> pure (layer (ArrayCreate ty (annList sized) empty) sp)
  where
    classify d = nodeKind d >>= \case
      "dimensions_expr" -> do
        named <- namedChildren d
        case named of
          [e] -> DimSized <$> convertExp e
          _   -> DimEmpty <$> countBracketPairs d
      "dimensions" -> DimEmpty <$> countBracketPairs d
      k            -> bail $ "unexpected child of array_creation_expression dimensions: "
                           <> BS8.unpack k

data DimPart = DimSized (JTerm ExpL) | DimEmpty Int

convertAssignExp :: TS.Node -> Convert (JTerm ExpL)
convertAssignExp n = do
  sp   <- spanOf n
  lhsN <- requireField "left"  n
  rhsN <- requireField "right" n
  op   <- requireField "operator" n
  opT  <- nodeText op
  lhs  <- convertLhs lhsN
  rhs  <- convertExp rhsN
  asg  <- parseAssignOp op opT
  pure (layer (Assign lhs asg rhs) sp)

-- | Critical shape: assignment LHS is one of NameLhs (single ident),
-- FieldLhs (anything qualified), or ArrayLhs. Never NameLhs of a
-- multi-ident Name.
convertLhs :: TS.Node -> Convert (JTerm LhsL)
convertLhs n = do
  sp <- spanOf n
  nodeKind n >>= \case
    "identifier"       -> do
      i <- convertIdent n
      let nm = layer (Name (annList [i])) sp
      pure (layer (NameLhs nm) sp)
    "field_access"     -> do
      fa <- convertFieldAccess n
      pure (layer (FieldLhs fa) sp)
    "array_access"     -> do
      ai <- convertArrayAccess n
      pure (layer (ArrayLhs ai) sp)
    "scoped_identifier" -> do
      scope <- requireField "scope" n
      name  <- requireField "name"  n
      e     <- convertExp scope
      i     <- convertIdent name
      let fa = layer (PrimaryFieldAccess e i) sp
      pure (layer (FieldLhs fa) sp)
    k -> bail $ "unexpected assignment LHS: " <> BS8.unpack k

parseAssignOp :: TS.Node -> ByteString -> Convert (JTerm AssignOpL)
parseAssignOp opNode t = do
  sp <- spanOf opNode
  case t of
    "="    -> pure (layer EqualA sp)
    "*="   -> pure (layer MultA sp)
    "/="   -> pure (layer DivA sp)
    "%="   -> pure (layer RemA sp)
    "+="   -> pure (layer AddA sp)
    "-="   -> pure (layer SubA sp)
    "<<="  -> pure (layer LShiftA sp)
    ">>="  -> pure (layer RShiftA sp)
    ">>>=" -> pure (layer RRShiftA sp)
    "&="   -> pure (layer AndA sp)
    "^="   -> pure (layer XorA sp)
    "|="   -> pure (layer OrA sp)
    _      -> bail $ "unknown assignment operator: " <> BS8.unpack t

convertBinExp :: TS.Node -> Convert (JTerm ExpL)
convertBinExp n = do
  sp  <- spanOf n
  lhs <- requireField "left"  n >>= convertExp
  rhs <- requireField "right" n >>= convertExp
  opN <- requireField "operator" n
  op  <- nodeText opN
  o   <- parseBinOp opN op
  pure (layer (BinOp lhs o rhs) sp)

parseBinOp :: TS.Node -> ByteString -> Convert (JTerm OpL)
parseBinOp opNode t = do
  sp <- spanOf opNode
  case t of
    "*"   -> pure (layer Mult sp)
    "/"   -> pure (layer Div sp)
    "%"   -> pure (layer Rem sp)
    "+"   -> pure (layer Add sp)
    "-"   -> pure (layer Sub sp)
    "<<"  -> pure (layer LShift sp)
    ">>"  -> pure (layer RShift sp)
    ">>>" -> pure (layer RRShift sp)
    "<"   -> pure (layer LThan sp)
    ">"   -> pure (layer GThan sp)
    "<="  -> pure (layer LThanE sp)
    ">="  -> pure (layer GThanE sp)
    "=="  -> pure (layer Equal sp)
    "!="  -> pure (layer NotEq sp)
    "&"   -> pure (layer And sp)
    "|"   -> pure (layer Or sp)
    "^"   -> pure (layer Xor sp)
    "&&"  -> pure (layer CAnd sp)
    "||"  -> pure (layer COr sp)
    _     -> bail $ "unknown binary operator: " <> BS8.unpack t

convertUnaryExp :: TS.Node -> Convert (JTerm ExpL)
convertUnaryExp n = do
  sp <- spanOf n
  op <- requireField "operator" n >>= nodeText
  eN <- requireField "operand"  n
  e  <- convertExp eN
  case op of
    "+" -> pure (layer (PrePlus e) sp)
    -- The JAR (= javac's parser) folds @-N@ into a negative literal
    -- only when the positive @N@ would overflow its declared type
    -- (i.e. @-Integer.MIN_VALUE@ and @-Long.MIN_VALUE@). Smaller values
    -- stay as 'PreMinus' applied to a positive literal.
    "-" -> case JF.untranslate (stripA e) :: J.Exp of
             J.Lit (J.Int  m) | m >  intMax  -> do
               spLit <- spanOf eN
               let lit = layer (JF.Int  (negate m)) spLit
               pure (layer (Lit lit) sp)
             J.Lit (J.Word m) | m >  longMax -> do
               spLit <- spanOf eN
               let lit = layer (JF.Word (negate m)) spLit
               pure (layer (Lit lit) sp)
             _                               ->
               pure (layer (PreMinus e) sp)
    "~" -> pure (layer (PreBitCompl e) sp)
    "!" -> pure (layer (PreNot e) sp)
    t   -> bail $ "unknown unary operator: " <> BS8.unpack t
  where
    intMax  = (2 :: Integer) ^ (31 :: Int) - 1
    longMax = (2 :: Integer) ^ (63 :: Int) - 1

convertUpdateExp :: TS.Node -> Convert (JTerm ExpL)
convertUpdateExp n = do
  sp <- spanOf n
  -- update_expression: e++, e--, ++e, --e. tree-sitter exposes both the
  -- operand and the operator as children (no field name). The named
  -- child is always the operand; the anonymous "++"/"--" token decides
  -- prefix vs postfix by its position relative to the operand.
  cs <- allChildren n
  case cs of
    [a, b] -> do
      aIsNamed <- liftIO (TS.nodeIsNamed a)
      bIsNamed <- liftIO (TS.nodeIsNamed b)
      let (operandNode, opNode, isPostfix) = case (aIsNamed, bIsNamed) of
            (True,  False) -> (a, b, True)   -- operand first → postfix
            (False, True)  -> (b, a, False)  -- operator first → prefix
            _              -> (a, b, True)   -- fallback; shouldn't happen
      operand <- convertExp operandNode
      opTxt   <- nodeText opNode
      case (isPostfix, opTxt) of
        (True,  "++") -> pure (layer (PostIncrement operand) sp)
        (True,  "--") -> pure (layer (PostDecrement operand) sp)
        (False, "++") -> pure (layer (PreIncrement  operand) sp)
        (False, "--") -> pure (layer (PreDecrement  operand) sp)
        (_,     t)    -> bail $ "unknown update operator: " <> BS8.unpack t
    _ -> bail "malformed update_expression: expected exactly 2 children"

convertCastExp :: TS.Node -> Convert (JTerm ExpL)
convertCastExp n = do
  sp <- spanOf n
  -- @cast_expression.type@ is a /repeated/ field so tree-sitter can carry
  -- Java 8+ intersection casts like @(Foo & Bar) x@. Legacy 'Cast' only
  -- holds a single 'Type', so reject the multi-bound form rather than
  -- silently dropping all but the first bound (which 'requireField'
  -- would do).
  tyNodes <- fieldAll "type" n
  ty <- case tyNodes of
    [t] -> convertType t
    []  -> bail "missing 'type' field on cast_expression"
    _   -> unsupported "Java 8+ intersection cast (multi-bound type)" n
  e  <- requireField "value" n >>= convertExp
  pure (layer (Cast ty e) sp)

convertTernaryExp :: TS.Node -> Convert (JTerm ExpL)
convertTernaryExp n = do
  sp <- spanOf n
  c <- requireField "condition"   n >>= convertExp
  t <- requireField "consequence" n >>= convertExp
  f <- requireField "alternative" n >>= convertExp
  pure (layer (Cond c t f) sp)

convertInstanceofExp :: TS.Node -> Convert (JTerm ExpL)
convertInstanceofExp n = do
  sp <- spanOf n
  -- Java 16+ extends instanceof with a type-pattern binding —
  -- @obj instanceof String s@ exposes the binder via the optional
  -- @name@ field. Java 21+ record patterns surface via @pattern@.
  -- The legacy AST has no representation for either, so reject
  -- explicitly rather than silently producing a plain @InstanceOf@.
  field "name" n >>= \case
    Nothing -> pure ()
    Just p  -> unsupported "Java 16+ instanceof type-pattern binding" p
  field "pattern" n >>= \case
    Nothing -> pure ()
    Just p  -> unsupported "Java 21+ instanceof record pattern" p
  e  <- requireField "left"  n >>= convertExp
  ty <- requireField "right" n >>= convertRefType
  pure (layer (InstanceOf e ty) sp)

convertClassLiteral :: TS.Node -> Convert (JTerm ExpL)
convertClassLiteral n = do
  sp    <- spanOf n
  named <- namedChildren n
  case named of
    [t] -> nodeKind t >>= \case
      "void_type" -> pure (layer (ClassLit (annMaybe Nothing)) sp)
      _           -> do
        ty <- convertType t
        pure (layer (ClassLit (annMaybe (Just ty))) sp)
    _ -> bail "malformed class_literal"

--------------------------------------------------------------------------------
-- Literal parsing

-- | Tree-sitter gives us the literal lexeme verbatim (including underscore
-- separators, type suffixes, and base prefixes). Decode it the way the
-- JAR oracle does:
--
--   * @int@ literal → 'Int' fragment; @long@ literal → 'Word' fragment.
--   * Hex / octal / binary literals that have the high bit set in their
--     declared width are sign-extended to a negative value (matching
--     Java's signed two's-complement interpretation).
parseIntLit :: String -> Literal JTerm LiteralL
parseIntLit raw =
  let stripped         = filter (/= '_') raw
      (digits, isLong) = case reverse stripped of
        ('L':rs) -> (reverse rs, True)
        ('l':rs) -> (reverse rs, True)
        _        -> (stripped,   False)
      (isDecimal, n) = readIntBase digits
      width  = if isLong then 64 else 32 :: Int
      signed = if isDecimal then n else signExtend width n
  in if isLong then JF.Word signed else JF.Int signed
  where
    readIntBase ('0':'x':rs) = (False, readBase 16 rs)
    readIntBase ('0':'X':rs) = (False, readBase 16 rs)
    readIntBase ('0':'b':rs) = (False, readBase 2  rs)
    readIntBase ('0':'B':rs) = (False, readBase 2  rs)
    readIntBase ('0':rs) | all isDigit rs && not (null rs)
                             = (False, readBase 8 rs)
    readIntBase rs           = (True,  readBase 10 rs)

    readBase b = foldl (\acc c -> acc * fromIntegral b + fromIntegral (digitValue c)) 0
    digitValue c
      | isDigit c    = ord c - ord '0'
      | isHexDigit c = 10 + ord c - ord (if c >= 'a' then 'a' else 'A')
      | otherwise    = 0

hasNonJavaOctalPrefix :: String -> Bool
hasNonJavaOctalPrefix raw =
  case filter (/= '_') raw of
    '0' : p : _ -> p == 'o' || p == 'O'
    _           -> False

-- | Sign-extend an unsigned magnitude into a signed integer of the given
-- bit width (32 or 64).
signExtend :: Int -> Integer -> Integer
signExtend width n
  | n < high  = n
  | otherwise = n - (1 `shiftL` width)
  where
    high = 1 `shiftL` (width - 1)

-- | Decode a Java floating-point literal lexeme. tree-sitter's lexer
-- should already accept only well-formed source, but if it ever doesn't
-- (or 'parseJavaFloat' is incomplete) we want a 'Convert' error rather
-- than an uncatchable runtime crash, so return 'Either' and let the
-- caller 'bail' with span info.
parseFloatLit :: String -> Either String (Literal JTerm LiteralL)
parseFloatLit raw =
  let stripped = filter (/= '_') raw
      (body, isFloat) = case reverse stripped of
        ('f':rs) -> (reverse rs, True)
        ('F':rs) -> (reverse rs, True)
        ('d':rs) -> (reverse rs, False)
        ('D':rs) -> (reverse rs, False)
        _        -> (stripped,   False)
  in case parseJavaFloat body of
       Nothing -> Left "could not parse the lexeme as a Double"
       Just v  -> Right (if isFloat then JF.Float v else JF.Double v)

parseJavaFloat :: String -> Maybe Double
parseJavaFloat s
  | any (`elem` ("pP" :: String)) s = parseJavaHexFloat s
  | otherwise = readMaybe (normaliseJavaDecimalFloat s)

normaliseJavaDecimalFloat :: String -> String
normaliseJavaDecimalFloat s =
  let s1 = case s of
        '.':_ -> '0' : s
        _     -> s
      (mantissa, exponentPart) = break (`elem` ("eE" :: String)) s1
      mantissa' = case reverse mantissa of
        '.':_ -> mantissa <> "0"
        _     -> mantissa
  in mantissa' <> exponentPart

parseJavaHexFloat :: String -> Maybe Double
parseJavaHexFloat s = do
  body <- stripHexPrefix =<< Just s
  let (mantissa, exponentPart) = break (`elem` ("pP" :: String)) body
  exponentDigits <- case exponentPart of
    []       -> Nothing
    (_:rest) -> Just rest
  -- @readMaybe \@Int@ doesn't accept a leading @+@, but Java's hex-float
  -- exponent syntax does (e.g. @0x1.0p+1@). Strip an explicit @+@ so both
  -- signed and unsigned forms parse.
  let exponentDigits' = case exponentDigits of
        '+':rest -> rest
        _        -> exponentDigits
  exponentValue <- readMaybe exponentDigits'
  let (wholeDigits, fracPart) = break (== '.') mantissa
      fracDigits = case fracPart of
        []       -> ""
        (_:rest) -> rest
  if null wholeDigits && null fracDigits
    then Nothing
    else do
      wholeValue <- parseHexInteger wholeDigits
      fracValue  <- parseHexFraction fracDigits
      pure ((fromIntegral wholeValue + fracValue) * (2 ^^ exponentValue))
  where
    stripHexPrefix ('0':'x':rest) = Just rest
    stripHexPrefix ('0':'X':rest) = Just rest
    stripHexPrefix _              = Nothing

parseHexInteger :: String -> Maybe Integer
parseHexInteger = foldlM step 0
  where
    step acc c = do
      d <- hexDigitValue c
      pure (acc * 16 + fromIntegral d)

parseHexFraction :: String -> Maybe Double
parseHexFraction digits =
  foldr step (Just 0) digits
  where
    step c acc = do
      d <- hexDigitValue c
      rest <- acc
      pure ((fromIntegral d + rest) / 16.0)

hexDigitValue :: Char -> Maybe Int
hexDigitValue c
  | isDigit c = Just (ord c - ord '0')
  | c >= 'a' && c <= 'f' = Just (10 + ord c - ord 'a')
  | c >= 'A' && c <= 'F' = Just (10 + ord c - ord 'A')
  | otherwise = Nothing

-- | Decode a tree-sitter @character_literal@ lexeme (with surrounding
-- single quotes) into a 'JF.Char'. tree-sitter-java's grammar accepts
-- one /or more/ body characters, but javac requires exactly one, so we
-- reject multi-character and empty literals here rather than silently
-- truncating to the first character.
parseCharLit :: String -> Either String (Literal JTerm LiteralL)
parseCharLit raw = case decodeJavaEscapes (unquote raw) of
  [c]   -> Right (JF.Char c)
  []    -> Left "empty character literal"
  cs    -> Left ("expected exactly one character, got " <> show (length cs))
  where
    unquote ('\'':rest) = case reverse rest of
      ('\'':rs) -> reverse rs
      _         -> rest
    unquote s = s

-- | Extract the contents of a Java string literal, decoding escape sequences.
stringLiteralText :: TS.Node -> Convert String
stringLiteralText n = do
  raw <- nodeTextS n
  pure $ decodeJavaEscapes $ case raw of
    ('"':rest) -> dropTrailingQuote rest
    _          -> raw
  where
    dropTrailingQuote s = case reverse s of
      ('"':rs) -> reverse rs
      _        -> s

-- | Decode a Java char-or-string body's escape sequences (per JLS 3.10.6).
decodeJavaEscapes :: String -> String
decodeJavaEscapes ('\\':c:rest) = case c of
  'n'  -> '\n' : decodeJavaEscapes rest
  't'  -> '\t' : decodeJavaEscapes rest
  'r'  -> '\r' : decodeJavaEscapes rest
  'b'  -> '\b' : decodeJavaEscapes rest
  'f'  -> '\f' : decodeJavaEscapes rest
  '\\' -> '\\' : decodeJavaEscapes rest
  '\'' -> '\'' : decodeJavaEscapes rest
  '"'  -> '"'  : decodeJavaEscapes rest
  's'  -> ' '  : decodeJavaEscapes rest        -- text-block escape, harmless here
  'u'  -> let dropExtraUs (xu:xs) | xu == 'u' = dropExtraUs xs
              dropExtraUs xs                  = xs
              hexBody = dropExtraUs rest
          in case hexBody of
               (h1:h2:h3:h4:rs) | all isHexDigit [h1,h2,h3,h4] ->
                 chr (readHex [h1,h2,h3,h4]) : decodeJavaEscapes rs
               _ -> 'u' : decodeJavaEscapes rest
  _ | isOctalDigit c ->
      let (digs, after) = takeOctal 2 (rest, [c])
          val           = readOctal digs
      in chr val : decodeJavaEscapes after
    | otherwise -> c : decodeJavaEscapes rest
  where
    takeOctal :: Int -> (String, String) -> (String, String)
    takeOctal 0 (s, acc) = (reverse acc, s)
    takeOctal k (s@(d:ds), acc)
      | isOctalDigit d
      , length acc < 3
      , not (length acc == 2 && firstAccChar acc > '3')
        = takeOctal (k - 1) (ds, d : acc)
      | otherwise = (reverse acc, s)
    takeOctal _ ([], acc) = (reverse acc, [])

    firstAccChar acc = case reverse acc of
      (c:_) -> c
      _     -> '\0'

    readOctal = foldl (\a d -> a * 8 + (ord d - ord '0')) 0
    readHex   = foldl (\a c' -> a * 16 + hexDig c') 0
    hexDig c'
      | isDigit c'    = ord c' - ord '0'
      | isHexDigit c' = 10 + ord c' - ord (if c' >= 'a' then 'a' else 'A')
      | otherwise     = 0
    isOctalDigit c' = c' >= '0' && c' <= '7'
decodeJavaEscapes (c:rest) = c : decodeJavaEscapes rest
decodeJavaEscapes []       = []
