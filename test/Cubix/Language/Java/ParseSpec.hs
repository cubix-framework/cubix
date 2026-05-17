{-# LANGUAGE TypeApplications #-}

module Cubix.Language.Java.ParseSpec (spec) where

import Control.Exception (SomeException, evaluate, try)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import System.IO.Temp (writeSystemTempFile)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldContain, shouldReturn)

import Data.Comp.Multi (AnnTerm, E (..))
import Data.Comp.Multi.Generic qualified as Generic

import Cubix.Language.Java.Parse qualified as JParse
import Cubix.ParsePretty qualified as ParsePretty

import Language.Java.Pretty (prettyPrint)
import Language.Java.Parser qualified as JP
import Language.Java.Syntax
  ( Block (Block)
  , BlockStmt (BlockStmt)
  , ClassBody (ClassBody)
  , ClassDecl (ClassDecl)
  , ClassType (ClassType)
  , CompilationUnit (CompilationUnit)
  , Decl (MemberDecl)
  , FieldAccess (PrimaryFieldAccess)
  , FormalParam (FormalParam)
  , Ident (Ident)
  , Literal (String)
  , MemberDecl (FieldDecl, MethodDecl)
  , MethodBody (MethodBody)
  , MethodInvocation (PrimaryMethodCall)
  , Modifier (Public, Static)
  , Name (Name)
  , PrimType (IntT)
  , RefType (ArrayType, ClassRefType)
  , Stmt (ExpStmt)
  , Type (PrimType, RefType)
  , TypeDecl (ClassTypeDecl)
  , VarDecl (VarDecl)
  , VarDeclId (VarId)
  , VarInit (InitExp)
  )
import Language.Java.Syntax qualified as J
import Cubix.Language.Java.Parametric.Common (MJavaSig)
import Cubix.Language.Java.Parametric.Full (JavaSig)
import Cubix.Language.Info (SourceSpan)
import Cubix.Sin.Compdata.Annotation (getAnn)
import Cubix.Language.Parametric.SourcePos.Test (sliceSpan)

spec :: Spec
spec = describe "tree-sitter Java parser" $ do
  it "parses helloWorld.java to the JAR's expected shape" $ do
    let src = unlines
          [ "public class HelloWorld {"
          , "    public static void main(String[] args) {"
          , "        System.out.println(\"Hello, World!\");"
          , "    }"
          , "}"
          ]
    path <- writeSystemTempFile "HelloWorld.java" src
    res <- JParse.parse path
    case res of
      Left err -> expectationFailure $ "parse failed: " <> err
      Right cu -> cu `shouldBe` expectedHelloWorld

  it "parses Foo.java (control flow) without errors" $ do
    res <- JParse.parseFile "input-files/java/Foo.java"
    case res of
      Nothing -> expectationFailure "tree-sitter failed to parse Foo.java"
      Just _  -> pure ()

  it "parses Bar.java without errors" $ do
    res <- JParse.parseFile "input-files/java/Bar.java"
    case res of
      Nothing -> expectationFailure "tree-sitter failed to parse Bar.java"
      Just _  -> pure ()

  it "parses modifiers on varargs parameters" $ do
    _ <- parseSnippet "FinalVarargs.java" $ unlines
      [ "class FinalVarargs {"
      , "    void f(final String... xs) {"
      , "    }"
      , "}"
      ]
    pure ()

  it "parses annotation array element values" $ do
    _ <- parseSnippet "AnnotationArrayValue.java" annotationArrayValueSrc
    pure ()

  it "proves the legacy language-java parser accepted annotation array element values" $ do
    res <- legacyParseCompilationUnit annotationArrayValueSrc
    case res of
      Left err -> expectationFailure $ "legacy parser failed: " <> err
      Right _  -> pure ()

  it "parses comments before constructor super()/this() invocations" $ do
    _ <- parseSnippet "CtorCommentBeforeSuper.java" ctorCommentBeforeSuperSrc
    pure ()

  it "proves the legacy language-java parser accepted comments before constructor super()" $ do
    res <- legacyParseCompilationUnit ctorCommentBeforeSuperSrc
    case res of
      Left err -> expectationFailure $ "legacy parser failed: " <> err
      Right _  -> pure ()

  it "parses inline comments inside return/throw/assert/break/continue/labeled" $ do
    _ <- parseSnippet "InlineComments.java" inlineCommentsSrc
    pure ()

  it "proves the legacy language-java parser accepted inline statement comments" $ do
    res <- legacyParseCompilationUnit inlineCommentsSrc
    case res of
      Left err -> expectationFailure $ "legacy parser failed: " <> err
      Right _  -> pure ()

  it "parses awkwardly-placed comments across class/method/expression structure" $ do
    -- Stress-test: comments wedged between every kind of named child that
    -- tree-sitter exposes — class header (extends/implements), method
    -- signature (return type, throws, params), expressions (binary, cast,
    -- ternary, instanceof), parenthesised conditions, and statement
    -- operands. Tree-sitter classifies comments as named children, so any
    -- arity check or positional index on @namedChildren@ that doesn't
    -- filter them will mis-parse these inputs.
    _ <- parseSnippet "AwkwardComments.java" awkwardCommentsSrc
    pure ()

  it "proves the legacy language-java parser accepted awkwardly-placed comments" $ do
    res <- legacyParseCompilationUnit awkwardCommentsSrc
    case res of
      Left err -> expectationFailure $ "legacy parser failed: " <> err
      Right _  -> pure ()

  it "rejects type-use annotations on type parameters" $ do
    -- tree-sitter exposes @\@A@ as the first named child of
    -- @type_parameter@, so the unchecked code path would call
    -- 'convertIdent' on the annotation and emit
    -- @TypeParam (Ident "@A") …@ — a malformed AST that hides the
    -- annotation /and/ mislabels the type parameter.
    -- (Use @\@Deprecated@ so the annotation type is on the classpath,
    -- avoiding the orthogonal annotation_type_declaration rejection.)
    res <- JParse.parse =<< writeSystemTempFile "AnnotatedTParam.java" (unlines
      [ "class AnnotatedTParam<@Deprecated T> {"
      , "    T value;"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for annotated type parameter, got: " <> show cu

  it "accepts Java Unicode escapes outside string/char bodies" $ do
    -- @\\u003b@ is the encoded form of @;@ and is recognised by javac as
    -- a pre-lexical source transformation. Likewise @caf\\u00e9@ is a
    -- legal identifier spelling @café@. If we don't decode these before
    -- handing the bytes to tree-sitter, the file is rejected or the
    -- identifier text is wrong.
    cu <- parseSnippet "UnicodeEscapes.java" $ unlines
      [ "class UnicodeEscapeT {"
      , "    int x\\u003b"
      , "    String caf\\u00e9 = null;"
      , "}"
      ]
    case cu of
      CompilationUnit _ _
        [ ClassTypeDecl (ClassDecl _ _ _ _ _
            (ClassBody
              [ MemberDecl (FieldDecl _ _ [VarDecl (VarId (Ident a)) _])
              , MemberDecl (FieldDecl _ _ [VarDecl (VarId (Ident b)) _])
              ]))
        ] -> do
          a `shouldBe` "x"
          b `shouldBe` "caf\233"  -- 4 chars: c, a, f, é (U+00E9)
      _ -> expectationFailure $ "unexpected AST: " <> show cu

  it "rejects non-Java 0o/0O octal integer prefixes" $ do
    -- tree-sitter-java's @octal_integer_literal@ accepts @0o@/@0O@ as
    -- well as the spec's leading-zero form. javac doesn't, so feeding
    -- @0o7@ through @parseIntLit@'s digit-fold would silently produce
    -- @Int 7@ instead of failing the parse.
    res <- JParse.parse =<< writeSystemTempFile "OctalPrefix.java" (unlines
      [ "class OctalPrefix {"
      , "    int x = 0o7;"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for 0o-prefixed integer, got: " <> show cu

  it "rejects unqualified <T>m() calls (explicit type args, no qualifier)" $ do
    -- @this.<T>m()@ would round-trip through TypeMethodCall; the
    -- unqualified @<T>m()@ form has no MethodCall arm that carries type
    -- arguments, so the parser silently drops <T> if we let it build
    -- @MethodCall (Name [m]) args@. Reject explicitly.
    res <- JParse.parse =<< writeSystemTempFile "UnqualGenericCall.java" (unlines
      [ "class UnqualGenericCall {"
      , "    <T> T m() { return null; }"
      , "    void g() { this.m(); UnqualGenericCall.<String>m(); }"
      , "    void h() { <String>m(); }"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for unqualified <T>m() call, got: " <> show cu

  it "rejects parameterized qualified object creation (obj.new Inner<T>())" $ do
    -- @QualInstanceCreation@ takes only an @Ident@ for the inner type,
    -- so @obj.new Inner<String>()@ has nowhere to store @<String>@.
    -- Reject the same way the diamond @obj.new Inner<>()@ form is.
    res <- JParse.parse =<< writeSystemTempFile "QualNewParam.java" (unlines
      [ "class QualNewParam {"
      , "    class Inner<T> {}"
      , "    Object f(QualNewParam o) { return o.new Inner<String>(); }"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for parameterized qualified new, got: "
          <> show cu

  it "rejects multi-character char literals" $ do
    -- tree-sitter-java's @character_literal@ accepts one /or more/ body
    -- characters, but javac rejects anything but a single character.
    -- Without explicit validation the parser silently keeps only the
    -- first decoded char and produces @Char 'a'@ for @'ab'@.
    res <- JParse.parse =<< writeSystemTempFile "MultiCharLit.java" (unlines
      [ "class MultiCharLit {"
      , "    char c = 'ab';"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for multi-character char literal, got: " <> show cu

  it "accepts a single-character literal expressed via a Unicode escape" $ do
    -- Sanity check: a literal that *decodes* to one character but whose
    -- source is multiple bytes (\\u00e9) must still parse successfully.
    _ <- parseSnippet "UnicodeEscapeCharLit.java" $ unlines
      [ "class UnicodeEscapeCharLit {"
      , "    char c = '\\u00e9';"
      , "}"
      ]
    pure ()

  it "applies Java Unicode escapes before lexing structural tokens" $ do
    _ <- parseSnippet "UnicodeEscapeSemicolon.java" $ unlines
      [ "class UnicodeEscapeSemicolon {"
      , "    int x\\u003b"
      , "}"
      ]
    pure ()

  it "applies Java Unicode escapes before lexing identifiers" $ do
    cu <- parseSnippet "UnicodeEscapeIdentifier.java" $ unlines
      [ "class UnicodeEscapeIdentifier {"
      , "    int caf\\u00e9;"
      , "}"
      ]
    case cu of
      CompilationUnit _ _
        [ ClassTypeDecl (ClassDecl _ _ _ _ _
            (ClassBody
              [ MemberDecl
                  (FieldDecl _ _
                    [ VarDecl
                        (VarId (Ident name))
                        Nothing
                    ])
              ]))
        ] -> name `shouldBe` "café"
      _ -> expectationFailure $ "unexpected AST: " <> show cu

  it "does not pre-lex ineligible Unicode escapes after escaped backslashes" $ do
    cu <- parseSnippet "IneligibleUnicodeEscape.java" $ unlines
      [ "class IneligibleUnicodeEscape {"
      , "    String s = \"\\\\u0041\";"
      , "}"
      ]
    case cu of
      CompilationUnit _ _
        [ ClassTypeDecl (ClassDecl _ _ _ _ _
            (ClassBody
              [ MemberDecl
                  (FieldDecl _ _
                    [ VarDecl
                        (VarId (Ident "s"))
                        (Just (InitExp (J.Lit (String litVal))))
                    ])
              ]))
        ] -> litVal `shouldBe` "\\u0041"
      _ -> expectationFailure $ "unexpected AST: " <> show cu

  it "uses TypeMethodCall for type-qualified generic invocations (A.<T>m())" $ do
    cu <- parseSnippet "TypeQualifiedGenericCall.java" $ unlines
      [ "class TypeQualifiedGenericCall {"
      , "    static <T> T m() { return null; }"
      , "    Object a = TypeQualifiedGenericCall.<String>m();"
      , "    Object b = java.util.Collections.<String>emptyList();"
      , "}"
      ]
    -- The qualifier is a Name (identifier / scoped_identifier) and an
    -- explicit type-arg list is present, so language-java models this
    -- with the dedicated 'TypeMethodCall' constructor. Without this
    -- distinction the parser collapses everything to PrimaryMethodCall
    -- and downstream consumers can't tell them apart.
    let shown = show cu
    shown `shouldContain` "TypeMethodCall"

  it "rejects type-use annotations on qualified type segments (Outer.@A Inner)" $ do
    -- @Outer.\@A Inner@ surfaces the @\@A@ as a child of
    -- @scoped_type_identifier@; without rejection it would be silently
    -- dropped from the segment list, producing a 'ClassType' that hides
    -- the JSR-308 annotation.
    res <- JParse.parse =<< writeSystemTempFile "QualAnn.java" (unlines
      [ "class QualAnn {"
      , "    @interface A {}"
      , "    static class Outer { static class Inner {} }"
      , "    Outer.@A Inner f;"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for annotated qualified type, got: " <> show cu

  it "rejects Java 8+ type-use annotations (JSR 308)" $ do
    -- @\@Nullable String s@ would silently drop the annotation in
    -- 'convertType'/'convertRefType'. Reject so the AST stays faithful.
    res <- JParse.parse =<< writeSystemTempFile "TypeUseAnn.java" (unlines
      [ "class TypeUseAnn {"
      , "    @interface Nullable {}"
      , "    @Nullable String s;"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for type-use annotation, got: " <> show cu

  it "rejects modifiers/annotations on enum constants" $ do
    -- @\@Deprecated FOO@: language-java's @EnumConstant@ has no slot
    -- for modifiers, so we must reject rather than silently drop.
    res <- JParse.parse =<< writeSystemTempFile "EnumConstMods.java" (unlines
      [ "enum E {"
      , "    @Deprecated FOO"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for annotated enum constant, got: " <> show cu

  it "rejects Java 8+ intersection casts" $ do
    -- @(Foo & Bar) x@ has multiple types on cast_expression.type;
    -- requireField would take only the first, silently dropping the
    -- rest. The legacy AST can't represent the intersection, so we
    -- must hard-fail.
    res <- JParse.parse =<< writeSystemTempFile "IntersectionCast.java" (unlines
      [ "interface Foo {}"
      , "interface Bar {}"
      , "class IntersectionCast {"
      , "    Object f(Object o) { return (Foo & Bar) o; }"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for intersection cast, got: " <> show cu

  it "rejects Java 16+ instanceof pattern bindings" $ do
    -- @obj instanceof String s@ binds @s@; the legacy AST has no slot
    -- for that, so we must hard-fail instead of producing a plain
    -- @InstanceOf@ and silently dropping the binding.
    res <- JParse.parse =<< writeSystemTempFile "InstanceofPattern.java" (unlines
      [ "class InstanceofPattern {"
      , "    boolean f(Object o) { return o instanceof String s; }"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for instanceof pattern, got: " <> show cu

  it "rejects Java 8+ 'default' modifier on interface methods" $ do
    -- @default void f() {}@ would build a plain MethodDecl without the
    -- @default@ modifier, silently changing semantics. Reject like the
    -- other unsupported keywords.
    res <- JParse.parse =<< writeSystemTempFile "DefaultMod.java" (unlines
      [ "interface I {"
      , "    default void f() {}"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for 'default' modifier, got: " <> show cu

  it "rejects Java 17+ 'sealed'/'non-sealed' modifiers" $ do
    res <- JParse.parse =<< writeSystemTempFile "SealedMod.java" (unlines
      [ "sealed class S permits T {}"
      , "non-sealed class T extends S {}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for sealed/non-sealed, got: " <> show cu

  it "rejects Java 14+ arrow switch rules instead of silently dropping them" $ do
    -- Arrow form @case X -> ...@ surfaces as @switch_rule@; the legacy
    -- parser doesn't accept it, and we'd rather hard-fail than build a
    -- Switch with all labels and bodies silently elided.
    res <- JParse.parse =<< writeSystemTempFile "ArrowSwitch.java" (unlines
      [ "class ArrowSwitch {"
      , "    int f(int x) {"
      , "        switch (x) {"
      , "            case 1 -> { return 10; }"
      , "            default -> { return 0; }"
      , "        }"
      , "        return -1;"
      , "    }"
      , "}"
      ])
    case res of
      Left _  -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for arrow switch, got: " <> show cu

  it "preserves generic type arguments on scoped class types (Outer<T>.Inner)" $ do
    cu <- parseSnippet "ScopedGenericType.java" $ unlines
      [ "class ScopedGenericType {"
      , "    Outer<String>.Inner field;"
      , "}"
      ]
    -- Both segments survive: outer carries <String>, inner is bare.
    -- Without preserving the qualifier's type-args the parser drops
    -- @<String>@ silently or fails outright.
    let shown = show cu
    shown `shouldContain` "Outer"
    shown `shouldContain` "Inner"
    shown `shouldContain` "String"

  it "proves the legacy language-java parser accepted scoped generics" $ do
    res <- legacyParseCompilationUnit $ unlines
      [ "class ScopedGenericType {"
      , "    Outer<String>.Inner field;"
      , "}"
      ]
    case res of
      Left err -> expectationFailure $ "legacy parser failed: " <> err
      Right _  -> pure ()

  it "parses hex float literals with signed exponents" $ do
    cu <- parseSnippet "HexFloatSignedExp.java" hexFloatSignedExpSrc
    -- Pretty-printing forces the lazily-parsed Double payloads, so a
    -- crashing literal would surface here rather than hiding in a thunk.
    _ <- evaluate (length (prettyPrint cu))
    pure ()

  it "proves the legacy language-java parser accepted signed hex float exponents" $ do
    res <- legacyParseCompilationUnit hexFloatSignedExpSrc
    case res of
      Left err -> expectationFailure $ "legacy parser failed: " <> err
      Right _  -> pure ()

  it "parses Java floating literals that Haskell read rejects" $ do
    cu <- parseSnippet "FloatLiterals.java" $ unlines
      [ "class FloatLiterals {"
      , "    double trailingDot = 1.;"
      , "    double leadingDot = .5;"
      , "    double hexFloat = 0x1.0p0;"
      , "}"
      ]
    _ <- evaluate (length (prettyPrint cu))
    pure ()

  it "rejects non-Java 0o integer prefixes accepted by tree-sitter-java" $ do
    res <- JParse.parse =<< writeSystemTempFile "OctalOPrefix.java" (unlines
      [ "class OctalOPrefix {"
      , "    int x = 0o7;"
      , "}"
      ])
    case res of
      Left _   -> pure ()
      Right cu -> expectationFailure $
        "expected hard failure for non-Java 0o integer prefix, got: " <> show cu

  it "rejects recovered tree-sitter syntax-error trees" $ do
    path <- writeSystemTempFile "RecoveredError.java" $ unlines
      [ "class RecoveredError {"
      , "    int x"
      , "}"
      ]
    JParse.parseFile path `shouldReturn` Nothing

  it "keeps generic ParseFile failures as Nothing for unsupported Java" $ do
    path <- writeSystemTempFile "UnsupportedLambda.java" $ unlines
      [ "class UnsupportedLambda {"
      , "    Object f = (Runnable) (() -> {});"
      , "}"
      ]
    ParsePretty.parseFile @MJavaSig path `shouldReturn` Nothing

  it "decodes UTF-8 identifiers and string literals" $ do
    cu <- parseSnippet "Utf8Text.java" $ unlines
      [ "class Utf8Text {"
      , "    String café = \"héllo\";"
      , "}"
      ]
    -- Pretty-printing this AST round-trips the non-ASCII chars as
    -- \uXXXX escapes (a language-java quirk), so observe decoding
    -- directly on the parsed tree instead.
    case cu of
      CompilationUnit _ _
        [ ClassTypeDecl (ClassDecl _ _ _ _ _
            (ClassBody
              [ MemberDecl
                  (FieldDecl _ _
                    [ VarDecl
                        (VarId (Ident name))
                        (Just (InitExp (J.Lit (String litVal))))
                    ])
              ]))
        ] -> do
          name   `shouldBe` "café"
          litVal `shouldBe` "héllo"
      _ -> expectationFailure $ "unexpected AST: " <> show cu

  it "preserves diamond constructor type-arg syntax" $ do
    cu <- parseSnippet "DiamondCtor.java" $ unlines
      [ "import java.util.ArrayList;"
      , "class DiamondCtor {"
      , "    ArrayList<Integer> xs = new ArrayList<>();"
      , "}"
      ]
    -- The diamond `<>` is distinct from a raw `new ArrayList()` — Java 7+
    -- requires inference to resolve the type args. Language.Java.Syntax
    -- represents it via TypeDeclSpecifier{,Unqualified}WithDiamond, so a
    -- round-trip must keep the diamond marker (not collapse to the
    -- non-generic raw-type form).
    show cu `shouldContain` "WithDiamond"

  it "folds method trailing dimensions into the return type" $ do
    cu <- parseSnippet "TrailingDims.java" $ unlines
      [ "class TrailingDims {"
      , "    int f()[] {"
      , "        return new int[]{1, 2, 3};"
      , "    }"
      , "}"
      ]
    -- C-style trailing dims (`int f()[]`) name the same return type as the
    -- canonical `int[] f()`. The parser must surface that on MethodDecl.
    case cu of
      CompilationUnit _ _
        [ ClassTypeDecl (ClassDecl _ _ _ _ _
            (ClassBody [ MemberDecl (MethodDecl _ _ retT _ _ _ _ _) ]))
        ] -> retT `shouldBe` Just (RefType (ArrayType (PrimType IntT)))
      _ -> expectationFailure $ "unexpected AST: " <> show cu

  it "stores UTF-8 source columns as character columns" $ do
    let src = unlines
          [ "class Utf8Columns {"
          , "    String café = after;"
          , "}"
          ]
    path <- writeSystemTempFile "Utf8Columns.java" src
    res <- JParse.parseFile path
    case res of
      Nothing -> expectationFailure "parse failed"
      Just t -> do
        let slices = sourceSlices (T.pack src) t
        slices `shouldContain` ["after"]

  it "stores Unicode-escape source columns in original-file coordinates" $ do
    let src = unlines
          [ "class UnicodeEscapeColumns {"
          , "    int caf\\u00e9 = after;"
          , "}"
          ]
    path <- writeSystemTempFile "UnicodeEscapeColumns.java" src
    res <- JParse.parseFile path
    case res of
      Nothing -> expectationFailure "parse failed"
      Just t -> do
        let slices = sourceSlices (T.pack src) t
        slices `shouldContain` ["after"]

parseSnippet :: FilePath -> String -> IO CompilationUnit
parseSnippet name src = do
  path <- writeSystemTempFile name src
  res <- JParse.parse path
  case res of
    Left err -> expectationFailure ("parse failed: " <> err) >> fail "unreachable"
    Right cu -> pure cu

legacyParseCompilationUnit :: String -> IO (Either String CompilationUnit)
legacyParseCompilationUnit src = do
  r <- try @SomeException (evaluate (JP.parser JP.compilationUnit src))
  pure $ case r of
    Left e          -> Left ("parser/lexer crashed: " <> show e)
    Right (Left e)  -> Left (show e)
    Right (Right v) -> Right v

annotationArrayValueSrc :: String
annotationArrayValueSrc = unlines
  [ "class AnnotationArrayValue {"
  , "    @SuppressWarnings({\"unchecked\", \"rawtypes\"})"
  , "    void f() {"
  , "    }"
  , "}"
  ]

ctorCommentBeforeSuperSrc :: String
ctorCommentBeforeSuperSrc = unlines
  [ "class CtorCommentBeforeSuper {"
  , "    CtorCommentBeforeSuper() {"
  , "        /* explanatory note */"
  , "        super();"
  , "    }"
  , "}"
  ]

inlineCommentsSrc :: String
inlineCommentsSrc = unlines
  [ "class InlineComments {"
  , "    int r(int x)    { return /* why */ x; }"
  , "    void t()        { throw /* note */ new RuntimeException(); }"
  , "    void a(int x)   { assert /* c */ x > 0; }"
  , "    void l()        { loop: /* lbl */ while (true) break /* via */ loop; }"
  , "    void c()        { while (true) continue /* skip */; }"
  , "}"
  ]

awkwardCommentsSrc :: String
awkwardCommentsSrc = unlines
  [ "class AwkwardComments /* a */ extends /* b */ Object /* c */ implements /* d */ Runnable /* e */ {"
  , "    /* f */ int /* g */ x /* h */ = /* i */ 1 /* j */ ;"
  , "    /* k */ public /* l */ <T /* m */ extends /* n */ Number> /* o */"
  , "    int /* p */ f /* q */ ( /* r */ int /* s */ y /* t */ ) /* u */"
  , "        throws /* v */ RuntimeException /* w */ {"
  , "        int z = /* x1 */ (int) /* x2 */ y /* x3 */ + /* x4 */ 1 /* x5 */ ;"
  , "        boolean b = z /* y1 */ instanceof /* y2 */ Integer;"
  , "        return /* z1 */ z /* z2 */ > /* z3 */ 0 /* z4 */ ? /* z5 */ y /* z6 */ : /* z7 */ z;"
  , "    }"
  , "    public void run /* α */ ( /* β */ ) /* γ */ {"
  , "        if /* δ */ ( /* ε */ true /* ζ */ ) /* η */ {"
  , "            for /* θ */ ( /* ι */ int i = 0; /* κ */ i < 3; /* λ */ i++) {}"
  , "        }"
  , "    }"
  , "}"
  ]

hexFloatSignedExpSrc :: String
hexFloatSignedExpSrc = unlines
  [ "class HexFloatSignedExp {"
  , "    double a = 0x1.0p+1;"
  , "    double b = 0x1.0p-1;"
  , "}"
  ]

sourceSlices :: T.Text -> AnnTerm (Maybe SourceSpan) JavaSig l -> [String]
sourceSlices src root =
  mapMaybe (\(E sub) -> fmap (T.unpack . sliceSpan src) (getAnn sub :: Maybe SourceSpan))
           (Generic.subterms root)

expectedHelloWorld :: CompilationUnit
expectedHelloWorld =
    CompilationUnit Nothing [] [classTypeDecl]
  where
    stringArrayTy =
      RefType (ArrayType (RefType (ClassRefType (ClassType [(Ident "String", [])]))))

    helloLit = J.Lit (String "Hello, World!")
    sysOut   = J.FieldAccess (PrimaryFieldAccess
                                (J.ExpName (Name [Ident "System"]))
                                (Ident "out"))
    println  = PrimaryMethodCall sysOut [] (Ident "println") [helloLit]
    bodyStmt = BlockStmt (ExpStmt (J.MethodInv println))
    methodBody = MethodBody (Just (Block [bodyStmt]))
    formalParam = FormalParam [] stringArrayTy False (VarId (Ident "args"))
    methodDecl = MethodDecl [Public, Static] [] Nothing (Ident "main")
                            [formalParam] [] Nothing methodBody
    classBody  = ClassBody [MemberDecl methodDecl]
    classDecl  = ClassDecl [Public] (Ident "HelloWorld") [] Nothing [] classBody
    classTypeDecl = ClassTypeDecl classDecl
