module Language.JavaScript.Pretty.Printer.Extended
        ( module Language.JavaScript.Pretty.Printer
        , prettyPrint
        ) where

import           Language.JavaScript.Pretty.Printer

import                      Text.PrettyPrint.Leijen
import qualified Text.PrettyPrint.Leijen as P ((<$>))
import                      Text.Printf (printf)

import                      Language.JavaScript.Parser.AST

prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec inheritedPrec currentPrec t
        | inheritedPrec <= 0          = t
        | inheritedPrec < currentPrec = parens t
        | otherwise                   = t

instance Pretty JSAST where
        pretty (JSAstProgram xs a)   = vcat $ map pretty xs
        pretty (JSAstStatement s a)  = pretty s
        pretty (JSAstExpression e a) = pretty e
        pretty (JSAstLiteral x a)    = pretty x

instance Pretty JSExpression where
        -- Terminals
        pretty (JSIdentifier     annot s) = text s
        pretty (JSDecimal        annot i) = text i
        pretty (JSLiteral        annot l) = text l
        pretty (JSHexInteger     annot i) = text i
        pretty (JSOctal          annot i) = text i
        pretty (JSStringLiteral  annot s) = text s
        pretty (JSRegEx          annot s) = text s
--
--     -- Non-Terminals
        pretty (JSArrayLiteral         als xs ars)             = text "[" <> hsep (map pretty xs) <> text "]"
        pretty (JSAssignExpression     lhs op rhs)             = pretty lhs <+> pretty op <+> pretty rhs
        pretty (JSCallExpression       ex lb xs rb)            = pretty ex <> parens (pretty xs)
        pretty (JSCallExpressionDot    ex os xs)               = pretty ex <> text "." <> pretty xs
        pretty (JSCallExpressionSquare ex als xs ars)          = pretty ex <> text "[" <> pretty xs <> text "]"
        pretty (JSCommaExpression      le c re)                = parens (pretty le <> text "," <> pretty re)
        pretty (JSExpressionBinary     lhs op rhs)             = pretty lhs <+> pretty op <+> pretty rhs
        pretty (JSExpressionParen      alp e arp)              = parens (pretty e)
        pretty (JSExpressionPostfix    xs op)                  = pretty xs <> pretty op
        pretty (JSExpressionTernary    cond h v1 c v2)         = pretty cond <+> text "?" <+> pretty v1 <+> text ":" <+> pretty v2
        pretty (JSFunctionExpression   annot n lb x2s rb x3)   = text "function" <+> pretty n <> parens (pretty x2s) <+> pretty x3
        pretty (JSMemberDot            xs dot n)               = pretty xs <> text "." <> pretty n
        pretty (JSMemberExpression     e lb a rb)              = pretty e <> text "(" <> pretty a <> text ")"
        pretty (JSMemberNew            a lb n rb s)            = text "new" <+> (pretty lb) <> parens (pretty rb)
        pretty (JSMemberSquare         xs als e ars)           = pretty xs <> text "[" <> pretty e <> text "]"
        pretty (JSNewExpression        n e)                    = text "new" <+> pretty e
        pretty (JSObjectLiteral        alb xs arb)             = prettyNestedBracesBlock xs
        pretty (JSUnaryExpression      op x)                   = pretty op <> pretty x
        pretty (JSVarInitExpression    x1 x2)                  = pretty x1 <> pretty x2

instance Pretty JSBinOp where
        pretty (JSBinOpAnd        annot)  = text "&&"
        pretty (JSBinOpBitAnd     annot)  = text "&"
        pretty (JSBinOpBitOr      annot)  = text "|"
        pretty (JSBinOpBitXor     annot)  = text "^"
        pretty (JSBinOpDivide     annot)  = text "/"
        pretty (JSBinOpEq         annot)  = text "=="
        pretty (JSBinOpGe         annot)  = text ">="
        pretty (JSBinOpGt         annot)  = text ">"
        pretty (JSBinOpIn         annot)  = text "in"
        pretty (JSBinOpInstanceOf annot)  = text "instanceof"
        pretty (JSBinOpLe         annot)  = text "<="
        pretty (JSBinOpLsh        annot)  = text "<<"
        pretty (JSBinOpLt         annot)  = text "<"
        pretty (JSBinOpMinus      annot)  = text "-"
        pretty (JSBinOpMod        annot)  = text "%"
        pretty (JSBinOpNeq        annot)  = text "!="
        pretty (JSBinOpOr         annot)  = text "||"
        pretty (JSBinOpPlus       annot)  = text "+"
        pretty (JSBinOpRsh        annot)  = text ">>"
        pretty (JSBinOpStrictEq   annot)  = text "==="
        pretty (JSBinOpStrictNeq  annot)  = text "!=="
        pretty (JSBinOpTimes      annot)  = text "*"
        pretty (JSBinOpUrsh       annot)  = text ">>>"


instance Pretty JSUnaryOp where
        pretty (JSUnaryOpDecr   annot) = text "--"
        pretty (JSUnaryOpDelete annot) = text "delete "
        pretty (JSUnaryOpIncr   annot) = text "++"
        pretty (JSUnaryOpMinus  annot) = text "-"
        pretty (JSUnaryOpNot    annot) = text "!"
        pretty (JSUnaryOpPlus   annot) = text "+"
        pretty (JSUnaryOpTilde  annot) = text "~"
        pretty (JSUnaryOpTypeof annot) = text "typeof "
        pretty (JSUnaryOpVoid   annot) = text "void "


instance Pretty JSAssignOp where
        pretty (JSAssign       annot) = text "="
        pretty (JSTimesAssign  annot) = text "*="
        pretty (JSDivideAssign annot) = text "/="
        pretty (JSModAssign    annot) = text "%="
        pretty (JSPlusAssign   annot) = text "+="
        pretty (JSMinusAssign  annot) = text "-="
        pretty (JSLshAssign    annot) = text "<<="
        pretty (JSRshAssign    annot) = text ">>="
        pretty (JSUrshAssign   annot) = text ">>>="
        pretty (JSBwAndAssign  annot) = text "&="
        pretty (JSBwXorAssign  annot) = text "^="
        pretty (JSBwOrAssign   annot) = text "|="


instance Pretty JSSemi where
        pretty (JSSemi annot) = text ";"
        pretty JSSemiAuto     = text ""


instance Pretty JSTryCatch where
        pretty (JSCatch anc alb x1 arb x3) = hsep [text "catch", parens (pretty x1)] <+> pretty x3
        pretty (JSCatchIf anc alb x1 aif ex arb x3) = hsep [text "catch", parens (pretty x1 <+> text "if" <+> pretty ex)] <+> pretty x3


instance Pretty JSTryFinally where
        pretty (JSFinally      annot x) = text "finally" <+> pretty x
        pretty JSNoFinally              = text ""


instance Pretty JSSwitchParts where
        pretty (JSCase    annot x1 c x2s) = text "case" <+> pretty x1 <> colon <+> align (vsep (map pretty x2s))
        pretty (JSDefault annot c xs)     = text "default" <> colon <+> align (vsep (map pretty xs))


instance Pretty JSStatement where
        pretty (JSStatementBlock alb blk arb s)             = braceBlock $ map pretty blk -- Not printing semicolon b/c came like this, didn't fix
        pretty (JSBreak annot mi s)                         = text "break" <+> pretty mi <> pretty s
        pretty (JSContinue annot mi s)                      = text "continue" <+> pretty mi <> pretty s
        pretty (JSConstant annot xs s)                      = text "const" <+> pretty xs <> pretty s
        pretty (JSDoWhile ad x1 aw alb x2 arb x3)           = text "do" <+> pretty x1 <+> text "while" <+> parens (pretty x2) <+> pretty x3
        pretty (JSFor af alb x1s s1 x2s s2 x3s arb x4)      = text "for" <> parens (hsep [pretty x1s <> text ";" , pretty x2s <> text ";" , pretty x3s]) <+> pretty x4
        pretty (JSForIn af alb x1s i x2 arb x3)             = text "for" <> parens (pretty x1s <+> pretty i <+> pretty x2) <+> pretty x3
        pretty (JSForVar af alb v x1s s1 x2s s2 x3s arb x4) = text "for" <> parens (text "var " <+> pretty x1s <> text ";" <+> pretty x2s <> text ";" <+> pretty x3s) <+> pretty x4
        pretty (JSForVarIn af alb v x1 i x2 arb x3)         = text "for" <> parens (text "var " <+> pretty x1 <+> pretty i <+> pretty x2) <+> pretty x3
        pretty (JSFunction af n alb x2s arb x3 s)           = text "function" <+> pretty n <> parens (pretty x2s) <+> pretty x3
        pretty (JSIf annot alb x1 arb x2s)                  = text "if" <+> parens (pretty x1) <+> prettyNestedStmt x2s
        pretty (JSIfElse annot alb x1 arb x2s ea x3s)       = text "if" <+> parens (pretty x1) <+> prettyNestedStmt x2s <+> text "else" <+> prettyNestedStmt x3s
        pretty (JSLabelled l c v)                           = pretty l <> colon <+> pretty v
        pretty (JSEmptyStatement a)                         = text ";"
        pretty (JSExpressionStatement l s)                  = pretty l <> pretty s
        pretty (JSAssignStatement lhs op rhs s)             = pretty lhs <+> pretty op <+> pretty rhs <> pretty s
        pretty (JSMethodCall e lp a rp s)                   = pretty e <> parens (pretty a) <> pretty s
        pretty (JSReturn annot me s)                        = text "return" <+> (maybePP me) <> pretty s
        pretty (JSSwitch annot alp x arp alb x2 arb s)      = text "switch" <+> parens (pretty x) <+> braceBlock (map pretty x2)
        pretty (JSThrow annot x s)                          = text "throw" <+> pretty x <> pretty s
        pretty (JSTry annot tb tcs tf)                      = text "try" <+> pretty tb <$$>
                                                                                                                                    vcat (map pretty tcs ++ (pretty tf):[])
        pretty (JSVariable annot xs s)                      = text "var" <+> pretty xs <> pretty s
        pretty (JSWhile annot alp x1 arp x2)                = text "while" <+> parens (pretty x1) <+> pretty x2
        pretty (JSWith annot alp x1 arp x s)                = text "with" <+> parens (pretty x1) <+> pretty x <> pretty s
        pretty _s             = error "TODO JSStatement"
--
--
-- instance Pretty [JSStatement] where
--     (|>) = foldl' (|>)
--
instance Pretty JSBlock where
        pretty (JSBlock alb ss arb) = braceBlock $ map pretty ss
--
instance Pretty JSObjectProperty where
        pretty (JSPropertyAccessor     s n alp ps arp b)       = pretty s <+> pretty n <+> parens (vcat $ map pretty ps) <+> pretty b
        pretty (JSPropertyNameandValue n c vs)                 = pretty n <> colon <+> vcat (map pretty vs)
--
instance Pretty JSPropertyName where
        pretty (JSPropertyIdent an1 i)  = text i
        pretty (JSPropertyString an1 s) = text s
        pretty (JSPropertyNumber an1 n) = text n
--
instance Pretty JSAccessor where
        pretty (JSAccessorGet an1) = text "get"
        pretty (JSAccessorSet an1) = text "set"
--
instance Pretty JSArrayElement where
        pretty (JSArrayElement e) = pretty e
        pretty (JSArrayComma a)   = text ","
--
-- instance Pretty [JSArrayElement] where
--     (|>) = foldl' (|>)
--
instance Pretty a => Pretty (JSCommaList a) where
        pretty (JSLCons pl a i) = pretty pl <> text "," <+> pretty i
        pretty (JSLOne i)       = pretty i
        pretty JSLNil           = text ""
--
instance Pretty a => Pretty (JSCommaTrailingList a) where
        pretty (JSCTLComma xs a) = pretty xs <> text ","
        pretty (JSCTLNone xs)   = pretty xs
--
instance Pretty JSIdent where
        pretty (JSIdentName a s) = text s
        pretty JSIdentNone       = text ""
--
-- instance Pretty (Maybe JSExpression) where
--     pretty (Just e) = pacc |> e
--     pretty Nothing  = pacc
--
instance Pretty JSVarInitializer where
        pretty (JSVarInit a x) = text " = " <+> pretty x
        pretty JSVarInitNone   = text ""

-- instance Pretty JSObjectPropertyList where
--         pretty (JSCommaTrailingList prop) = text "{" <$$> pretty prop <$$> text "}"

-- EOF

-----------------------------------------------------------------------
-- Help functionality
prettyNestedStmt :: JSStatement -> Doc
prettyNestedStmt b@(JSStatementBlock _ _ _ _) = pretty b
prettyNestedStmt stmt = nest 2 (pretty stmt)

prettyNestedBracesBlock :: Pretty a => a -> Doc
prettyNestedBracesBlock p = nest 2 (char '{' P.<$> pretty p) P.<$> char '}'

prettyNestedBlock :: Pretty a => a -> Doc
prettyNestedBlock p = nest 2 (pretty p)

maybePP :: Pretty a => Maybe a -> Doc
maybePP = maybe empty (pretty)

opt :: Bool -> Doc -> Doc
opt x a = if x then a else empty

braceBlock :: [Doc] -> Doc
braceBlock xs = nest 2 (char '{' <$$> (vcat xs))
                <$$> char '}'

bracketBlock :: [Doc] -> Doc
bracketBlock xs = char '['
        <$$> nest 2 (hcat xs)
        <$$> char ']'

escapeGeneral :: Char -> String
escapeGeneral '\b' = "\\b"
escapeGeneral '\t' = "\\t"
escapeGeneral '\n' = "\\n"
escapeGeneral '\f' = "\\f"
escapeGeneral '\r' = "\\r"
escapeGeneral '\\' = "\\\\"
escapeGeneral c | c >= ' ' && c < '\DEL' = [c]
                                | c <= '\xFFFF' = printf "\\u%04x" (fromEnum c)
                                | otherwise = error $ "Language.JavaScript.Pretty.escapeGeneral: Char " ++ show c ++ " too large for JavaScript char"

escapeChar :: Char -> String
escapeChar '\'' = "\\'"
escapeChar c = escapeGeneral c

escapeString :: Char -> String
escapeString '"' = "\\\""
escapeString c | c <= '\xFFFF' = escapeGeneral c
               | otherwise = escapeGeneral lead ++ escapeGeneral trail
                   where c' = fromEnum c - 0x010000
                         lead = toEnum $ 0xD800 + c' `div` 0x0400
                         trail = toEnum $ 0xDC00 + c' `mod` 0x0400
