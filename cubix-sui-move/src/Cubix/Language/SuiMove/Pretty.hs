{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pretty printer for Sui Move
--
-- This module provides a pretty printer that reconstructs source code from
-- the Modularized AST. It satisfies: parse(pretty(parse(x))) == parse(x)
--
-- Uses Text.PrettyPrint.Leijen for proper indentation and layout.
module Cubix.Language.SuiMove.Pretty (
    pretty
  , prettyTerm
  ) where

import Data.Text (Text)
import Data.Text qualified as Text

import Data.Comp.Multi (Term, unTerm, All, caseCxt, Sum, project)

import Text.PrettyPrint.Leijen (Doc, (<+>))
import Text.PrettyPrint.Leijen qualified as PP

import Cubix.Language.Parametric.Syntax (ListF(..), MaybeF(..), PairF(..))

import Cubix.Language.SuiMove.Modularized

--------------------------------------------------------------------------------
-- Pretty class and instances
--------------------------------------------------------------------------------

class Pretty f where
  prettyF :: f (Term MoveSig) l -> Doc

instance {-# OVERLAPPING #-} (All Pretty fs) => Pretty (Sum fs) where
  prettyF = caseCxt @Pretty prettyF

-- | Pretty print a term to Doc
prettyTerm :: MoveTerm l -> Doc
prettyTerm = prettyF . unTerm

-- | Pretty print the root (SourceFile) to a String
pretty :: MoveTerm SourceFileL -> String
pretty t = PP.displayS (PP.renderPretty 0.9 120 (prettyTerm t)) ""

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Create Doc from Text
text :: Text -> Doc
text = PP.text . Text.unpack

-- | Check if a list term is empty
isEmptyListTerm :: MoveTerm [l] -> Bool
isEmptyListTerm t = case project @ListF t of
  Just NilF -> True
  Just (ConsF _ _) -> False
  Nothing -> error "isEmptyListTerm: expected ListF"

-- | Check if a maybe term is Nothing
isNothingTerm :: MoveTerm (Maybe l) -> Bool
isNothingTerm t = case project @MaybeF t of
  Just NothingF -> True
  Just (JustF _) -> False
  Nothing -> error "isNothingTerm: expected MaybeF"

-- | Space if not Nothing term
spMaybe :: MoveTerm (Maybe l) -> Doc
spMaybe t = if isNothingTerm t then PP.empty else PP.space

-- | Pretty print a list term with separator (horizontal)
prettyListSepH :: Doc -> MoveTerm [l] -> Doc
prettyListSepH sep t = case project @ListF t of
  Just NilF -> PP.empty
  Just (ConsF x xs) -> prettyTerm x PP.<> rest xs
  Nothing -> error "prettyListSepH: expected ListF"
  where
    rest :: MoveTerm [l'] -> Doc
    rest r = case project @ListF r of
      Just NilF -> PP.empty
      Just (ConsF y ys) -> sep PP.<> prettyTerm y PP.<> rest ys
      Nothing -> error "prettyListSepH: expected ListF in rest"

-- | Pretty print a list term with vertical separator (each on new line)
prettyListSepV :: MoveTerm [l] -> Doc
prettyListSepV t = case project @ListF t of
  Just NilF -> PP.empty
  Just (ConsF x xs) -> prettyTerm x PP.<> rest xs
  Nothing -> error "prettyListSepV: expected ListF"
  where
    rest :: MoveTerm [l'] -> Doc
    rest r = case project @ListF r of
      Just NilF -> PP.empty
      Just (ConsF y ys) -> PP.line PP.<> prettyTerm y PP.<> rest ys
      Nothing -> error "prettyListSepV: expected ListF in rest"

-- | Pretty print a list term with double newlines (for top-level items)
prettyListSepVV :: MoveTerm [l] -> Doc
prettyListSepVV t = case project @ListF t of
  Just NilF -> PP.empty
  Just (ConsF x xs) -> prettyTerm x PP.<> rest xs
  Nothing -> error "prettyListSepVV: expected ListF"
  where
    rest :: MoveTerm [l'] -> Doc
    rest r = case project @ListF r of
      Just NilF -> PP.empty
      Just (ConsF y ys) -> PP.line PP.<> PP.line PP.<> prettyTerm y PP.<> rest ys
      Nothing -> error "prettyListSepVV: expected ListF in rest"

-- | Braced block with indentation
braceBlock :: Doc -> Doc
braceBlock content =
  PP.lbrace PP.<> PP.nest 4 (PP.line PP.<> content) PP.<> PP.line PP.<> PP.rbrace

-- | Braced block that might be empty
braceBlockOrEmpty :: Doc -> Doc
braceBlockOrEmpty content =
  if isEmpty content
  then PP.text "{ }"
  else braceBlock content
  where
    isEmpty d = null (PP.displayS (PP.renderCompact d) "")

--------------------------------------------------------------------------------
-- Parametric syntax instances
--------------------------------------------------------------------------------

instance Pretty ListF where
  prettyF NilF = PP.empty
  prettyF (ConsF x xs) = prettyTerm x PP.<> prettyTerm xs

instance Pretty MaybeF where
  prettyF NothingF = PP.empty
  prettyF (JustF x) = prettyTerm x

instance Pretty PairF where
  prettyF (PairF a b) = prettyTerm a PP.<> prettyTerm b

--------------------------------------------------------------------------------
-- Token instances - these produce their literal text
--------------------------------------------------------------------------------

instance Pretty Token where
  prettyF ExclamationMarkTok = PP.text "!"
  prettyF ExclamationMarkEqualsSignTok = PP.text "!="
  prettyF NumberSignLeftSquareBracketTok = PP.text "#["
  prettyF DollarSignTok = PP.text "$"
  prettyF PercentSignTok = PP.text "%"
  prettyF AmpersandTok = PP.text "&"
  prettyF AmpersandAmpersandTok = PP.text "&&"
  prettyF ApostropheTok = PP.text "'"
  prettyF LeftParenthesisTok = PP.text "("
  prettyF RightParenthesisTok = PP.text ")"
  prettyF AsteriskTok = PP.text "*"
  prettyF PlusSignTok = PP.text "+"
  prettyF CommaTok = PP.text ","
  prettyF HyphenMinusTok = PP.text "-"
  prettyF HyphenMinusGreaterThanSignTok = PP.text "->"
  prettyF FullStopTok = PP.text "."
  prettyF FullStopFullStopTok = PP.text ".."
  prettyF SolidusTok = PP.text "/"
  prettyF SolidusAsteriskTok = PP.text "/*"
  prettyF SolidusSolidusTok = PP.text "//"
  prettyF ColonTok = PP.text ":"
  prettyF ColonColonTok = PP.text "::"
  prettyF SemicolonTok = PP.text ";"
  prettyF LessThanSignTok = PP.text "<"
  prettyF LessThanSignLessThanSignTok = PP.text "<<"
  prettyF LessThanSignEqualsSignTok = PP.text "<="
  prettyF EqualsSignTok = PP.text "="
  prettyF EqualsSignEqualsSignTok = PP.text "=="
  prettyF EqualsSignEqualsSignGreaterThanSignTok = PP.text "==>"
  prettyF EqualsSignGreaterThanSignTok = PP.text "=>"
  prettyF GreaterThanSignTok = PP.text ">"
  prettyF GreaterThanSignEqualsSignTok = PP.text ">="
  prettyF GreaterThanSignGreaterThanSignTok = PP.text ">>"
  prettyF CommercialAtTok = PP.text "@"
  prettyF LeftSquareBracketTok = PP.text "["
  prettyF RightSquareBracketTok = PP.text "]"
  prettyF CircumflexAccentTok = PP.text "^"
  prettyF AbortTok = PP.text "abort"
  prettyF AbortsIfTok = PP.text "aborts_if"
  prettyF AbortsWithTok = PP.text "aborts_with"
  prettyF AddressTok = PP.text "address"
  prettyF ApplyTok = PP.text "apply"
  prettyF AsTok = PP.text "as"
  prettyF AssertTok = PP.text "assert"
  prettyF AssumeTok = PP.text "assume"
  prettyF BoolTok = PP.text "bool"
  prettyF BreakTok = PP.text "break"
  prettyF ConstTok = PP.text "const"
  prettyF ContinueTok = PP.text "continue"
  prettyF CopyTok = PP.text "copy"
  prettyF DecreasesTok = PP.text "decreases"
  prettyF DropTok = PP.text "drop"
  prettyF ElseTok = PP.text "else"
  prettyF EnsuresTok = PP.text "ensures"
  prettyF EntryTok = PP.text "entry"
  prettyF EnumTok = PP.text "enum"
  prettyF ExceptTok = PP.text "except"
  prettyF ExistsTok = PP.text "exists"
  prettyF ExtendTok = PP.text "extend"
  prettyF FalseTok = PP.text "false"
  prettyF ForallTok = PP.text "forall"
  prettyF FriendTok = PP.text "friend"
  prettyF FunTok = PP.text "fun"
  prettyF GlobalTok = PP.text "global"
  prettyF HasTok = PP.text "has"
  prettyF IfTok = PP.text "if"
  prettyF InTok = PP.text "in"
  prettyF IncludeTok = PP.text "include"
  prettyF InternalTok = PP.text "internal"
  prettyF InvariantTok = PP.text "invariant"
  prettyF KeyTok = PP.text "key"
  prettyF LetTok = PP.text "let"
  prettyF LocalTok = PP.text "local"
  prettyF LoopTok = PP.text "loop"
  prettyF MacroTok = PP.text "macro"
  prettyF MatchTok = PP.text "match"
  prettyF ModifiesTok = PP.text "modifies"
  prettyF ModuleTok = PP.text "module"
  prettyF MoveTok = PP.text "move"
  prettyF MutTok = PP.text "mut"
  prettyF NativeTok = PP.text "native"
  prettyF PackTok = PP.text "pack"
  prettyF PackageTok = PP.text "package"
  prettyF PhantomTok = PP.text "phantom"
  prettyF PostTok = PP.text "post"
  prettyF PragmaTok = PP.text "pragma"
  prettyF PublicTok = PP.text "public"
  prettyF RequiresTok = PP.text "requires"
  prettyF ReturnTok = PP.text "return"
  prettyF SchemaTok = PP.text "schema"
  prettyF SignerTok = PP.text "signer"
  prettyF SpecTok = PP.text "spec"
  prettyF StoreTok = PP.text "store"
  prettyF StructTok = PP.text "struct"
  prettyF SucceedsIfTok = PP.text "succeeds_if"
  prettyF ToTok = PP.text "to"
  prettyF TrueTok = PP.text "true"
  prettyF U128Tok = PP.text "u128"
  prettyF U16Tok = PP.text "u16"
  prettyF U256Tok = PP.text "u256"
  prettyF U32Tok = PP.text "u32"
  prettyF U64Tok = PP.text "u64"
  prettyF U8Tok = PP.text "u8"
  prettyF UnpackTok = PP.text "unpack"
  prettyF UpdateTok = PP.text "update"
  prettyF UseTok = PP.text "use"
  prettyF VectorLessThanSignTok = PP.text "vector<"
  prettyF VectorLeftSquareBracketTok = PP.text "vector["
  prettyF WhereTok = PP.text "where"
  prettyF WhileTok = PP.text "while"
  prettyF WithTok = PP.text "with"
  prettyF LeftCurlyBracketTok = PP.text "{"
  prettyF VerticalLineTok = PP.text "|"
  prettyF VerticalLineVerticalLineTok = PP.text "||"
  prettyF RightCurlyBracketTok = PP.text "}"

--------------------------------------------------------------------------------
-- Content nodes - these contain actual text
--------------------------------------------------------------------------------

instance Pretty Identifier where
  prettyF (Identifier t) = text t

instance Pretty NumLiteral where
  prettyF (NumLiteral t _suffix) = text t

instance Pretty NumLiteralInternal0 where
  prettyF (NumLiteralInternal0U8 _) = PP.text "u8"
  prettyF (NumLiteralInternal0U16 _) = PP.text "u16"
  prettyF (NumLiteralInternal0U32 _) = PP.text "u32"
  prettyF (NumLiteralInternal0U64 _) = PP.text "u64"
  prettyF (NumLiteralInternal0U128 _) = PP.text "u128"
  prettyF (NumLiteralInternal0U256 _) = PP.text "u256"

instance Pretty AddressLiteral where
  prettyF (AddressLiteral t) = text t

instance Pretty ByteStringLiteral where
  prettyF (ByteStringLiteral t) = text t

instance Pretty HexStringLiteral where
  prettyF (HexStringLiteral t) = text t

instance Pretty StringLiteral where
  prettyF (StringLiteral t) = text t

instance Pretty SpecApplyNamePattern where
  prettyF (SpecApplyNamePattern t) = text t

--------------------------------------------------------------------------------
-- Structure nodes
--------------------------------------------------------------------------------

-- Source file
instance Pretty SourceFile where
  prettyF (SourceFile items) = prettyListSepVV items

instance Pretty SourceFileInternal0 where
  prettyF (SourceFileInternal0ModuleExtensionDefinition med) = prettyTerm med
  prettyF (SourceFileInternal0ModuleDefinition md) = prettyTerm md

-- Module extension definition
instance Pretty ModuleExtensionDefinition where
  prettyF (ModuleExtensionDefinition _ md) = PP.text "extend" <+> prettyTerm md

-- Module definition
instance Pretty ModuleDefinition where
  prettyF (ModuleDefinition _ identity body) =
    PP.text "module" <+> prettyTerm identity PP.<> prettyTerm body

instance Pretty ModuleBody where
  prettyF (ModuleBody opener items closer) =
    case opener of
      ModuleBodyInternal0LeftCurlyBracket' _ ->
        PP.space PP.<> PP.lbrace PP.<> PP.nest 4 (PP.line PP.<> prettyListSepVV items) PP.<> PP.line PP.<> PP.rbrace
      _ ->
        prettyTerm opener PP.<> PP.line PP.<> prettyListSepVV items

instance Pretty ModuleBodyInternal0 where
  prettyF (ModuleBodyInternal0Semicolon _) = PP.text ";"
  prettyF (ModuleBodyInternal0LeftCurlyBracket _) = PP.text "{"

instance Pretty ModuleBodyInternal1 where
  prettyF (ModuleBodyInternal1UseDeclaration ud) = prettyTerm ud
  prettyF (ModuleBodyInternal1FriendDeclaration fd) = prettyTerm fd
  prettyF (ModuleBodyInternal1Constant c) = prettyTerm c
  prettyF (ModuleBodyInternal1FunctionItem fi) = prettyTerm fi
  prettyF (ModuleBodyInternal1StructItem si) = prettyTerm si
  prettyF (ModuleBodyInternal1EnumItem ei) = prettyTerm ei
  prettyF (ModuleBodyInternal1SpecBlock sb) = prettyTerm sb

instance Pretty ModuleIdentity where
  prettyF (ModuleIdentity internal _ modId) =
    prettyTerm internal PP.<> PP.text "::" PP.<> prettyTerm modId

instance Pretty ModuleIdentityInternal0 where
  prettyF (ModuleIdentityInternal0NumLiteral n) = prettyTerm n
  prettyF (ModuleIdentityInternal0ModuleIdentifier i) = prettyTerm i

instance Pretty HiddenModuleIdentifier where
  prettyF (HiddenModuleIdentifier i) = prettyTerm i

-- Use declarations
instance Pretty UseDeclaration where
  prettyF (UseDeclaration maybePub internal) =
    prettyTerm maybePub PP.<> spMaybe maybePub PP.<> PP.text "use" <+> prettyTerm internal PP.<> PP.text ";"

instance Pretty UseDeclarationInternal0 where
  prettyF (UseDeclarationInternal0UseFun uf) = prettyTerm uf
  prettyF (UseDeclarationInternal0UseModule um) = prettyTerm um
  prettyF (UseDeclarationInternal0UseModuleMember umm) = prettyTerm umm
  prettyF (UseDeclarationInternal0UseModuleMembers ums) = prettyTerm ums

instance Pretty UseFun where
  prettyF (UseFun ma rest) =
    PP.text "fun" <+> prettyTerm ma <+> PP.text "as" <+> prettyTerm rest

instance Pretty UseModule where
  prettyF (UseModule mi maybeAlias) = prettyTerm mi PP.<> prettyAlias maybeAlias
    where
      prettyAlias :: MoveTerm (Maybe (AsTokL, HiddenModuleIdentifierL)) -> Doc
      prettyAlias maybeA = case project @MaybeF maybeA of
        Just NothingF -> PP.empty
        Just (JustF aliasPair) -> case project @PairF aliasPair of
          Just (PairF _ aliasId) -> PP.text " as" <+> prettyTerm aliasId
          Nothing -> prettyTerm maybeA
        Nothing -> prettyTerm maybeA

instance Pretty UseModuleMember where
  prettyF (UseModuleMember mi _ member) = prettyTerm mi PP.<> PP.text "::" PP.<> prettyTerm member

instance Pretty UseModuleMembers where
  prettyF (UseModuleMembers1 internal _ members) =
    prettyTerm internal PP.<> PP.text "::{" PP.<> prettyListSepH (PP.text ", ") members PP.<> PP.text "}"
  prettyF (UseModuleMembers2 mi _ members) =
    prettyTerm mi PP.<> PP.text "::{" PP.<> prettyListSepH (PP.text ", ") members PP.<> PP.text "}"

instance Pretty UseMember where
  prettyF (UseMember1 i _ members) = prettyTerm i PP.<> PP.text "::{" PP.<> prettyListSepH (PP.text ", ") members PP.<> PP.text "}"
  prettyF (UseMember2 i1 _ i2 maybeAlias) = prettyTerm i1 PP.<> PP.text "::" PP.<> prettyTerm i2 PP.<> prettyMemberAlias maybeAlias
  prettyF (UseMember3 i maybeAlias) = prettyTerm i PP.<> prettyMemberAlias maybeAlias

prettyMemberAlias :: MoveTerm (Maybe (AsTokL, IdentifierL)) -> Doc
prettyMemberAlias maybeA = case project @MaybeF maybeA of
  Just NothingF -> PP.empty
  Just (JustF aliasPair) -> case project @PairF aliasPair of
    Just (PairF _ aliasId) -> PP.text " as" <+> prettyTerm aliasId
    Nothing -> prettyTerm maybeA
  Nothing -> prettyTerm maybeA

-- Friend declaration
instance Pretty FriendDeclaration where
  prettyF (FriendDeclaration fa) = PP.text "friend" <+> prettyTerm fa PP.<> PP.text ";"

instance Pretty FriendAccess where
  prettyF (FriendAccessLocalModule ident) = prettyTerm ident
  prettyF (FriendAccessFullyQualifiedModule mi) = prettyTerm mi

-- Constants
instance Pretty Constant where
  prettyF (Constant name ty expr) =
    PP.text "const" <+> prettyTerm name PP.<> PP.text ":" <+> prettyTerm ty <+> PP.text "=" <+> prettyTerm expr PP.<> PP.text ";"

-- Hidden function item
instance Pretty HiddenFunctionItem where
  prettyF (HiddenFunctionItemNativeFunctionDefinition nfd) = prettyTerm nfd
  prettyF (HiddenFunctionItemMacroFunctionDefinition mfd) = prettyTerm mfd
  prettyF (HiddenFunctionItemFunctionDefinition fd) = prettyTerm fd

-- Function definition
instance Pretty FunctionDefinition where
  prettyF (FunctionDefinition sig block) = prettyTerm sig <+> prettyTerm block

instance Pretty HiddenFunctionSignature where
  prettyF (HiddenFunctionSignature mod1 mod2 mod3 _ funcId typeParams params retType) =
    prettyTerm mod1 PP.<> spMaybe mod1 PP.<>
    prettyTerm mod2 PP.<> spMaybe mod2 PP.<>
    prettyTerm mod3 PP.<> spMaybe mod3 PP.<>
    PP.text "fun" <+> prettyTerm funcId PP.<>
    prettyTerm typeParams PP.<>
    prettyTerm params PP.<>
    prettyTerm retType

instance Pretty HiddenFunctionIdentifier where
  prettyF (HiddenFunctionIdentifier i) = prettyTerm i

instance Pretty FunctionParameters where
  prettyF (FunctionParameters params) = PP.parens (prettyListSepH (PP.text ", ") params)

instance Pretty FunctionParametersInternal0 where
  prettyF (FunctionParametersInternal0MutFunctionParameter mfp) = prettyTerm mfp
  prettyF (FunctionParametersInternal0FunctionParameter fp) = prettyTerm fp

instance Pretty FunctionParameter where
  prettyF (FunctionParameter internal _ ty) =
    prettyTerm internal PP.<> PP.text ":" <+> prettyTerm ty

instance Pretty FunctionParameterInternal0 where
  prettyF (FunctionParameterInternal0Name vi) = prettyTerm vi
  prettyF (FunctionParameterInternal02 _ vi) = PP.text "$" PP.<> prettyTerm vi

instance Pretty MutFunctionParameter where
  prettyF (MutFunctionParameter _ fp) = PP.text "mut" <+> prettyTerm fp

instance Pretty HiddenVariableIdentifier where
  prettyF (HiddenVariableIdentifier i) = prettyTerm i

instance Pretty RetType where
  prettyF (RetType _ ty) = PP.text ":" <+> prettyTerm ty

instance Pretty Modifier where
  prettyF (Modifier1 _ internal) = PP.text "public" PP.<> prettyTerm internal
  prettyF (ModifierEntry _) = PP.text "entry"
  prettyF (ModifierNative _) = PP.text "native"

instance Pretty ModifierInternal0 where
  prettyF (ModifierInternal0Package _) = PP.text "(package)"
  prettyF (ModifierInternal0Friend _) = PP.text "(friend)"

-- Native function definition
instance Pretty NativeFunctionDefinition where
  prettyF (NativeFunctionDefinition sig _) = prettyTerm sig PP.<> PP.text ";"

-- Macro function definition
instance Pretty MacroFunctionDefinition where
  prettyF (MacroFunctionDefinition mod' _ sig block) =
    prettyTerm mod' PP.<> spMaybe mod' PP.<> PP.text "macro" <+> prettyTerm sig <+> prettyTerm block

instance Pretty HiddenMacroSignature where
  prettyF (HiddenMacroSignature mod' _ funcId typeParams params retType) =
    prettyTerm mod' PP.<> spMaybe mod' PP.<>
    PP.text "fun" <+> prettyTerm funcId PP.<>
    prettyTerm typeParams PP.<>
    prettyTerm params PP.<>
    prettyTerm retType

-- Type parameters
instance Pretty TypeParameters where
  prettyF (TypeParameters params) = PP.angles (prettyListSepH (PP.text ", ") params)

instance Pretty TypeParameter where
  prettyF (TypeParameter dollar phantom typeId abilities) =
    prettyTerm dollar PP.<>
    prettyTerm phantom PP.<> spMaybe phantom PP.<>
    prettyTerm typeId PP.<>
    prettyAbilities abilities
    where
      prettyAbilities :: MoveTerm (Maybe (ColonTokL, [AbilityL])) -> Doc
      prettyAbilities maybeA = case project @MaybeF maybeA of
        Just NothingF -> PP.empty
        Just (JustF colonAbils) -> case project @PairF colonAbils of
          Just (PairF _ abilityList) -> PP.text ":" <+> prettyListSepH (PP.text " + ") abilityList
          Nothing -> prettyTerm maybeA
        Nothing -> prettyTerm maybeA

instance Pretty HiddenTypeParameterIdentifier where
  prettyF (HiddenTypeParameterIdentifier i) = prettyTerm i

-- Type arguments
instance Pretty TypeArguments where
  prettyF (TypeArguments args) = PP.angles (prettyListSepH (PP.text ", ") args)

-- Hidden struct item
instance Pretty HiddenStructItem where
  prettyF (HiddenStructItemNativeStructDefinition nsd) = prettyTerm nsd
  prettyF (HiddenStructItemStructDefinition sd) = prettyTerm sd

-- Struct definition
instance Pretty StructDefinition where
  prettyF (StructDefinition maybePub sig fields maybePostfixAbilities) =
    prettyTerm maybePub PP.<> spMaybe maybePub PP.<> prettyTerm sig PP.<> prettyFieldsWithSpace fields PP.<> prettyTerm maybePostfixAbilities PP.<> trailingSemicolon
    where
      isPositional = case project @DatatypeFields fields of
        Just (DatatypeFieldsPositionalFields _) -> True
        _ -> False
      prettyFieldsWithSpace :: MoveTerm DatatypeFieldsL -> Doc
      prettyFieldsWithSpace f
        | isPositional = prettyTerm f
        | otherwise    = PP.space PP.<> prettyTerm f
      trailingSemicolon
        | isPositional = PP.text ";"
        | not (isNothingTerm maybePostfixAbilities) = PP.text ";"
        | otherwise = PP.empty

instance Pretty HiddenStructSignature where
  prettyF (HiddenStructSignature _ structId typeParams abilities) =
    PP.text "struct" <+> prettyTerm structId PP.<>
    prettyTerm typeParams PP.<>
    prettyTerm abilities

instance Pretty HiddenStructIdentifier where
  prettyF (HiddenStructIdentifier i) = prettyTerm i

-- Native struct definition
instance Pretty NativeStructDefinition where
  prettyF (NativeStructDefinition maybePub sig) =
    prettyTerm maybePub PP.<> spMaybe maybePub PP.<> PP.text "native" <+> prettyTerm sig PP.<> PP.text ";"

instance Pretty PostfixAbilityDecls where
  prettyF (PostfixAbilityDecls abilities) = PP.text " has" <+> prettyListSepH (PP.text ", ") abilities

instance Pretty AbilityDecls where
  prettyF (AbilityDecls _ abilities) = PP.text " has" <+> prettyListSepH (PP.text ", ") abilities

instance Pretty Ability where
  prettyF (AbilityCopy _) = PP.text "copy"
  prettyF (AbilityDrop _) = PP.text "drop"
  prettyF (AbilityStore _) = PP.text "store"
  prettyF (AbilityKey _) = PP.text "key"

instance Pretty DatatypeFields where
  prettyF (DatatypeFieldsPositionalFields pf) = prettyTerm pf
  prettyF (DatatypeFieldsNamedFields nf) = prettyTerm nf

instance Pretty PositionalFields where
  prettyF (PositionalFields types) = PP.parens (prettyListSepH (PP.text ", ") types)

instance Pretty NamedFields where
  prettyF (NamedFields fields) = PP.braces (PP.space PP.<> prettyListSepH (PP.text ", ") fields PP.<> PP.space)

instance Pretty FieldAnnotation where
  prettyF (FieldAnnotation fieldId _ ty) = prettyTerm fieldId PP.<> PP.text ":" <+> prettyTerm ty

instance Pretty HiddenFieldIdentifier where
  prettyF (HiddenFieldIdentifier i) = prettyTerm i

-- Hidden enum item
instance Pretty HiddenEnumItem where
  prettyF (HiddenEnumItem ed) = prettyTerm ed

-- Enum definition
instance Pretty EnumDefinition where
  prettyF (EnumDefinition maybePub sig variants maybePostfixAbilities) =
    prettyTerm maybePub PP.<> spMaybe maybePub PP.<> prettyTerm sig <+> prettyTerm variants PP.<> prettyTerm maybePostfixAbilities

instance Pretty HiddenEnumSignature where
  prettyF (HiddenEnumSignature _ enumId typeParams abilities) =
    PP.text "enum" <+> prettyTerm enumId PP.<>
    prettyTerm typeParams PP.<>
    prettyTerm abilities

instance Pretty HiddenEnumIdentifier where
  prettyF (HiddenEnumIdentifier i) = prettyTerm i

instance Pretty EnumVariants where
  prettyF (EnumVariants variants) =
    PP.lbrace PP.<> PP.nest 4 (PP.line PP.<> prettyListSepH (PP.text "," PP.<> PP.line) variants) PP.<> PP.line PP.<> PP.rbrace

instance Pretty Variant where
  prettyF (Variant variantId maybeDf) = prettyTerm variantId PP.<> prettyTerm maybeDf

instance Pretty HiddenVariantIdentifier where
  prettyF (HiddenVariantIdentifier i) = prettyTerm i

-- Block
instance Pretty Block where
  prettyF (Block _ useDecls blockItems maybeExpr _) =
    let useDeclsDoc = prettyTerm useDecls
        blockItemsDoc = prettyListSepV blockItems
        exprDoc = prettyTerm maybeExpr
        content = useDeclsDoc PP.<>
                  (if isEmptyListTerm useDecls then PP.empty else PP.line) PP.<>
                  blockItemsDoc PP.<>
                  (if isEmptyListTerm blockItems && isNothingTerm maybeExpr then PP.empty else
                   if isEmptyListTerm blockItems then PP.empty else PP.line) PP.<>
                  exprDoc
        isEmpty = isEmptyListTerm useDecls && isEmptyListTerm blockItems && isNothingTerm maybeExpr
    in if isEmpty
       then PP.text "{ }"
       else PP.lbrace PP.<> PP.nest 4 (PP.line PP.<> content) PP.<> PP.line PP.<> PP.rbrace

instance Pretty BlockItem where
  prettyF (BlockItem internal _) = prettyTerm internal PP.<> PP.text ";"

instance Pretty BlockItemInternal0 where
  prettyF (BlockItemInternal0Expression e) = prettyTerm e
  prettyF (BlockItemInternal0LetStatement ls) = prettyTerm ls

-- Let statement
instance Pretty LetStatement where
  prettyF (LetStatement _ bindList maybeType maybeInit) =
    PP.text "let" <+> prettyTerm bindList PP.<>
    prettyMaybeType maybeType PP.<>
    prettyMaybeInit maybeInit
    where
      prettyMaybeType :: MoveTerm (Maybe (ColonTokL, HiddenTypeL)) -> Doc
      prettyMaybeType mt = case project @MaybeF mt of
        Just NothingF -> PP.empty
        Just (JustF pair) -> case project @PairF pair of
          Just (PairF _ ty) -> PP.text ":" <+> prettyTerm ty
          Nothing -> prettyTerm mt
        Nothing -> prettyTerm mt
      prettyMaybeInit :: MoveTerm (Maybe (EqualsSignTokL, HiddenExpressionL)) -> Doc
      prettyMaybeInit mi = case project @MaybeF mi of
        Just NothingF -> PP.empty
        Just (JustF pair) -> case project @PairF pair of
          Just (PairF _ expr) -> PP.text " =" <+> prettyTerm expr
          Nothing -> prettyTerm mi
        Nothing -> prettyTerm mi

instance Pretty BindList where
  prettyF (BindListBind b) = prettyTerm b
  prettyF (BindListCommaBindList cbl) = prettyTerm cbl
  prettyF (BindListOrBindList obl) = prettyTerm obl

instance Pretty CommaBindList where
  prettyF (CommaBindList binds) = PP.parens (prettyListSepH (PP.text ", ") binds)

instance Pretty OrBindList where
  prettyF (OrBindList maybeOpenParen binds maybeCloseParen) =
    prettyTerm maybeOpenParen PP.<> prettyListSepH (PP.text " | ") binds PP.<> prettyTerm maybeCloseParen

instance Pretty HiddenBind where
  prettyF (HiddenBindBindInternal0 bi) = prettyTerm bi
  prettyF (HiddenBindBindUnpack bu) = prettyTerm bu
  prettyF (HiddenBindAtBind ab) = prettyTerm ab
  prettyF (HiddenBindLiteralValue lv) = prettyTerm lv

instance Pretty HiddenBindInternal0 where
  prettyF (HiddenBindInternal0MutBindVar mbv) = prettyTerm mbv
  prettyF (HiddenBindInternal0VariableIdentifier vi) = prettyTerm vi

instance Pretty MutBindVar where
  prettyF (MutBindVar _ vi) = PP.text "mut" <+> prettyTerm vi

instance Pretty BindUnpack where
  prettyF (BindUnpack ne maybeBf) = prettyTerm ne PP.<> prettyTerm maybeBf

instance Pretty BindFields where
  prettyF (BindFieldsBindPositionalFields bpf) = prettyTerm bpf
  prettyF (BindFieldsBindNamedFields bnf) = prettyTerm bnf

instance Pretty BindPositionalFields where
  prettyF (BindPositionalFields fields) = PP.parens (prettyListSepH (PP.text ", ") fields)

instance Pretty BindNamedFields where
  prettyF (BindNamedFields fields) = PP.braces (PP.space PP.<> prettyListSepH (PP.text ", ") fields PP.<> PP.space)

instance Pretty BindNamedFieldsInternal0 where
  prettyF (BindNamedFieldsInternal0BindField bf) = prettyTerm bf
  prettyF (BindNamedFieldsInternal0MutBindField mbf) = prettyTerm mbf

instance Pretty BindField where
  prettyF (BindField1 bindList maybeBind) =
    prettyTerm bindList PP.<> prettyTerm maybeBind
  prettyF (BindFieldSpreadOperator so) = prettyTerm so

instance Pretty MutBindField where
  prettyF (MutBindField _ bf) = PP.text "mut" <+> prettyTerm bf

instance Pretty HiddenSpreadOperator where
  prettyF (HiddenSpreadOperator _) = PP.text ".."

instance Pretty AtBind where
  prettyF (AtBind vi _ bindList) = prettyTerm vi <+> PP.text "@" <+> prettyTerm bindList

-- Expressions
instance Pretty HiddenExpression where
  prettyF (HiddenExpressionCallExpression ce) = prettyTerm ce
  prettyF (HiddenExpressionMacroCallExpression mce) = prettyTerm mce
  prettyF (HiddenExpressionLambdaExpression le) = prettyTerm le
  prettyF (HiddenExpressionIfExpression ie) = prettyTerm ie
  prettyF (HiddenExpressionWhileExpression we) = prettyTerm we
  prettyF (HiddenExpressionReturnExpression re) = prettyTerm re
  prettyF (HiddenExpressionAbortExpression ae) = prettyTerm ae
  prettyF (HiddenExpressionAssignExpression ae) = prettyTerm ae
  prettyF (HiddenExpressionUnaryExpression ue) = prettyTerm ue
  prettyF (HiddenExpressionBinaryExpression be) = prettyTerm be
  prettyF (HiddenExpressionCastExpression ce) = prettyTerm ce
  prettyF (HiddenExpressionQuantifierExpression qe) = prettyTerm qe
  prettyF (HiddenExpressionMatchExpression me) = prettyTerm me
  prettyF (HiddenExpressionVectorExpression ve) = prettyTerm ve
  prettyF (HiddenExpressionLoopExpression le) = prettyTerm le
  prettyF (HiddenExpressionIdentifiedExpression ie) = prettyTerm ie

instance Pretty HiddenUnaryExpression where
  prettyF (HiddenUnaryExpression internal) = prettyTerm internal

instance Pretty HiddenUnaryExpressionInternal0 where
  prettyF (HiddenUnaryExpressionInternal0UnaryExpression ue) = prettyTerm ue
  prettyF (HiddenUnaryExpressionInternal0BorrowExpression be) = prettyTerm be
  prettyF (HiddenUnaryExpressionInternal0DereferenceExpression de) = prettyTerm de
  prettyF (HiddenUnaryExpressionInternal0MoveOrCopyExpression mce) = prettyTerm mce
  prettyF (HiddenUnaryExpressionInternal0ExpressionTerm et) = prettyTerm et

instance Pretty HiddenExpressionTerm where
  prettyF (HiddenExpressionTermCallExpression ce) = prettyTerm ce
  prettyF (HiddenExpressionTermBreakExpression be) = prettyTerm be
  prettyF (HiddenExpressionTermContinueExpression ce) = prettyTerm ce
  prettyF (HiddenExpressionTermNameExpression ne) = prettyTerm ne
  prettyF (HiddenExpressionTermMacroCallExpression mce) = prettyTerm mce
  prettyF (HiddenExpressionTermPackExpression pe) = prettyTerm pe
  prettyF (HiddenExpressionTermLiteralValue lv) = prettyTerm lv
  prettyF (HiddenExpressionTermUnitExpression ue) = prettyTerm ue
  prettyF (HiddenExpressionTermExpressionList el) = prettyTerm el
  prettyF (HiddenExpressionTermAnnotationExpression ae) = prettyTerm ae
  prettyF (HiddenExpressionTermBlock b) = prettyTerm b
  prettyF (HiddenExpressionTermSpecBlock sb) = prettyTerm sb
  prettyF (HiddenExpressionTermIfExpression ie) = prettyTerm ie
  prettyF (HiddenExpressionTermDotExpression de) = prettyTerm de
  prettyF (HiddenExpressionTermIndexExpression ie) = prettyTerm ie
  prettyF (HiddenExpressionTermVectorExpression ve) = prettyTerm ve
  prettyF (HiddenExpressionTermMatchExpression me) = prettyTerm me

instance Pretty UnaryExpression where
  prettyF (UnaryExpression op expr) = prettyTerm op PP.<> prettyTerm expr

instance Pretty UnaryOp where
  prettyF (UnaryOp _) = PP.text "!"

instance Pretty BorrowExpression where
  prettyF (BorrowExpression pair) = prettyTerm pair

instance Pretty DereferenceExpression where
  prettyF (DereferenceExpression pair) = prettyTerm pair

instance Pretty MoveOrCopyExpression where
  prettyF (MoveOrCopyExpression pair) = prettyTerm pair

instance Pretty MoveOrCopyExpressionInternal0 where
  prettyF (MoveOrCopyExpressionInternal0Move _) = PP.text "move "
  prettyF (MoveOrCopyExpressionInternal0Copy _) = PP.text "copy "

-- Binary expressions
instance Pretty BinaryExpression where
  prettyF (BinaryExpression1 lhs _ rhs) = prettyTerm lhs <+> PP.text "==>" <+> prettyTerm rhs
  prettyF (BinaryExpression2 lhs _ rhs) = prettyTerm lhs <+> PP.text "||" <+> prettyTerm rhs
  prettyF (BinaryExpression3 lhs _ rhs) = prettyTerm lhs <+> PP.text "&&" <+> prettyTerm rhs
  prettyF (BinaryExpression4 lhs _ rhs) = prettyTerm lhs <+> PP.text "==" <+> prettyTerm rhs
  prettyF (BinaryExpression5 lhs _ rhs) = prettyTerm lhs <+> PP.text "!=" <+> prettyTerm rhs
  prettyF (BinaryExpression6 lhs _ rhs) = prettyTerm lhs <+> PP.text "<" <+> prettyTerm rhs
  prettyF (BinaryExpression7 lhs _ rhs) = prettyTerm lhs <+> PP.text ">" <+> prettyTerm rhs
  prettyF (BinaryExpression8 lhs _ rhs) = prettyTerm lhs <+> PP.text "<=" <+> prettyTerm rhs
  prettyF (BinaryExpression9 lhs _ rhs) = prettyTerm lhs <+> PP.text ">=" <+> prettyTerm rhs
  prettyF (BinaryExpression10 lhs _ rhs) = prettyTerm lhs <+> PP.text ".." <+> prettyTerm rhs
  prettyF (BinaryExpression11 lhs _ rhs) = prettyTerm lhs <+> PP.text "|" <+> prettyTerm rhs
  prettyF (BinaryExpression12 lhs _ rhs) = prettyTerm lhs <+> PP.text "^" <+> prettyTerm rhs
  prettyF (BinaryExpression13 lhs _ rhs) = prettyTerm lhs <+> PP.text "&" <+> prettyTerm rhs
  prettyF (BinaryExpression14 lhs _ rhs) = prettyTerm lhs <+> PP.text "<<" <+> prettyTerm rhs
  prettyF (BinaryExpression15 lhs _ rhs) = prettyTerm lhs <+> PP.text ">>" <+> prettyTerm rhs
  prettyF (BinaryExpression16 lhs _ rhs) = prettyTerm lhs <+> PP.text "+" <+> prettyTerm rhs
  prettyF (BinaryExpression17 lhs _ rhs) = prettyTerm lhs <+> PP.text "-" <+> prettyTerm rhs
  prettyF (BinaryExpression18 lhs _ rhs) = prettyTerm lhs <+> PP.text "*" <+> prettyTerm rhs
  prettyF (BinaryExpression19 lhs _ rhs) = prettyTerm lhs <+> PP.text "/" <+> prettyTerm rhs
  prettyF (BinaryExpression20 lhs _ rhs) = prettyTerm lhs <+> PP.text "%" <+> prettyTerm rhs

-- Assign expression
instance Pretty AssignExpression where
  prettyF (AssignExpression pair) =
    case project @PairF pair of
      Just (PairF lhsEq rhs) ->
        case project @PairF lhsEq of
          Just (PairF lhs _eqTok) ->
            prettyTerm lhs <+> PP.text "=" <+> prettyTerm rhs
          Nothing -> prettyTerm pair
      Nothing -> prettyTerm pair

-- Cast expression
instance Pretty CastExpression where
  prettyF (CastExpression pair) =
    case project @PairF pair of
      Just (PairF exprAs typ) ->
        case project @PairF exprAs of
          Just (PairF expr _asTok) ->
            prettyTerm expr <+> PP.text "as" <+> prettyTerm typ
          Nothing -> prettyTerm pair
      Nothing -> prettyTerm pair

-- If expression
instance Pretty IfExpression where
  prettyF (IfExpression pair) =
    case project @PairF pair of
      Just (PairF mainPart maybeElse) ->
        case project @PairF mainPart of
          Just (PairF ifCond body) ->
            case project @PairF ifCond of
              Just (PairF _ cond) ->
                PP.text "if" <+> PP.parens (prettyTerm cond) <+> prettyTerm body PP.<> prettyElseClause maybeElse
              Nothing -> prettyTerm pair
          Nothing -> prettyTerm pair
      Nothing -> prettyTerm pair
    where
      prettyElseClause :: MoveTerm (Maybe (ElseTokL, HiddenExpressionL)) -> Doc
      prettyElseClause maybeE = case project @MaybeF maybeE of
        Just NothingF -> PP.empty
        Just (JustF elsePair) -> case project @PairF elsePair of
          Just (PairF _ elseBody) -> PP.text " else" <+> prettyTerm elseBody
          Nothing -> PP.text " else" <+> prettyTerm elsePair
        Nothing -> PP.empty

-- While expression
instance Pretty WhileExpression where
  prettyF (WhileExpression _ cond body) =
    PP.text "while" <+> PP.parens (prettyTerm cond) <+> prettyTerm body

-- Loop expression
instance Pretty LoopExpression where
  prettyF (LoopExpression _ body) = PP.text "loop" <+> prettyTerm body

-- Return expression
instance Pretty ReturnExpression where
  prettyF (ReturnExpression1 _ maybeLabel expr) =
    PP.text "return" PP.<> prettyTerm maybeLabel <+> prettyTerm expr
  prettyF (ReturnExpression2 _ maybeLabel) =
    PP.text "return" PP.<> prettyTerm maybeLabel

-- Abort expression
instance Pretty AbortExpression where
  prettyF (AbortExpression _ maybeExpr) =
    PP.text "abort" PP.<> (if isNothingTerm maybeExpr then PP.empty else PP.space PP.<> prettyTerm maybeExpr)

-- Break expression
instance Pretty BreakExpression where
  prettyF (BreakExpression _ maybeLabel maybeExpr) =
    PP.text "break" PP.<> prettyTerm maybeLabel PP.<>
    (if isNothingTerm maybeExpr then PP.empty else PP.space PP.<> prettyTerm maybeExpr)

-- Continue expression
instance Pretty ContinueExpression where
  prettyF (ContinueExpression _ maybeLabel) = PP.text "continue" PP.<> prettyTerm maybeLabel

-- Label
instance Pretty Label where
  prettyF (Label _ ident) = PP.text " '" PP.<> prettyTerm ident

instance Pretty BlockIdentifier where
  prettyF (BlockIdentifier label _colonTok) = prettyTerm label PP.<> PP.text ":"

-- Name expression
instance Pretty NameExpression where
  prettyF (NameExpression maybeColonColon ma) =
    prettyTerm maybeColonColon PP.<> prettyTerm ma

-- Module access
instance Pretty ModuleAccess where
  prettyF (ModuleAccess1 _ i) = PP.text "$" PP.<> prettyTerm i
  prettyF (ModuleAccess2 _ i) = PP.text "@" PP.<> prettyTerm i
  prettyF (ModuleAccessMember ri) = prettyTerm ri
  prettyF (ModuleAccess4 i maybeTypeArgs) = prettyTerm i PP.<> prettyTerm maybeTypeArgs
  prettyF (ModuleAccess5 modId maybeTypeArgs _ i) =
    prettyTerm modId PP.<> prettyTerm maybeTypeArgs PP.<> PP.text "::" PP.<> prettyTerm i
  prettyF (ModuleAccess6 mi _ i typeArgs) =
    prettyTerm mi PP.<> PP.text "::" PP.<> prettyTerm i PP.<> prettyTerm typeArgs
  prettyF (ModuleAccess7 mi maybeTypeArgs) = prettyTerm mi PP.<> prettyTerm maybeTypeArgs
  prettyF (ModuleAccess8 mi maybeTypeArgs _ i) =
    prettyTerm mi PP.<> prettyTerm maybeTypeArgs PP.<> PP.text "::" PP.<> prettyTerm i
  prettyF (ModuleAccess9 mi _ i1 maybeTypeArgs _ i2) =
    prettyTerm mi PP.<> PP.text "::" PP.<> prettyTerm i1 PP.<> prettyTerm maybeTypeArgs PP.<> PP.text "::" PP.<> prettyTerm i2

instance Pretty HiddenReservedIdentifier where
  prettyF (HiddenReservedIdentifierForall f) = prettyTerm f
  prettyF (HiddenReservedIdentifierExists e) = prettyTerm e
  prettyF (HiddenReservedIdentifierSpec _) = PP.text "spec"

instance Pretty HiddenForall where
  prettyF (HiddenForall _) = PP.text "forall"

instance Pretty HiddenExists where
  prettyF (HiddenExists _) = PP.text "exists"

-- Call expression
instance Pretty CallExpression where
  prettyF (CallExpression pair) = prettyTerm pair

instance Pretty ArgList where
  prettyF (ArgList args) = PP.parens (prettyListSepH (PP.text ", ") args)

-- Macro call expression
instance Pretty MacroCallExpression where
  prettyF (MacroCallExpression mma maybeTypeArgs al) =
    prettyTerm mma PP.<> prettyTerm maybeTypeArgs PP.<> prettyTerm al

instance Pretty MacroModuleAccess where
  prettyF (MacroModuleAccess ma _) = prettyTerm ma PP.<> PP.text "!"

-- Literal values
instance Pretty HiddenLiteralValue where
  prettyF (HiddenLiteralValueAddressLiteral al) = prettyTerm al
  prettyF (HiddenLiteralValueBoolLiteral bl) = prettyTerm bl
  prettyF (HiddenLiteralValueNumLiteral nl) = prettyTerm nl
  prettyF (HiddenLiteralValueHexStringLiteral hl) = prettyTerm hl
  prettyF (HiddenLiteralValueByteStringLiteral bl) = prettyTerm bl
  prettyF (HiddenLiteralValueStringLiteral sl) = prettyTerm sl

instance Pretty BoolLiteral where
  prettyF (BoolLiteralTrue _) = PP.text "true"
  prettyF (BoolLiteralFalse _) = PP.text "false"

-- Unit expression
instance Pretty UnitExpression where
  prettyF (UnitExpression _ _) = PP.text "()"

-- Expression list (tuple)
instance Pretty ExpressionList where
  prettyF (ExpressionList exprs) = PP.parens (prettyListSepH (PP.text ", ") exprs)

-- Pack expression
instance Pretty PackExpression where
  prettyF (PackExpression ne fil) = prettyTerm ne <+> prettyTerm fil

instance Pretty FieldInitializeList where
  prettyF (FieldInitializeList fields) = PP.braces (PP.space PP.<> prettyListSepH (PP.text ", ") fields PP.<> PP.space)

instance Pretty ExpField where
  prettyF (ExpField fi maybePair) = prettyTerm fi PP.<> prettyTerm maybePair

-- Dot expression
instance Pretty DotExpression where
  prettyF (DotExpression pair) = prettyTerm pair

-- Index expression
instance Pretty IndexExpression where
  prettyF (IndexExpression pair) =
    case project @PairF pair of
      Just (PairF expr indices) ->
        prettyTerm expr <+> PP.text "[" PP.<> prettyListSepH (PP.text ", ") indices PP.<> PP.text "]"
      Nothing -> prettyTerm pair PP.<> PP.text "]"

-- Lambda expression
instance Pretty LambdaExpression where
  prettyF (LambdaExpression bindings maybeRetType body) =
    prettyTerm bindings PP.<>
    prettyTerm maybeRetType <+> prettyTerm body

instance Pretty LambdaBindings where
  prettyF (LambdaBindings binds) = PP.text "|" PP.<> prettyListSepH (PP.text ", ") binds PP.<> PP.text "|"

instance Pretty LambdaBinding where
  prettyF (LambdaBindingCommaBindList cbl) = prettyTerm cbl
  prettyF (LambdaBindingBind b) = prettyTerm b
  prettyF (LambdaBinding3 b maybePair) = prettyTerm b PP.<> prettyTerm maybePair

-- Vector expression
instance Pretty VectorExpression where
  prettyF (VectorExpression internal exprs _) =
    prettyTerm internal PP.<> prettyListSepH (PP.text ", ") exprs PP.<> PP.text "]"

instance Pretty VectorExpressionInternal0 where
  prettyF (VectorExpressionInternal0VectorLeftSquareBracket _) = PP.text "vector["
  prettyF (VectorExpressionInternal02 types _) = PP.text "vector<" PP.<> prettyListSepH (PP.text ", ") types PP.<> PP.text ">["

-- Match expression
instance Pretty MatchExpression where
  prettyF (MatchExpression _ expr body) =
    PP.text "match" <+> PP.parens (prettyTerm expr) <+> prettyTerm body

instance Pretty HiddenMatchBody where
  prettyF (HiddenMatchBody arms) =
    PP.lbrace PP.<> PP.nest 4 (PP.line PP.<> prettyListSepH (PP.text "," PP.<> PP.line) arms) PP.<> PP.line PP.<> PP.rbrace

instance Pretty MatchArm where
  prettyF (MatchArm pat maybeGuard _ body) =
    prettyTerm pat PP.<> prettyTerm maybeGuard <+> PP.text "=>" <+> prettyTerm body

instance Pretty MatchCondition where
  prettyF (MatchCondition _ expr) = PP.text " if" <+> PP.parens (prettyTerm expr)

-- Quantifier expression
instance Pretty QuantifierExpression where
  prettyF (QuantifierExpression pair) =
    case project @PairF pair of
      Just (PairF innerPair expr) ->
        case project @PairF innerPair of
          Just (PairF bindingsTrigger _colonTok) ->
            case project @PairF bindingsTrigger of
              Just (PairF quantKeywordBindings maybeTrigger) ->
                case project @PairF quantKeywordBindings of
                  Just (PairF keyword bindings) ->
                    prettyTerm keyword <+> prettyTerm bindings PP.<> wherePart maybeTrigger <+> PP.text ":" <+> prettyTerm expr
                  Nothing -> prettyTerm pair
              Nothing -> prettyTerm pair
          Nothing -> prettyTerm pair
      Nothing -> prettyTerm pair
    where
      wherePart mt = case project @MaybeF mt of
        Just NothingF -> PP.empty
        Just (JustF pair') -> case project @PairF pair' of
          Just (PairF _ whereExpr) -> PP.space PP.<> PP.text "where" <+> prettyTerm whereExpr
          Nothing -> PP.space PP.<> prettyTerm pair'
        Nothing -> prettyTerm mt

instance Pretty QuantifierExpressionInternal0 where
  prettyF (QuantifierExpressionInternal0Forall f) = prettyTerm f
  prettyF (QuantifierExpressionInternal0Exists e) = prettyTerm e

instance Pretty QuantifierBindings where
  prettyF (QuantifierBindings binds) = prettyListSepH (PP.text ", ") binds

instance Pretty QuantifierBinding where
  prettyF (QuantifierBinding1 name _ ty) = prettyTerm name PP.<> PP.text ":" <+> prettyTerm ty
  prettyF (QuantifierBinding2 name _ expr) = prettyTerm name <+> PP.text "in" <+> prettyTerm expr

-- Identified expression
instance Pretty IdentifiedExpression where
  prettyF (IdentifiedExpression blockId expr) =
    prettyTerm blockId <+> prettyTerm expr

-- Annotation expression
instance Pretty AnnotationExpression where
  prettyF (AnnotationExpression expr ty _) = PP.parens (prettyTerm expr PP.<> PP.text ":" <+> prettyTerm ty)

-- Types
instance Pretty HiddenType where
  prettyF (HiddenTypeApplyType at) = prettyTerm at
  prettyF (HiddenTypeRefType rt) = prettyTerm rt
  prettyF (HiddenTypeTupleType tt) = prettyTerm tt
  prettyF (HiddenTypeFunctionType ft) = prettyTerm ft
  prettyF (HiddenTypePrimitiveType pt) = prettyTerm pt

instance Pretty ApplyType where
  prettyF (ApplyType pair) = prettyTerm pair

instance Pretty RefType where
  prettyF (RefType ref ty) = prettyTerm ref PP.<> prettyTerm ty

instance Pretty HiddenReference where
  prettyF (HiddenReferenceImmRef ir) = prettyTerm ir
  prettyF (HiddenReferenceMutRef mr) = prettyTerm mr

instance Pretty ImmRef where
  prettyF (ImmRef _) = PP.text "&"

instance Pretty MutRef where
  prettyF (MutRef _ _) = PP.text "&mut "

instance Pretty TupleType where
  prettyF (TupleType types) = PP.parens (prettyListSepH (PP.text ", ") types)

instance Pretty FunctionType where
  prettyF (FunctionType params maybeRet) =
    prettyTerm params PP.<> prettyTerm maybeRet

instance Pretty FunctionTypeParameters where
  prettyF (FunctionTypeParameters types) = PP.text "|" PP.<> prettyListSepH (PP.text ", ") types PP.<> PP.text "|"

instance Pretty PrimitiveType where
  prettyF (PrimitiveTypeU8 _) = PP.text "u8"
  prettyF (PrimitiveTypeU16 _) = PP.text "u16"
  prettyF (PrimitiveTypeU32 _) = PP.text "u32"
  prettyF (PrimitiveTypeU64 _) = PP.text "u64"
  prettyF (PrimitiveTypeU128 _) = PP.text "u128"
  prettyF (PrimitiveTypeU256 _) = PP.text "u256"
  prettyF (PrimitiveTypeBool _) = PP.text "bool"
  prettyF (PrimitiveTypeAddress _) = PP.text "address"
  prettyF (PrimitiveTypeSigner _) = PP.text "signer"

-- Spec block
instance Pretty SpecBlock where
  prettyF (SpecBlock _ internal) =
    PP.text "spec" <+> prettyTerm internal

instance Pretty SpecBlockInternal0 where
  prettyF (SpecBlockInternal01 maybeTarget body) =
    prettyTerm maybeTarget PP.<> prettyTerm body
  prettyF (SpecBlockInternal0SpecFunction sf) = prettyTerm sf

instance Pretty HiddenSpecBlockTarget where
  prettyF (HiddenSpecBlockTargetIdentifier i) = prettyTerm i <+> PP.empty
  prettyF (HiddenSpecBlockTargetModule _) = PP.text "module "
  prettyF (HiddenSpecBlockTargetSpecBlockTargetSchema sbs) = prettyTerm sbs <+> PP.empty

instance Pretty SpecBlockTargetSchema where
  prettyF (SpecBlockTargetSchema _ structId typeParams) =
    PP.text "schema" <+> prettyTerm structId PP.<> prettyTerm typeParams

instance Pretty HiddenSpecFunction where
  prettyF (HiddenSpecFunctionNativeSpecFunction nsf) = prettyTerm nsf
  prettyF (HiddenSpecFunctionUsualSpecFunction usf) = prettyTerm usf
  prettyF (HiddenSpecFunctionUninterpretedSpecFunction uisf) = prettyTerm uisf

instance Pretty NativeSpecFunction where
  prettyF (NativeSpecFunction _ sig) = PP.text "native" <+> prettyTerm sig PP.<> PP.text ";"

instance Pretty HiddenSpecFunctionSignature where
  prettyF (HiddenSpecFunctionSignature funcId typeParams params retType) =
    PP.text "fun" <+> prettyTerm funcId PP.<> prettyTerm typeParams PP.<> prettyTerm params PP.<> prettyTerm retType

instance Pretty UninterpretedSpecFunction where
  prettyF (UninterpretedSpecFunction sig) = prettyTerm sig PP.<> PP.text ";"

instance Pretty UsualSpecFunction where
  prettyF (UsualSpecFunction _ sig block) =
    prettyTerm sig <+> prettyTerm block

instance Pretty SpecBody where
  prettyF (SpecBody _ useDecls members _) =
    PP.lbrace PP.<> PP.nest 4 (PP.line PP.<> prettyTerm useDecls PP.<> prettyListSepV members) PP.<> PP.line PP.<> PP.rbrace

instance Pretty HiddenSpecBlockMemeber where
  prettyF (HiddenSpecBlockMemeberSpecInvariant si) = prettyTerm si
  prettyF (HiddenSpecBlockMemeberSpecFunction sf) = prettyTerm sf
  prettyF (HiddenSpecBlockMemeberSpecCondition sc) = prettyTerm sc
  prettyF (HiddenSpecBlockMemeberSpecInclude si) = prettyTerm si
  prettyF (HiddenSpecBlockMemeberSpecApply sa) = prettyTerm sa
  prettyF (HiddenSpecBlockMemeberSpecPragma sp) = prettyTerm sp
  prettyF (HiddenSpecBlockMemeberSpecVariable sv) = prettyTerm sv
  prettyF (HiddenSpecBlockMemeberSpecLet sl) = prettyTerm sl

instance Pretty SpecApply where
  prettyF (SpecApply expr patterns maybeExcept _) =
    PP.text "apply" <+> prettyTerm expr <+> PP.text "to" <+> prettyListSepH (PP.text ", ") patterns PP.<> exceptPart PP.<> PP.text ";"
    where
      exceptPart = case project @MaybeF maybeExcept of
        Just NothingF -> PP.empty
        Just (JustF pair) -> case project @PairF pair of
          Just (PairF _ exceptPatterns) ->
            PP.space PP.<> PP.text "except" <+> prettyListSepH (PP.text ", ") exceptPatterns
          Nothing -> prettyTerm maybeExcept
        Nothing -> prettyTerm maybeExcept

instance Pretty SpecApplyPattern where
  prettyF (SpecApplyPattern maybeInternal pat typeParams) =
    prettyTerm maybeInternal PP.<> prettyTerm pat PP.<> prettyTerm typeParams

instance Pretty SpecApplyPatternInternal0 where
  prettyF (SpecApplyPatternInternal0Public _) = PP.text "public "
  prettyF (SpecApplyPatternInternal0Internal _) = PP.text "internal "

instance Pretty SpecCondition where
  prettyF (SpecConditionSpecCondition sc) = prettyTerm sc
  prettyF (SpecConditionSpecAbortIf sai) = prettyTerm sai
  prettyF (SpecConditionSpecAbortWithOrModifies sawm) = prettyTerm sawm

instance Pretty HiddenSpecAbortIf where
  prettyF (HiddenSpecAbortIf _ maybeProps expr maybeWith _) =
    PP.text "aborts_if" PP.<> prettyTerm maybeProps <+> prettyTerm expr PP.<> withPart PP.<> PP.text ";"
    where
      withPart = case project @MaybeF maybeWith of
        Just (JustF pair) -> case project @PairF pair of
          Just (PairF _ withExpr) -> PP.space PP.<> PP.text "with" <+> prettyTerm withExpr
          Nothing -> prettyTerm maybeWith
        _ -> PP.empty

instance Pretty ConditionProperties where
  prettyF (ConditionProperties props) = PP.text " [" PP.<> prettyListSepH (PP.text ", ") props PP.<> PP.text "]"

instance Pretty SpecProperty where
  prettyF (SpecProperty ident maybePair) = prettyTerm ident PP.<> prettyTerm maybePair

instance Pretty HiddenSpecAbortWithOrModifies where
  prettyF (HiddenSpecAbortWithOrModifies internal maybeProps exprs _) =
    prettyTerm internal PP.<> prettyTerm maybeProps <+> prettyListSepH (PP.text ", ") exprs PP.<> PP.text ";"

instance Pretty HiddenSpecAbortWithOrModifiesInternal0 where
  prettyF (HiddenSpecAbortWithOrModifiesInternal0AbortsWith _) = PP.text "aborts_with"
  prettyF (HiddenSpecAbortWithOrModifiesInternal0Modifies _) = PP.text "modifies"

instance Pretty HiddenSpecCondition where
  prettyF (HiddenSpecCondition internal maybeProps expr _) =
    prettyTerm internal PP.<> prettyTerm maybeProps <+> prettyTerm expr PP.<> PP.text ";"

instance Pretty HiddenSpecConditionInternal0 where
  prettyF (HiddenSpecConditionInternal0Kind kind) = prettyTerm kind
  prettyF (HiddenSpecConditionInternal02 _ maybeModule) = PP.text "requires" PP.<> spMaybe maybeModule PP.<> prettyTerm maybeModule

instance Pretty HiddenSpecConditionKind where
  prettyF (HiddenSpecConditionKindAssert _) = PP.text "assert"
  prettyF (HiddenSpecConditionKindAssume _) = PP.text "assume"
  prettyF (HiddenSpecConditionKindDecreases _) = PP.text "decreases"
  prettyF (HiddenSpecConditionKindEnsures _) = PP.text "ensures"
  prettyF (HiddenSpecConditionKindSucceedsIf _) = PP.text "succeeds_if"

instance Pretty SpecInclude where
  prettyF (SpecInclude expr) = PP.text "include" <+> prettyTerm expr PP.<> PP.text ";"

instance Pretty SpecInvariant where
  prettyF (SpecInvariant _ maybeInternal maybeProps expr _) =
    PP.text "invariant" PP.<> prettyTerm maybeInternal PP.<> prettyTerm maybeProps <+> prettyTerm expr PP.<> PP.text ";"

instance Pretty SpecInvariantInternal0 where
  prettyF (SpecInvariantInternal0Update _) = PP.text " update"
  prettyF (SpecInvariantInternal0Pack _) = PP.text " pack"
  prettyF (SpecInvariantInternal0Unpack _) = PP.text " unpack"
  prettyF (SpecInvariantInternal0Module _) = PP.text " module"

instance Pretty SpecLet where
  prettyF (SpecLet _ maybePost ident expr) =
    PP.text "let" <+> prettyTerm maybePost PP.<> spMaybe maybePost PP.<> prettyTerm ident <+> PP.text "=" <+> prettyTerm expr PP.<> PP.text ";"

instance Pretty SpecPragma where
  prettyF (SpecPragma props) = PP.text "pragma" <+> prettyListSepH (PP.text ", ") props PP.<> PP.text ";"

instance Pretty SpecVariable where
  prettyF (SpecVariable maybeInternal ident typeParams ty) =
    prettyTerm maybeInternal PP.<> prettyTerm ident PP.<> prettyTerm typeParams PP.<> PP.text ":" <+> prettyTerm ty PP.<> PP.text ";"

instance Pretty SpecVariableInternal0 where
  prettyF (SpecVariableInternal0Global _) = PP.text "global "
  prettyF (SpecVariableInternal0Local _) = PP.text "local "
