# The Cubix framework

The Cubix framework for multi-language transformation.

## Build commands with [devenv](https://devenv.sh/) (recommended)

```bash
devenv                                 # Enter dev shell with GHC, cabal, HLS
cabal build                            # Build main cubix library and executables
cabal build cubix-sui-move             # Build associated Sui Move language support library and executables
cabal build cubix-examples             # Build programs that demonstrate cubix capabilities
cabal build cubix-benchmarks:mainbench # Build benchmarks
```

Cubix is a framework for language-parametric program transformation - defining single source-to-source transformations that work across multiple programming languages. It currently supports C, Java, JavaScript, Lua, and Python. The core innovation is "incremental parametric syntax" which allows language definitions to share common components while enabling transformations to be written generically.

## Architecture Overview

### Project Structure

```
├── src/Cubix/
│   ├── Language/
│   │   ├── Parametric/     # Core parametric syntax framework
│   │   ├── C/              # C language support (deprecated, language support should be it separate package)
│   │   ├── Java/           # Java language support (deprecated, language support should be it separate package)
│   │   ├── JavaScript/     # JavaScript language support (deprecated, language support should be it separate package)
│   │   ├── Lua/            # Lua language support (deprecated, language support should be it separate package)
│   │   └── Python/         # Python language support (deprecated, language support should be it separate package)
│   ├── Transformations/    # Program transformations
│   ├── Analysis/           # Program analyses
│   ├── Sin/                # Compatibility layer
│   ├── Essentials.hs       # Beginner-friendly exports
│   └── ParsePretty.hs      # Parsing and pretty-printing
├── compdata/               # Compositional data types library
├── compstrat/              # Strategy combinators library
├── comptrans/              # Transformation utilities library
├── cubix-solidity/         # Solidity language support
├── cubix-tree-sitter/      # Generating cubix language support based on tree-sitter grammar definitions
├── cubix-examples          # Demos of cubix capabilities
├── cubix-benchmarks/       # Cubix framework benchmarks
├── cubix-sui-move/         # Generated (from tree-sitter grammar) language support for Sui Move
└── tree-sitter-sui-move/   # Contains tree-sitter grammar for sui-move
```

### Language support package structure

Supporting new language should be realised by adding new package: `cubix-{language_name}`. Within that package there should be module hierarchy like:

```
Cubix.Language.{LanguageName}
├── Modularized # Modularized AST, generated automatically by `gen-mod` executable of `cubix-tree-sitter`
├── ParsePretty # Parser for language, generated from `gen-parser` executable of `cubix-tree-sitter`
└── IPS         # Incremental Parametric Syntax, where some of the modularized nodes have been replaced by the generic ones, from the Cubix framework
```

Note that the support modules for C, Java, JavaScript, Lua and Python are bundled within the main Cubix library. We want to move away from that by splitting them out to their own separate packages. This already happened for Solidity support modules.

Also note that legacy support modules have different names. `Cubix.Language.{LanguageName}.Full` for Modularized syntax module and `Cubix.Language.{LanguageName}.Common` for Incremental Parametric Syntax module.

### Core Components of Cubix framework

**Language Fragments (`src/Cubix/Language/Parametric/Syntax/`):**
- Base syntax definitions shared across languages
- Expression, Function, VarDecl, and Functor fragments
- Built using higher-kinded data types with kind `(* -> *) -> * -> *`

**Transformation Framework (`src/Cubix/Transformations/`):**
- `Hoist/` - Code hoisting transformations
- `TAC/` - Three-address code conversion
- `Plumbing/IPT.hs` - Interprocedural plumbing transformation
- `TestCoverage.hs` - Test coverage analysis
- `Variation.hs` - Program variation generation

**Analysis Framework (`src/Cubix/Analysis/`):**
- `Call/Trivial.hs` - Basic call analysis

### Key Types and Concepts

**Signatures:** Type-level lists of language fragments (e.g., `MLuaSig`, `MJavaSig`)

**Terms:** `Term fs l` represents terms of signature `fs` with sort `l`

**Language Fragments:** Defined using GADTs with explicit sort annotations

**Sort Injections:** Automatic conversions between different syntactic sorts

**Smart Constructors:** Auto-generated constructors with signature membership constraints

### Template Haskell Usage

The codebase extensively uses Template Haskell for deriving instances:
- `deriveAll` generates all standard instances for language fragments
- `createSortInclusionType` creates sort conversion types
- Smart constructors and pattern synonyms are auto-generated

### Adding new language support (based on tree-sitter)

1. Tree-sitter syntax generation cannot handle tree-sitter alias rule properly, therefore one needs to preprecess language `grammar.json` file with this jq filter, in the tree-sitter grammar definition project root directory:
```bash
cat <<< $(jq 'walk(if type != "object" then . else if .type == "ALIAS" then .content else . end end)' src/grammar.json) > src/grammar.json
```

2. Once processed you need to regenerate tree-sitter artefacts from processed grammar file:
```bash
tree-sitter generate --abi 14 src/grammar.json
```

3. Make a new cubix package that adds language support.

4. Investigate and make a `preserved_tokens.json` file, for tokens that tree-sitter would normally omit, but you want to preserve in the syntax tree.

5. Run `gen-mod` from `cubix-tree-sitter` to generate Modularized syntax tree
```bash
cabal run gen-mod -- \
    {path to grammar.json} \
    --start-rule-name {top level tree-sitter rule: usually source_file} \
    --module-name Cubix.Language.{LanguageName}.Modularized \
    --token-map {path to preserved_tokens.json} \
    -o {output file}
```

6. Run `gen-parser` from `cubix-tree-sitter` to generate Modularized syntax tree
```bash
cabal run gen-parser -- \
    {path to grammar.json} \
    --start-rule-name {top level tree-sitter rule: usually source_file} \
    --module-name Cubix.Language.{LanguageName}.ParsePretty \
    --token-map {path to preserved_tokens.json} \
    -o {output file}
```

7. Define `IPS.Types` submodule with appropriate nodes replacet by it's generic counterparts

8. Add `IPS.Trans` submodule with instances of the Trans typeclass for translation from modularized to parametric syntax

9. Add Untrans instance for translating back from parametric to modularized syntax

## Advanced References

### Papers
- **One Tool, Many Languages: Language-Parametric Transformation with Incremental Parametric Syntax** - Describes Cubix framework
- **One CFG-Generator to Rule Them All** - Original _compositional data types_ as in the `compdata` package. Note that this project uses it's own fork
- **Typed Multi-Language Strategy Combinators** - Describes `compstrat` package
