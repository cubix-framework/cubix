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

1. In the tree-sitter grammar definition project root dir, you need to initialize project, with appropriate config file `tree-sitter.json` by running:
```bash
tree-sitter init
```
You can find example config for Sui Move grammar in the `tree-sitter-sui-move/vendor/tree-sitter.json`. The important part is to enable bindings generation for the `C` language.

2. Tree-sitter syntax generation cannot handle tree-sitter alias rule properly, therefore one needs to preprecess language `grammar.json` file with this jq filter, in the tree-sitter grammar definition project root directory:
```bash
cat <<< $(jq 'walk(if type != "object" then . else if .type == "ALIAS" then .content else . end end)' src/grammar.json) > src/grammar.json
```

3. Once processed you need to regenerate tree-sitter artefacts from processed grammar file:
```bash
tree-sitter generate --abi 14 src/grammar.json
```

4. Make a new cubix package that adds language support.

5. Run `gen-mod` from `cubix-tree-sitter` to generate Modularized syntax tree
```bash
cabal run gen-mod -- \
    {path to grammar.json} \
    --start-rule-name {top level tree-sitter rule: usually source_file} \
    --module-name Cubix.Language.{LanguageName}.Modularized \
    --token-map {path to preserved_tokens.json} \
    -o {output file}
```

6. Run `gen-parser` from `cubix-tree-sitter` to generate parser for tree-sitter lexed tokens
```bash
cabal run gen-parser -- \
    {path to grammar.json} \
    --start-rule-name {top level tree-sitter rule: usually source_file} \
    --module-name Cubix.Language.{LanguageName}.ParsePretty \
    --token-map {path to preserved_tokens.json} \
    -o {output file}
```

7. Define `IPS.Types` submodule with appropriate nodes replaced by it's generic counterparts

8. Add `IPS.Trans` submodule with instances of the Trans typeclass for translation from modularized to parametric syntax

9. Add `Untrans` instance for translating back from parametric to modularized syntax

### Parametrizing Modular syntax

In `Types` module:
1. Identify node from modularized syntax that can be replaced by parameterized one

2. Define sort inclusion types by utilizing `TemplateHaskell` functions: `createSortInclusionTypes`, `deriveAllButSortInjection` and `createSortInclusionInfers`.

3. Update the `Declaring the IPS` section with appropriate nodes:
  - add defined sort injection to `${lang}SortInjections` array
  - add parametric node to list of additional nodes
  - potentially remove old node from list of names

4. Define additional sort injections if necessary

In `Trans` module:
1. Add sort injection to exemption list for generating default instance

2. Define `Trans` instance for node being replaced. Additionally define helper function that also changes the sort type. Be sure to utilize automatically derived Cubix smart constructors.

3. Define `Untrans` instance for getting back the Modularized syntax from parametric one. Similarly it might be easier to define helper function first, that also changes the sort type.

## Dealing with type errors
While compiling Cubix one can encounter scary looking, huge compilation errors. While intimidating at first are actually pretty easy once you learn how to read them. Whenever you gat a huge type error about missing instance for `KnownNat` with a lot of `1 +...` it means that some node that you are trying to access isn't in your signature. Look at the end of the message to learn which node is missing.

## Advanced References

### Papers
- **One Tool, Many Languages: Language-Parametric Transformation with Incremental Parametric Syntax** - Describes Cubix framework
- **One CFG-Generator to Rule Them All** - Original _compositional data types_ as in the `compdata` package. Note that this project uses it's own fork
- **Typed Multi-Language Strategy Combinators** - Describes `compstrat` package

## Code Guards: Protected Code Sections

**Code Guards mark critical code that has broken before and should rarely be changed.**

### 🚨 CRITICAL RULES for AI Agents

1. **Code within Code Guards should RARELY be changed**
2. **IF you want to change guarded code**:
   - You MUST ask the user for explicit permission first
   - Explain why the change is necessary
   - Describe what could break if done incorrectly
3. **AFTER changing guarded code**:
   - You MUST inform the user what was changed
   - Explain how you preserved the critical functionality
   - Recommend testing steps to verify nothing broke

### Identifying Code Guards

Code Guards are marked with special comments:

```haskell
-- CODE_GUARD_START
-- Name: top level translate interface
-- Description: Entry point for translation
translate :: F.MoveTerm l -> MSuiMoveTerm l
translate = trans . unTerm @(Sum F.MoveSig)
-- CODE_GUARD_END
```

**Key markers**:

- `CODE_GUARD_START` - Start of protected region
- `Name: [Guard Name]` - Human-readable name for this guard
- `Description: [Feature description]` - (Optional) Describe the feature it protects
- `CODE_GUARD_END` - End of protected region
