# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Cubix is a framework for language-parametric program transformation - defining single source-to-source transformations that work across multiple programming languages. It currently supports C, Java, JavaScript, Lua, and Python. The core innovation is "incremental parametric syntax" which allows language definitions to share common components while enabling transformations to be written generically.

## Build System and Development Commands

### Development Environment Setup

**Initialize submodules:**
```bash
git submodule update --init
```

**Enter development environment:**
```bash
devenv shell
```

### Build Commands

**Primary build command (within devenv shell):**
```bash
cabal build
```

**Build specific target:**
```bash
cabal build cubix
```

**Build examples:**
```bash
cabal build cubix-examples
```

**Build benchmarks:**
```bash
cabal build cubix-benchmarks:mainbench
```

### Running Transformations

**Main transformation driver:**
```bash
cabal exec examples-multi <language> <transform> <file>*
```

**Available languages:** `c`, `java`, `javascript`, `lua`, `python`

**Available transformations:** `debug`, `id`, `cfg`, `elementary-hoist`, `hoist`, `tac`, `testcov`, `ipt`

**Available analyses:** `anal-triv-call`

**Example:**
```bash
cabal exec examples-multi javascript tac Foo.js
```

### Test Commands

**Run CFG tests:**
```bash
cabal test cfg-test
```

**Run benchmarks:**
```bash
cabal bench cubix-benchmarks:mainbench
```

**Run language test suites:**
```bash
./scripts/test_<lang>.rb <transformation>
```

### Alternative Commands (Legacy)

The repository also supports Stack for legacy compatibility, but **cabal within devenv is the recommended approach**.

## Code Architecture

### Core Components

**Language Fragments (`src/Cubix/Language/Parametric/Syntax/`):**
- Base syntax definitions shared across languages
- Expression, Function, VarDecl, and Functor fragments
- Built using higher-kinded data types with kind `(* -> *) -> * -> *`

**Language-Specific Implementations (`src/Cubix/Language/*/`):**
- Each language has `Parametric/Common/` and `Parametric/Full/` modules
- `Common/` contains shared parametric syntax definitions
- `Full/` contains complete language-specific implementations
- `Parse.hs` handles parsing for each language

**Transformation Framework (`src/Cubix/Transformations/`):**
- `Hoist/` - Code hoisting transformations
- `TAC/` - Three-address code conversion
- `Plumbing/IPT.hs` - Interprocedural plumbing transformation
- `TestCoverage.hs` - Test coverage analysis
- `Variation.hs` - Program variation generation

**Analysis Framework (`src/Cubix/Analysis/`):**
- `Call/Trivial.hs` - Basic call analysis

**Supporting Libraries:**
- `compdata/` - Compositional data types library
- `compstrat/` - Strategy combinators library
- `comptrans/` - Transformation utilities library

**Separate Packages:**
- `cubix-examples/` - Example usage of the Cubix framework
- `cubix-benchmarks/` - Performance benchmarks for the Cubix framework
- `cubix-solidity/` - Solidity language support

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

## Project Structure

```
src/Cubix/
├── Language/
│   ├── Parametric/           # Core parametric syntax framework
│   ├── C/                    # C language support
│   ├── Java/                 # Java language support  
│   ├── JavaScript/           # JavaScript language support
│   ├── Lua/                  # Lua language support
│   └── Python/               # Python language support
├── Transformations/          # Program transformations
├── Analysis/                 # Program analyses
├── Sin/                      # Compatibility layer
├── Essentials.hs            # Beginner-friendly exports
└── ParsePretty.hs           # Parsing and pretty-printing
```

## Important Development Notes

### Compilation Performance

- **Never use `-O1` or `-O2`** - causes GHC to run out of memory
- Use `-O0` with increased heap sizes (configured in cabal.project)
- Performance issues are due to extensive Template Haskell and type-level programming
- The cabal.project file includes optimized GHC options for compilation

### Language Support

- Each language has extensive test suites in `integration-test-input-files/`
- Third-party parsers are used (forks of `language-c`, `language-java`, etc.)
- Parser dependencies are managed through git submodules

### Testing

- CFG tests verify control flow graph construction
- Language test suites run transformations on real codebases
- Use `count_loc` parameter to count lines of code in test suites

## Common Development Patterns

### Adding New Language Fragments

1. Define GADT in `src/Cubix/Language/Parametric/Syntax/`
2. Use `deriveAll [''YourFragment]` to generate instances
3. Add to appropriate language signature
4. Create sort inclusion types if needed

### Writing Transformations

1. Use strategy combinators from `compstrat` library
2. Build on `RewriteM` monad for stateful transformations
3. Use `alltdR` for top-down rewrites, `prunetdR` for pruning
4. Leverage `DynCase` for pattern matching on terms

### Working with Terms

- Use smart constructors (prefixed with `i`) for term construction
- Use `project` to extract specific node types from terms
- Use `inject` to embed terms into larger signatures
- Use `stripA` to remove annotations from terms

## External Dependencies

- Custom forks of language parsers (see `cabal.project` for git dependencies)
- JAR files for Java parsing: `javaparser-1.0.8.jar`, `javaparser-to-hs.jar`
- Various Haskell libraries for parsing and pretty-printing

This framework requires deep understanding of advanced Haskell concepts including higher-kinded types, GADTs, type families, and extensive Template Haskell usage.