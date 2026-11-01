Tree-sitter grammar for Python, preprocessed for Cubix
======================================================

The C parser of [tree-sitter-python](https://www.npmjs.com/package/tree-sitter-python)
0.23.6 (tree-sitter ABI 14, the newest that the pinned hs-tree-sitter
supports), exposing `tree_sitter_python` to Haskell as `TreeSitter.Python`.

The grammar is preprocessed before `parser.c` is generated, because the
Cubix generators (`gen-mod`, `gen-parser` from `cubix-tree-sitter`)
cannot handle every tree-sitter construct; the trees this parser produces
therefore differ from upstream's (no aliased node names, and supertype
rules such as `expression` appear as nodes).

- `vendor/grammar.json`: upstream `package/src/grammar.json`, unmodified.
- `cbits/src/grammar.json`: the preprocessed grammar, the input to both
  `tree-sitter generate` and the Cubix generators.
- `cbits/src/parser.c`, `node-types.json`, `tree_sitter/{alloc,array,parser}.h`:
  generated from the preprocessed grammar.
- `cbits/src/scanner.c`: upstream's external scanner, unmodified.

To regenerate (outside the repository, e.g. in a scratch directory):

1. `npm pack tree-sitter-python@0.23.6 && tar xzf tree-sitter-python-0.23.6.tgz`
2. Strip aliases and supertypes:
   ```bash
   jq -f cubix-tree-sitter/jq/alias.jq package/src/grammar.json \
     | jq -f cubix-tree-sitter/jq/supertypes.jq > grammar.json
   ```
3. `tree-sitter generate --abi 14 grammar.json` (writes `src/`).
4. Copy `grammar.json` and `src/{parser.c,node-types.json,tree_sitter/*.h}`
   to `cbits/src/`, and upstream's `scanner.c` next to them.
5. Regenerate the Cubix modules in `cubix-*` (see the devenv scripts
   `gen-python-mod` and `gen-python-parser`).
