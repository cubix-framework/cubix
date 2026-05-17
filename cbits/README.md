# Vendored tree-sitter-java parser

These files are vendored from
[tree-sitter/tree-sitter-java](https://github.com/tree-sitter/tree-sitter-java)
to provide a JVM-free Java parser for Cubix.

- **Upstream tag**: `v0.23.5`
- **Upstream commit**: `94703d5a6bed02b98e438d7cad1136c01a60ba2c`
- **tree-sitter ABI**: `14` (declared as `LANGUAGE_VERSION` in `parser.c`)

## Vendored files

| Local path                     | Upstream path                |
|--------------------------------|------------------------------|
| `cbits/parser.c`               | `src/parser.c`               |
| `cbits/tree_sitter/parser.h`   | `src/tree_sitter/parser.h`   |
| `cbits/tree_sitter/array.h`    | `src/tree_sitter/array.h`    |
| `cbits/tree_sitter/alloc.h`    | `src/tree_sitter/alloc.h`    |

`parser.c` is the only `c-sources` entry. It declares all of its `ts_*`
symbols `static`, so it does not collide with `hs-tree-sitter`'s runtime,
which provides `lib.c` and exports `ts_parser_*` etc.

The public entry point is `tree_sitter_java(void) -> const TSLanguage *`,
bound on the Haskell side via `TreeSitter.Java`.

## Re-syncing to a newer upstream

1. `git clone --depth 1 --branch <new-tag> https://github.com/tree-sitter/tree-sitter-java.git /tmp/tsj`
2. Copy `src/parser.c` and `src/tree_sitter/{parser,array,alloc}.h` over the
   files in this directory.
3. Update the tag/commit lines above.
4. Confirm `LANGUAGE_VERSION` in the new `parser.c` is in the
   `[MIN_COMPATIBLE_LANGUAGE_VERSION, LANGUAGE_VERSION]` range that
   `hs-tree-sitter`'s vendored runtime supports. The startup smoke check in
   `parseJavaTrackSources` will refuse to load an out-of-range language.
5. Re-run the validation harness in `cubix-examples/java-ts-validate/`
   against the K-Java corpus.
