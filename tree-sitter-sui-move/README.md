Haskell bindings for Sui Move tree-sitter
=========================================

In order to build this library there are several steps you need to take.

1. Make sure the submodule with the sui project is checked out by:
```bash
git submodule update --init
```

2. Go to the tree-sitter grammar definition project
```bash
cd vendor/tree-sitter-move/external-crates/move/tooling/tree-sitter/
```

3. Copy the tree-sitter config:
```bash
cp ../../../../../tree-sitter.json .
```

4. Generate missing `C` headers:
```bash
tree-sitter init
```

5. Tree-sitter syntax generation cannot handle tree-sitter alias rule properly, therefore one needs to preprecess language `grammar.json` file with this jq filter, in the tree-sitter grammar definition project root directory:
```bash
cat <<< $(jq 'walk(if type != "object" then . else if .type == "ALIAS" then .content else . end end)' src/grammar.json) > src/grammar.json
```

6. Regenerate tree-sitter artefacts from processed grammar file:
```bash
tree-sitter generate --abi 14 src/grammar.json
```
