{ config, inputs, pkgs, ... }:

let
  unstable = import inputs.unstable {
    config.allowUnfree = true;
    system = pkgs.stdenv.system;
  };

in {
  packages = with pkgs; [
    fourmolu
    ghcid
    tree-sitter
    unstable.claude-code
  ];

  # This sets gcc version brought into env by languages.c option
  stdenv = pkgs.gcc11Stdenv;

  # https://devenv.sh/languages/
  languages = {
    haskell = {
      enable = true;
      package = pkgs.haskell.compiler.ghc98;
    };

    # languages that cubix can work with
    # Jakub 2025.06.12 TODO: fix versions once it's figured out which
    #                  one are needed
    c.enable = true;
    java.enable = true;
    javascript.enable = true;
    lua = {
      enable = true;
      package = pkgs.lua53Packages.lua;
    };
    python.enable = true;

    # test running is in ruby
    ruby.enable = true;
  };

  scripts = {
    gen-sui-move.exec = ''
      pushd $DEVENV_ROOT
      cabal run cubix-tree-sitter:exe:gen-syntax -- \
        --language SuiMove \
        --nodes tree-sitter-sui-move/vendor/tree-sitter-move/external-crates/move/tooling/tree-sitter/src/node-types.json \
        | fourmolu --stdin-input-file cubix-sui-move/cubix-move.cabal \
        > cubix-sui-move/src/Language/SuiMove/Syntax.hs
      popd
    '';
  };

  env = {
    "LUA_TESTS" = "${config.devenv.root}/test/lua/lua-5.3.3-tests";
    # Jakub 2025.06.12: Package the various language specific test cases.
    # Right now I'll point to the jkoppel old location.
  };

  # See full reference at https://devenv.sh/reference/options/
}
