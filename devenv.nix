{ config, pkgs, ... }:

{
  packages = with pkgs; [
    ghcid
    lua53Packages.lua
    ruby
  ];

  # https://devenv.sh/languages/
  languages = {
    haskell = {
      enable = true;
      package = pkgs.haskell.compiler.ghc98;
    };
  };

  env = {
    "LUA_TESTS" = "${config.devenv.root}/test/lua/lua-5.3.3-tests";
  };

  # See full reference at https://devenv.sh/reference/options/
}
