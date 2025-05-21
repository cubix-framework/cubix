{ pkgs, ... }:

{
  packages = with pkgs; [
    ghcid
  ];

  # https://devenv.sh/languages/
  languages = {
    haskell = {
      enable = true;
      package = pkgs.haskell.compiler.ghc98;
    };
  };


  # See full reference at https://devenv.sh/reference/options/
}
