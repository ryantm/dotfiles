let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in rec {
  myXmonad = stdenv.mkDerivation rec {
    name = "myXmonad";
    buildInputs = [
      stdenv
      pkgs.haskellPackages.ghc
      pkgs.haskellPackages.xmonad
      pkgs.haskellPackages.xmobar
      pkgs.haskellPackages.xmonadContrib ];
    src = ./xmonad.hs;
    builder = ./builder.sh;
  };
}
