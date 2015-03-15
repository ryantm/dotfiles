{ }:
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
    buildCommand = ''
      mkdir -p $out/
      ghc -o $out/xmonad $src
    '';
    src = ./xmonad.hs;
  };
}
