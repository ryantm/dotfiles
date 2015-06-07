{ }:
let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in rec {
  myXmonad = stdenv.mkDerivation rec {
    name = "myXmonad";
    buildInputs = [
      stdenv
      (pkgs.haskell.packages.ghc784.ghcWithPackages (pkgs : with pkgs; [
        xmonad xmonad-contrib xmobar
      ]))
    ];
    buildCommand = ''
      mkdir -p $out/
      ghc -o $out/xmonad $src
    '';
    src = ./xmonad.hs;
  };
}
