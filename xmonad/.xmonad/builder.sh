source $stdenv/setup

mkdir $out
ghc -o $out/xmonad $src
