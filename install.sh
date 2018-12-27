#!/usr/bin/env bash
set -euxo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

nixpkgsConfig=${XDG_HOME_CONFIG:-~/.config}/nixpkgs

mkdir -p $nixpkgsConfig
ln -fs "$DIR/home.nix" "$nixpkgsConfig/home.nix"


NIX_PATH=nixpkgs=/home/ryantm/p/nixpkgs
HM_PATH=${HM_PATH:-https://github.com/rycee/home-manager/archive/master.tar.gz}

nix-shell $HM_PATH -A install
