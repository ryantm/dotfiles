#!/usr/bin/env bash
set -euxo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

nixpkgsConfig=${XDG_HOME_CONFIG-~/.config}/nixpkgs

mkdir -p $nixpkgsConfig
ln -s "$DIR/home.nix" "$nixpkgsConfig/home.nix"
