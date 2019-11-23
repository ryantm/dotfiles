#!/usr/bin/env bash
set -euxo pipefail

NIX_PATH=nixpkgs=https://releases.nixos.org/nixos/19.09/nixos-19.09.1320.4ad6f1404a8/nixexprs.tar.xz 
HM_PATH=${HM_PATH:-https://github.com/rycee/home-manager/archive/master.tar.gz}

home-manager build
