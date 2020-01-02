#!/usr/bin/env bash
set -euxo pipefail

NIX_PATH=nixpkgs=https://releases.nixos.org/nixos/unstable/nixos-20.03pre206632.b0bbacb5213/nixexprs.tar.xz
HM_PATH=${HM_PATH:-https://github.com/rycee/home-manager/archive/master.tar.gz}

home-manager switch
