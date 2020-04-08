let
  pinnedNixpkgs = import (fetchTarball
    "https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz") {
      config = {
        allowUnfree = true;
        minecraft.alsa = true;
      };
    };
in {
  allowUnfree = true;
  allowBroken = true;
  packageOverrides = pkgs_: with pkgs_; { };
}
