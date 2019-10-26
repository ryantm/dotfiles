let
  pinnedNixpkgs =
    import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {
      config = {
        allowUnfree = true;
        minecraft.alsa = true;
      };
    };
in
{
  allowUnfree = true;
  allowBroken = true;

  # On nixos
  # nix-env -f /home/ryantm/p/nixpkgs/ -i all

  # On non-nixos install with
  #   nix-env -iA nixpkgs.all

  packageOverrides = pkgs_: with pkgs_; {

    all = with pkgs; buildEnv {
      name = "all";
      paths = [
        rxvt_unicode
        openvpn
        google-chrome
        firefox
        ledger
        thunderbird
        remmina
        vlc
        scrot
        gimp
        calibre
        libreoffice
        qbittorrent
        fbreader
        usbutils
        zsnes
        ruby
        python
        evince
        yubikey-personalization-gui
        steam
      ];
    };

    haskellTools = with pinnedNixpkgs; buildEnv {
      name = "haskellTools";
      paths = [
        haskell.packages.ghc843.cabal-install
        haskell.packages.ghc843.cabal2nix
        haskell.packages.ghc843.hpack
      ];
    };

  };

}
