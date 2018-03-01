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

    haskellTools = with pkgs; buildEnv {
      name = "haskellTools";
      paths = [
        cabal-install
        cabal2nix
        haskellPackages.hpack
      ];
    };

  };

}
