{
  allowUnfree = true;
  allowBroken = true;

  # On nixos
  # nix-env -f /home/ryantm/p/nixpkgs/ -i all

  # On non-nixos install with
  #   nix-env -iA nixpkgs.all

  packageOverrides = pkgs_: with pkgs_; {  # pkgs_ is the original set of packages
    all = with pkgs; buildEnv {  # pkgs is your overriden set of packages itself
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
  };
}
