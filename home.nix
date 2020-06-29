{ pkgs, config, ... }:

let
  sources = import ./nix/sources.nix;
in

{
  nixpkgs.overlays = [
    (import sources.emacs-overlay)
  ];

  programs.home-manager.enable = true;
  programs.home-manager.path = "${sources.home-manager}/bin/home-manager";

  nixpkgs.config = import ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;

  home.packages = with pkgs; [
    cabal-install
    cabal2nix
    calibre
    evince
    firefox
    gimp
    git-crypt
    gnupg
    google-chrome
    haskellPackages.hpack
    haskellPackages.ghcid
    inkscape
    keybase-gui
    ledger
    libreoffice
    openvpn
    python
    qbittorrent
    ranger
    w3m
    remmina
    st
    scrot
    steam
    minecraft
    (import sources.ormolu {}).ormolu
    tmux
    thunderbird
    usbutils
    vlc
    yubikey-personalization-gui
    yubikey-manager
    zeal
    zsnes
    nixfmt
  ];

  home.keyboard.options = [ "ctrl:nocaps" ];

  programs.bash = {
    enable = true;
    historyControl = [ "ignoredups" ];
    historyIgnore = [ "ls" ];

    sessionVariables = {
      TERM = "xterm-256color";
      EDITOR = "emacs";
      BROWSER = "google-chrome-stable";
      TMUX_TMPDIR = "$XDG_RUNTIME_DIR";

      NIX_PATH = "nixpkgs=$HOME/p/nixpkgs";

      LEDGER_FILE = "~ledger/mulligan.ledger";
      LEDGER_STRICT = "true";
      LEDGER_PEDANTIC = "true";
    };

    shellAliases = {
      ls = "ls --color=auto";
      grep = "grep --color=auto";
      tmux = "tmux -f ~/${config.xdg.configFile.tmux.target}";
    };

    initExtra = ''
      function settitle {
        tmux rename-window "$1"
      }

      # Set terminal title during ssh session
      function ssh {
        settitle "$*"
        command ssh "$@"
        settitle "bash"
      }

      # Git user configuration scripts
      function git_config_user {
        git config --replace-all user.name "$1"
        git config --replace-all user.email "$2"
      }
      function git_check_user_config {
        echo "Name: `git config user.name`"
        echo "Email: `git config user.email`"
      }
      function git_work_private {
        git_config_user "Ryan Mulligan" "ryantm@pololu.com"
        git_check_user_config
      }
      function git_work_public {
        git_config_user "RyanTM (Pololu)" "dev-ryantm@pololu.com"
        git_check_user_config
      }
      function git_personal {
        git_config_user "Ryan Mulligan" "ryan@ryantm.com"
        git_check_user_config
      }
    '';

  };

  programs.emacs.enable = true;
  programs.emacs.extraPackages = epkgs:
    with epkgs; [
      bash-completion
      counsel
      csv-mode
      dhall-mode
      diminish
      elisp-slime-nav
      fill-column-indicator
      flycheck-haskell
      forge
      forge
      graphql-mode
      graphviz-dot-mode
      haml-mode
      hi2
      inf-ruby
      ivy
      ivy-hydra
      ledger-mode
      lxc
      magit
      magit-annex
      markdown-preview-mode
      multiple-cursors
      nix-mode
      ormolu
      paredit
      powerline
      purescript-mode
      rainbow-delimiters
      swiper
      use-package
      yaml-mode
      zeal-at-point
    ];

  programs.git = {
    userEmail = "";
    userName = "";
    enable = true;
    aliases = {
      review =
        "!sh -c 'git fetch -a && gitk origin/review/$1 --not origin/$2' -";
      unmerged = "!git branch -a --no-merged | grep remotes/";
      sync = "!git fetch --all --prune && git fetch --tags";
      delbranch =
        "!sh -c 'git branch -d dev/ryantm/$1 && git push origin :dev/ryantm/$1' -";
    };
    extraConfig = {
      user.useConfigOnly = true;
      color = {
        diff = "auto";
        status = "auto";
        branch = "auto";
      };
      push.default = "simple";
      github.user = "ryantm";
      merge.conflictstyle = "diff3";
    };
    ignores =
      [ "result" "*.elc" ".#*" ".stack-work/" "#*" ".markdown-preview.html" ];
  };

  home.file = {

    ".emacs.d" = {
      source = ./emacs;
      recursive = true;
      onChange = ''
        emacs --batch --eval '(byte-compile-file "~/.emacs.d/init.el")'
        emacs --batch --eval '(byte-compile-file "~/.emacs.d/custom-file.el")'
      '';
    };

    ".gemrc".text = "gem: --no-ri --no-rdoc";
    ".ghc/ghci.conf".source = ./ghc/ghci.conf;
    ".stack/config.yaml".source = ./stack/config.yaml;
    ".xinitrc".source = ./x/xinitrc;
    ".Xresources".source = ./x/Xresources;
    ".dir_colors".source = ./shell/dir_colors;
    ".asoundrc".source = ./alsa/asoundrc;
  };

  xdg.configFile.tmux = {
    target = "tmux/tmux.conf";
    text = ''
      set-option -g prefix C-z
      bind-key C-z send-prefix
      set-option -g renumber-windows on
    '';
  };

  xdg.configFile."fonts" = {
    source = ./config/fonts;
    recursive = true;
  };

  xdg.configFile."ranger/rc.conf".text = ''
    set preview_images true
    set preview_images_method urxvt
  '';

  # systemd.user.services.ssh-agent = {
  #   description = "SSH key agent";
  #   environment.SSH_AUTH_SOCK = "%t/ssh-agent";
  # };

  services.gpg-agent = {
    enable = true;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
    pinentryFlavor = "gnome3";
    verbose = true;
  };

}
