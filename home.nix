{ pkgs, config, sources, ... }:

{

  _module.args.sources = import ./nix/sources.nix;

  imports = [
    ./emacs
  ];

  programs.home-manager.enable = true;
  programs.home-manager.path = "${sources.home-manager}/bin/home-manager";

  nixpkgs.config = import ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;

  home.packages = with pkgs; [
    cabal-install
    cabal2nix
    calibre
    (import sources.comma { inherit pkgs; })
    cmus
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
    lf
    libreoffice
    openvpn
    python
    qbittorrent
    w3m
    remmina
    st
    scrot
    steam
    minecraft
    (import sources.nix-tree).nix-tree
    (import sources.ormolu { }).ormolu
    tmux
    thunderbird
    usbutils
    vlc
    yubikey-personalization-gui
    yubikey-manager
    zeal
    zsnes
    nixfmt
    (pkgs.writeScriptBin "rdp" ''
      xfreerdp /u:Pololu\\RyanTM /v:RYANTM0J330:3389 +clipboard /f /sound +fonts -wallpaper +auto-reconnect
    '')
    (pkgs.writeScriptBin "hms" ''
      pushd ~/p/dotfiles
      nix-shell --run "home-manager switch"
      popd
    '')
    (pkgs.writeScriptBin "hmud" ''
      pushd ~/p/dotfiles
      nix-shell --run "niv update"
      popd
    '')
  ];

  home.keyboard.options = [ "ctrl:nocaps" ];

  programs.bash = {
    enable = true;
    historyControl = [ "ignoredups" ];
    historyIgnore = [ "ls" ];

    sessionVariables = {
      TERM = "xterm-256color";
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

  programs.bat = {
    enable = true;
    config.theme = "GitHub";
  };

  programs.git = {
    userEmail = "";
    userName = "";
    enable = true;
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
    pinentryFlavor = "tty";
    verbose = true;
  };

  programs.beets = {
    enable = true;
    settings = {
      directory = "/data/music";
      library = "/data/music/musiclibrary.db";
      "import" = {
        move = "yes";
      };
    };
  };

}
