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
    (import sources.nixpkgs-update {})
    (import sources.comma { inherit pkgs; })
    nix-tree
    (import sources.ormolu {}).ormolu
    beancount
    cmus
    evince
    firefox
    freerdp
    gimp
    git-crypt
    gitAndTools.gh
    (import sources.nixpkgs-master {}).gitAndTools.git-delete-merged-branches
    gnupg
    google-chrome
    haskellPackages.hpack
    hydra-check
    inkscape
    jumpapp
    ledger
    lf
    libreoffice
    minecraft
    meld
    niv
    nixfmt
    nixpkgs-review
    openvpn
    python
    qbittorrent
    remmina
    scrot
    st
    thunderbird
    tmux
    usbutils
    virt-manager
    vlc
    w3m
    xterm
    yubikey-manager
    yubikey-personalization-gui
    zeal
    zsnes
    (
      pkgs.writeScriptBin "rdp" ''
        ${pkgs.gnome3.zenity}/bin/zenity --entry --title="Pololu\\RyanTM password" --text "Enter your _password:" --hide-text | \
          ${pkgs.freerdp}/bin/xfreerdp /u:Pololu\\RyanTM /v:RYANTM0J330.pololu.internal:3389 +clipboard /cert:tofu /floatbar /f /sound +fonts -wallpaper +auto-reconnect /from-stdin
      ''
    )
    (
      pkgs.writeScriptBin "hms" ''
        pushd ~/p/dotfiles
        nix develop -c home-manager switch
        popd
      ''
    )
    (
      pkgs.writeScriptBin "hmud" ''
        pushd ~/p/dotfiles
        rm -rf flake.lock
        nix flake update
        popd
      ''
    )
    (
      pkgs.writeScriptBin "nr" ''
        pushd ~/p/nixpkgs
        nixpkgs-review pr "$1"
        popd
      ''
    )
    (
      pkgs.writeScriptBin "pr" ''
        nixpkgs-review post-result
      ''
    )
  ];

  home.keyboard.options = [ "ctrl:nocaps" ];

  systemd.user.sessionVariables = config.home.sessionVariables;

  home.sessionVariables = {
    TERM = "xterm-256color";
    BROWSER = "google-chrome-stable";
    TMUX_TMPDIR = "$XDG_RUNTIME_DIR";

    LEDGER_FILE = "~ledger/mulligan.ledger";
    LEDGER_STRICT = "true";
    LEDGER_PEDANTIC = "true";
  };

  # Get systemd environment variables for ssh login shell too
  # home.sessionVariablesExtra = ''
  #   set -a
  #   eval $(/run/current-system/sw/lib/systemd/user-environment-generators/30-systemd-environment-d-generator)
  #   set +a
  # '';

  programs.alacritty = {
    enable = true;
    settings = {
      window.dynamic_title = false;
      font = {
        size = 16;
        normal = {
          family = "DejaVu Sans Mono";
        };
      };
      # The 'GNOME Light" theme from GNOME terminal.
      colors = {
        primary = {
          foreground = "#171421";
          background = "#ffffff";
        };
        normal = {
          black = "#171421";
          red = "#c01c28";
          green = "#26a269";
          yellow = "#a2734c";
          blue = "#12488b";
          magenta = "#a347ba";
          cyan = "#2aa1b3";
          white = "#d0cfcc";
        };
        bright = {
          black = "#5e5c64";
          red = "#f66151";
          green = "#33d17a";
          yellow = "#e9ad0c";
          blue = "#2a7bde";
          magenta = "#c061cb";
          cyan = "#33c7de";
          white = "#ffffff";
        };
      };
    };
  };

  programs.bash = {
    enable = true;
    historyControl = [ "ignoredups" ];
    historyIgnore = [ "ls" ];

    shellAliases = {
      ls = "ls --color=auto";
      grep = "grep --color=auto";
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
      set -g default-terminal "tmux-256color"
      set -g prefix C-z
      bind-key C-z send-prefix
      set -g renumber-windows on
    '';
  };

  xdg.configFile."fonts" = {
    source = ./config/fonts;
    recursive = true;
  };

  xdg.configFile."cmus/rc" = {
    text = ''
      set format_title=cmus: %t
      set altformat_title=cmus: %t
    '';
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
