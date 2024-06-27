{
  pkgs,
  lib,
  config,
  alejandra,
  ...
}: {
  imports = [
    ./emacs
  ];

  programs.home-manager.enable = true;

  nixpkgs.config = import ./nixpkgs-config.nix;

  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;

  home.packages = with pkgs; [
    _1password-gui

    alejandra.defaultPackage.x86_64-linux
    #nixpkgs-update.packages.x86_64-linux.default
    #(import comma {inherit pkgs;})
    nix-tree
    beancount
    cmus
    evince
    freerdp
    gh
    git-delete-merged-branches
    gnupg
    go
    gopls
    haskellPackages.hpack
    hydra-check
    inkscape
    jq
    ledger
    lf
    meld
    nil
    niv
    nixfmt
    nixpkgs-review
    openvpn
    qbittorrent
    remmina
    cargo
    rustc
    rust-analyzer
    scrot
    st
    tmux
    usbutils
    virt-manager
    vlc
    w3m
    xterm
    yubikey-manager
    yubikey-personalization-gui
    zeal
    zoom-us
    (
      pkgs.writeScriptBin "hms" ''
        home-manager switch --flake /home/ryantm/p/dotfiles#ryantm
      ''
    )
    (
      pkgs.writeScriptBin "hmud" ''
        pushd ~/p/dotfiles
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

  home.keyboard.options = ["ctrl:nocaps"];

  systemd.user.sessionVariables = config.home.sessionVariables;

  home.sessionVariables = {
    TERM = "xterm-256color";
    BROWSER = "google-chrome-stable";
    TMUX_TMPDIR = "$XDG_RUNTIME_DIR";
  };

  # Get systemd environment variables for ssh login shell too
  # home.sessionVariablesExtra = ''
  #   set -a
  #   eval $(/run/current-system/sw/lib/systemd/user-environment-generators/30-systemd-environment-d-generator)
  #   set +a
  # '';

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  programs.alacritty = {
    enable = true;
    settings = {
      window = {
        dynamic_title = false;
      };
      font = {
        size = 20;
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
    historyControl = ["ignoredups"];
    historyIgnore = ["ls"];

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
    '';
  };

  programs.bat = {
    enable = true;
    config.theme = "GitHub";
  };

  programs.git = {
    enable = true;
    extraConfig = {
      core.fsmonitor = true;
      user.name = "Ryan Mulligan";
      user.email = "ryan@ryantm.com";
      user.useConfigOnly = true;
      color = {
        diff = "auto";
        status = "auto";
        branch = "auto";
      };
      push.default = "simple";
      github.user = "ryantm";
      merge.conflictstyle = "diff3";
      init.defaultBranch = "main";
    };
    ignores = ["result" "*.elc" ".#*" ".stack-work/" "#*" ".markdown-preview.html"];
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
      set -g bell-action none
      set -g visual-bell off
      set -g monitor-bell off
    '';
  };

  xdg.configFile."fonts" = {
    source = ./config/fonts;
    recursive = true;
  };

  xdg.configFile."run-or-raise/shortcuts.conf" = {
    text = ''
      # window classes with Alt+f2 run `lg` go to windows
      <Super>f,google-chrome,,
      <Super>d,alacritty --title alacritty-home -e tmux new -As 0,,alacritty-home
    '';
  };

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      wlrobs
      obs-livesplit-one
      obs-backgroundremoval
      obs-pipewire-audio-capture
    ];
  };

  # systemd.user.services.ssh-agent = {
  #   description = "SSH key agent";
  #   environment.SSH_AUTH_SOCK = "%t/ssh-agent";
  # };
}
