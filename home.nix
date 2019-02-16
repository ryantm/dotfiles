{ pkgs, config, ... } :

{
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/master.tar.gz;

  nixpkgs.config = import ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;

  home.packages = with pkgs; [
    cabal-install
    cabal2nix
    calibre
    evince
    firefox
    gimp
    google-chrome
    haskellPackages.hpack
#    haskellPackages.intero
    haskellPackages.ghcid
    inkscape
    ledger
    libreoffice
    openvpn
    python
    qbittorrent
    ranger
    w3m
    remmina
    rxvt_unicode
    st
    scrot
    steam
    minecraft
    tmux
    thunderbird
    usbutils
    vlc
    yubikey-personalization-gui
    yubikey-manager
    zeal
    zsnes
  ];

  home.keyboard.options = [ "ctrl:nocaps" ];

  programs.bash = {
    enable = true;
    historyControl = ["ignoredups"];
    historyIgnore = [ "ls" ];

    sessionVariables = {
      TERM = "screen-256color";
      EDITOR = "emacs";
      BROWSER="google-chrome-stable";
      TMUX_TMPDIR="$XDG_RUNTIME_DIR";

      NIX_PATH = "nixpkgs=$HOME/p/nixpkgs";

      LEDGER_FILE = "~ledger/mulligan.ledger";
      LEDGER_STRICT = "true";
      LEDGER_PEDANTIC = "true";

      POLOLU_DIR = "$HOME/p/pololu";
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
      function git_bot {
        git_config_user "R. RyanTM" "ryantm+bot@ryantm.com"
        git_check_user_config
        git config core.sshCommand = "ssh -i ~/.ssh/r-ryantm"
      }

      # Track assembly
      export TRACK_ASSEMBLY_DIRECTORY=$POLOLU_DIR/track_assembly
      function track_assembly {
        pushd .
        cd $POLOLU_DIR/system2_for_track/website
        nix-shell --run "pololu-rails-env-preview rails runner script/track/track_assembly.rb"
        popd
      }

      # Track users permissions
      export TRACK_USERS_PERMISSIONS_DIRECTORY=$POLOLU_DIR/track_users_permissions
      function track_users_permissions {
        pushd .
        cd $POLOLU_DIR/system2_for_track/website
        nix-shell --run "pololu-rails-env-preview rails runner script/track/track_users_permissions.rb"
        popd
      }
    '';

  };

  programs.emacs.enable = true;
  programs.emacs.extraPackages = epkgs: with epkgs; [
    bash-completion
    color-theme-sanityinc-solarized
    csv-mode
    diminish
    dhall-mode
    elisp-slime-nav
    fill-column-indicator
    flycheck-haskell
    graphql-mode
    haml-mode
    helm
    hi2
    hindent
    inf-ruby
    ledger-mode
    lxc
    magit
    magit-annex
    markdown-preview-mode
    multiple-cursors
    nix-mode
    paredit
    powerline
    purescript-mode
    rainbow-delimiters
    use-package
    yaml-mode
    zeal-at-point
    intero
  ];

  programs.git = {
    userEmail = "";
    userName = "";
    enable = true;
    aliases = {
      review = "!sh -c 'git fetch -a && gitk origin/review/$1 --not origin/$2' -";
      unmerged = "!git branch -a --no-merged | grep remotes/";
      sync = "!git fetch --all --prune && git fetch --tags";
      delbranch = "!sh -c 'git branch -d dev/ryantm/$1 && git push origin :dev/ryantm/$1' -";
    };
    extraConfig = {
      user.useConfigOnly = true;
      color = {
        diff = "auto";
        status = "auto";
        branch = "auto";
      };
      push.default = "simple";
    };
    ignores = [
      "result"
      "*.elc"
      ".#*"
      ".stack-work/"
      "#*"
      ".markdown-preview.html"
    ];
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
    ".urxvt/ext/font-size".source = ./urxvt/font-size;
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

}
