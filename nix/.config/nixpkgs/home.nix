{ pkgs, ... } :

{
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/master.tar.gz;

  home.packages = with pkgs; [
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
    cabal-install
    cabal2nix
    haskellPackages.hpack
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

      NIX_PATH = "nixpkgs=$HOME/p/nixpkgs";

      LEDGER_FILE = "~ledger/mulligan.ledger";
      LEDGER_STRICT = "true";
      LEDGER_PEDANTIC = "true";

      POLOLU_DIR = "$HOME/p/pololu";
    };

    shellAliases = {
      ls = "ls --color=auto";
      grep = "grep --color=auto";
    };

    initExtra = ''
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
    '';

  };

  programs.emacs.enable = true;
  programs.emacs.extraPackages = epkgs: with epkgs; [
    magit
    color-theme-sanityinc-solarized
    multiple-cursors
    haskell-mode
    flycheck-haskell
    purescript-mode
    yaml-mode
    ledger-mode
    markdown-mode
    markdown-preview-mode
    paredit
    haml-mode
    zeal-at-point
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
      "*.elc"
      ".#*"
      ".stack-work/"
      "#*"
    ];
  };

  home.file = {
    ".emacs.d" = {
      source = ./emacs.d;
      recursive = true;
    };
  };
}
