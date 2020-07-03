{ pkgs, config, sources, ... }:

{

  nixpkgs.overlays = [ (import sources.emacs-overlay) ];

  programs.bash.sessionVariables.EDITOR = "emacs";

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

  home.file.".emacs.d" = {
    source = ./.;
    recursive = true;
    onChange = ''
      emacs --batch --eval '(byte-compile-file "~/.emacs.d/init.el")'
      emacs --batch --eval '(byte-compile-file "~/.emacs.d/custom-file.el")'
    '';
  };

}
