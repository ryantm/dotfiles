{ pkgs, config, sources, ... }:

{

  nixpkgs.overlays = [
    (import sources.emacs-overlay)
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient -t";
  };

  programs.bash = {
    shellAliases.e = "emacsclient -t";
  };

  services.emacs.enable = true;
  #services.emacs.client.enable = true;

  programs.emacs.enable = true;
  programs.emacs.package = (pkgs.emacsPackagesFor pkgs.emacsGit).emacsWithPackages (epkgs:
    with epkgs; [
      bash-completion
      counsel
#      csv-mode
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
      lsp-ivy
      lsp-mode
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
    ]);

  xdg.configFile."emacs" = {
    source = ./.;
    recursive = true;
  };

}
