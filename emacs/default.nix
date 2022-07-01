{
  pkgs,
  config,
  emacs-overlay,
  ...
}: {
  nixpkgs.overlays = [
    (import emacs-overlay)
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient -t";
  };

  programs.bash = {
    shellAliases.e = "emacsclient -t";
  };

  services.emacs.enable = true;
  #services.emacs.client.enable = true;

  home.packages = with pkgs; [
    nixpkgs-fmt
    rnix-lsp
    omnisharp-roslyn
  ];

  programs.emacs.enable = true;
  programs.emacs.package = (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages (
    epkgs:
      with epkgs; [
        bash-completion
        counsel
        csharp-mode
        #      csv-mode
        dhall-mode
        diminish
        elisp-slime-nav
        fill-column-indicator
        flycheck-haskell
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
        lsp-ui
        lxc
        magit
        markdown-preview-mode
        multiple-cursors
        nix-mode
        ormolu
        paredit
        powerline
        purescript-mode
        rainbow-delimiters
        rust-mode
        swiper
        use-package
        yaml-mode
        zeal-at-point
      ]
  );

  xdg.configFile."emacs" = {
    source = ./.;
    recursive = true;
  };
}
