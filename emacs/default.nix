{
  pkgs,
  emacs-overlay,
  ...
}: let
  overrides = self: super: {
    elpaPackages =
      super.elpaPackages
      // {
        seq = self.callPackage ({
          elpaBuild,
          fetchurl,
          lib,
        }:
        elpaBuild rec {
          pname = "seq";
          ename = "seq";
          version = "2.24";
          src = fetchurl {
            url = "https://elpa.gnu.org/packages/seq-2.24.tar";
            sha256 = "1w2cysad3qwnzdabhq9xipbslsjm528fcxkwnslhlkh8v07karml";
          };
          packageRequires = [];
          meta = {
            homepage = "https://elpa.gnu.org/packages/seq.html";
            license = lib.licenses.free;
          };
          # tests take a _long_ time to byte-compile, skip them
          postInstall = ''rm -r $out/share/emacs/site-lisp/elpa/${pname}-${version}/tests'';
        }) {};
      };
  };

in {
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
    omnisharp-roslyn
    nodejs
    nodePackages.prettier
  ];

  programs.emacs.enable = true;
  programs.emacs.package = ((pkgs.emacsPackagesFor pkgs.emacs-nox).overrideScope overrides).emacsWithPackages (
    epkgs:
      with epkgs; [
        bash-completion
        company
        company-go
        counsel
        csharp-mode
        #      csv-mode
        dhall-mode
        diminish
        elisp-slime-nav
        fill-column-indicator
        flycheck-haskell
        go-mode
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
        prettier
        purescript-mode
        rainbow-delimiters
        rust-mode
        seq
        swiper
        typescript-mode
        unisonlang-mode
        use-package
        yasnippet
        yaml-mode
        zeal-at-point
      ]
  );

  xdg.configFile."emacs" = {
    source = ./.;
    recursive = true;
  };
}
