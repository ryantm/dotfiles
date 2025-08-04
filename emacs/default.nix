{
  pkgs,
  emacs-overlay,
  ...
}:
let
  overrides = self: super: {
    elpaPackages = super.elpaPackages // {
      seq = self.callPackage (
        {
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
          packageRequires = [ ];
          meta = {
            homepage = "https://elpa.gnu.org/packages/seq.html";
            license = lib.licenses.free;
          };
          # tests take a _long_ time to byte-compile, skip them
          postInstall = ''rm -r $out/share/emacs/site-lisp/elpa/${pname}-${version}/tests'';
        }
      ) { };
    };
  };

in
{
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

  programs.emacs.enable = true;
  programs.emacs.package =
    ((pkgs.emacsPackagesFor pkgs.emacs30).overrideScope overrides).emacsWithPackages
      (
        epkgs: with epkgs; [
          ccls
          bash-completion
          corfu
          consult
          dhall-mode
          diminish
          elisp-slime-nav
          fill-column-indicator
          go-mode
          graphql-mode
          graphviz-dot-mode
          ledger-mode
          lsp-mode
          lsp-ui
          lsp-pyright
          lxc
          magit
          markdown-preview-mode
          multiple-cursors
          nix-mode
          nixfmt
          orderless
          paredit
          powerline
          prettier
          pytest
          rainbow-delimiters
          rust-mode
          seq
          swiper
          tide
          treesit-grammars.with-all-grammars
          typescript-mode
          use-package
          vertico
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
