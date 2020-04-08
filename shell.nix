let

  sources = import ./nix/sources.nix;
  pkgs = import sources."nixos-20.03" {};

in pkgs.mkShell rec {

  name = "dotfiles-shell";

  buildInputs = with pkgs; [
    nix
    niv
  ];

  shellHook = ''
    export NIX_PATH="nixpkgs=${sources."nixos-20.03"}"
    HM_PATH=$${HM_PATH:-https://github.com/rycee/home-manager/archive/master.tar.gz}
    DIR="$( cd "$( dirname "$${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

    function switch () {
      home-manager switch
    }

    function install () {
      mkdir -p $nixpkgsConfig
      ln -fs "$DIR/home.nix" "$nixpkgsConfig/home.nix"
      nix-shell $HM_PATH -A install
    }
  '';

}
