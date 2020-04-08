let

  sources = import ./nix/sources.nix;
  pkgs = import sources."nixos-20.03" {};

in pkgs.mkShell rec {

  name = "dotfiles-shell";

  buildInputs = with pkgs; [
    nix
    niv
    sources.home-manager
  ];

  shellHook = ''
    export NIX_PATH="nixpkgs=${sources."nixos-20.03"}"

    function switch () {
      home-manager switch
    }
  '';

}
