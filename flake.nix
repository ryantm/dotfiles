{
  description = "ryantm's dotfiles";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.home-manager.url = "github:nix-community/home-manager";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  inputs.emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nixpkgs-update.url = "/home/ryantm/p/nixpkgs-update";
  inputs.comma.url = "github:Shopify/comma";
  inputs.comma.flake = false;
  inputs.alejandra.url = "github:kamadorueda/alejandra";
  inputs.alejandra.inputs.nixpkgs.follows = "nixpkgs";

  outputs = {
    self,
    flake-utils,
    nixpkgs,
    home-manager,
    comma,
    emacs-overlay,
    nixpkgs-update,
    alejandra,
  }: let
    username = "ryantm";
  in {
    homeConfigurations.${username} = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        ./home.nix
        {
          home.username = username;
          home.homeDirectory = "/home/${username}";
          home.stateVersion = "22.05";
        }
      ];
      extraSpecialArgs = {inherit comma emacs-overlay nixpkgs-update alejandra nixpkgs;};
    };
  };
}
