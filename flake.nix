{
  description = "ryantm's dotfiles";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.home-manager.url = "github:nix-community/home-manager";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  inputs.nixpkgs-update.url = "/home/ryantm/p/nixpkgs-update";
  inputs.comma.url = "github:Shopify/comma";
  inputs.comma.flake = false;
  inputs.alejandra.url = "github:kamadorueda/alejandra";

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
      configuration = import ./home.nix;
      inherit username;
      system = "x86_64-linux";
      homeDirectory = "/home/${username}";
      stateVersion = "21.11";
      extraSpecialArgs = {inherit comma emacs-overlay nixpkgs-update alejandra;};
    };
  };
}
