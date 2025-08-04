{
  description = "ryantm's dotfiles";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.home-manager.url = "github:nix-community/home-manager";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  inputs.emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nixpkgs-update.url = "github:ryantm/nixpkgs-update";
  inputs.comma.url = "github:Shopify/comma";
  inputs.comma.flake = false;

  outputs =
    inputs@{
      self,
      flake-utils,
      nixpkgs,
      home-manager,
      comma,
      emacs-overlay,
      nixpkgs-update,
    }:
    let
      username = "ryantm";
    in
    {
      homeConfigurations.${username} = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        modules = [
          ./home.nix
          {
            home.username = username;
            home.homeDirectory = "/home/${username}";
            home.stateVersion = "24.05";
          }
        ];
        extraSpecialArgs = inputs;
      };
    };
}
