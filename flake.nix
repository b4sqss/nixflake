{
  description = "My system config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";

    nur.url = "github:nix-community/NUR";

    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, emacs-overlay, ... }@inputs:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true;};
      };

      lib = nixpkgs.lib;

    in {
      homeManagerConfigurations = {
        basqs = home-manager.lib.homeManagerConfiguration {
          inherit system pkgs;
          username = "basqs";
          homeDirectory = "/home/basqs";
          stateVersion = "22.05";
          configuration = {
            nixpkgs = { inherit (pkgs) config overlays; };
            imports = [
              ({ config, pkgs, ... }: {
                nixpkgs.overlays = with inputs; [
                  emacs-overlay.overlay
                  nur.overlay
                ];
              })
              ./hm/modules/desktop.nix
              ./hm/modules/editors.nix
              ./hm/modules/git.nix
              ./hm/modules/mail.nix
              ./hm/modules/media.nix
              ./hm/modules/x11.nix
              ./hm/modules/shell.nix
            ];
          };
        };
      };

      nixosConfigurations = {
        nixos = lib.nixosSystem {
          inherit system;
          specialArgs = inputs;
          modules = [
            ./system/configuration.nix
          ];
        };
      };
    };
}
