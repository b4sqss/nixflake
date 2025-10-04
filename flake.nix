{
  description = "My NixOs config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    winapps = {
      url = "github:winapps-org/winapps";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
      # Requires "nur.modules.nixos.default" to be added to the host modules
    };

    xremap = {
      url = "github:xremap/nix-flake";
    };

    zen-browser = {
      url = "github:youwen5/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
      {
        nixosConfigurations = {
          notebook = nixpkgs.lib.nixosSystem {
            specialArgs = {inherit inputs;};
            modules = [
              ./notebook/configuration.nix

           ##  inputs.home-manager.nixosModules.home-manager {
           ##    home-manager.useGlobalPkgs = true;
           ##    home-manager.useUserPackages = true;
           ##    home-manager.users.tp = {
           ##      imports = [ ./notebook/home.nix ];
           ##    };
           ##  }

              inputs.home-manager.nixosModules.home-manager {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
              }
              inputs.home-manager.nixosModules.default

              inputs.xremap.nixosModules.default {
                services.xremap.config.modmap = [
                  {
                    name = "barra";
                    remap = {"KEY_RIGHTCTRL" = "KEY_RO";};
                  }
                  {
                    name = "caps";
                    remap = {"CapsLock" = {alone = "Esc"; held = "CONTROL_L";};
                            };
                  }
                ];
              }
            ];
          };
        };

       ##  homeConfigurations = {
       ##      tp = inputs.home-manager.lib.homeManagerConfiguration {
       ##          username = "tp";
       ##          system = "x86_64-linux";
       ##          homeDirectory = "/home/tp";
       ##          specialArgs = {inherit inputs;};
       ##          # inherit pkgs;
       ##          modules = [ ./notebook/home.nix ];
       ##      };
       ##  };

      };
}
