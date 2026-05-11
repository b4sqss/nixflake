{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zen-browser.url = "github:youwen5/zen-browser-flake";
    xremap.url = "github:xremap/nix-flake";
    nix4nvchad = {
      url = "github:nix-community/nix4nvchad";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dedsec-grub-theme = {
      url = gitlab:VandalByte/dedsec-grub-theme;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, dedsec-grub-theme, ... } @ inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in {
      nixosConfigurations.tp = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs self; };
        modules = [
          home-manager.nixosModules.home-manager
            dedsec-grub-theme.nixosModule
          ./notebook/configuration.nix

          inputs.xremap.nixosModules.default {
            services.xremap.enable = true;
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

      homeConfigurations."tp" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = { inherit inputs self; };
        modules = [ ./notebook/home.nix ];
      };
    };
}
