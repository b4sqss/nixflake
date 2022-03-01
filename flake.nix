{
  description = "My system config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.11";

    nur.url = "github:nix-community/NUR";

    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
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
          stateVersion = "21.11";
          configuration = {
            imports = [ 
              ({ config, pkgs, ... }: {
                nixpkgs.overlays = with inputs; [
                  emacs-overlay.overlay
                  nur.overlay
                  neovim-overlay.overlay
                ];
              })
              ./users/home.nix 
            ];
            nixpkgs = { inherit (pkgs) config overlays; };
          };
        };
      };

      nixosConfigurations = {
        nixos = lib.nixosSystem {
          inherit system;

          modules = [
            ./system/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                # users.basqs = {
                #   imports = [
                #     ./user/home.nix
                #   ];
                # };
              };
            }
            
            ({ config, pkgs, lib, ... }: {
              # Configure overlays
              nixpkgs = {
                overlays = with inputs; [
                  nur.overlay
                  neovim-overlay.overlay
                  emacs-overlay.overlay
                ];
              };

              # Power management
              services.tlp.enable = true; # keep my ports controlle
              services.thermald.enable = true; # keep my battery controlled
              powerManagement.enable = true;
              powerManagement.cpuFreqGovernor = lib.mkDefault "powersave"; # keep my cpu frequency controlled

              # Network settings.
              networking = {
                hostName = "nixbox"; # Hostname
                useDHCP = false; # Deprecated, so set explicitly to false
                wireless.enable = false;
                networkmanager.enable = true;
                networkmanager.wifi.powersave = true;
                firewall.enable = false; # I had issues, for some reason
              };
            })
          ];
        };
      };
    };
}
