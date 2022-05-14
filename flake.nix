{
  description = "My system config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.11";
    # nixpkgs.url = "github:nixos/nixpkgs/release-21.11";

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
          nixpkgs = { inherit (pkgs) config overlays; };
          home.packages = with pkgs; [
    ## terminal stuff
    yt-dlp
    pfetch
    rsync
    nnn

    ## Games
    minecraft
    dwarf-fortress
    steam
    lutris
    protontricks
  ];
  imports = [
    ({ config, pkgs, ... }: {
      nixpkgs.overlays = with inputs; [
        emacs-overlay.overlay
        nur.overlay
        neovim-overlay.overlay
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
