{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    sqlite
    go
    ghc
    cabal2nix
    cabal-install
    emacs-all-the-icons-fonts
    nodejs # For coc-nvim
    haskellPackages.haskell-language-server
  ];

  services.emacs.enable = true;
  
  programs.emacs = {
    enable = true;
    extraPackages = (epkgs: [
      epkgs.vterm
      epkgs.nix-mode
      epkgs.magit
    ] );
  };
}
