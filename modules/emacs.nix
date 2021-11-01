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
    auctex
  ];

  # home.file = {
  #   ".emacs.d" = {
  #     source = ./emacs.d;
  #     recursive = true;
  #   };
  # };

  services.emacs.enable = true;
    
  programs.emacs = {
    enable = true;
    extraPackages = (epkgs: [
      epkgs.vterm
      epkgs.nix-mode
      epkgs.magit
      epkgs.org
      epkgs.org-bullets
      epkgs.org-journal
      epkgs.org-roam
      epkgs.org-plus-contrib
      epkgs.org-super-agenda        
      epkgs.emacsql
      epkgs.emacsql-sqlite
      epkgs.treemacs
      epkgs.projectile
      epkgs.ox-pandoc
      epkgs.use-package     
    ] );
  };
}
