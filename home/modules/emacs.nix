{ config, pkgs, ... }:

{
  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  #   }))
  # ];

  home.packages = with pkgs; [
    sqlite
    go
    gopls
    pythonFull
    ghc
    gnumake
    clang
    ccls
    nodePackages.coc-clangd
    cabal2nix
    cabal-install
    emacs-all-the-icons-fonts
    nodejs
    haskellPackages.haskell-language-server
    auctex
    rnix-lsp
  ];

  # home.sessionVariables = rec {
  #   EDITOR = ''emacsclient -nw -a \"\"'';
  #   GIT_EDITOR = EDITOR;
  #   VISUAL = ''emacsclient -cna \"\"'';
  # };

  programs.emacs = {
    enable = true;
    extraPackages = (epkgs: [
      epkgs.nix-mode
      epkgs.magit
      epkgs.org
      epkgs.org-bullets
      epkgs.org-journal
      epkgs.org-roam
      epkgs.org-plus-contrib
      epkgs.org-super-agenda        
      epkgs.ox-pandoc
      epkgs.emacsql
      epkgs.emacsql-sqlite
      epkgs.consult
      epkgs.vertico
      epkgs.treemacs
      epkgs.projectile
      epkgs.use-package     
    ] );
  };

   # home.file.".emacs.d/".source = ../configs/emacs;

  services.emacs.enable = true;
    
}
