{ config, pkgs, ... }: {
  imports = [
    ./modules/desktop.nix
    ./modules/emacs.nix
    ./modules/git.nix
    ./modules/mail.nix
    ./modules/media.nix
    ./modules/nvim.nix
    ./modules/tmux.nix
    ./modules/zsh.nix
  ]; 

  # Let Home Manager install and manage itself.programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "basqs";
  home.homeDirectory = "/home/basqs";

  # home-manager.useGlobalPkgs = true;
  home.packages = with pkgs; [

    ## Tui
    gdu fzf newsboat 

    ## Cli
    exa fd duf youtube-dl pfetch rdfind imagemagick ffmpeg bc

    ## Documents
    zathura texlive.combined.scheme-full pandoc libreoffice-fresh

    ## Browsers
    firefox brave tor tor-browser-bundle-bin qutebrowser

    ## GUI
    rambox bitwarden pcmanfm signal-desktop element-desktop calibre gnome.pomodoro anki

    ## Open media
    sxiv 

    ## Graphics
    inkscape gimp gimpPlugins.resynthesizer krita blender freecad

    ## Games
    minecraft crawl dwarf-fortress curseofwar

    ## CC
    # clang bear gdb cmake llvmPackages.libcxx ccls rtags llvm gnumake

    ## Python
    python39 python39Packages.pip nodePackages.pyright

    ## Haskell
    ghc ghcid haskell-language-server

    ## Js
    nodejs yarn nodePackages.npm nodePackages.typescript nodePackages.typescript-language-server deno

    ## Rust
    cargo rustc rust-analyzer clippy

    ## Go
    go gopls

    ## Scheme
    scheme-manpages mitscheme
  ];
  
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";
}
