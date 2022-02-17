{ config, pkgs, ... }: {
  imports = [
    ./modules/desktop.nix
    ./modules/emacs.nix
    ./modules/git.nix
    ./modules/mail.nix
    ./modules/media.nix
    ./modules/nvim.nix
    ./modules/tmux.nix
    ./modules/x11.nix
    ./modules/zsh.nix
  ]; 

  home.packages = with pkgs; [
    ## Tui
    gdu

    ## Cli
    fd duf youtube-dl pfetch rdfind imagemagick ffmpeg bc

    ## Documents
    texlive.combined.scheme-full pandoc unoconv

    ## Browsers
    firefox brave tor tor-browser-bundle-bin qutebrowser

    ## GUI
    rambox bitwarden pcmanfm signal-desktop element-desktop calibre gnome.pomodoro anki

    ## Games
    minecraft crawl dwarf-fortress curseofwar
  ];
}
