{ config, pkgs, ... }: {
  imports = [
    ./modules/desktop.nix
    ./modules/editors.nix
    ./modules/git.nix
    ./modules/mail.nix
    ./modules/media.nix
    ./modules/x11.nix
    ./modules/zsh.nix
  ]; 

  home.packages = with pkgs; [
    ## terminal stuff
    youtube-dl
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
}
