{configs, pkgs, ...}: let
  clr = import ../theme/solarized.nix;
in {

  home.packages = with pkgs; [
    xwallpaper
    pamixer
    i3lock-color
    brightnessctl
    dmenu
    xautolock
    scrot
    playerctl
    xsel
    i3
    sxhkd
    polybarFull
  ];

  xsession.windowManager.i3 = {
    enable = true;
    package = pkgs.i3;
    #package = pkgs.i3-gaps;
  };
  # xdg.configFile."nvim/lua" = {
  #   source = ../configs/nvim;
  #   recursive = true;
  # };
}
