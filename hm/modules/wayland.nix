{configs, pkgs, ...}: let
  clr = import ../theme/solarized.nix;
in {

  home.packages = with pkgs; [
    xwayland
    pamixer
    brightnessctl
    scrot
    playerctl
  ];

}
