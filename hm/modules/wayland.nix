{configs, pkgs, ...}: let
  clr = import ../theme/solarized.nix;
in {
  home.packages = with pkgs; [
    xwayland
    pamixer
    brightnessctl
    light
    playerctl
    bemenu
    river
    swaybg
    waybar
    foot
    wl-clipboard
    # interception-tools
    imv
    wf-recorder
  ];

}
