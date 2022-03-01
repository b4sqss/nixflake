{configs, pkgs, ...}: {

  home.packages = with pkgs; [
    wofi
    mako
    light
    brightnessctl
    waybar
    swayidle
    swaybg
    wl-clipboard
    # qtile
    river
    foot
    seatd
  ];

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true ;
  };

  # services.swayidle.enable = true;
}
