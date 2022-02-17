{configs, pkgs, ...}: {
  home.packages = with pkgs; [
    wofi
    waybar
  ];

  wayland.windowManager.sway = {
    enable = true;
  };

services.swayidle.enable = true;
}
