{ config, pkgs, hostName, lib, ...}:

{
  home.packages = with pkgs; [
    xmobar
    rofi
    xwallpaper
    dunst
    eww
  ];

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = pkgs.writeText "xmonad.hs" ''
        ${builtins.readFile ../configs/xmonad/xmonad.hs}
      '';
    };
  };
}
