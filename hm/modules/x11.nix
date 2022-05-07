{configs, pkgs, ...}: let
  clr = import ../theme/solarized.nix;
in {

  home.packages = with pkgs; [
    xwallpaper
    xautolock i3lock-color
    pamixer
    brightnessctl
    dmenu
    betterlockscreen
    playerctl
    xsel
  ];

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ../configs/xmonad.hs;
    };
  };

  programs.xmobar = {
    enable = true;
    extraConfig = ''
Config { font = "xft:Iosevka:pixelsize=14:antialias=true:hinting=true,fontawesome:pixelsize=14"
       , bgColor = "#002b36"
       , fgColor = "#839496"
       , alpha = 255
       , position = Static {xpos = 0, ypos = 0, width = 1920, height = 20}
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = False
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [
                      Run Date " %B %d, %A   %H:%M" "date" 10

           , Run Battery [ "--template" , "<acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o", "  <left>% - <timeleft>"
                                       -- AC "on" status
                                       , "-O", " <left>%"
                                       -- charged status
                                       , "-i", " <left>%" ] 50
        , Run Com "pamixer" ["--get-volume-human"] "vol" 10
        , Run UnsafeStdinReader
         ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %UnsafeStdinReader% } { %battery%   %vol%  %date%"
                                          }
      '';  };

  xdg.configFile."sx/sxrc" = {
    executable = true;
    text = " exec xmonad ";
  };
}
