{configs, pkgs, ...}: let
  clr = import ../theme/solarized.nix;
in {

nixpkgs.overlays = [
  (self: super: {
    dwm = super.dwm.overrideAttrs (oldAttrs: rec {
      patches = [
        ../configs/dwm/dwm.diff
      ];
    });
  })
];

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
    sxhkd
    bsp-layout
    st
    dwm
    sxhkd
  ];

#   services.sxhkd = {
#     enable = true;
#     extraConfig = ''
# # screenshots
#       Print
#       scrot  -e 'mv $f ~/Pics/screenshots'
#
# # screencasts
#       super + alt + 5
#       dunstify -u CRIT "gravando" -t 800 && ffmpeg -f x11grab -s 1920x1080 -i :1 $HOME/Docs/videos/$(date +"%d_%m_%Y_%I_%M").mp4
#       super + alt + 6
#       pkill ffmpeg && dunstify -u LOW "screencast saved"
#
# # media keys
#       XF86AudioRaiseVolume
#       pamixer -i 5
#       XF86AudioLowerVolume
#       pamixer -d 5
#       XF86AudioMute
#       pamixer -t
#
#       XF86MonBrightnessUp
#       brightnessctl set +5%
#       XF86MonBrightnessDown
#       brightnessctl set 5%-
#
# # applications
#       super + a
#         emacsclient -cn
#       super + shift + a
#         emacs
#       super + f
#         emacsclient -c -a 'emacs' --eval '(dired nil)'
#       super + o
#         firefox
#       super + w
#         brave
#       super + shift + w
#         qutebrowser
#     '';
#   };
  # xdg.configFile."nvim/lua" = {
  #   source = ../configs/nvim;
  #   recursive = true;
  # };
}
