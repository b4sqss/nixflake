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
    sxhkd
    bsp-layout
  ];

  xsession.windowManager = {
    bspwm = {
      enable = true;
      extraConfig = ''
bspc rule -r "*"

sxhkd &
pkill polybar
polybar main &

# xwallpaper --zoom ~/Pics/wallpapers/iso.png &
# xwallpaper --zoom ~/Pics/wallpapers/by_upload1_2560.jpg &
xwallpaper --zoom ~/Pics/wallpapers/by_upload2_2560.jpg &
# xwallpaper --zoom ~/Pics/wallpapers/by_housevisit_2560.jpg &
# xwallpaper --zoom ~/Pics/wallpapers/634.jpg &
##~/.config/polybar/launch.sh &
dunst &
xrdb ~/.Xresources &
emacs --daemon &
xautolock -time 15 -locker 'sh ~/.local/bin/lock.sh' &
xsetroot -cursor_name left_ptr &
picom &

bspc monitor -d 1 2 3 4 5 6 7 8 9 10

bspc config border_width         	0
bspc config window_gap           	0

bspc config split_ratio          	0.55
bspc config borderless_monocle   	true
bspc config gapless_monocle      	true

# bspc config automatic_scheme longest_side
# bspc config automatic_scheme spiral
# bspc config initial_polarity first_child

bspc config removal_adjustment true
bsp-layout set tall -- --master-size 0.55

bspc config focus_follows_pointer true

# bspc config external_rules_command ~/.config/bspwm/scripts/autopresel

bspc rule -a \* node=.floating layer=above
bspc rule -a mpv state='floating'
bspc rule -a sxiv state='floating'
bspc rule -a zathura state='floating'
bspc rule -a Gimp desktop='^5' state=floating
'';
    };
  };
  services.polybar = {
    enable = true;
    script = "polybar main";
    package = pkgs.polybarFull;
    settings = {
      "bar/main" = {
        width = "100%";
        height = 25;
        enable-ipc = true;
        fixed-center = true;

        background = clr.background;
        foreground = clr.foreground;

        line-size = 1;

        padding-left = 1;
        padding-right = 1;

        module-margin-left = 2;

        font-0 = "Iosevka:pixelsize=12:weight=regular";
        font-1 = "Iosevka:pixelsize=12:weight=bold";
        font-2 = "Font Awesome:size=12";

        modules-left = "bspwm xwindow";
        modules-right = "pulseaudio network battery date";

        cursor-click = "pointer";

        wm-restack = "bspwm";
      };
    };
    extraConfig = ''
[module/xwindow]
type = internal/xwindow
label = %title%
label-maxlen = 75

[module/bspwm]
type = internal/bspwm

label-focused-font = 2
label-empty =

[module/date]
type = internal/date
interval = 5

time = %H:%M
time-alt =%a, %d %b %Y

label = %time%

[module/battery]
type = internal/battery
full-at = 99
format-charging = <animation-charging> <label-charging>
format-discharging = <ramp-capacity> <label-discharging>
label-charging = Charging %percentage%%
label-discharging = Discharging %percentage%%

label-full =  %percentage%%

ramp-capacity-0 = 
ramp-capacity-1 = 
ramp-capacity-2 = 
ramp-capacity-3 = 
ramp-capacity-4 = 

animation-charging-0 = 

animation-charging-framerate = 750
animation-discharging-framerate = 500

animation-discharging-0 = 
animation-discharging-1 = 
animation-discharging-2 = 
animation-discharging-3 = 
animation-discharging-4 = 
animation-charging-1 = 
animation-charging-2 = 
animation-charging-3 = 
animation-charging-4 = 

[module/network]
type = internal/network
interface = wlp61s0
format-connected = <label-connected>
label-connected = 

format-disconnected = <label-disconnected>
label-disconnected = 

[module/pulseaudio]
type = internal/pulseaudio
format-volume = <ramp-volume> <label-volume>

label-muted =  %percentage%%

ramp-volume-0 = 
ramp-volume-1 = 
'';
  };
  services.sxhkd = {
    enable = true;
    extraConfig = ''
# screenshots
Print
  scrot  -e 'mv $f ~/Pics/screenshots'

# screencasts
super + ctrl + 5
  dunstify -u CRIT "on air" -t 800 && ffmpeg -f x11grab -s 1920x1080 -i :1 $HOME/Docs/videos/$(date +"%d_%m_%Y_%I_%M").mp4
super + ctrl + 6
   pkill ffmpeg && dunstify -u LOW "screencast saved"

# media keys
XF86AudioRaiseVolume
  pamixer -i 5
XF86AudioLowerVolume
  pamixer -d 5
XF86AudioMute
  pamixer -t

XF86MonBrightnessUp
  brightnessctl set +5%
XF86MonBrightnessDown
  brightnessctl set 5%-

# applications
super + Return
  alacritty
super + p
  dmenu_run
super + a
        emacsclient -cn
super + shift + a
        emacs
super + f
        emacsclient -c -a 'emacs' --eval '(dired nil)'
super + o
  firefox
super + w
  brave
super + shift + w
  qutebrowser
super + m
  udiskie-dmenu -h 24

# make sxhkd reload its configuration files:
super + shift + r
  pkill -USR1 -x sxhkd && $HOME/.config/bspwm/bspwmrc


# BSPWM

# close
super + shift + c
  bspc node -k

# superernate between the tiled and monocle layout
super + shift + f
  bspc desktop -l next

# swap the current node and the biggest local node
super + Backspace
  bspc node -s biggest.local

#
# state/flags
#

# set the window state
super + {t,shift + t,s}
  bspc node -t {tiled,pseudo_tiled,floating}


# focus/swap

# focus the node in the given direction
super + {_,shift + }{h,j,k,l}
  bspc node -{f,s} {west,south,north,east}

# focus the node for the given path jump
super + {p,b,comma,period}
  bspc node -f @{parent,brother,first,second}

# focus or swap the next node
super + Tab
  bspc desktop -f last

# focus the older or newer node in the focus history

# focus or send to the given desktop
super + {_,shift + }{1-9,0}
  bspc {desktop -f,node -d} ^{1-9,10}

#
# preselect
#

# preselect the direction
super + {v,g}
  bspc node -p {south,east}

# preselect the ratio
super + ctrl + {1-9}
  bspc node -o 0.{1-9}

# cancel the preselection for the focused node
super + ctrl + space
  bspc node -p cancel

# cancel the preselection for the focused desktop
super + space
  bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel

#
# move/resize
#

# expand a window by moving one of its side outward
super + control + {h,j,k,l}
  bspc node -z {right -20 0,bottom 0 20,bottom 0 -20,right 20 0}

# contract a window by moving one of its side inward

# move a floating window
super + alt {h,j,k,l}
	bspc node -v {-20 0,0 20,0 -20,20 0}
'';
  };
  # xdg.configFile."nvim/lua" = {
  #   source = ../configs/nvim;
  #   recursive = true;
  # };
}
