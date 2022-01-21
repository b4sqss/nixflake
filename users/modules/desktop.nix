{ config, pkgs, ... }:
let
  clr = import ../theme/tomorrow-night.nix;
in {
  home.packages = with pkgs; [
    xwallpaper
    pamixer
    i3lock-color
    brightnessctl
    libnotify
    foliate
    gtk3
    qbittorrent
    tint2
    dmenu
    xsel
    rofi
    xautolock
    xcompmgr
    scrot 
  ];

  gtk = {
    font = {
      name = "JetBrains Mono Nerd Font";
      size = "10";
    };
    # theme = "everforest-gtk";
    theme = "tomorrow-night";
    iconTheme.name = "Vimix";
  };
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = "/bin/sh /home/basqs/.local/bin/lock.sh";
  };

  services.sxhkd = {
    enable = true;
    extraConfig = ''
# Take a screenshot
Print
  scrot  -e 'mv $f ~/Pictures/screenshots'

XF86AudioLowerVolume
  pamixer -d 5

XF86AudioMute
  pamixer -t

XF86AudioNext
  mpc next

XF86AudioPlay
  mpc toggle

XF86AudioPrev
  mpc prev

XF86AudioRaiseVolume
  pamixer -i 5

XF86MonBrightnessDown
  brightnessctl set 10%-

XF86MonBrightnessUp
  brightnessctl set +10%

super + control + {j,n}
  {emacsclient -c -a emacs -e '(org-roam-dailies-capture-today)',emacsclient -c -a emacs -e '(org-roam-node-find}

super + d
  emacsclient -c -a emacs -e "(dired \"$@\")"

super + o
  firefox -P normal

super + shift + Return
  emacsclient -c -a emacs -e "(vterm)"

super + a
  emacsclient -c

super + shift + a
  emacs

super + shift + o
  firefox -P contas

super + w
  brave

super + p
  dmenu_run -fn 'JetBrains Mono nerd font' -nb '#1d1f21' -nf '#969896' -sb '#f0c674' -sf '#1d1f21'

super + {s,n,m,z}
  {~/.local/bin/dmenu_websearch,~/.local/bin/dmenuumount,~/.local/bin/dmenumount,~/.local/bin/dmenuhandler}
'';
  };
  
  xdg.mime.enable = true;
  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    "application/pdf" = "zathura.desktop";

    "text/html" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
    "x-scheme-handler/about" = "firefox.desktop";
    "x-scheme-handler/unknown" = "firefox.desktop";

    "application/x-bittorrent" = "com.transmissionbt.Transmission.desktop";
    "x-scheme-handler/magnet" = "com.transmissionbt.Transmission.desktop";

    "application/vnd.apple.mpegurl" = "mpv.desktop";
    "application/x-mpegurl" = "mpv.desktop";
    "video/3gpp" = "mpv.desktop";
    "video/mp4" = "mpv.desktop";
    "video/mpeg" = "mpv.desktop";
    "video/ogg" = "mpv.desktop";
    "video/quicktime" = "mpv.desktop";
    "video/webm" = "mpv.desktop";
    "video/x-m4v" = "mpv.desktop";
    "video/ms-asf" = "mpv.desktop";
    "video/x-ms-wmv" = "mpv.desktop";
    "video/x-msvideo" = "mpv.desktop";
  };
  xresources.properties = {
    "*background" = clr.background;
    "*foreground" = clr.foreground;

    "*color0" =  clr.black;
    "*color8" =  clr.black-br;

    "*color1" =  clr.red;
    "*color9" =  clr.red-br;

    "*color2" =  clr.green;
    "*color10" = clr.green-br;

    "*color3" =  clr.yellow;
    "*color11" = clr.yellow-br;

    "*color4" =  clr.blue;
    "*color12" = clr.blue-br;

    "*color5" =  clr.magenta;
    "*color13" = clr.magenta-br;

    "*color6" =  clr.cyan;
    "*color14" = clr.cyan-br;
    
    "*color7" =  clr.white;
    "*color15" = clr.white-br;

    "*.font" = "JetBrains Mono Nerd Font";
  };

  home.sessionVariables = {
    TERMINAL = "alacritty";
    PAGER = "less";
  };

  # programs.urxvt = {
  #   enable = true;
  #   extraConfig = {
  #     urgentOnBell = true;
  #     lineSpace=  "0";
  #     geometry = "92x24";
  #     internalBorder = "10";
  #     cursorBlink = "true";
  #     cursorUnderline = "false";
  #     saveline = "2048";
  #     perl-ext-common = "default,clipboard,font-size,url-select,keyboard-select";
  #     urlLauncher = "firefox";
  #     underlineURLs = true;
  #   };
  #   fonts = ["xft:JetBrains Mono Nerd Font:size=9"];
  #   iso14755 = false;
  #   scroll.bar.enable = false;
  #   keybindings = {
  #     "M-c" = "perl:clipboard:copy";
  #     "M-v" = "perl:clipboard:paste";

  #     "M-u" = "perl:url-select:select_next";

  #     "M-Escape" = "perl:keyboard-select:activate";
  #     "M-s" = "perl:keyboard-select:search";

  #     "Control-Right" = "\\033[1;5C";
  #     "Control-Left" = "\\033[1;5D";
  #   };
  # };

  programs.alacritty = {
    enable = true;
    settings = {
      window.dimensions = {
        lines = 3;
        columns = 200;
      };
      font = {
        normal = {
          family = "JetBrains mono Nerd Font";
        style = "regular";
        };
        size = 8;
      };
      colors = {
        primary = {
          background = clr.background;
          foreground = clr.foreground;
        };
        normal = {
          black =  clr.black;
          red =  clr.red;
          green =  clr.green;
          yellow =  clr.yellow;
          blue  =  clr.blue;
          magenta  =  clr.magenta;
          cyan =  clr.cyan;
          white =  clr.white;
        };
        bright = {
          black =  clr.black-br;
          red =  clr.red-br;
          green = clr.green-br;
          yellow  = clr.yellow-br;
          blue = clr.blue-br;
          magenta = clr.magenta-br;
          cyan = clr.cyan-br;
          white = clr.white-br;
        };
      };
      key_bindings = [
        {
          key = "V";
          mods = "Alt";
          action = "Paste";
        }
        {
          key = "C";
          mods = "Alt";
          action = "Copy";
        }
      ];
    };
  };

  services.dunst = {
    enable = true;
    settings = {
      global = {
        monitor = 0;
        follow = "mouse";
        shrink = false;
        padding = 20;
        horizontal_padding = 20;

        width = 275;
        height = 100;
        offset = "10x50";
        origin = "bottom-right";

        frame_width = 0;
        separator_height = 0;
        frame_color = clr.background;
        separator_color = clr.background;

        sort = false;
        font = "JetBrains Mono Nerd Font 10";
        markup = "full";
        format = "<b>%s</b>\n%b";
        alignment = "left";
        show_age_threshold = 60;
        word_wrap = true;
        igfalsere_newline = false;
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = true;

        icon_position = "left";
        max_icon_size= 60;
        sticky_history = false;
        history_length = 6;
        title = "Dunst";
        class = "Dunst";
        corner_radius = 0;

        mouse_left_click = "close_current";
        mouse_middle_click = "do_action";
        mouse_right_click = "close_all";
      };
      urgency_low = {
        background = clr.green;
        foreground = clr.background;
        timeout = 5;
      };
      urgency_normal = {
        background = clr.background ;
        foreground = clr.foreground;
        timeout = 10;
      };
      urgency_critical = {
        background = clr.red;
        foreground = clr.background;
        timeout = 20;
      };
    };
  };
}
