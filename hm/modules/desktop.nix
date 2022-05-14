{ config, pkgs, ... }:
let
  clr = import ../theme/tomorrow-night.nix;
in {
  # needed for logseq
  nixpkgs.config.permittedInsecurePackages = [
    "electron-13.6.9"
  ];
  home.packages = with pkgs; [
    libnotify
    gtk3
    gtk-engine-murrine
    gtk_engines
    gsettings-desktop-schemas
    lxappearance

    ## Browsers
    firefox
    brave
    ungoogled-chromium
    tor
    nyxt
    tor-browser-bundle-bin

    ## GUI
    logseq
    anki
    libreoffice arkpandora_ttf #Arial font
  ];

  gtk = {
    enable = true;
    font = {
      name = "Iosevka";
      size = 10;
    };
    iconTheme = {
      name = "Adwaita-dark";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
  };
  xdg.mime.enable = true;
  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    "image/jpeg" = "sxiv.desktop";
    "image/png" = "sxiv.desktop";

    "application/pdf" = " org.pwmt.zathura.desktop";
    "application/epub+zip" = " org.pwmt.zathura.desktop";

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
    "*.background" = clr.background;
    "*.foreground" = clr.foreground;

    "*.color0" =  clr.black;
    "*.color8" =  clr.black-br;

    "*.color1" =  clr.red;
    "*.color9" =  clr.red-br;

    "*.color2" =  clr.green;
    "*.color10" = clr.green-br;

    "*.color3" =  clr.yellow;
    "*.color11" = clr.yellow-br;

    "*.color4" =  clr.blue;
    "*.color12" = clr.blue-br;

    "*.color5" =  clr.magenta;
    "*.color13" = clr.magenta-br;

    "*.color6" =  clr.cyan;
    "*.color14" = clr.cyan-br;

    "*.color7" =  clr.white;
    "*.color15" = clr.white-br;

    "URxvt.font" = "xft:Iosevka:size=10";
    "*.font" = "Iosevka";
  };

  home.sessionVariables = {
    TERMINAL = "alacritty";
    PAGER = "less";
  };

  ##programs.urxvt = {
  ##	enable = true;
  ##	extraConfig = {
  ##		urgentOnBell = true;
  ##		lineSpace=  "0";
  ##		geometry = "92x24";
  ##		internalBorder = "10";
  ##		cursorBlink = "true";
  ##		cursorUnderline = "false";
  ##		saveline = "2048";
  ##		perl-ext-common = "default,clipboard,font-size,url-select,keyboard-select";
  ##		urlLauncher = "firefox";
  ##		underlineURLs = true;
  ##	};
  ##	fonts = ["xft:Iosevka:Font:size=10"];
  ##	iso14755 = false;
  ##	scroll.bar.enable = false;
  ##	keybindings = {
  ##		"M-c" = "perl:clipboard:copy";
  ##		"M-v" = "perl:clipboard:paste";

  ##		"M-u" = "perl:url-select:select_next";

  ##		"M-Escape" = "perl:keyboard-select:activate";
  ##		"M-s" = "perl:keyboard-select:search";
  ##	};
  ##};

  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal = {
          family = "Iosevka";
          style = "regular";
        };
        size = 10;
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
        font = "Iosevka 10";
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
  programs.qutebrowser = {
    enable = true;
  };
  xdg.configFile."qutebrowser/config.py".source = ../configs/qutebrowser.py;

  services.picom = {
    enable = true;
    extraOptions = ''
            backend = "xrender";
        shadow = false;
        fading = false;
        mark-ovredir-focused = true;
        inactive-dim = 0.1;
        detect-client-opacity = true;
  '';
  };
}
