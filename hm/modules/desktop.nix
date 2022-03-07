{ config, pkgs, ... }:
let
  clr = import ../theme/solarized.nix;
in {
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
tor
tor-browser-bundle-bin
qutebrowser

## GUI
anki
nicotine-plus
libreoffice
    ];

    gtk = {
      font = {
        name = "Iosevka";
        size = "10";
      };
      theme = {
        name = "numix-solarized-gtk-theme";
        package = pkgs.numix-solarized-gtk-theme;
      };
      iconTheme = {
##name "papirus";
package = pkgs.papirus-icon-theme;
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

        "URxvt.font" = "xft:Iosevka:size=12";
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
##	fonts = ["xft:Iosevka:Font:size=12"];
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
  extraConfig = ''
            import subprocess
# Autogenerated config.py
# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

# Uncomment this to still load settings configured via autoconfig.yml
# config.load_autoconfig()

# Always restore open sites when qutebrowser is reopened.
# Type: Bool
            c.auto_save.session = False

            config.load_autoconfig(False)

            config.bind("<Windows-1>", "tab-focus 1")
            config.bind("<Windows-2>", "tab-focus 2")
            config.bind("<Windows-3>", "tab-focus 3")
            config.bind("<Windows-4>", "tab-focus 4")
            config.bind("<Windows-5>", "tab-focus 5")
            config.bind("<Windows-6>", "tab-focus 6")
            config.bind("<Windows-7>", "tab-focus 7")
            config.bind("<Windows-8>", "tab-focus 8")
            config.bind("<Windows-9>", "tab-focus 9")
            config.bind("<Windows-0>", "tab-focus 10")
# Show javascript alerts.
# Type: Bool
            c.content.javascript.alert = True

# Enable JavaScript.
# Type: Bool
            config.set('content.javascript.enabled', True, 'file://*')

# Enable JavaScript.
# Type: Bool
            config.set('content.javascript.enabled', True, 'chrome://*/*')

# Enable JavaScript.
# Type: Bool
            config.set('content.javascript.enabled', True, 'qute://*/*')

# When to show favicons in the tab bar.
# Type: String
# Valid values:
#   - always: Always show favicons.
#   - never: Always hide favicons.
#   - pinned: Show favicons only on pinned tabs.
            c.tabs.favicons.show = 'never'

# Width (in pixels) of the progress indicator (0 to disable).
# Type: Int
            c.tabs.indicator.width = 0

# Search engines which can be used via the address bar. Maps a search
# engine name (such as `DEFAULT`, or `ddg`) to a URL with a `{}`
# placeholder. The placeholder will be replaced by the search term, use
# `{{` and `}}` for literal `{`/`}` signs. The search engine named
# `DEFAULT` is used when `url.auto_search` is turned on and something
# else than a URL was entered to be opened. Other search engines can be
# used by prepending the search engine name to the search term, e.g.
# `:open google qutebrowser`.
# Type: Dict
#c.url.searchengines = {'DEFAULT': 'https://new.startpage.com/do/dsearch?query={}'}

# Page(s) to open at the start.
# Type: List of FuzzyUrl, or FuzzyUrl
            c.url.start_pages = 'file:///home/basqs/.config/startpage/index.html'

# Default Iosevka fonts. Whenever "Iosevka" is used in a font
# setting, it's replaced with the fonts listed here.
# Type: Font
# c.fonts.Iosevka = 'Iosevka'

# Font used in the completion widget.
# Type: Font
            c.fonts.completion.entry = '9pt Iosevka'

# Font used in the completion categories.
# Type: Font
            c.fonts.completion.category = 'bold 9pt Iosevka'

# Font used for the debugging console.
# Type: QtFont
            c.fonts.debug_console = '9pt Iosevka'

# Font used for the downloadbar.
# Type: Font
            c.fonts.downloads = '9pt Iosevka'

# Font used for the hints.
# Type: Font
            c.fonts.hints = 'bold 9pt Iosevka'

# Font used in the keyhint widget.
# Type: Font
            c.fonts.keyhint = '9pt Iosevka'

# Font used for error messages.
# Type: Font
            c.fonts.messages.error = '9pt Iosevka'

# Font used for info messages.
# Type: Font
            c.fonts.messages.info = '9pt Iosevka'

# Font used for warning messages.
# Type: Font
            c.fonts.messages.warning = '9pt Iosevka'

# Font used for prompts.
# Type: Font
            c.fonts.prompts = '9pt Iosevka'

# Font used in the statusbar.
# Type: Font
            c.fonts.statusbar = '9pt Iosevka'

# Font used in the tab bar.
# Type: QtFont
# c.fonts.tabs = '9pt Iosevka'

# COLORS

            def read_xresources(prefix):
                props = {}
        x = subprocess.run(['xrdb', '-query'], stdout=subprocess.PIPE)
            lines = x.stdout.decode().split('\n')
            for line in filter(lambda l : l.startswith(prefix), lines):
                prop, _, value = line.partition(':\t')
                props[prop] = value
                return props

                xresources = read_xresources('*')
                    c.colors.completion.fg = xresources['*.foreground']
                    c.colors.completion.odd.bg = xresources['*.background']
                    c.colors.completion.even.bg = xresources['*.background']
                    c.colors.completion.category.bg = xresources['*.background']
                    c.colors.completion.category.border.top = xresources['*.background']
                    c.colors.completion.category.border.bottom = xresources['*.background']
                    c.colors.completion.item.selected.bg = xresources['*.foreground']
                    c.colors.completion.item.selected.border.bottom = xresources['*.foreground']
                    c.colors.completion.item.selected.border.top = xresources['*.foreground']
                    c.colors.completion.item.selected.fg = xresources['*.background']
                    c.colors.completion.match.fg = xresources['*.color9']
                    c.colors.completion.scrollbar.fg = xresources['*.background']
                    c.colors.completion.scrollbar.bg = xresources['*.background']
                    c.colors.downloads.bar.bg = xresources['*.background']
                    c.colors.downloads.start.bg = xresources['*.color4']
                    c.colors.downloads.stop.bg = xresources['*.color2']
                    c.colors.messages.error.fg = xresources['*.foreground']
                    c.colors.messages.error.bg = xresources['*.color1']
                    c.colors.messages.error.border = xresources['*.color1']
                    c.colors.messages.warning.bg = xresources['*.color3']
                    c.colors.messages.warning.border = xresources['*.color3']
                    c.colors.messages.info.bg = xresources['*.color8']
                    c.colors.messages.info.border = xresources['*.color8']
                    c.colors.prompts.bg = xresources['*.color8']
                    c.colors.prompts.fg = xresources['*.foreground']
                    c.colors.statusbar.normal.bg = xresources['*.background']
                    c.colors.statusbar.normal.fg = xresources['*.foreground']
                    c.colors.statusbar.insert.bg = xresources['*.background']
                    c.colors.statusbar.insert.fg = xresources['*.color10']
                    c.colors.statusbar.passthrough.fg = xresources['*.color12']
                    c.colors.statusbar.passthrough.bg = xresources['*.background']
                    c.colors.statusbar.command.fg = xresources['*.foreground']
                    c.colors.statusbar.command.bg = xresources['*.background']
                    c.colors.statusbar.url.fg = xresources['*.color8']
                    c.colors.statusbar.url.hover.fg = xresources['*.color7']
                    c.colors.statusbar.url.success.http.fg = xresources['*.color8']
                    c.colors.statusbar.url.success.https.fg = xresources['*.foreground']
                    c.colors.tabs.odd.fg = xresources['*.color8']
                    c.colors.tabs.odd.bg = xresources['*.background']
                    c.colors.tabs.even.fg = xresources['*.color8']
                    c.colors.tabs.even.bg = xresources['*.background']
                    c.colors.tabs.selected.odd.fg = xresources['*.foreground']
                    c.colors.tabs.selected.odd.bg = xresources['*.background']
                    c.colors.tabs.selected.even.fg = xresources['*.foreground']
                    c.colors.tabs.selected.even.bg = xresources['*.background']
                    c.colors.webpage.bg = "white"

# web fonts
                    c.fonts.web.family.serif = "Liberation Serif"
                    c.fonts.web.family.sans_serif = "Liberation Sans"
                    c.fonts.web.family.standard = "Liberation Sans"
                    c.fonts.web.family.fixed = "Fira Code"
# custom css

  '';
};

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
