{ config, pkgs, inputs, ... }:
{
  # Informações básicas
  home.username = "tp";
  home.homeDirectory = "/home/tp";

  # Programas
  home.packages = with pkgs; [
    inputs.zen-browser.packages.${pkgs.system}.default
    ## CLI
    fd fzf ripgrep htop btop zinit pulsemixer pavucontrol playerctl libnotify nnn blueberry

    hunspell hunspellDicts.pt_BR hunspellDicts.en_US
    texlive.combined.scheme-full
    pandoc
    zip unzip unrar-wrapper
    tealdeer
    ffmpeg-full
    manix
    wlr-randr jq
    imagemagick yt-dlp
    mprocs dua just eza
    yazi hyperfine
    zellij tmux

    ## Mail
    isync lieer notmuch

    ## GUI
    gimp inkscape okular zathura latexrun evince masterpdfeditor brave pcmanfm dolphin kdePackages.qtsvg     libsForQt5.qtstyleplugin-kvantum
    libsForQt5.qt5ct lxappearance imv gtk4 libreoffice-qt arkpandora_ttf qbittorrent
    cameractrls guvcview
    obs-studio

    freecad qcad
    calibre foliate
    marvin
    dropbox-cli

    ## PROGRAMMING
    pyright python3Full python312Packages.pip python312Packages.pyngrok python312Packages.scrapy
    nodejs typescript-language-server typescript
    clang ccls gnumake libtool
    zls zig
    julia
    lua lua54Packages.luarocks tree-sitter
    nil nixfmt-rfc-style
    R multimarkdown

    ## DESKTOP UTILITIES
    grim slurp seatd maim wl-clipboard wlogout networkmanagerapplet
    swaybg clight brightnessctl waybar swaynotificationcenter
    sx rofi pulseaudio
  ];

  # Serviços
  services.syncthing = {
    enable = true;
  };

  # Programas com configurações
  programs = {
    emacs = {
      enable = true;
      extraPackages = epkgs: [
        epkgs.mu4e
      ];
    };

    neovim = {
      enable = true;
      withPython3 = true;

      extraPython3Packages = (ps: with ps; [
        pynvim
        unidecode
        black
        isort
      ]);
    };

    mpv = {
      enable = true;
      package = (
        pkgs.mpv-unwrapped.wrapper {
          mpv = pkgs.mpv-unwrapped.override {
            ffmpeg = pkgs.ffmpeg-full;
          };
        }
      );
    };

    zoxide = {
      enable = true;
      enableNushellIntegration = true;
      enableZshIntegration = true;
    };

    kitty = {
      enable = true;
      font = {
        name = "Iosevka Nerd Font";
        size = 15;
      };
      settings = {
        enable_audio_bell = false;
        confirm_os_window_close = 0;
      };

      shellIntegration.enableZshIntegration = true;
      themeFile = "Nord";
    };

    nushell = { enable = true;
                extraConfig = ''
                let carapace_completer = {|spans|
                    carapace $spans.0 nushell ...$spans | from json
                                         }
            $env.config = {
show_banner: false,
             completions: {
case_sensitive: false # case-sensitive completions
                    quick: true    # set to false to prevent auto-selecting completions
                    partial: true    # set to false to prevent partial filling of the prompt
                    algorithm: "fuzzy"    # prefix or fuzzy
                    external: {
# set to false to prevent nushell looking into $env.PATH to find more suggestions
enable: true
# set to lower can improve completion performance at the cost of omitting some options
            max_results: 100
            completer: $carapace_completer # check 'carapace_completer'
                    }
             }
                                         }
            $env.PATH = ($env.PATH |
                    split row (char esep) |
                    prepend /home/tp/.local/bin/ |
                    append /usr/bin/env
                          )
                '';
                shellAliases = {
                  vi = "nvim";
                  vim = "nvim";
                  nano = "nvim";
                };
              };
    carapace.enable = true;
    carapace.enableNushellIntegration = true;


    git = {
      package = pkgs.gitAndTools.gitFull;
      #lazygit
      enable = true;
      userName = "b4sqss";
      userEmail = "basqs@tutanota.com";
      ignores = [
        "**/.~*"
        "*.swp"
        "*.swo"
        ".nix-*"
        ".postgres"
        ".envrc"
        ".direnv"
        ".ccls-cache/"
      ];
      aliases = {
        co = "checkout";
        ci = "commit";
        s = "status";
        st = "status";
        cl = "clone";
      };
      extraConfig = {
        core.editor = "emacs";
        protocol.keybase.allow = "always";
        pull.rebase = "false";
      };
    };
  };


  # Aparência
  gtk = {
    enable = true;
    theme = {
      name = "Breeze-Dark";
      package = pkgs.libsForQt5.breeze-gtk;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders.override {
        flavor = "mocha";
        accent = "lavender";
      };
    };
    cursorTheme = {
      name = "Breeze";
    };
    gtk3 = {
      extraConfig.gtk-application-prefer-dark-theme = true;
    };
    };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style = {
      name = "gtk2";
      package = pkgs.libsForQt5.breeze-qt5;
    };
  };

  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal
        xdg-desktop-portal-gtk
      ];
      config.common.default = "*";
    };
  };

  # Programas padrões
  xdg.mime.enable = true;
  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    "image/jpeg" = "imv.desktop";
    "image/png" = "imv.desktop";

    "application/pdf" = "org.kde.okular.desktop";
    "application/epub+zip" = "com.github.johnfactotum.Foliate.desktop";

    "text/html" = "zen.desktop";
    "x-scheme-handler/http" = "zen.desktop";
    "x-scheme-handler/https" = "zen.desktop";
    "x-scheme-handler/about" = "zen.desktop";
    "x-scheme-handler/unknown" = "zen.desktop";

    "application/x-bittorrent" = "org.qbittorrent.qBittorrent.desktop";
    "x-scheme-handler/magnet" = "org.qbittorrent.qBittorrent.desktop";

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

  # Alternativa ao stow
  home.file = {
    ".zshrc".source = "${config.home.homeDirectory}/nixflake/notebook/config/zsh/.zshrc";
    ".zprofile".source = "${config.home.homeDirectory}/nixflake/notebook/config/zsh/.zprofile";
    ".p10k.zsh".source = "${config.home.homeDirectory}/nixflake/notebook/config/zsh/.p10k.zsh";

    ".config/alacritty/alacritty.toml".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/alacritty.toml";
    ".config/tmux".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/tmux";

    # ".config/nvim".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/nvim";
    ".config/emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/emacs/init.el";
    ".config/emacs/etc/yasnippet".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/emacs/etc/yasnippet";

    ".config/river".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/river";
    # ".config/waybar".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/waybar";
    ".config/swaync".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/swaync";
  };

  home.stateVersion = "24.11";
  programs.home-manager.enable = true;

}
