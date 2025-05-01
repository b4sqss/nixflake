{ config, pkgs, inputs, ... }:
{
  # Informações básicas
  home.username = "tp";
  home.homeDirectory = "/home/tp";

  # Programas
  home.packages = with pkgs; [
    ## CLI
    fd fzf ripgrep htop btop gotop zinit pulsemixer pavucontrol pamixer i3lock playerctl libnotify nnn
    hunspell hunspellDicts.pt_BR hunspellDicts.en_US
    rsync cyrus-sasl-xoauth2 gsasl cyrus_sasl isync offlineimap
    texlive.combined.scheme-full
    pandoc
    zip unzip unrar-wrapper
    tealdeer
    ffmpeg-full
    tmux
    manix
    wlr-randr jq
    imagemagick yt-dlp

    ## Mail
    isync lieer notmuch

    ## GUI
    gimp okular evince masterpdfeditor brave pcmanfm dolphin lxappearance imv gtk4 libreoffice arkpandora_ttf qbittorrent
    vlc
    ladybird
    cameractrls guvcview
    obs-studio
    nextcloud-client

    freecad qcad
    calibre foliate
    chemtool marvin

    ## PROGRAMMING
    pyright python3Full python312Packages.pip python312Packages.pyngrok
    nodejs typescript-language-server typescript
    clang ccls gnumake libtool
    zls zig
    nil nixfmt-rfc-style
    arduino-ide
    R multimarkdown
    pinentry-emacs
    # pinentry-all
    ## DESKTOP UTILITIES
    bemenu redshift grim slurp seatd maim xclip networkmanagerapplet
    swaybg clight waybar swaynotificationcenter
    sxhkd sx picom polybar feh dunst rofi kanata dmenu pulseaudio polybar-pulseaudio-control brightnessctl
    appflowy affine
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
		  enableZshIntegration = true;
	  };

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
    font = {
      name = "Iosevka";
      size = 12;
    };
    iconTheme = {
      name = "Breeze";
      package = pkgs.breeze-icons;
      # package = pkgs.whitesur-icon-theme;
    };
    theme = {
      name = "Breeze-Dark";
      package = pkgs.breeze-gtk;
    };
    cursorTheme = {
      name = "Breeze";
    };
  };


  # Programas padrões
  xdg.mime.enable = true;
  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    "image/jpeg" = "imv.desktop";
    "image/png" = "imv.desktop";

    "application/pdf" = "org.kde.okular.desktop";
    "application/epub+zip" = "org.kde.okular.desktop";

    "text/html" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
    "x-scheme-handler/about" = "firefox.desktop";
    "x-scheme-handler/unknown" = "firefox.desktop";

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

    ".config/nvim".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/nvim";
    ".config/emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/emacs/init.el";
    ".config/emacs/etc/yasnippet".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/emacs/etc/yasnippet";

    ".config/bspwm".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/bspwm";
    ".config/sxhkd".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/sxhkd";
    ".config/polybar".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/polybar";
    ".config/dunst".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/dunst";
    ".config/sx".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixflake/notebook/config/sx";
  };

  home.stateVersion = "24.11";
  programs.home-manager.enable = true;

}
