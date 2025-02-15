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
    zip unzip
    tealdeer
    ffmpeg-full
    tmux

    ## GUI
    gimp okular brave pcmanfm lxappearance imv gtk4 libreoffice arkpandora_ttf qbittorrent
    vlc

    freecad qcad

    ## PROGRAMMING
    pyright python3Full python312Packages.pip python312Packages.pyngrok
    nodejs typescript-language-server typescript
    clang ccls gnumake libtool
    zls zig
    nil
    arduino-ide
    R
    ## DESKTOP UTILITIES
    bemenu  clightd grim slurp seatd maim xclip networkmanagerapplet
    sxhkd sx picom polybar feh dunst rofi kanata dmenu pulseaudio polybar-pulseaudio-control brightnessctl

    inputs.zen-browser.packages."${system}".specific
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
      name = "WhiteSur-light";
      # package = pkgs.breeze-icons;
      package = pkgs.whitesur-icon-theme;
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
  xdg.configFile = {
    "alacritty/alacritty.toml".source = ./config/alacritty.toml;
    "tmux".source = ./config/tmux;

    "nvim".source = ./config/nvim;
    "emacs".source = ./config/emacs;

    "bspwm".source = ./config/bspwm;
    "sxhkd".source = ./config/sxhkd;
    "polybar".source = ./config/polybar;
    "dunst".source = ./config/dunst;
};

  home.file= {
    ".zshrc".source = ./config/zsh/.zshrc;
    ".zprofile".source = ./config/zsh/.zprofile;
    ".p10k.zsh".source = ./config/zsh/.p10k.zsh;
  };

  home.stateVersion = "24.11";
  programs.home-manager.enable = true;
}
