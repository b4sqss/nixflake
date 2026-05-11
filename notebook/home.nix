{ config, pkgs, inputs, self, ... }:
{

  imports = [
    inputs.nix4nvchad.homeManagerModule
  ];

  # Informações básicas
  home.username = "tp";
  home.homeDirectory = "/home/tp";

  # Programas
  home.packages = with pkgs; [
    inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default

    freerdp dialog netcat-gnu iproute2 dive podman-tui podman-compose docker-compose
    apptainer

    ## CLI
    fd fzf ripgrep htop zinit pulsemixer pamixer pavucontrol playerctl libnotify nnn blueman gemini-cli

    hunspell hunspellDicts.pt_BR hunspellDicts.en_US hunspellDicts.de_DE
    texlive.combined.scheme-full
    pandoc
    zip unzip unrar-wrapper
    tealdeer
    ffmpeg-full
    manix
    wlr-randr jq
    imagemagick yt-dlp ghostscript
    mprocs dua just eza
    yazi hyperfine
    zellij tmux
    gdu
    gcr
    winetricks wineWow64Packages.waylandFull bottles
    playonlinux
    winboat
    bambu-studio
    ltspice

    localsend

    virt-manager
    paraview gmsh

    poppler-utils

    ## Mail
    isync lieer notmuch
    veusz gnuplot labplot

    ## GUI
    gnomeExtensions.tiling-shell
    thunderbird ladybird obsidian zotero
    vikunja
    gimp inkscape-with-extensions krita kdePackages.okular stirling-pdf zathura latexrun brave firefox pcmanfm kdePackages.dolphin imv gtk4 libreoffice-qt arkpandora_ttf qbittorrent
    cameractrls
    obs-studio
    lutris steam
    strawberry museeks amberol
    freecad
    calibre foliate
    marvin
    dropbox-cli
    hledger hledger-web hledger-utils hledger-iadd hledger-ui
    unciv

    ## PROGRAMMING
    spyder
    pyright rembg #python3Full python312Packages.pip python312Packages.pyngrok python312Packages.scrapy
    jupyter-all
    nodejs typescript-language-server typescript
    jdk21
    clang ccls gnumake libtool
    zls zig
    julia
    cargo
    #dwsim
    cmake
    lua lua54Packages.luarocks tree-sitter
    nil nixfmt
    R multimarkdown
    kdePackages.kdenlive

    bitwig-studio yabridge lmms ardour

    ## DESKTOP UTILITIES
    river-classic hyprland lightlocker hypridle hyprlock wlopm hyprpaper hyprsunset cliphist
    hyprpanel power-profiles-daemon grimblast wf-recorder hyprpicker btop
    quickshell
    grim slurp seatd maim wl-clipboard wlogout networkmanagerapplet
    mpv-unwrapped
    swaybg wlsunset brightnessctl waybar eww ristate swaynotificationcenter
    sx rofi pulseaudio wofi
    xmobar xclip

    vscode
  ];

  # Serviços
  services.syncthing = {
    enable = true;
  };

  services.gnome-keyring.enable = true;

  # Programas com configurações
  programs = {
    emacs = {
      enable = true;
      extraPackages = epkgs: [
        epkgs.mu4e
      ];
    };

    nvchad = {
      enable = true;
      extraPackages = with pkgs; [
        docker-compose-language-service
        dockerfile-language-server
        emmet-language-server
        rPackages.languageserver
        rust-analyzer
        nixd
        (python3.withPackages(ps: with ps; [
          python-lsp-server
          flake8
          jedi
        ]))
      ];
      hm-activation = false;
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
    package = pkgs.gitFull;
    #lazygit
    enable = true;
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
    settings = {
      user = {
        name = "b4sqss";
        email = "basqs@tutanota.com";
      };
      alias = {
        co = "checkout";
        ci = "commit";
        s = "status";
        st = "status";
        cl = "clone";
      };
      core.editor = "emacs";
      protocol.keybase.allow = "always";
      pull.rebase = "false";
      safe.directory = "/home/tp/nixflake";
    };
  };
};


# Aparência
gtk.gtk4.theme = config.gtk.theme;
gtk = {
  enable = true;
  theme = {
    # name = "Breeze-Dark";
    # package = pkgs.libsForQt5.breeze-gtk;
    name = "Qogir-Dark";
    package = pkgs.qogir-theme;
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
  platformTheme.name = "gtk";
  style = {
    name = "gtk2";
    package = pkgs.kdePackages.breeze;
  };
};

xdg = {
  portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
    ];
    config.common.default = "*";
  };
  autostart.enable = true; # Enable creation of XDG autostart entries.
};

# Programas padrões
xdg.mime.enable = true;
xdg.mimeApps.enable = true;
xdg.mimeApps.defaultApplications = {
  "image/jpeg" = "imv.desktop";
  "image/png" = "imv.desktop";

  "application/pdf" = "org.kde.okular.desktop";
  "application/epub+zip" = "com.github.johnfactotum.Foliate.desktop";

  "application/vnd.openxmlformats-officedocument.presentationml.presentation" = "libreoffice";
  "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "libreoffice";
  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
  " = "libreoffice";

  "x-scheme-handler/mailto" = "thunderbird.desktop";


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
  ".zshrc".source = "${self}/notebook/config/zsh/.zshrc";
  ".zprofile".source = "${self}/notebook/config/zsh/.zprofile";
  ".p10k.zsh".source = "${self}/notebook/config/zsh/.p10k.zsh";

  ".config/emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "${self}/notebook/config/emacs/init.el";
  ".config/emacs/etc/yasnippet".source = config.lib.file.mkOutOfStoreSymlink "${self}/notebook/config/emacs/etc/yasnippet";

  #   ".config/hypr/hypridle.conf".source = config.lib.file.mkOutOfStoreSymlink "${self}/notebook/config/hypr/hypridle.conf";
  #   ".config/hypr/hyprland.conf".source = config.lib.file.mkOutOfStoreSymlink "${self}/notebook/config/hypr/hyprland.conf";
  #   ".config/hypr/hyprpaper.conf".source = config.lib.file.mkOutOfStoreSymlink "${self}/notebook/config/hypr/hyprpaper.conf";
  #   ".config/hypr/hyprsunset.conf".source = config.lib.file.mkOutOfStoreSymlink "${self}/notebook/config/hypr/hyprsunset.conf";

  #   ".config/hyprpanel/config.json".source = config.lib.file.mkOutOfStoreSymlink "${self}/notebook/config/hyprpanel/config.json";
};

home.stateVersion = "24.11";
programs.home-manager.enable = true;

}
