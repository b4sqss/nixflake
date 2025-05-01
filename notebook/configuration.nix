# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, ... }:

{
  # Enable flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      inputs.home-manager.nixosModules.default
    ];

  # Bootloader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    initrd.kernelModules = [ "amdgpu" ];
    # initrd.luks.devices.cryptroot.device = "/dev/disk/by-uuid/2EEC612FEC60F30F";
  };

  # Wifi and stuff
  networking = {
    hostName = "nixos"; # Define your hostname.
    networkmanager.enable = true;
  };

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";
  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "pt_BR.UTF-8";
    LC_IDENTIFICATION = "pt_BR.UTF-8";
    LC_MEASUREMENT = "pt_BR.UTF-8";
    LC_MONETARY = "pt_BR.UTF-8";
    LC_NAME = "pt_BR.UTF-8";
    LC_NUMERIC = "pt_BR.UTF-8";
    LC_PAPER = "pt_BR.UTF-8";
    LC_TELEPHONE = "pt_BR.UTF-8";
    LC_TIME = "pt_BR.UTF-8";
  };
  # Configure console keymap
  console.keyMap = "br-abnt2";

  # Taking care of the battery
  powerManagement.enable = true;
  services = {
    thermald.enable = true;
    auto-cpufreq.enable = true;
    auto-cpufreq.settings = {
      battery = {
        governor = "powersave";
        turbo = "never";
        enable_thresholds = true;
        start_treshold = 60;
        stop_threshold = 75;
      };
      charger = {
        governor = "performance";
        turbo = "auto";
      };
    };

    # Taking care of my ssd
    cron = {
      enable = true;
      systemCronJobs = [
        "0 0 * * 0      root    fstrim /"
      ];
    };

    # Trying out self-hosting
    nextcloud = {
      enable = false;
      package = pkgs.nextcloud30;
      hostName = "localhost";
      config.adminpassFile = "/etc/nextcloud-admin-pass";
      config.dbtype = "sqlite";
    };

    # AI
    ollama = {
      enable = false;
      acceleration = "rocm";
    };

    # GUI and xserver
    xserver = {
      enable = true;
      ## desktopManager.plasma5.enable = true;
      displayManager.sx.enable = true;
      windowManager.bspwm.enable = true;
      videoDrivers = [ "amdgpu" ];
      xkb = {
        layout = "br";
        variant = "thinkpad";
      };
    };


    # Enable printing
    printing.enable = true;
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };

  #   virtualisation.docker = {
  #     enable = true;
  #     enableOnBoot = true;
  #     autoPrune.enable =

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;
  };
  security.sudo.enable = false;
  security.doas.enable = true;
  security.doas.extraRules = [{
    users = [ "basqs" ];
    keepEnv = true;
    persist = true;
  }];
  security.doas.extraConfig = ''
        permit nopass :wheel as root cmd reboot
        permit nopass :wheel as root cmd poweroff
        permit nopass :wheel as root cmd mount
        permit nopass :wheel as root cmd umount
        permit nopass :wheel as root cmd nixos-rebuild
        permit nopass :wheel as root cmd xremap
  '';

  programs.dconf.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;


  documentation.dev.enable = true;
  documentation.man.enable = true;
  documentation.man.generateCaches = true;
  documentation.nixos.enable = true;
  environment.extraOutputsToInstall = [ "info" "man" "devman" ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.tp = {
    shell = pkgs.zsh;
    isNormalUser = true;
    description = "bernardo";
    extraGroups = [ "networkmanager" "wheel" "docker" "seat" "uinput" "dialout"];
    packages = with pkgs; [
      thunderbird firefox neovim river bemenu alacritty git swaynotificationcenter
      home-manager cmake
    ];

  };
  environment.pathsToLink = ["/share/zsh"];
  environment.binsh = "${pkgs.dash}/bin/dash";

  nixpkgs.config.allowUnfree = true;

  home-manager = {
    extraSpecialArgs = {inherit inputs;};
    users = {
      "tp" = import ./home.nix;
    };
  };

  services.seatd = {
    enable = true;
  };

  programs.zsh = {
    enable = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget neovim firefox spotify
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  fonts = {
    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [ "Iosevka" ];
        serif = [ "IBM Plex Serif" ];
        sansSerif = [ "IBM Plex Sans" ];
      };
    };

    packages = with pkgs; [
      font-awesome
      iosevka
      ibm-plex
      emacs-all-the-icons-fonts
    ];
  };

  # Auto updates
  system.autoUpgrade.enable = true;
  system.autoUpgrade.dates = "weekly";

  # Auto cleanup
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 10d";
  };
  nix.settings.auto-optimise-store = true;

  system.stateVersion = "24.11";
}
