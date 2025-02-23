# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, ... }:

{

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      inputs.home-manager.nixosModules.default
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.kernelModules = [ "amdgpu" ];

  # boot.initrd.luks.devices.cryptroot.device = "/dev/disk/by-uuid/2EEC612FEC60F30F";

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

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

services.thermald.enable = true;
powerManagement.enable = true;

services.auto-cpufreq.enable = true;
services.auto-cpufreq.settings = {
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

  services.cron = {
    enable = true;
    systemCronJobs = [
      "0 0 * * 0      root    fstrim /"
    ];
  };

#   virtualisation.docker = {
#     enable = true;
#     enableOnBoot = true;
#     autoPrune.enable = true;
#   };

# services.ollama = {
#   enable = true;
#     acceleration = "rocm";
# };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    ## desktopManager.plasma5.enable = true;
    displayManager.sx.enable = true;
  windowManager.bspwm.enable = true;
  videoDrivers = [ "amdgpu" ];
  };
  programs.dconf.enable = true;


  # Configure keymap in X11
   services.xserver.xkb = {
     layout = "br";
     variant = "thinkpad";
   };

  # Configure console keymap
  console.keyMap = "br-abnt2";

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.avahi = {
  enable = true;
  nssmdns4 = true;
  openFirewall = true;
};


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
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
  system.autoUpgrade.enable = true;
}
