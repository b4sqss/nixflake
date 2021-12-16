{ config, pkgs, ... }:
let
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

#boot.loader = {
#  efi = {
#    canTouchEfiVariables = true;
#    efiSysMountPoint = "/boot/efi"; # ← use the same mount point here.
#  };
#  grub = {
#     efiSupport = true;
#     #efiInstallAsRemovable = true; # in case canTouchEfiVariables doesn't work for your system
#     device = "nodev";
#  };
#};

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
    networkmanager.wifi.powersave = true;
    interfaces.enp60s0.useDHCP = true;
    interfaces.wlp61s0.useDHCP = true;
  };

  time.timeZone = "America/Sao_Paulo";
  i18n.defaultLocale = "en_US.UTF-8";

  powerManagement = {
    cpuFreqGovernor = "powersave";
    enable = true;
    powertop.enable = true;
  };

  boot.kernelModules = [ "kvm-intel" "vfio-pci" "nvidia" ];
  boot.kernelParams = [ "workqueue.power_efficient=y" "intel_iommu=on" "iommu=pt" ];

  services = {
    upower.enable = true;
    tlp.enable = false;
    tlp.extraConfig = ''
      TLP_DEFAULT_MODE=BAT
      SOUND_POWER_SAVE_ON_AC=1
      WIFI_PWR_ON_AC=on
      DEVICES_TO_DISABLE_ON_STARTUP="bluetooth"
      CPU_SCALING_GOVERNOR_ON_BAT=powersave
      CPU_SCALING_GOVERNOR_ON_AC=powersave
      CPU_MAX_PERF_ON_BAT=200
      CPU_SCALING_MAX_FREQ_ON_BAT=4400000
      CPU_BOOST_ON_BAT=1
      AHCI_RUNTIME_PM_ON_BAT=auto
    '';
    # undervolt = {
    # 	  enable = true;
    # 	  coreOffset = -40; 
    # 	};
usbmuxd.enable = true;


    transmission = {
      enable = true;
      settings = {
        download-dir = "/var/lib/transmission/Downloads";
        incomplete-dir = "/var/lib/transmission/.incomplete";
        incomplete-dir-enabled = true;
        message-level = 1;
        peer-port = 51413;
        peer-port-random-high = 65535;
        peer-port-random-low = 49152;
        peer-port-random-on-start = false;
        rpc-bind-address = "127.0.0.1";
        rpc-port = 9091;
        script-torrent-done-enabled = false;
        script-torrent-done-filename = "";
        umask = 2;
        utp-enabled = true;
        watch-dir = "/var/lib/transmission/watchdir";
        watch-dir-enabled = false;
      };
    };

    dbus = {
      enable = true;
      packages = [ pkgs.gnome3.dconf ];
    };

    xserver = {
      enable = true;
      displayManager.startx.enable = true;
      windowManager.xmonad.enable = true;
      windowManager.dwm.enable = true;

      videoDrivers = [ "nvidia" ];
      useGlamor = true;
      deviceSection = ''
    Option "DRI" "2"
    Option "TearFree" "true"
    '';
      layout = "br";
      xkbOptions = "caps:swapescape";

      libinput = {
        enable = true;
        touchpad.naturalScrolling = false;
      };
    };

  printing.enable = true;
  printing.drivers = with pkgs; [ epson-escpr ];

  cron = {
    enable = true;
    systemCronJobs = [
      "0 0 * * 0      root    fstrim /"
    ];
  };
  };

 sound.enable = true;

 virtualisation.libvirtd = {
   enable = true;
   onBoot = "ignore";
    onShutdown = "shutdown";
    qemuOvmf = true;
 };
 virtualisation.lxd.enable = true;
 virtualisation.docker.enable = true;
 networking.firewall.checkReversePath = false;

  hardware = {
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
    cpu.intel.updateMicrocode = true;
    nvidia.prime = {
      offload.enable = true;
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
    opengl = {
      extraPackages = [
        pkgs.libGL_driver
        pkgs.intel-compute-runtime
        pkgs.linuxPackages.nvidia_x11.out
      ];
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
             };
  };

  users.users.basqs = {
    shell = pkgs.zsh;
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "lp" "docker" "lxd" "libvirtd" ];
  };

  security.apparmor.enable = true;
  #   security.sudo.enable = false;
  #   security.doas.enable = true;
  #   security.doas.extraRules = [{
	# users = [ "basqs" ];
	# keepEnv = true;
  #       persist = true;
  #    }];

  documentation.dev.enable = true;
  environment.extraOutputsToInstall = [ "info" "man" "devman" ];

  nix.autoOptimiseStore = true;
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
 (st.overrideAttrs (oldAttrs: rec {
    src = fetchFromGitHub {
      owner = "LukeSmithxyz";
      repo = "st";
      rev = "8ab3d03681479263a11b05f7f1b53157f61e8c3b";
      sha256 = "1brwnyi1hr56840cdx0qw2y19hpr0haw4la9n0rqdn0r2chl8vag";
    };
    buildInputs = oldAttrs.buildInputs ++ [ harfbuzz ];
  }))

    neovim
    wget

    qemu
    virtmanager
    nixos-shell

    niv
    lorri

    dmenu
    rofi

    alacritty
    pciutils

    man
    man-pages
    posix_man_pages
    stdman

    mesa
    nvidia-offload
    glxinfo
    brightnessctl

    system-config-printer

    xorg.xf86videointel
    xorg.xf86inputevdev
    xorg.xf86inputsynaptics
    xorg.xf86inputlibinput

    libnotify

st

    acpi

    pfetch
    xorg.xinit
    libinput
libimobiledevice

    firefox
    w3m

    rxvt-unicode
    rsync
    nnn
    scrot
    git
  ];

nixpkgs.overlays = [
  (self: super: {
    dwm = super.dwm.overrideAttrs (oldAttrs: rec {
      patches = [
        ./dwm.diff
      ];
    });
    dmenu = super.dmenu.overrideAttrs (oldAttrs: rec {
      patches = [
        ./dmenu.diff
      ];
    });
  })
];

  fonts.fonts = with pkgs; [
    nerdfonts
    cozette
    iosevka
    roboto-mono
    jetbrains-mono
    font-awesome
  ];

  #boot.kernelPackages = pkgs.linuxPackages_latest;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
  #system.autoUpgrade = {
  #  enable = true;
  #  channel = https://nixos.org/channels/nixos-unstable;
  #};
}

