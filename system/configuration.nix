{ config, pkgs, ... }:
let
	nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
		export __NV_PRIME_RENDER_OFFLOAD=1
		export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
		export __GLX_VENDOR_LIBRARY_NAME=nvidia
		export __VK_LAYER_NV_optimus=NVIDIA_only
		exec -a "$0" "$@"
	'';
in {
	imports = [ ./hardware-configuration.nix ];

	nix = {
		extraOptions = "experimental-features = nix-command flakes";
		package = pkgs.nixFlakes;
		gc = {
			automatic = true;
			options = "--delete-older-than 14d";
		};
		autoOptimiseStore = true;
	};

	users.users.basqs = {
		shell = pkgs.zsh;
		isNormalUser = true;
		extraGroups = [ "wheel" "networkmanager" "libvirtd" ];
	};
	environment.pathsToLink = [ "/share/zsh" ]; # So that zsh completion works
	environment.binsh = "${pkgs.dash}/bin/dash";

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
	'';


	boot = {
		loader = {
			systemd-boot.enable = true;
			efi.canTouchEfiVariables = true;
		};
		kernelPackages = pkgs.linuxPackages_latest;
		kernelModules = [ "kvm-intel" "vfio-pci" "nvidia" ];
		kernelParams = [ "workqueue.power_efficient=y" "intel_iommu=on" "iommu=pt" ];
	};
	# hardware.enableRedistributableFirmware = true; # fsf :ha:

	time.timeZone = "America/Sao_Paulo";
	i18n.defaultLocale = "en_US.UTF-8";

	sound.enable = false;
	hardware.pulseaudio.enable = false;
	services.pipewire = {
		enable = true;
		alsa.enable = true;
		alsa.support32Bit = true;
		pulse.enable = true;
		jack.enable = true;
	};


	# Power management
	services.tlp= {
    enable = true;
    settings = {
      CPU_BOOST_ON_BAT = 0;
      CPU_BOOST_ON_AC = 1;
      DISK_IDLE_SECS_ON_AC= 0;
      DISK_IDLE_SECS_ON_BAT= 2;
      MAX_LOST_WORK_SECS_ON_AC= 15;
      MAX_LOST_WORK_SECS_ON_BAT= 60;
      SCHED_POWERSAVE_ON_AC= 0;
      SCHED_POWERSAVE_ON_BAT= 1;
      START_CHARGE_THRESH_BAT0 = 70;
      STOP_CHARGE_THRESH_BAT0 = 90;
      USB_AUTOSUSPEND = 1;
      USB_BLACKLIST_WWAN= 1;
      WOL_DISABLE = 1;
      USB_BLACKLIST_BTUSB = 0;
      USB_BLACKLIST_PHONE = 0;
      DEVICES_TO_DISABLE_ON_LAN_CONNECT = "wifi wwan";
      DEVICES_TO_DISABLE_ON_WIFI_CONNECT = "wwan";
      DEVICES_TO_DISABLE_ON_WWAN_CONNECT = "wifi";
      DEVICES_TO_ENABLE_ON_LAN_DISCONNECT = "wifi wwan";
      DEVICES_TO_ENABLE_ON_WIFI_DISCONNECT = "";
      DEVICES_TO_ENABLE_ON_WWAN_DISCONNECT = "";
    };
    };

	services.thermald.enable = true; # keep my battery controlled
	powerManagement.enable = true;
	powerManagement.cpuFreqGovernor = "powersave"; # keep my cpu frequency controlled
  services.undervolt = {
    enable = true;
    coreOffset = -75;
    gpuOffset = -50;
  };
  systemd.services.undervolt.wantedBy = [ "post-resume.target" "multi-user.target" ];
  systemd.services.undervolt.after = [ "post-resume.target" ];

	# Network settings.
	networking = {
		hostName = "nixos"; # Hostname
		useDHCP = false; # Deprecated, so set explicitly to false
		wireless.enable = false;
		networkmanager.enable = true;
		networkmanager.wifi.powersave = true;
		firewall.enable = false; # I had issues, for some reason
		hostId = "6319fcc9";
	};

	services = {
		upower.enable = true;
		acpid.enable = true;
	};

	services.cron = {
		enable = true;
		systemCronJobs = [
			"0 0 * * 0      root    fstrim /"
		];
	};

	services.xserver = {
		enable = true;
		displayManager.sx.enable = true;
		windowManager.bspwm = {
			enable = true;
		};
		layout = "br";
		xkbOptions = "caps:swapescape";
		libinput = {
			enable = true;
			touchpad.naturalScrolling = false;
		};
	};
	programs.dconf.enable = true;
	# environment.pathsToLink = [ "/libexec" ];

	services.xserver = {
		videoDrivers = [ "nvidia" ];
		useGlamor = true;
		deviceSection = ''
						Option "DRI" "2"
						Option "TearFree" "true"
	'';
	};
	hardware = {
		cpu.intel.updateMicrocode = true;
		nvidia.modesetting.enable = true;
		nvidia.prime = {
			offload.enable = true;
			intelBusId = "PCI:0:2:0";
			nvidiaBusId = "PCI:1:0:0";
		};
		opengl = {
			enable = true;
			driSupport = true;
			driSupport32Bit = true;
			extraPackages = [
				pkgs.mesa.drivers
				pkgs.intel-compute-runtime
			];
		};
	};

	virtualisation.libvirtd.enable = true;

	documentation.dev.enable = true;
	documentation.man.enable = true;
	documentation.man.generateCaches = true;
	documentation.nixos.enable = true;
	environment.extraOutputsToInstall = [ "info" "man" "devman" ];

	nixpkgs.config.allowUnfree = true;
	environment.systemPackages = with pkgs; [
		git

		mesa
		nvidia-offload

		qemu_full
		virt-manager
		libvirt
	];

	fonts = {
		fontconfig = {
			enable = true;
			defaultFonts = {
				monospace = [ "Iosevka" ];
				serif = [ "IBM Plex Serif" ];
				sansSerif = [ "IBM Plex Sans" ];
			};
		};

		fonts = with pkgs; [
			font-awesome
			iosevka
      ibm-plex
      emacs-all-the-icons-fonts
    ];
	};

	system.stateVersion = "22.05";
	system.autoUpgrade.enable = true;
}
