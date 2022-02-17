{config, pkgs, ...}: {

  home.packages = with pkgs; [
    thunderbird
  ];

  programs.neomutt = {
    enable = true;
    checkStatsInterval = 120;
    editor = "nvim";
    vimKeys = true;
  };

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "curses";
  };

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
  };

  programs.mbsync.enable = true;
  home.file.".mbsyncrc".source = ../configs/mbsyncrc;
  }
