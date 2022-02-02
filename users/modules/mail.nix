{config, pkgs, ...}: {

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
