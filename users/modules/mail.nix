{config, pkgs, ...}: {
  programs.password-store.enable = true;

  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

programs.mbsync.enable = true;
home.file.".mbsyncrc".source = ../configs/mbsyncrc;
  
  # services.transmission = {
  #     enable = true;
  #     settings = {
  #       download-dir = "/home/basqs/Documents/torrents";
  #       incomplete-dir = "/home/basqs/Documents/torrents/.incomplete";
  #       incomplete-dir-enabled = true;
  #       message-level = 1;
  #       peer-port = 51413;
  #       peer-port-random-high = 65535;
  #       peer-port-random-low = 49152;
  #       peer-port-random-on-start = false;
  #       rpc-bind-address = "127.0.0.1";
  #       rpc-port = 9091;
  #       script-torrent-done-enabled = false;
  #       umask = 2;
  #       utp-enabled = true;
  #       watch-dir-enabled = false;
  #     };
  #   };

# services.mbsync = {
#     enable = true;
#     frequency = "10m";
#   };
# home.packages = with pkgs; [ mu ];

}
