{configs, pkgs, ...}: {

  containers.nextcloud = {
    autoStart = true;
    privateNetwork = true;
    localAddress = "10.50.0.2";
    hostAddress = "10.50.0.1";
    config = { pkgs, config, lib, ... }: with lib; {
      services.sshd.enable = true;
      networking.firewall.enable = false;

      services.nextcloud = {
        enable = true;
        home = "/var/lib/nextcloud";
        hostName = "cloud.narratron.io";
        https = true;
        maxUploadSize = "2G";
        config = {
          dbtype = "pgsql";
          dbhost = "pgsql001.local";
          dbuser = "basqsCloud";
          dbname = "nextcloud";
          dbpassFile = "/etc/nixos/secrets/nextcloud_dbpass";
          adminuser = "admin";
          adminpassFile = "/etc/nixos/secrets/nextcloud_adminpass";
        };
      };
    };
  };
}
