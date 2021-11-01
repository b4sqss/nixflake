{ config, pkgs, ... }:

{
  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "b4sqss";
    userEmail = "basqs@tutanota.com";
    aliases = {
      co = "checkout";
      ci = "commit";
      s = "status";
      st = "status";
      cl = "clone";
    };
    extraConfig = {
      core.editor = "nvim";
      protocol.keybase.allow = "always";
      pull.rebase = "false";
    };
  };
}
