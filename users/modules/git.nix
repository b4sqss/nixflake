{ config, pkgs, ... }:

{
  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    #lazygit
    enable = true;
    userName = "b4sqss";
    userEmail = "basqs@tutanota.com";
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
    aliases = {
      co = "checkout";
      ci = "commit";
      s = "status";
      st = "status";
      cl = "clone";
    };
    extraConfig = {
      core.editor = "emacs";
      protocol.keybase.allow = "always";
      pull.rebase = "false";
    };
  };

  home.packages = with pkgs; [
    git-crypt     # git files encryption
  ];

}
