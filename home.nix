{ config, pkgs, ... }:

{
   imports = [
     ./xmonad.nix
     ./alacritty.nix
     ./emacs.nix
     ./nvim.nix
     ./git.nix
     ./ncmpcpp.nix
   ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "basqs";
  home.homeDirectory = "/home/basqs";

    home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "emacs";
    TERMINAL = "alacritty";
    SHELL = "zsh";
  };

nixpkgs.config.allowUnfree = true;

    home.packages = with pkgs; [
      htop
      fortune
      mu
      isync
      tmux
      telnet
      jq

      pulsemixer
      scrot
      ueberzug
      nnn
      ffmpeg
      pandoc
      youtube-dl

      firefox
      brave
      tor
      vivaldi
      qutebrowser
      gopher

      discord
      spotify
      bitwarden
      mailspring

      sxiv
      mpv
      rofi

      zathura

      exa
      procs
      ripgrep
      fzf
      fd
      dig

      minecraft
      steam
      steamcmd
      steam-tui
      lutris
      dwarf-fortress
    ];

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
