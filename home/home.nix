{ config, pkgs, ... }:

{
   imports = [
./modules/desktop.nix
./modules/dunst.nix
./modules/emacs.nix
./modules/git.nix
./modules/mail.nix
./modules/mpv.nix
./modules/ncmpcpp.nix
./modules/nvim.nix
./modules/tmux.nix
./modules/zsh.nix
./modules/zathura.nix
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
      qutebrowser

      discord
      spotify
      bitwarden

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
      gdu

      minecraft
      steam
      lutris
      legendary-gl
      dwarf-fortress
      wineWowPackages.staging
      protontricks
      winetricks

      jetbrains-mono
    ];

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
