{ config, pkgs, lib, inputs, ... }: {

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.dircolors = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  home.sessionVariables = {
    SHELL = "zsh";
    NIX_BUILD_SHELL = "zsh";
    LC_COLLATE = "C";                                                    
    NNN_FIFO = "/tmp/nnn.fifo";                                            
    NNN_FCOLORS = "$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER";
    BLK = "0B";
    CHR = "0B";
    DIR = "04";
    EXE = "02";
    REG = "00";
    HARDLINK = "06";
    SYMLINK = "06";
    MISSING = "00";
    ORPHAN = "09";
    FIFO = "06";
    SOCK = "0B";
    OTHER = "06";
    PF_INFO = "ascii title os wm uptime pkgs palette";
  };

  programs.zsh = {
    enable = true;
    enableCompletion = false;
    enableAutosuggestions = true;
    autocd = true;

    initExtra = ''
      autoload -U colors && colors	# Load colors
      PS1="%{$fg[blue]%}%~%  %{$fg[yellow]%}% | "
      # PROMPT="%{$fg[yellow]%}-> %{$fg[red]%}[ %{$fg[blue]%}%1~%{$fg[red]%} ]%}%{$fg[yellow]%} :: %{$reset_color%}"

      _comp_options+=(globdots)		# Include hidden files.

      bindkey -s '^o' 'fzf-cd\n'

      bindkey -s '^k' 'fzf-kill\n'

      bindkey -s '^a' 'bc -lq\n'

# # If not running interactively, do not do anything
#     [[ $- != *i* ]] && return
# #   Otherwise start tmux
#     [[ -z "$TMUX" ]] && exec tmux
    '';

    dotDir = ".config/zsh";

    # defaultKeymap = "vicmd";
    history.extended = true;
    history.ignoreDups = true;
    history.save = 10000000;
    history.size = 10000000;

    zplug = {
      enable = true;
      plugins = [
        { name = "zdharma-continuum/fast-syntax-highlighting"; }
        { name = "chisui/zsh-nix-shell"; }
      ];
    };

    profileExtra = ''
      export PATH=/home/basqs/.local/bin:$PATH

      if [ "$(tty)" = "/dev/tty1" ]; then
      sx
      fi

      [ -f $HOME/.nix-profile/etc/profile ] && source $HOME/.nix-profile/etc/profile
    '';

    shellAliases = {
      cd = "z";
      ga = "git add";
      gc = "git commit";
      gcm = "git commit -m";
      gs = "git status";
      gsb = "git status -sb";
      ".." = "cd ..";
      panik = "rm -rf ~/.furry-porn";
      l = "ls -lF";
      la = "l -a";
      ll = "ls -l";
      ls = "ls --group-directories-first --color";
      cp = "cp -iv";
      mv = "mv -iv";
      query_vim_pkgs = "nix-env -f '<nixpkgs>' -qaP -A vimPlugins";
      scu = "systemctl --user";
      sudo = "doas";
      start_nix_shell = "nix-shell --pure -E 'with import<nixpkgs> {}; callPackage ./. {}";
      ytdl = "youtube-dl";
      ytdla = "youtube-dl -x --audio-format mp3";
};
};
}
