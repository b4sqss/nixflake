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
  home.packages = with pkgs; [
    htop
    wget
    pulsemixer
    ripgrep
    zip unzip
    fd
    bc
  ];
  programs.tmux = {
    enable = true;
    clock24 = true;
    customPaneNavigationAndResize = true;
    keyMode = "vi";
    disableConfirmationPrompt = true;
    escapeTime = 0;
    # shell = "\${pkgs.zsh}/bin/zsh";
    terminal = "screen-256color";
    extraConfig = ''
      set-window-option -g automatic-rename on

      set -g status-style fg=white,bg=default
      set -g status-right "#{pane_title} %a, %d %b %H:%M"
      set -g status-position bottom
      set -g pane-active-border-style bg=default,fg=default
      set -g pane-border-style fg=default
      set -g window-status-current-format "#[fg=black]#[bg=blue] #I #[bg=brightblack]#[fg=white] #W #[fg=black]#[bg=default]"

      # set -g message-style 'fg=colour255,bg=black'

      setw -g pane-base-index     1

      set -g mouse on

      unbind C-b
      set -g prefix C-x
# bind-key x kill-pane
# bind q kill-window

      unbind 2
      bind 2 split-window -v -c "#{pane_current_path}"
      unbind 3
      bind 3 split-window -h -c "#{pane_current_path}"

bind -n M-0 select-window -t 0
bind -n M-1 select-window -t 1
bind -n M-2 select-window -t 2
bind -n M-3 select-window -t 3
bind -n M-4 select-window -t 4
bind -n M-5 select-window -t 5
bind -n M-6 select-window -t 6
bind -n M-7 select-window -t 7
bind -n M-8 select-window -t 8
bind -n M-9 select-window -t 9

      unbind ^F
      bind ^F select-pane -t :.+
    '';
  };
}
