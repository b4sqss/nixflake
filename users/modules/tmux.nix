{
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
