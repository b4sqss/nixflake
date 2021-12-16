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

set -g status-fg black
set -g status-style bg=black

set -g status-position bottom
set -g status-justify left
set -g status-right-length  50
set -g status-left-length   30
set -g status-left '#{?client_prefix,#[bg=white]  ,#[bg=white]  }#[fg=white,bg=black] '
set -g status-right '#[fg=white,bg=black] #[fg=colour255,bg=black]  #[fg=colour255,bg=black] %d.%m.%y #[fg=white,bg=black] #[fg=colour255,bg=black]  #[fg=colour255,bg=black] %H:%M '
set -g status-bg default
set -g base-index 1

set -g message-style 'fg=colour255,bg=black'

# set active pane background
# set -g window-style 'fg=white,bg=black'
# set -g window-active-style 'fg=white,bg=black'

setw -g pane-base-index     1

setw -g window-status-format '#[fg=white,bg=black]謹#[fg=white,bg=black] #W '
setw -g window-status-current-format '#[fg=colour255,bg=black]謹#[fg=colour255 bold,bg=black] #W ' 

set -g mouse on

unbind C-b
set -g prefix C-x
# bind-key x kill-pane
# bind q kill-window

unbind 2
bind 2 split-window -v -c "#{pane_current_path}"
unbind 3
bind 3 split-window -h -c "#{pane_current_path}"

unbind ^F
bind ^F select-pane -t :.+
'';
  };
}
