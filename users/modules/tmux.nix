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

   # styling
   set -g status-bg default
   set -g status-fg white
   set -g status-style fg=white,bg=default
   set -g status-left ""
   set -g status-right ""
   set -g status-justify centre
   set -g status-position bottom
   set -g pane-active-border-style bg=default,fg=default
   set -g pane-border-style fg=default
   set -g window-status-current-format "#[fg=yellow] #[fg=black]#[bg=yellow]#I #[bg=brightblack]#[fg=white] #W#[fg=brightblack]#[bg=default] #[bg=default] #[fg=green]#[fg=black]#[bg=green]λ #[fg=white]#[bg=brightblack] %a %d %b #[fg=black]%R#[fg=brightblack]#[bg=default]"

   set -g window-status-format "#[fg=green] #[fg=black]#[bg=green]#I #[bg=brightblack]#[fg=white] #W#[fg=brightblack]#[bg=default] "

# set -g status-fg black
# set -g status-style bg=black

# set -g status-position bottom
# set -g status-justify left
# set -g status-right-length  50
# set -g status-left-length   30
# set -g status-left '#{?client_prefix,#[bg=white]  ,#[bg=white]  }#[fg=white,bg=black] '
# set -g status-right '#[fg=white,bg=black] #[fg=colour255,bg=black]  #[fg=colour255,bg=black] %d.%m.%y #[fg=white,bg=black] #[fg=colour255,bg=black]  #[fg=colour255,bg=black] %H:%M '
# set -g status-bg default
# set -g base-index 1

set -g message-style 'fg=colour255,bg=black'

# set active pane background
# set -g window-style 'fg=white,bg=black'
# set -g window-active-style 'fg=white,bg=black'

setw -g pane-base-index     1

# setw -g window-status-format '#[fg=white,bg=black]謹#[fg=white,bg=black] #W '
# setw -g window-status-current-format '#[fg=colour255,bg=black]謹#[fg=colour255 bold,bg=black] #W ' 

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

  # programs.tmux.extraConfig = ''
  #  # make sure fish works in tmux
  #  set -g  default-terminal   "xterm-256color"
  #  set -sa terminal-overrides ',xterm-256color:RGB'
  #  # so that escapes register immidiately in vim
  #  set -sg escape-time 1
  #  # mouse support
  #  set -g mouse on
  #  # change prefix to C-a
  #  set -g prefix C-a
  #  unbind C-b
  #  bind C-a send-prefix
  #  # extend scrollback
  #  set-option -g history-limit 5000
  #  # vim-like pane resizing
  #  bind -r C-k resize-pane -U
  #  bind -r C-j resize-pane -D
  #  bind -r C-h resize-pane -L
  #  bind -r C-l resize-pane -R
  #  # vim-like pane switching
  #  bind -r k select-pane -U
  #  bind -r j select-pane -D
  #  bind -r h select-pane -L
  #  bind -r l select-pane -R
  #  # and now unbind keys
  #  unbind Up
  #  unbind Down
  #  unbind Left
  #  unbind Right
  #  unbind C-Up
  #  unbind C-Down
  #  unbind C-Left
  #  # styling
  #  set -g status-bg default
  #  set -g status-fg white
  #  set -g status-style fg=white,bg=default
  #  set -g status-left ""
  #  set -g status-right ""
  #  set -g status-justify centre
  #  set -g status-position bottom
  #  set -g pane-active-border-style bg=default,fg=default
  #  set -g pane-border-style fg=default
  #  set -g window-status-current-format "#[fg=yellow]#[fg=black]#[bg=yellow]#I #[bg=brightblack]#[fg=white] #W#[fg=brightblack]#[bg=default] #[bg=default] #[fg=green]#[fg=black]#[bg=green]λ #[fg=white]#[bg=brightblack] %a %d %b #[fg=green]%R#[fg=brightblack]#[bg=default]"
  #  set -g window-status-format "#[fg=green]#[fg=black]#[bg=green]#I #[bg=brightblack]#[fg=white] #W#[fg=brightblack]#[bg=default] "
  # '';
}
