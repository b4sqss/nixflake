# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

  # Prompt
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Set the directory we want to store zinit and plugins
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"


# Download Zinit, if it's not there yet
if [ ! -d "$ZINIT_HOME" ]; then
   mkdir -p "$(dirname $ZINIT_HOME)"
   git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

# Source/Load zinit
source "${ZINIT_HOME}/zinit.zsh"

#  Prompt
zinit ice depth=1; zinit light romkatv/powerlevel10k

# Plugins
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-completions
zinit light Aloxaf/fzf-tab

typeset -U path cdpath fpath manpath
autoload -Uz compinit && compinit
# eval "$(/usr/libexec/path_helper)"

# use emacs keys
bindkey -e
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

HISTSIZE=100000
SAVEHIST=100000
HISTDUP=erase
HISTFILE="$HOME/.config/zsh/zsh_history"
mkdir -p "$(dirname "$HISTFILE")"
dotDir="$HOME/.config/zsh"
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
unsetopt HIST_EXPIRE_DUPS_FIRST
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu no #While using the fzf-tab
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'
zstyle ':fzf-tab:complete:zoxide:*' fzf-preview 'ls --color $realpath'

_comp_options+=(globdots)

# fzf
eval "$(fzf --zsh)"
setopt autocd

# functions
cm() {
    mkdir $@;
    cd $@
}

docx-pdf() {
soffice --convert-to pdf $@;
rm $@
}

nnn-cd () {
    # Block nesting of nnn in subshells
    [ "${NNNLVL:-0}" -eq 0 ] || {
        echo "nnn is already running"
        return
    }

    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    command nnn "$@"

    [ ! -f "$NNN_TMPFILE" ] || {
        . "$NNN_TMPFILE"
        rm -f -- "$NNN_TMPFILE" > /dev/null
    }
}

# aliases
alias dops="docker ps"
alias dlog="docker logs"
alias dlogf="docker logs -f"

bindkey -s '^o' 'cd $(find * -type d | fzf)\n'
bindkey -s '^k' 'fzf-kill\n'

alias ga="git add"
alias gc="git commit"
alias gcm="git commit -m"
alias gs="git status"
alias gsb="git status -sb"
alias ls="exa --group-directories-first"
alias l="ls -lF"
alias la="l -a"
alias ll="ls -l"
alias cp="cp -iv"
alias mv="mv -iv"
alias sxiv="sxiv -b"
alias ytdl="yt-dlp"
alias ytdla="yt-dlp -x --audio-format mp3"
alias fzf-yt="nix-shell -p jq ueberzug && ytfzf"
alias grep="grep --colour=auto"
alias dirdu="du -ah . | sort -hr | head -n 20"
alias vim="nvim"
alias icloud-download="ICLOUD_EMAIL=bernardoquintao@icloud.com icloud-download"
alias sudo="doas"

# exports
export EDITOR="nvim"
export MANPAGER="nvim +Man!"
export FZF_DEFAULT_COMMAND="rg --files"
export FZF_DEFAULT_OPTS="-m --border"
export PATH="~/.cargo/bin:$PATH"
export PATH="~/.local/bin:$PATH"
export LEDGER_FILE="/home/tp/Sync/financas/hledger.journal"

# nnn config
export BLK="0B"
export CHR="0B"
export DIR="04"
export EXE="02"
export REG="00"
export HARDLINK="06"
export SYMLINK="06"
export MISSING="00"
export ORPHAN="09"
export FIFO="06"
export SOCK="0B"
export OTHER="06"
export NNN_FIFO="/tmp/nnn.fifo"
export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"
export NNN_BMS='d:~/Documents;h:~/;D:~/Downloads/'
export NNN_PLUG='f:fzopen;t:nmount;v:imgview'
if [ -f /usr/share/nnn/quitcd/quitcd.bash_sh_zsh ]; then
    source /usr/share/nnn/quitcd/quitcd.bash_sh_zsh
fi

# vterm for emacs
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
## Old prompt
## PROMPT='%(?.%F{green}√.%F{red}?%?)%f %B%F{240}%1~%f%b %# '
# open tmux on start
# [ -z "$TMUX"  ] && { tmux attach || exec tmux new-session && exit;}

eval "$(zoxide init --cmd cd zsh)"
