{ pkgs, ... }:
let
  emacs-open = pkgs.writeShellScriptBin "emacs-open" ''
if [ $# -eq 0 ]; then
emacsclient -c -n
exit
fi

emacsclient -e "(frames-on-display-list \"$DISPLAY\")" &>/dev/null

if [ $? -eq 0 ]; then
emacsclient -n "$*"
else
emacsclient -c -n "$*"
fi
'';
in {

  home.sessionVariables = {
    EDITOR = "vim";
    VISUAL = "emacs-open";
  };

  home.packages = with pkgs; [
    emacs-open
    sqlite
    emacs-all-the-icons-fonts
    auctex
    rnix-lsp
    languagetool
    hunspell aspell aspellDicts.pt_BR
    coreutils
    ## Documents
    texlive.combined.scheme-full
    luarocks # for digestify
    pandoc
    unoconv
    languagetool
    gnumeric
    sbcl
    ## C
    clang bear gdb cmake llvmPackages.libcxx ccls rtags llvm gnumake

    ## Python
    python39 python39Packages.pip nodePackages.pyright python39Packages.matplotlib

    ## Haskell
    ghc ghcid
    # haskell-language-server

    ## Js
    nodejs yarn nodePackages.npm nodePackages.typescript nodePackages.typescript-language-server deno

    ## Go
    go gopls

    ## Scheme
    # scheme-manpages mitscheme

    ## Zig
    zls zig

    ## Sml
    smlnj

    ## Emacs everywhere
    xdotool xorg.xwininfo xclip
    haskellPackages.citeproc

    mu
    isync
    (pkgs.writeShellScriptBin "mail-init" ''
      ${pkgs.mu} init --maildir="~/Mail" --my-address="bequintao@gmail.com"
      ${pkgs.mu} index
    '')
    (pkgs.writeShellScriptBin "mail-sync" ''
      ${pkgs.isync}/bin/mbsync -a
    '')
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsNativeComp;
    extraPackages = (epkgs:
      (with epkgs.melpaPackages; [
        vterm
        evil
        dashboard
        magit
        docker
        no-littering
        consult
        company
        yasnippet
        lsp-pyright
        apheleia
        web-mode
        web-beautify
        lispy lispyville
        orderless
        pandoc
        ox-reveal
        org-roam
        org-bullets
        lsp-mode
        lsp-ui
        company
        flycheck
        pdf-tools
      ])
    );
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraConfig = ''
set nocompatible

filetype plugin indent on  " Load plugins according to detected filetype.
syntax on                  " Enable syntax highlighting.

set autoindent             " Indent according to previous line.
set expandtab              " Use spaces instead of tabs.
set softtabstop =4         " Tab key indents by 4 spaces.
set shiftwidth  =4         " >> indents by 4 spaces.
set shiftround             " >> indents to next multiple of 'shiftwidth'.

set backspace   =indent,eol,start  " Make backspace work as you would expect.
set hidden                 " Switch between buffers without having to save first.
set laststatus  =2         " Always show statusline.
set display     =lastline  " Show as much as possible of the last line.

set mouse=a

set showmode               " Show current mode in command-line.
set showcmd                " Show already typed keys when more are expected.

set incsearch              " Highlight while searching with / or ?.
set hlsearch               " Keep matches highlighted.

set ttyfast                " Faster redrawing.
set lazyredraw             " Only redraw when necessary.

set splitbelow             " Open new windows below the current window.
set splitright             " Open new windows right of the current window.

set cursorline             " Find the current line quickly.
set wrapscan               " Searches wrap around end-of-file.
set report      =0         " Always report changed lines.
set synmaxcol   =200       " Only highlight the first 200 columns.

set list                   " Show non-printable characters.
if has('multi_byte') && &encoding ==# 'utf-8'
  let &listchars = 'tab:▸ ,extends:❯,precedes:❮,nbsp:±'
else
  let &listchars = 'tab:> ,extends:>,precedes:<,nbsp:.'
endif

  "set shell=usr/bin/env zsh

" Put all temporary files under the same directory.
" https://github.com/mhinz/vim-galore#temporary-files
set backup
set backupdir   =$HOME/.vim/files/backup/
set backupext   =-vimbackup
set backupskip  =
set directory   =$HOME/.vim/files/swap//
set updatecount =100
set undofile
set undodir     =$HOME/.vim/files/undo/
set history=500
set viminfo     ='100,n$HOME/.vim/files/info/viminfo

set list listchars=tab:▸\ ,extends:›,precedes:‹,nbsp:·,trail:·",eol:¬
set termguicolors
set background=dark
colorscheme ayu-mirage
set cursorline
set t_Co=256

let mapleader=" "

nnoremap <M-j> :resize -2<CR>
nnoremap <M-k> :resize +2<CR>
nnoremap <M-l> :vertical resize -2<CR>
nnoremap <M-h> :vertical resize +2<CR>

nnoremap <C-x>3 :vsplit<CR>
nnoremap <C-x>2 :split<CR>
nnoremap <C-x>0 :close<CR>

tnoremap <C-w> <C-\><C-n><C-w>

nnoremap <Leader>b :Buffers<CR>

map <leader>s :e ~/scratch.org<CR>

'';
    plugins = with pkgs.vimPlugins; [
      fzf-vim
      neovim-ayu
      vim-nix
    ];
  };

  xdg.userDirs.documents = "$HOME/Documents";
  xdg.userDirs.music = "$HOME/Documents/Music";
  xdg.userDirs.pictures = "$HOME/Pictures";
}
