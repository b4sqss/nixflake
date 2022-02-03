{ config, lib, pkgs, ... }: {
  programs.neovim = {
    enable = true;
    withPython3 = true;
    vimAlias = true;
    viAlias = true;
    plugins = with pkgs.vimPlugins; [
      vim-nix
      nerdtree
      nerdtree-git-plugin
      vim-one
      vim-devicons
      vim-fugitive
      goyo
      nerdcommenter
      hop-nvim
      vim-orgmode

      (nvim-treesitter.withPlugins (plugins: pkgs.tree-sitter.allGrammars))

      vim-surround
      vimagit
      vimwiki
      vim-commentary
      vim-css-color

      nvim-cmp
    ];
    extraConfig = ''
      set history=500

        filetype indent plugin on

        let base16colorspace=256
        set paste
        set title
        set number " relativenumber
        set encoding=utf8
        set ignorecase
        set smartcase
        set hlsearch
        set mouse=a
        set lazyredraw
        set nobackup
        set nowb
        set noswapfile
        set wildmenu
        set cursorline
        set clipboard=unnamedplus

        set termguicolors
        syntax on
        " colorscheme base16-tomorrow-night
        colorscheme one
        set background=dark

        " Hardcore mode, disable arrow keys.
        " noremap <Up> <NOP>
        " noremap <Down> <NOP>
        " noremap <Left> <NOP>
        " noremap <Right> <NOP>

        set undofile " Maintain undo history between sessions
        set undodir=~/.cache/nvim/undodir

        set smarttab
        " 1 tab == 4 spaces
        set shiftwidth=4
        " set tabstop=4

        " Linebreak
        set lbr

        set ai "Auto indent
        set si "Smart indent
        set wrap "Wrap lines
        set matchpairs+=<:>

        let mapleader = " "

        map <C-t> :NERDTreeToggle<CR>
        map <leader>t :NERDTreeToggle<CR>
        let g:NERDTreeDirArrowExpandable = '▸'
        let g:NERDTreeDirArrowCollapsible = '▾'
        let NERDTreeShowHidden=0

        " Goyo plugin makes text more readable when writing prose:
        map <leader>f :Goyo \| set bg=light \| set linebreak<CR>

        map <;> :HopChar1

        " Quickly open a buffer
        map <leader>q :e ~/buffer<cr>
        map <leader>x :e ~/buffer.org<cr>

        map <leader>gdi :Gdiff<cr>
        map <leader>gst :Git<cr>
        map <leader>dup :diffupdate<cr>

        " Compile document, be it groff/LaTeX/markdown/etc.
        map <leader>c :w! \| !compiler "<c-r>%"<CR>

        " Open corresponding .pdf/.html or preview
        map <leader>p :!opout <c-r>%<CR><CR>

        " Always show the status line
        set laststatus=2

        " Format the status line
        set statusline=
        set statusline+=%<\                       " cut at start
        set statusline+=%2*[%n%H%M%R%W]%*\        " flags and buf no
        set statusline+=%-40f\                    " path
        set statusline+=%=%1*%y%*%*\              " file type
        set statusline+=%{FugitiveStatusline()}   " git status
        " set statusline+=%10((%l,%c)%)\            " line and column
        " set statusline+=%P                        " percentage of file

      function! s:MyFollowSymlink()
      silent! let s:fname = resolve(expand('%:p'))
      silent! bwipeout
      silent! exec "edit " .s:fname
      endfunction
      command! FollowSymlink call s:MyFollowSymlink()

      augroup followsymlink
      autocmd!
      autocmd BufReadPost * FollowSymlink
      augroup END
    '';
  };
}
