{ config, lib, pkgs, ... }: {
  programs.neovim = {
    enable = true;
    withPython3 = true;
    vimAlias = true;
    viAlias = true;
    plugins = with pkgs.vimPlugins; [
      vim-nix
      vim-go
      nerdtree
      nerdtree-git-plugin
      nvim-base16
      # vim-devicons
      vim-fugitive
      # goyo
      # nerdcommenter
      # Hop
      # nvim-cmp
      # vim-symlink
    ];
    extraConfig = ''
      set history=500

      filetype indent plugin on

      let base16colorspace=256
      set paste
      set number relativenumber

      syntax on
      colorscheme base16-tomorrow-night
      " colorscheme slate

        " Hardcore mode, disable arrow keys.
        " noremap <Up> <NOP>
        " noremap <Down> <NOP>
        " noremap <Left> <NOP>
        " noremap <Right> <NOP>

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

      map <C-t> :NERDTreeToggle<CR>  
      let g:NERDTreeDirArrowExpandable = '▸'
      let g:NERDTreeDirArrowCollapsible = '▾'
      let NERDTreeShowHidden=0

        let mapleader = ","

        " Quickly open a buffer
        map <leader>q :e ~/buffer<cr>
        map <leader>x :e ~/buffer.md<cr>

        map <leader>gdi :Gdiff<cr>
        map <leader>gst :Git<cr>
        map <leader>dup :diffupdate<cr>

        " Always show the status line
        set laststatus=2

        " Format the status line
      set statusline=
      set statusline+=%<\                       " cut at start
      set statusline+=%2*[%n%H%M%R%W]%*\        " flags and buf no
      set statusline+=%-40f\                    " path
      set statusline+=%{FugitiveStatusline()}   " git status
      set statusline+=%=%1*%y%*%*\              " file type
      set statusline+=%10((%l,%c)%)\            " line and column
      set statusline+=%P                        " percentage of file

      " VIM-GO CONFIGS
      " Syntax highlighting
      let g:go_highlight_fields = 1
      let g:go_highlight_functions = 1
      let g:go_highlight_function_calls = 1
      let g:go_highlight_extra_types = 1
      let g:go_highlight_operators = 1
      " Enable auto formatting on saving
      let g:go_fmt_autosave = 1
      " Run `goimports` on your current file on every save
      let g:go_fmt_command = "goimports"
      " Status line types/signatures
      let g:go_auto_type_info = 1

      " Go Add Tags
      let g:go_addtags_transform = 'camelcase'
      noremap gat :GoAddTags<cr>

      function! MyFollowSymlink(...)
      if exists('w:no_resolve_symlink') && w:no_resolve_symlink
      return
      endif
      let fname = a:0 ? a:1 : expand('%')
      if fname =~ '^\w\+:/'
      " do not mess with 'fugitive://' etc
      return
      endif
      let fname = simplify(fname)

      let resolvedfile = resolve(fname)
      if resolvedfile == fname
      return
      endif
      let resolvedfile = fnameescape(resolvedfile)
      echohl WarningMsg | echomsg 'Resolving symlink' fname '=>' resolvedfile | echohl None
      " exec 'noautocmd file ' . resolvedfile
      " XXX: problems with AutojumpLastPosition: line("'\"") is 1 always.
      exec 'file ' . resolvedfile
      endfunction

      command! FollowSymlink call MyFollowSymlink()
      command! ToggleFollowSymlink let w:no_resolve_symlink = !get(w:, 'no_resolve_symlink', 0) | echo "w:no_resolve_symlink =>" w:no_resolve_symlink
      au BufReadPost * call MyFollowSymlink(expand('<afile>'))
    '';
  };
}
