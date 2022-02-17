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
			syntastic

			fzf-vim
			indent-blankline-nvim

			(nvim-treesitter.withPlugins (plugins: pkgs.tree-sitter.allGrammars))

			vim-surround
			vimagit
			vim-css-color

			nvim-cmp
			vim-polyglot
			YouCompleteMe

			coc-tsserver
		];
		extraConfig = ''
			set history=500

			filetype plugin indent on
			syntax enable

			let base16colorspace=256
			set paste
			set title
			set number " relativenumber
			set nocompatible
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

			set tabstop=4 softtabstop=4 shiftwidth=4 autoindent     " tab width

			au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

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

			let g:indent_blankline_use_treesitter = v:true

			" Linebreak
			set lbr

			set si "Smart indent
			set wrap "Wrap lines
			set matchpairs+=<:>

			let g:syntastic_always_populate_loc_list = 1
			let g:syntastic_auto_loc_list = 1
			let g:syntastic_check_on_open = 1
			let g:syntastic_check_on_wq = 0

			let mapleader = " "

			map <C-t> :NERDTreeToggle<CR>
			map <leader>t :NERDTreeToggle<CR>
			let g:NERDTreeDirArrowExpandable = '▸'
			let g:NERDTreeDirArrowCollapsible = '▾'
			let NERDTreeShowHidden=0

			let g:NERDSpaceDelims = 1
			let g:NERDDefaultAlign = 'left'
			map <M-/> :NERDCommenterToggle<CR>

			map <C-c>o :Goyo <bar> <CR>
			map <F5> :!pandoc % -o %:t:r.pdf<cr>

			" Spell checking
			map <F6> :setlocal spell! spelllang=pt_br<CR>
			map <F7> :setlocal spell! Spelllang=en_us<CR>
			map <leader>ss :setlocal spell!<cr>

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
			set statusline+=%#warningmsg#
			set statusline+=%{SyntasticStatuslineFlag()}

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
		coc = {
			enable = true;
			settings = {
				"colorSupport" = true;
				"snippetStatusText" = "Ⓢ ";
				"formatOnSave" = true;
				"suggest.noselect" = true;
				"suggest.enablePreview" = true;
				"suggest.enablePreselect" = false;
				"suggest.disableKind" = true;

				"diagnostic.displayByAle" = false;
				"diagnostic.refreshOnInsertMode" = true;
				"diagnostic.errorSign" = "✘";
				"diagnostic.warningSign" = "⚠";
				"diagnostic.infoSign" = "";
				"diagnostic.hintSign" = "ஐ";
				"diagnostic.checkCurrentLine" = true;
				"diagnostic.virtualTextPrefix" = " ❯❯❯ ";
				"diagnostic.virtualText" = true;
				"diagnostic.enableMessage" = "never";

				languageserver = {
					haskell = {
						command = "haskell-language-server-wrapper";
						args = [ "--lsp" ];
						rootPatterns = [
							"*.cabal"
							"stack.yaml"
							"cabal.project"
							"package.yaml"
							"hie.yaml"
						];
						filetypes = [ "haskell" "lhaskell" ];
					};
					go = {
						command = "gopls";
						filetypes = [ "go" ];
					};
					ccls = {
						command = "ccls";
						filetypes = ["c" "cc" "cpp" "c++" "objc" "objcpp"];
						initializationOptions.cache.directory = "/tmp/ccls";
					};
					nix = {
						command = "rnix-lsp";
						filetypes = [ "nix" ];
					};
				};

			};
		};
	};
}
