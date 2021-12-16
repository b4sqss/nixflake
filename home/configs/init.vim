let g:markdown_fenced_languages = [
			\ 'html',
			\ 'python',
			\ 'bash=sh',
			\ 'java',
			\ 'c',
			\ 'cpp',
			\ 'php',
			\ 'sql',
			\ 'js=javascript',
			\ 'pro=prolog',
			\ 'vim',
			\ 'help',
			\ ]
let g:pandoc#syntax#codeblocks#embeds#langs = [
			\ "php",
			\ "html",
			\ "bash=sh",
			\ "java",
			\ "c",
			\ "cpp",
			\ "python",
			\ "sql",
			\ "js=javascript",
			\ 'pro=prolog',
			\ 'vim',
			\ 'help',
			\ ]

let g:markdown_syntax_conceal = 1
let g:markdown_minlines = 100

let g:tex_flavor='latex'
let g:vimtex_compiler_latexmk = {
			\ 'options' : [
				\ '-shell-escape',
				\ '-verbose',
				\ '-file-line-error',
				\ '-synctex=1',
				\ '-interaction=nonstopmode',
				\ ],
				\}

augroup pandoc_syntax
	au! BufNewFile,BufFilePre,BufRead *.md set filetype=markdown.pandoc
augroup END
autocmd BufEnter,BufNewFile,BufFilePre,BufRead *.md :syntax sync fromstart

source $HOME/.config/nvim/configs/plugins.vim
source $HOME/.config/nvim/configs/basic.vim
source $HOME/.config/nvim/configs/appearance.vim
source $HOME/.config/nvim/configs/statusline.vim
source $HOME/.config/nvim/configs/coc.vim
source $HOME/.config/nvim/configs/snippets.vim
source $HOME/.config/nvim/configs/keys.vim
source $HOME/.config/nvim/configs/fzf.vim
