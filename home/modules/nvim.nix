{ config, lib, pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    withPython3 = true;
    vimAlias = true;
    extraConfig = ''
            let base16colorspace=256
            set paste
            set number relativenumber
            colorscheme base16-tomorrow-night
            set ignorecase
            set smartcase
            set mouse=a
            set noswapfile
            let NERDTreeShowHidden = 1
            map<F7> :NERDTree <CR>
            map <C-g> :Goyo <CR>
            set cursorline

let g:limelight_conceal_ctermfg = 'gray'
let g:goyo_width = 100
let g:goyo_height = 95
let b:code = "no"

"Toggle Goyo and Limelight on and off
map <C-g> :Goyo<CR>

autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

          '';

	  plugins = with pkgs.vimPlugins; [
    coc-nvim
    coc-pyright
    coc-go
    vim-nix
    vim-pandoc
    goyo
    limelight-vim
    gitgutter
    nvim-web-devicons
    nvim-colorizer-lua
    nvim-treesitter
    plenary-nvim
    telescope-nvim
    telescope-coc-nvim
    telescope-project-nvim
    hop-nvim
    molokai
    nvim-base16
    lush-nvim
    gruvbox-nvim
    nerdtree
	  ];
  #       coc = {
  #         enable = true;
  #         settings = {
  #           "suggest.noselect" = true;
  #           "suggest.enablePreview" = true;
  #           "suggest.enablePreselect" = false;
  #           languageserver = {
  #             nix = {
  #               command = "rnix-lsp";
  #               filetypes = [ "nix" ];
  #             };
  #           };
  # };
  #       };
  };
}
