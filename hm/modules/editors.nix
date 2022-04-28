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

    ## C
    clang bear gdb cmake llvmPackages.libcxx ccls rtags llvm gnumake

    ## Python
    python39 python39Packages.pip nodePackages.pyright

    ## Haskell
    ghc ghcid
    haskell-language-server

    ## Js
    nodejs yarn nodePackages.npm nodePackages.typescript nodePackages.typescript-language-server deno

    ## Go
    go gopls

    ## Scheme
    scheme-manpages mitscheme

    ## Zig
    zls zig

    ## Sml
    smlnj
  ];

  programs.neovim = {
    enable = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withPython3 = true;
    withRuby = true;
    extraPython3Packages = (ps: with ps; [
      pyx
      pynvim
    ]);
    package = pkgs.neovim-nightly;
    extraConfig = ''
"      lua require("init")
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
\ 'executable' : 'latexmk',
\ 'options' : [
\ '-shell-escape',
\ '-verbose',
\ '-file-line-error',
\ '-synctex=1',
\ '-interaction=nonstopmode',
\ ],
\}
"let ch_syntax_for_h = 1
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
source $HOME/.config/nvim/configs/text.vim
let g:AutoPairsCenterLine = 0
let g:AutoPairsMapSpace = 0
let g:AutoPairsMapCR = 0
let g:c_syntax_for_h = 1
'';
    coc = {
      enable = true;
      settings = {
        "hover.target" = "float";
        "coc.preferences.currentFunctionSymbolAutoUpdate" = true;
        "coc.source.word.filetypes" = [
          "latex"
          "tex"
          "markdown"
          "markdown.pandoc"
        ];
        "coc.source.dictionary.filetypes" = [
          "latex"
          "tex"
          "markdown"
          "markdown.pandoc"
        ];
        "coc.preferences.formatOnSaveFiletypes" = [
          "java"
          "c"
          "cpp"
          "jsonc"
          "sql"
          "python"
          "html"
          "php"
        ];
        "diagnostic.warningSign" = "!!";
        "diagnostic.enableSign" = false;
        "diagnostic.refreshOnInsertMode" = true;
        "diagnostic.messageTarget" = "float";
        "signature.target" = "echo";
        "session.directory" = "~/.config/nvim/sessions";
        "session.saveOnVimLeave" = false;
        "suggest.noselect" = false;
        "suggest.snippetIndicator" = "►";
        "suggest.floatEnable" = false;
        "suggest.autoTrigger" = "always";
        "suggest.echodocSupport" = true;
        "suggest.acceptSuggestionOnCommitCharacter" = true;
        "snippets.extends" = {
          "cpp" = "c";
          "html" = "php";
          "php" = "html";
          "markdown.pandoc" = "tex";
        };
      };
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
    extraPackages = (epkgs:
      (with epkgs.melpaPackages; [
        vterm
        magit
        no-littering
        consult
        orderless
        org-ql
        pandoc
        ox-reveal
        org-roam
        lsp-mode
        lsp-ui
        company
        flycheck
        geiser
        pdf-tools
      ])
    );
  };

  home.file.".emacs.d/emacs.org".source = ../configs/emacs.org;
  xdg.configFile."emacs/emacs.org" = {
    source = ../configs/emacs.org;
  };
  xdg.userDirs.documents = "$HOME/Docs";
  xdg.userDirs.music = "$HOME/Docs/Music";
  xdg.userDirs.pictures = "$HOME/Pics";
}
