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
    EDITOR = "nvim";
    VISUAL = "emacs-open";
  };

  home.packages = with pkgs; [
    emacs-open
    sqlite
    emacs-all-the-icons-fonts
    auctex
    rnix-lsp
    languagetool
    coreutils

    ## Documents
    texlive.combined.scheme-full
    pandoc
    unoconv

    ## C
    clang bear gdb cmake llvmPackages.libcxx ccls rtags llvm gnumake

    ## Python
    python39 python39Packages.pip nodePackages.pyright

    ## Haskell
    ghc ghcid haskell-language-server

    ## Js
    nodejs yarn nodePackages.npm nodePackages.typescript nodePackages.typescript-language-server deno

    ## Go
    go gopls

    ## Scheme
    scheme-manpages mitscheme

    ## Zig 
    zls zig

    (aspellWithDicts (ds: with ds; [ br en en-computers en-science ]))
  ];

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    package = pkgs.neovim-nightly;
    #    extraConfig = ''
    #      lua require("init")
    #    '';
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
    extraPackages = (epkgs:
      (with epkgs.melpaPackages; [
        vterm
        magit
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

  #  xdg.configFile."nvim/lua" = {
  #    source = ../configs/nvim;
  #    recursive = true;
  #  };
}
