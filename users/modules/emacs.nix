# with import <nixpkgs> { };
{pkgs, config, ...}:
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
    EDITOR = "emacs-open";
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
  ];

  programs.emacs = {
    enable = true;
    # package = pkgs.emacsGcc;
    extraPackages = (epkgs:
      (with epkgs.melpaPackages; [
        evil
        evil-collection
        evil-nerd-commenter
        evil-leader
        consult
        marginalia
        orderless
        which-key
        helpful
        vterm
        vterm-toggle
        all-the-icons-dired
        dired-hide-dotfiles
        peep-dired
        magit
        no-littering
        olivetti
        crux
        emms
        dashboard
        doom-modeline
        doom-themes
        org-evil
        org-pomodoro
        org-bullets
        org-mime
        org-journal
        org-ql
        pandoc
        ox-reveal
        ox-pandoc
        org-roam
        lsp-mode
        lsp-ui
        company
        company-box
        flycheck
        flycheck-popup-tip
        yasnippet
        dap-mode
        ccls
        irony
        c-eldoc
        irony-eldoc
        python-mode
        lsp-pyright
        js2-mode
        web-mode
        typescript-mode
        tide
        haskell-mode
        lsp-haskell
        go-mode
        flycheck-gometalinter
        company-go
        rustic
        flycheck-rust
        nix-mode
        aggressive-indent
        highlight-indent-guides
        geiser
      ]) ++ (with epkgs.elpaPackages; [
        rainbow-mode
        undo-tree
        xclip
      ])
    );
  };

  services.emacs.enable = true;

}
