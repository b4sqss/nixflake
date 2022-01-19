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

  emacs-opent = pkgs.writeShellScriptBin "emacs-opent" '' 
if [ $# -eq 0 ]; then
    emacsclient -n -t
    exit
fi

emacsclient -e "(frames-on-display-list \"$DISPLAY\")" &>/dev/null

if [ $? -eq 0 ]; then
    emacsclient -n -t "$*"
else
    emacsclient -t -n "$*"
fi
'';

#   bintools = binutils.bintools;
#   emacs' = emacs.overrideAttrs (old: rec {
#     # emacs' = (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {}).emacs.overrideDerivation (old: rec {
#     withXwidgets = true;
#     postInstall = 
#       (old.postInstall + ''
#         # mu4e
#         wrapProgram $out/bin/emacs --prefix PATH : ${lib.makeBinPath [ mu ]}
#         wrapProgram $out/bin/emacs --set MU4E ${mu}
#       '');
#   });

#   mu4e = emacsPackages.trivialBuild {
#     pname = "mu4e";
#     src = "${mu}/share/emacs/site-lisp/mu4e/";
#     # packageRequires = [ mu ];
#   };
    
in {
#   nixpkgs.overlays = [
# (self: super: {
#   emacs = super.emacs.overrideAttrs (old: {
#     configureFlags = (old.configureFlags or []) ++ ["--with-x-toolkit=lucid"];
#   });
#   })
#   ];

  home.sessionVariables = {
    EDITOR = "emacs-opent";
    VISUAL = "emacs-open";

  };
  home.packages = with pkgs; [
    emacs-open
    emacs-opent
    sqlite
    gopls
    emacs-all-the-icons-fonts
    auctex
    rnix-lsp
    libvterm
    (aspellWithDicts (ds: with ds; [ en pl ]))
    languagetool
    coreutils
  ];

  programs.emacs = {
    enable = true;
    # package = emacs';
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

   # home.file.".emacs.d/".source = ../configs/emacs;

  services.emacs.enable = true;

}
