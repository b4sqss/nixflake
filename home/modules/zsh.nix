{ config, pkgs, lib, inputs, ... }:
{
    programs.autojump = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.dircolors = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    home.sessionVariables = {

      NIX_BUILD_SHELL = "zsh";
      NNN_BMS = "d:$HOME/Documents;D:$HOME/Downloads/";
      LC_COLLATE = "C";                                                    
      NNN_FIFO = "/tmp/nnn.fifo";                                            
      NNN_PLUG = "o:fzopen;f:fzcd;m:nmount;.:preview-tui;x:_chmod +x $nnn";
      NNN_ARCHIVE = "\\.(7z|bz2|gz|tar|tgz|zip)$";
      NNN_FCOLORS = "$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER";
      BLK = "0B";
      CHR = "0B";
      DIR = "04";
      EXE = "02";
      REG = "00";
      HARDLINK = "06";
      SYMLINK = "06";
      MISSING = "00";
      ORPHAN = "09";
      FIFO = "06";
      SOCK = "0B";
      OTHER = "06";

    };

    programs.zsh = {
      enable = true;
      enableCompletion = true;
      enableAutosuggestions = true;
      # enableSyntaxHighlighting = true;
      autocd = true;

      #programs.zsh.defaultKeymap = "vicmd";
      history.extended = true;
      history.ignoreDups = true;
      history.save = 10000000;
      history.share = true;
      history.size = 10000000;

      zplug = {
        enable = true;
        plugins = [
          { name = "chisui/zsh-nix-shell"; } # Simple plugin installation
          { name = "zdharma-continuum/fast-syntax-highlighting"; }# tags = [ as:theme depth:1 ]; } # Installations with additional options. For the list of options, please refer to Zplug README.
        ];
      };

      shellAliases = {
        ga = "git add";
        gc = "git commit";
        gcm = "git commit -m";
        gs = "git status";
        gsb = "git status -sb";
        ".." = "cd ..";
        panik = "rm -rf ~/.furry-porn";
        build_root = "sudo nixos-rebuild switch";
        delete_gens = "nix-env -p /nix/var/nix/profiles/system --delete-generations old";
        hms = "home-manager switch";
        l = "ls -lF --time-style=long-iso --grid --icons";
        la = "l -a";
        ll = "ls -l";
        ls = "exa -h --git --color=auto --group-directories-first -s extension";
        lstree = "ls --tree";
        query_vim_pkgs = "nix-env -f '<nixpkgs>' -qaP -A vimPlugins";
        re = "systemctl --user restart emacs.service";
        scu = "systemctl --user";
        start_nix_shell = "nix-shell --pure -E 'with import<nixpkgs> {}; callPackage ./. {}";
        tree = "lstree";
        # Keep these two aliases in this specific order, otherwise highlighting gets fucked!!!!
        nixman = "manix '' | grep '^# ' | sed 's/^# (.*) (.*/1/;s/ (.*//;s/^# //' | sed 's/</\\\\</g' | sed 's/>/\\\\>/g'| fzf --ansi --preview=\"manix '{}' | sed 's/type: /> type: /g' | bat -l Markdown --color=always --plain\"";
        opt = "manix '' | grep '^# ' | sed 's/^# \(.*\) (.*/\1/;s/ (.*//;s/^# //' | fzf --ansi --preview=\"manix '{}' | sed 's/type: /> type: /g' | bat -l Markdown --color=always --plain\"";

      };
      # keys.sh contains a bunch of my keys
      initExtra = builtins.readFile ../configs/zshrc;
    };
}
