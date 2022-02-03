{ config, pkgs, lib, inputs, ... }: {

    programs.zoxide = {
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

programs.direnv = {
enable = true;
enableZshIntegration = true;
nix-direnv.enable = true;
};

home.sessionVariables = {
SHELL = "zsh";
NIX_BUILD_SHELL = "zsh";
LC_COLLATE = "C";                                                    
NNN_FIFO = "/tmp/nnn.fifo";                                            
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
PF_INFO = "ascii title os wm uptime pkgs palette";
};

programs.zsh = {
enable = true;
enableCompletion = false;
enableAutosuggestions = true;
autocd = true;

    # defaultKeymap = "vicmd";
    history.extended = true;
    history.ignoreDups = true;
    history.save = 10000000;
    history.size = 10000000;

    zplug = {
    enable = true;
    plugins = [
    { name = "zdharma-continuum/fast-syntax-highlighting"; }
    ];
};

shellAliases = {
cd = "z";
ga = "git add";
gc = "git commit";
gcm = "git commit -m";
gs = "git status";
gsb = "git status -sb";
".." = "cd ..";
panik = "rm -rf ~/.furry-porn";
l = "ls -lF";
la = "l -a";
ll = "ls -l";
ls = "ls --group-directories-first --color";
cp = "cp -iv";
mv = "mv -iv";
query_vim_pkgs = "nix-env -f '<nixpkgs>' -qaP -A vimPlugins";
re = "systemctl --user restart emacs.service";
scu = "systemctl --user";
sudo = "doas";
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
