 {
    programs.nnn = {
      enable = true;
      bookmarks = {
        d = "~/Documents";
        D = "~/Downloads";
        g = "~/Documents/git";
      };
      extraPackages = with pkgs; [
        ffmpegthumbnailer
        sxiv
        tmux
        tree
        file
        mktemp
        unzip
        man
        atool
        bat
        ueberzug
        imagemagick
        ffmpeg
        libreoffice
        poppler
        lowdown
        w3m
      ];
      plugins = {
        plugins.src = (pkgs.fetchFromGitHub {
          owner = "jarun";
          repo = "nnn";
          rev = "v4.0";
          sha256 = "sha256-Hpc8YaJeAzJoEi7aJ6DntH2VLkoR6ToP6tPYn3llR7k=";
        }) + "/plugins";
        mappings = {
          c = "fzcd";
          f = "fzopen";
          p = "pdfread";
          P = "preview-tui";
          r = "rsynccp";
          v = "imgview";
      };
      };

 }
