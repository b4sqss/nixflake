{ config, pkgs, lib, ... } :
{

  home.packages = with pkgs; [zsh];

  programs.alacritty = {
    enable = true;
    settings = {

      #### PADDING

      window = {
        padding.x = 10;
        padding.y = 10;
        decorations = "Full";
      };

      #### FONTS ----------------

      font = {
        size = 6.0;
        use_thin_strokes = true;

        normal = {
          family = "Iosevka";
          style = "Regular";
        };

        bold = {
          family = "Iosevka";
          style = "Bold";
        };

        italic = {
          family = "Iosevka";
          style = "Italic";
        };
      };

      #### STYLING --------------

      cursor.style = "Block";

      shell = {
        program = "zsh";
      };

      #### COLOR SCHEME --------------

      colors = {
        # Default colors
        primary = {
          background = "#323d43";
          foreground = "#d3c6aa";
        };

        normal = {
          black =   "#4b565c";
          red =     "#e67e80";
          green =   "#a7c080";
          yellow =  "#dbbc7f";
          blue =    "#7fbbb3";
          magenta = "#d699b6";
          cyan =    "#83c092";
          white =   "#d3c6aa";
        };
        bright = {
          black =   "#4b565c";
          red =     "#e67e80";
          green =   "#a7c080";
          yellow =  "#dbbc7f";
          blue =    "#7fbbb3";
          magenta = "#d699b6";
          cyan =    "#83c092";
          white =   "#d3c6aa";
        };
      };
    };
  };
}
  
