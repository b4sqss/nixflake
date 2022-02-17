let
  clr = import ../theme/one.nix;
in

{
  programs.zathura = {
    enable = true;
    options = {
      default-bg = clr.background;
      default-fg = clr.foreground;
      
      statusbar-fg = clr.foreground;
      statusbar-bg = clr.background;
      
      inputbar-bg = clr.black;
      inputbar-fg = clr.foreground;
      
      notification-bg = clr.black;
      notification-fg = clr.foreground;
      
      notification-error-bg = clr.black;
      notification-error-fg = clr.red;
      
      notification-warning-bg = clr.black;
      notification-warning-fg = clr.yellow;
      
      highlight-color = clr.green;
      highlight-active-color = clr.cyan;
      
      completion-bg = clr.black;
      completion-fg = clr.cyan;
      
      completion-highlight-fg = clr.foreground;
      completion-highlight-bg = clr.background;
      
      recolor-lightcolor = clr.background;
      recolor-darkcolor = clr.foreground;

      recolor = "true";
      recolor-keephue = "true";

      page-padding = 1;
      font = "Iosevka";
    };
  };
}
