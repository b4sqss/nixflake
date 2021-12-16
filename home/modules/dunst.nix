let
  clr = import ../theme/tomorrow-night.nix;
in
{
  services.dunst = {
    enable = true;
    settings = {
      global = {
        monitor = 0;
        geometry = "342x3-34+34";
        shrink = "yes";
        transparency = 10;
        padding = 5;
        horizontal_padding = 5;
        frame_width = 3;
        frame_color = clr.background;
        separator_height = 1;
        font = "JetBrains Mono 13";
        line_height = 4;
        idle_threshold = 120;
        markup = "full";
        format = ''<b>%s</b>\n%b'';
        alignment = "center";
        vertical_alignment = "top";
        icon_position = "left";
        word_wrap = "yes";
        ignore_newline = "no";
        show_indicators = "yes";
        sort = true;
        stack_duplicates = true;
      };
      urgency_low = {
        background = clr.background;
        foreground = clr.foreground;
        frame_color = clr.yellow;
        timeout = 3;
      };
      urgency_normal = {
        background = clr.background;
        foreground = clr.foreground;
        frame_color = clr.yellow;
        timeout = 3;
      };
      urgency_critical = {
        background = clr.background;
        foreground = clr.foreground;
        frame_color = clr.red;
        timeout = 6;
      };
    };
  };
}
