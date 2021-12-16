{ config, pkgs, ... }:
{

  home.packages = with pkgs; [
    mpd
    mpc_cli
  ];

services.mpd.enable = true;
  
  programs.ncmpcpp = {
    enable = true;
    settings =  {
      # message_delay_time = 1;
      # playlist_disable_highlight_delay = 2;
      # autocenter_mode = "yes";
      # centered_cursor = "yes";
      # ignore_leading_the = "yes";
      # allow_for_physical_item_deletion = "no";

      # # Appearance;
      # colors_enabled = "yes";
      # playlist_display_mode = "columns";
      # user_interface = "classic";
      # volume_color = "white";

      # # Window;
      # song_window_title_format = "Music";
      # statusbar_visibility = "yes";
      # header_visibility = "no";
      # titles_visibility = "no";
      # # now_playing_prefix = "$3> $b";

      # # Progress bar;
      # progressbar_look = "▂▂▂";
      # progressbar_color = "black";
      # progressbar_elapsed_color = "cyan";

      # # Alternative UI;
      # alternative_ui_separator_color = "black";
      # alternative_header_first_line_format = "$b$5«$/b$5« $b$8{%t}|{%f}$/b $5»$b$5»$/b";
      # alternative_header_second_line_format = "{$b{$2%a$9}{ - $7%b$9}{ ($2%y$9)}}|{%D}";

      # # Song list;
      # song_list_format = " $8%a • %t";
      # song_status_format = " $6%a  $7%t  $5%b ";
      # song_columns_list_format = "  (15)[blue]{t} (5)[green]{a}(8f) [black]{l}";

      # # Colors;
      # main_window_color = "blue";
      # current_item_prefix = "$(blue)$r";
      # current_item_suffix = "$/r$(end)";
      # current_item_inactive_column_prefix = "red";
      # current_item_inactive_column_suffix = "red";

      # color1 = "white";
      # color2 = "red";


# Mpd configs
mpd_host = "localhost";
mpd_port = "6600";
mpd_music_dir = "~/Music";

# General;
lyrics_directory = "/home/basqs/.cache/lyrics/";
external_editor = "vim";
message_delay_time = "1";
playlist_disable_highlight_delay = 1;
enable_window_title = "yes";
media_library_hide_album_dates = "yes";
connected_message_on_startup = "no";
space_add_mode = "add_remove";
ignore_diacritics = "yes";
screen_switcher_mode = "previous";
cyclic_scrolling = "no";
use_console_editor = "yes";
follow_now_playing_lyrics = "yes";
lines_scrolled = 1;

# Aesthetics;
user_interface = "classic";
playlist_display_mode = "columns";
song_columns_list_format = "(20)[4]{a} (25)[]{t} (20)[9]{b} (5)[]{l}";
song_status_format = "$8%a '%b' - %t$9";

colors_enabled = "yes";
discard_colors_if_item_is_selected = "yes";

header_visibility = "yes";
titles_visibility = "no";
statusbar_visibility = "yes";
display_remaining_time = "no";
browser_display_mode = "columns";
search_engine_display_mode = "columns";
playlist_editor_display_mode = "columns";

volume_color = "2";
statusbar_color = "4";
statusbar_time_color = "9";
header_window_color = "2";
main_window_color = "8";
empty_tag_color = "9";
color1 = "8";
color2 = "8";

now_playing_prefix = "$2$b";
now_playing_suffix = "$9$/b";
current_item_prefix = "$(2_1)$b";
current_item_suffix = "$/b$(end)";
current_item_inactive_column_prefix = "$(4_1)";
current_item_inactive_column_suffix = "$(end)";
state_line_color = "1";
state_flags_color = "4";
window_border_color = "9";
active_window_border = "9";
player_state_color = "9";

progressbar_color = "1"  ;
progressbar_look= "━━━";
#progressbar_look = "─╼ ";
progressbar_elapsed_color = "6";

# Visualizer settings;
visualizer_data_source = "/tmp/mpd.fifo";
visualizer_output_name = "visulaizer";
visualizer_in_stereo = "yes";
visualizer_type = "wave_filled";
visualizer_look = "┃┃";
visualizer_color = "9, yellow, red";
visualizer_fps = 30;
    };
  };
}
