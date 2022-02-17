{config, pkgs, ...}: let
  clr = import ../theme/one.nix;
in {
  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    foliate
    qbittorrent
    sxiv 
    qimgv
    inkscape 
    gimp 
    krita 
    blender 

    ardour
    audacity

    spotify-unwrapped spotify-tui
    discord discocss

    mpd
  ];

  programs.mpv = {
    package = pkgs.mpv-with-scripts;
    enable = true;
    config = {
      prefetch-playlist = "yes";
      profile = "protocol.https";
      screenshot-tag-colorspace = "yes";
      screenshot-directory = "/home/basqs/Pictures/mpvshots";
      screenshot-template = "%f-%wH.%wM.%wS.%w";
      screenshot-format = "png";
      keep-open = "yes";
    };
  };

  programs.ncmpcpp = {
    enable = true;
    package = pkgs.ncmpcpp.override { visualizerSupport = true; };
    bindings = [
      { key = "j"; command = "scroll_down"; }
      { key = "k"; command = "scroll_up"; }
      { key = "J"; command = [ "select_item" "scroll_down" ]; }
      { key = "K"; command = [ "select_item" "scroll_up" ]; }
    ];
    settings = {
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

  services.mpd = {
    enable = true;
    dataDir = "/home/basqs/.local/share/mpd";
    musicDirectory = "/home/basqs/Music";
  };

  programs.newsboat = {
    enable = true;
    autoReload = true;
    extraConfig = ''
      ignore-mode "display"

      bind-key j down
      bind-key k up
      bind-key j next articlelist
      bind-key J next-feed articlelist
      bind-key K prev-feed articlelist
      bind-key G end
      bind-key g home
      bind-key d pagedown
      bind-key u pageup
      bind-key l open
      bind-key h quit
      bind-key a toggle-article-read
      bind-key n next-unread
      bind-key N prev-unread
      bind-key D pb-download
      bind-key U show-urls
      bind-key x pb-delete

      color listnormal blue default
      color listfocus yellow black bold
      color listnormal_unread white default bold
      color listfocus_unread yellow black bold
      color info yellow black bold
      color article white default bold

      browser "linkhandler %u"
      macro w set browser "lynx %u"; open-in-browser ; set browser "linkhandler %u"
      macro f set browser "qutebrowser %u"; open-in-browser ; set browser "linkhandler %u"
      macro v set browser "setsid -f mpv" ; open-in-browser ; set browser linkhandler
      macro c set browser "xsel -b <<<" ; open-in-browser ; set browser linkhandler

# feedlist format and colours
#feedlist-format "%?l?├──╼ %t &├───%t?"
      feedlist-format "%?l?┃%4i %n %8u %t &┣━━━━━━━━━ ■ %t?"

#highlight feedlist ".*─────.*" green

      highlight feedlist "[┃┃]" red default bold
      highlight feedlist "┣━.*" red default bold
      highlight feedlist "■" color8 default
# article format and colours
#articlelist-format "%?T? │ %2i %f  %t?"
      articlelist-format " %2i  %t"

#highlight all "---.*---" yellow
#highlight feedlist ".*(0/0))" black
      highlight article "(^Feed:.*|^Title:.*|^Author:.*)" green default bold
      highlight article "(^Link:.*|^Date:.*)" default default
      highlight article "https?://[^ ]+" red default
      highlight article "^(Title):.*$" blue default
      highlight article "\\[[0-9][0-9]*\\]" magenta default bold
      highlight article "\\[image\\ [0-9]+\\]" red default bold
      highlight article "\\[embedded flash: [0-9][0-9]*\\]" red default bold
      highlight article ":.*\\(link\\)$" green default
      highlight article ":.*\\(image\\)$" blue default
      highlight article ":.*\\(embedded flash\\)$" magenta default
    '';
    reloadTime = 120;
    urls = [
      { tags = [ "linux" ] ;
        url = "";
        title = "linux"; }
      { tags = [ "linux" ] ;
        url = "https://weekly.nixos.org/feeds/all.rss.xml";
        title = "nixos"; }
      { tags = [ "linux" ] ;
        url = "https://www.phoronix.com/rss.php";
        title = "phoronix"; }
      { tags = [ "linux" ] ;
        url = "https://suckless.org/atom.xml";
        title = "suckless"; }
      { tags = [ "linux" ] ;
        url = "https://lwn.net/headlines/rss";
        title = "lwn"; }
      { tags = [ "linux" ] ;
        url = "https://www.technologyreview.com/feed/";
        title = "mit"; }

      { tags = [ "foss" ] ;
        url = "";
        title = "foss"; }
      { tags = [ "foss" ] ;
        url = "https://act.eff.org/action.atom";
        title = "act eff"; }
      { tags = [ "foss" ] ;
        url = "https://planet.gnu.org/atom.xml";
        title = "planet gnu"; }
      { tags = [ "foss" ] ;
        url = "https://itsfoss.com/feed/";
        title = "itsfoss"; }

      { tags = [ "blogs" ] ;
        url = "";
        title = "blogs"; }
      { tags = [ "blogs" ] ;
        url = "https://seirdy.one/";
        title = "seirdy"; }
      { tags = [ "blogs" ] ;
        url = "https://yujiri.xyz/rss.xml";
        title = "yujiri"; }
      { tags = [ "blogs" ] ;
        url = "https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-and-content.xml";
        title = "karl voit"; }
      { tags = [ "blogs" ] ;
        url = "https://lukesmith.xyz/rss.xml";
        title = "luke smith"; }
      { tags = [ "blogs" ] ;
        url = "https://drewdevault.com/blog/index.xml";
        title = "drewdevault"; }

      { tags = [ "comics" ] ;
        url = "";
        title = "comics"; }
      { tags = [ "comics" ] ;
        url = "https://xkcd.com/atom.xml";
        title = "xkcd"; }
      { tags = [ "comics" ] ;
        url = "https://www.smbc-comics.com/comic/rss";
        title = "smbc"; }

      { tags = [ "science" ] ;
        url = "";
        title = "science"; }
      { tags = [ "science" ] ;
        url = "https://www.cell.com/AJHG/inpress.rss "; }
      { tags = [ "science" ] ;
        url = "https://academic.oup.com/rss/site_5325/3191.xml";
        title = ""; }
      { tags = [ "science" ] ;
        url = "https://academic.oup.com/rss/site_5281/3147.xml";
        title = ""; }
      { tags = [ "science" ] ;
        url = "http://feeds.plos.org/plosgenetics/NewArticles";
        title = ""; }
      { tags = [ "science" ] ;
        url = "https://academic.oup.com/rss/site_5139/3001.xml";
        title = ""; }
      { tags = [ "science" ] ;
        url = "https://science.sciencemag.org/rss/twis.xml";
        title = ""; }

      { tags = [ "noticias" ] ;
        url = "";
        title = "noticias"; }
      { tags = [ "noticias" ] ;
        url = "https://www.em.com.br/rss/noticia/internacional/rss.xml";
        title = "em"; }
      { tags = [ "noticias" ] ;
        url = "https://veja.abril.com.br/feed/";
        title = "veja"; }
      { tags = [ "noticias" ] ;
        url = "https://feeds.folha.uol.com.br/mundo/rss091.xml";
        title = "folha"; }
      { tags = [ "noticias" ] ;
        url = "https://www.gazetadopovo.com.br/feed/rss/mundo.xml";
        title = "gazetadopovo"; }
      { tags = [ "noticias" ] ;
        url = "https://rss.dw.com/rdf/rss-br-all";
        title = "dw"; }

      { tags = [ "astronomy/flight" ] ;
        url = "";
        title = "astronomy/flight"; }
      { tags = [ "astronomy/flight" ] ;
        url = "https://apod.nasa.gov/apod.rss";
        title = "APOD"; }
      { tags = [ "astronomy/flight" ] ;
        url = "https://aeroflap.com.br/feed";
        title = "aeroflap"; }
      { tags = [ "astronomy/flight" ] ;
        url = "https://simpleflying.com/feed/";
        title = "simpleflying"; }
      { tags = [ "astronomy/flight" ] ;
        url = "https://theaviationist.com/feed/";
        title = "aviationist"; }
    ];
  };

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
    };
  };

  services.spotifyd = {
    enable = true;
    settings = {
      global = {
        username = "jk43vcs9j36fn446gi97ucnhj";
        password_cmd = "pass spotify/password";
        device_name = "nix";
        use_mpris = true;
        bitrate = 160;
      };
    };
  };
}
