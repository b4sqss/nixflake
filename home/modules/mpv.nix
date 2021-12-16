{ config, pkgs, ... }:

{
  programs.mpv = {
    # package = pkgs.wrapMpv (pkgs.mpv-unwrapped.override { vapoursynthSupport = true; }) { youtubeSupport = true; };
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
}
