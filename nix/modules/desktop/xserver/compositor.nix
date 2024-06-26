{vSync}:
# Picom compositor configuration
{
  backend = "xrender";
  enable = true;
  activeOpacity = 1.0;
  inactiveOpacity = 1.0;
  menuOpacity = 0.9;
  settings = {
    blur = {
      method = "gaussian";
      size = 30;
      deviation = 5.0;
    };
    blur-background-exclude = [
      "window_type = 'dock'"
      "name ~= 'slop'"
      "_GTK_FRAME_EXTENTS@:c"
      "name ~= 'firefox-devedition'"
      "name ~= 'firefox'"
    ];
    mark-overdir-focused = true;
  };
  inherit vSync;
}
