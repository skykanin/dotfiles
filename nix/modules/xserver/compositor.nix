{ enable, vSync }:

# Picom compositor configuration
{
  backend = "glx";
  inherit enable;
  refreshRate = 0; # automatically detect monitor refresh rate
  activeOpacity = 1.0;
  inactiveOpacity = 1.0;
  menuOpacity = 0.9;
  experimentalBackends = true;
  settings = {
    blur = {
      method = "gaussian";
      size = 30;
      deviation = 5.0;
    };
    mark-overdir-focused = true;
  };
  inherit vSync;
}