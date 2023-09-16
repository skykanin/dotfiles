{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.desktop.waybar;
in {
  options.local.desktop.waybar = with lib; {};

  # TODO: Turn into systemd service perhaps?
  config.programs.waybar.enable = true;
}
