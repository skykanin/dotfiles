{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.desktop.hyprland;
in {
  options.local.desktop.hyprland = with lib; {
    enable = mkEnableOption "Enable hyprland module";
    xwayland.enable = mkEnableOption "Enable XWayland";
  };

  config = lib.mkIf cfg.enable {
    # Make pam module for swaylock
    security.pam.services.swaylock.text = ''
      auth include login
    '';

    programs = {
      hyprland = {
        enable = true;
        xwayland.enable = cfg.xwayland.enable;
        # Options are bugged so we need to override the package ourselves
        package = pkgs.hyprland.override {
          enableXWayland = cfg.xwayland.enable;
        };
      };

      # Enable bar
      waybar.enable = true;
    };

    xdg.portal.enable = true;

    # Additional packages
    environment.systemPackages = with pkgs; [
      fribidi # used in statusbar spotify script
      grim
      obs-cmd
      rofi
      slurp
      swaybg
      swayidle
      swaylock-effects
      wl-clipboard
      wlr-randr
    ];
  };
}
