{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.desktop.hyprland;
in {
  imports = [
    ./waybar.nix
  ];

  options.local.desktop.hyprland = with lib; {
    enable = mkEnableOption "Enable hyprland module";
    xwayland.enable = mkEnableOption "Enable XWayland";
  };

  config = lib.mkIf cfg.enable {
    # Simple login manager
    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${lib.getExe config.programs.hyprland.package}";
          user = config.local.user.name;
        };
      };
    };

    # Make pam module for swaylock
    security.pam.services.swaylock = {
      text = ''
        auth include login
      '';
    };

    programs.hyprland = {
      enable = true;
      xwayland.enable = cfg.xwayland.enable;
      # Options are bugged so we need to override the package ourselves
      package = pkgs.hyprland.override {
        enableXWayland = cfg.xwayland.enable;
      };
    };

    # Additional packages
    environment.systemPackages = with pkgs; [
      fribidi # used in statusbar spotify script
      grim
      rofi-wayland
      slurp
      swaybg
      swayidle
      swaylock-effects
      wl-clipboard
      wlr-randr
    ];
  };
}
