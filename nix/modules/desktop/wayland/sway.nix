{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.desktop.sway;
in {
  options.local.desktop.sway = with lib; {
    enable = mkEnableOption "Enable sway module";
    xwayland.enable = mkEnableOption "Enable XWayland support";
  };

  config = lib.mkIf cfg.enable {
    # Make pam module for swaylock
    security.pam.services.swaylock.text = ''
      auth include login
    '';

    programs = {
      sway = {
        enable = true;
        xwayland.enable = cfg.xwayland.enable;

        extraPackages = with pkgs; [
          grim
          obs-cmd
          rofi-wayland
          slurp
          swaybg
          swayidle
          swaylock-effects
          wl-clipboard
          wofi
        ];

        extraSessionCommands = ''
          # SDL:
          export SDL_VIDEODRIVER=wayland
          # QT (needs qt5.qtwayland in systemPackages):
          export QT_QPA_PLATFORM=wayland-egl
          export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
          # Fix for some Java AWT applications (e.g. Android Studio),
          # use this if they aren't displayed properly:
          export _JAVA_AWT_WM_NONREPARENTING=1
        '';

        wrapperFeatures = {
          base = true;
          gtk = true;
        };
      };

      # Enable statusbar
      waybar.enable = true;
    };

    xdg.portal = {
      enable = true;
      config = {
        default = {
          "org.freedesktop.impl.portal.Secret" = [ "gnome-keyring" ];
          "org.freedesktop.portal.FileChooser" = [ "xdg-desktop-portal-gtk" ];
        };
      };
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };

    # Additional packages
    environment.systemPackages = with pkgs; [
      fribidi # used in statusbar spotify script
      wlr-randr
    ];
  };
}
