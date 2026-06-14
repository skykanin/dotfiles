{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.desktop.niri;
  inherit (lib) lists;
in {
  options.local.desktop.niri = with lib; {
    enable = mkEnableOption "Enable niri module";
    xwayland.enable = mkEnableOption "Enable XWayland support";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; ([
      cliphist
      grim
      obs-cmd
      slurp
      swappy
      wl-clipboard
      wlr-randr
    ] ++ lists.optional
           cfg.xwayland.enable
           xwayland-satellite
    );

    programs.niri.enable = true;
    programs.noctalia = {
      enable = true;
      systemd.enable = true;
    };

    xdg.portal = {
      enable = true;
      config = {
        default = {
          "org.freedesktop.impl.portal.Secret" = ["gnome-keyring"];
          "org.freedesktop.portal.FileChooser" = ["xdg-desktop-portal-gtk"];
        };
      };
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-gnome
      ];
    };
  };
}
