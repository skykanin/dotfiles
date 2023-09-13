{ config, lib, pkgs, ... }:

let
  cfg = config.local.desktop.hyprland;
in
{
  imports = [
    ./waybar.nix
  ];

  options.local.desktop.hyprland = with lib; {
    enable = mkEnableOption "Enable hyprland module";
    enableNvidiaPatches = mkEnableOption "Enable Nvidia GPU support";
    xwayland.enable = mkEnableOption "Enable XWayland";
  };

  config = lib.mkIf cfg.enable {
    programs.hyprland = {
      enable = true;
      inherit (cfg) enableNvidiaPatches;
      xwayland.enable = cfg.xwayland.enable;
    };

    # Additional packages
    environment.systemPackages = with pkgs; [
      hyprpaper
      rofi-wayland
      swayidle
      swaylock-effects
      wl-clipboard
      wlr-randr
    ];
  };
}
