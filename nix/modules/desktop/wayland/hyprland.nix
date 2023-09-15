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
    services.greetd = {
        enable = true;
        settings = {
          default_session = {
            command = with pkgs; "${lib.makeBinPath [ hyprland ]}/${hyprland.meta.mainProgram}";
            user = config.local.user.name;
          };
        };
      };

    programs.hyprland = {
      enable = true;
      inherit (cfg) enableNvidiaPatches;
      xwayland.enable = cfg.xwayland.enable;
    };

    # Additional packages
    environment.systemPackages = with pkgs; [
      fribidi # used in statusbar spotify script
      rofi-wayland
      swaybg
      swayidle
      swaylock-effects
      wl-clipboard
      wlr-randr
    ];
  };
}
