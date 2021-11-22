{ config, lib, pkgs, ... }:

let
  cfg = config.services.custom.polybar;
  polybar-config = lib.strings.readFile ../../../dots/polybar/config.ini;

in with lib; {
  options.services.custom.polybar = {
    enable = mkEnableOption "Enable polybar service";
    extraConfig = mkOption {
      type = lib.types.lines;
      default = "";
    };
    package = mkOption {
      type = lib.types.package;
      default = pkgs.polybarFull;
    };
  };

  config = mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."polybar/config.ini".text = polybar-config;
    };

    services.dbus = { enable = true; };

    systemd.user.services.polybar = {
      description = "Polybar status bar";
      path = [
        pkgs.coreutils
        pkgs.dbus
        cfg.package
        pkgs.gnugrep
        pkgs.xorg.xrandr
        pkgs.procps
      ];
      serviceConfig = {
        Type = "forking";
        ExecStart = let
          script = ''
            MONITOR=DP-2 polybar primary -c /etc/polybar/config.ini &
            MONITOR=HDMI-0 polybar secondary -c /etc/polybar/config.ini &
          '';
          scriptPkg = pkgs.writeShellScriptBin "polybar-start" script;
        in "${scriptPkg}/bin/polybar-start";
        Restart = "on-failure";
      };
      wantedBy = [ "graphical-session.target" ];
    };
  };
}
