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
    startup-script = mkOption {
      type = lib.types.lines;
      default = ''
        polybar primary -c /etc/polybar/config.ini &
      '';
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
        pkgs.fribidi
      ];
      restartTriggers = [ config.environment.etc."polybar/config.ini".source ];
      script = cfg.startup-script;
      serviceConfig = {
        Type = "forking";
        Restart = "always";
      };
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
    };
  };
}
