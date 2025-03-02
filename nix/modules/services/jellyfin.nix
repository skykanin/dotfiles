{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.services.jellyfin;
in {
  options.local.services.jellyfin = with lib; {
    enable = mkEnableOption "Enable Jellyfin service";
  };
  config = lib.mkIf cfg.enable {
    services = {
      caddy = {
        enable = false;
        enableReload = true;
        configFile = ./Caddyfile;
      };

      dnsmasq = {
        enable = false;
        settings = {
          address = "/jellyfin.lan/192.168.1.205";
        };
      };
      jellyfin = {
        enable = true;
        openFirewall = true;
        user = config.local.user.name;
      };
    };
  };
}
