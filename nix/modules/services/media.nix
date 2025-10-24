{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.services.media;
in {
  options.local.services.media = with lib; {
    enable = mkEnableOption "Enable media service";
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
          address = ["/jellyfin.lan/192.168.1.205"];
          conf-file = null;
          # server = [ "8.8.8.8" "8.8.4.4" ];
          # local = "/lan/";
          # domain = "lan";
          # expand-hosts = true;
          port = "5353";
        };
      };

      # Port 8096
      jellyfin = {
        enable = true;
        openFirewall = true;
        user = config.local.user.name;
      };

      # Port 5055
      jellyseerr = {
        enable = true;
        openFirewall = true;
      };

      # Port 7878
      radarr = {
        enable = true;
        openFirewall = true;
      };

      # Port 8989
      sonarr = {
        enable = true;
        openFirewall = true;
      };

      # Port 9696
      prowlarr = {
        enable = true;
        openFirewall = true;
      };

      # Port 8191
      flaresolverr = {
        enable = false;
        openFirewall = true;
        package = pkgs.flaresolverr.overrideAttrs (prev: {
          src = pkgs.fetchFromGitHub {
            owner = "FlareSolverr";
            repo = "FlareSolverr";
            rev = "8b1851eeb16cd88063394205ba2b893090a69db9";
            hash = "sha256-QOMpPg+0DWZLmoUWrAebipHsTxKdxbF0IFH6pRn8oVA=";
          };

          meta = prev.meta // {broken = false;};
        });
      };
    };

    virtualisation.oci-containers.containers.openvpn-as = {
      autoStart = true;
      capabilities.NET_ADMIN = true;
      image = "openvpn/openvpn-as";
      ports = [
        "943:943/tcp"
        "443:443/tcp"
        "1194:1194/udp"
      ];
      privileged = true;
      volumes = ["/home/${config.local.user.name}/.config/openvpn-as:/openvpn"];
    };
  };
}
