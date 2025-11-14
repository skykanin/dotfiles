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
      jellyfin = {
        enable = true;
        user = config.local.user.name;
      };
    };

    # virtualisation.oci-containers.containers.openvpn-as = {
    #   autoStart = true;
    #   capabilities.NET_ADMIN = true;
    #   image = "openvpn/openvpn-as";
    #   ports = [
    #     "943:943/tcp"
    #     "443:443/tcp"
    #     "1194:1194/udp"
    #   ];
    #   privileged = true;
    #   volumes = ["/home/${config.local.user.name}/.config/openvpn-as:/openvpn"];
    # };
  };
}
