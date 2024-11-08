{
  config,
  lib,
  pkgs,
  options,
  ...
}: let
  cfg = config.local.networking;
in {
  options.local.networking = with lib; {
    firewall.enable = mkEnableOption "Enable firewall";
    networkmanager.enable = mkEnableOption "Enable Networkmanager";
  };
  config.networking = {
    firewall = lib.mkIf cfg.firewall.enable {
      enable = true;
      allowedTCPPorts = [53 80 443 3333 4568 8080 8096 8920 9630 44330 19000 19001 19002 19003];
      allowedUDPPorts = [53 80 443 9630 44330 19000 19001 19002 19003];
    };
    networkmanager = {
      enable = cfg.networkmanager.enable;
      plugins = with pkgs; [
        networkmanager-openconnect
        networkmanager-openvpn
      ];
      wifi = {
        # backend = "iwd";
        # Fixes bug with intermittent wifi disconnects when using iwd backend:
        # https://forums.gentoo.org/viewtopic-p-8776744.html?sid=47d852651f18223fa1d6d7ff49dadea3
        powersave = false;
      };
    };
    timeServers = options.networking.timeServers.default ++ ["ntp.example.com"];
  };
}
