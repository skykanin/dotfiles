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
      allowedTCPPorts = [80 8080 3333 4568 9630 44330 19000 19001 19002 19003];
      allowedUDPPorts = [80 9630 44330 19000 19001 19002 19003];
    };
    # nameservers = [ "9.9.9.11" "8.8.8.8" ];
    networkmanager = {
      enable = cfg.networkmanager.enable;
      wifi.backend = "iwd";
    };
    timeServers = options.networking.timeServers.default ++ ["ntp.example.com"];
  };
}
