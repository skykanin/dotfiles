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
    services.jellyfin = {
      enable = true;
      openFirewall = true;
    };
  };
}
