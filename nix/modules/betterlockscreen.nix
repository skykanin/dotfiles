{ config, pkgs, ... }:

{
  # Define systemd service for betterlockscreen to run on suspend/sleep
  # A bit janky cause I had to manually cache it the first time for root cache
  systemd.services.betterlockscreen = {
    enable = true;
    aliases = [ "betterlockscreen@skykanin.service" ];
    description = "Locks screen when going to sleep/suspend";
    environment = { DISPLAY = ":0"; };
    serviceConfig = {
      User = "skykanin";
      Type = "simple";
      ExecStart = ''${pkgs.betterlockscreen}/bin/betterlockscreen -l dim'';
      TimeoutSec = "infinity";
    };
    wantedBy = [ "sleep.target" "suspend.target" ];
  };
}
