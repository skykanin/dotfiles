{ config, options, lib, pkgs, ... }:

let
  enableFirewall = true;
  enableNetworkmanager = true;
  xserverConfig = {
    compositorConfig = {
      enable = false;
      vSync = true;
    };
    videoDrivers = [ "nvidia" ];
    xautolockTimer = 20;
    xrandrHeads = [
      {
        output = "DP-2";
        primary = true;
      }
      { output = "HDMI-0"; }
    ];
  };
in {
  imports = [
    ../modules/bluetooth.nix
    ../modules/boot-efi.nix
    ../modules/nix.nix
    ../modules/hardware.nix
    (import ../modules/general.nix {
      inherit config options pkgs enableFirewall enableNetworkmanager;
    })
    ../modules/packages.nix
    ../modules/printing.nix
    ../modules/programs.nix
    ../modules/services/jellyfin.nix
    ../modules/services/polybar.nix
    ../modules/redshift.nix
    ../modules/sound.nix
    ../modules/ssh.nix
    ../modules/user.nix
    (import ../modules/xserver/xserver.nix
      ({ inherit config pkgs; } // xserverConfig))
  ];

  # Local modules
  local = {
    hardware.opentabletdriver.enable = true;
    nix = {
      extra-substituters = [
        "https://iohk.cachix.org"
      ];
      extra-trusted-public-keys = [
        "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      ];
    };
    programs.steam.enable = true;

    services = {
      jellyfin.enable = true;
      polybar = {
        enable = true;
        startup-script = ''
          MONITOR=DP-2 polybar primary -c /etc/polybar/config.ini &
          DEFAULT_NETWORK_INTERFACE=wlan0 MONITOR=HDMI-0 polybar secondary -c /etc/polybar/config.ini &
        '';
      };
    };
  };

  networking = {
    hostName = "emma";
    useDHCP = false;
    interfaces.wlan0.useDHCP = true;
  };

  environment.extraInit = ''
    xrandr --output DP-2 --mode 2560x1440 --rate 169 --output HDMI-0 --mode 2560x1440 --rotate normal --rate 74 --right-of DP-2
  '';
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
