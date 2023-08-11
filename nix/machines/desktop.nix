{ config, options, lib, pkgs, ... }:

let
  enableFirewall = true;
  enableNetworkmanager = true;
  enableLight = false;
  enableSteam = true;
  enableJellyfin = true;
  polybarConfig = {
    enable = true;
    startup-script = ''
      MONITOR=DP-2 polybar primary -c /etc/polybar/config.ini &
      DEFAULT_NETWORK_INTERFACE=wlan0 MONITOR=HDMI-0 polybar secondary -c /etc/polybar/config.ini &
    '';
  };
  noisetorchConfig = {
    enable = true;
    device-unit =
      "sys-devices-pci0000:00-0000:00:14.0-usb1-1\\x2d6-1\\x2d6:1.0-sound-card3-controlC3.device";
    device-id =
      "alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_REV8-00.analog-stereo";
  };
  threads = 2;
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
    (import ../modules/general.nix {
      inherit config options pkgs enableFirewall enableNetworkmanager enableJellyfin
        noisetorchConfig polybarConfig threads;
    })
    ../modules/packages.nix
    ../modules/printing.nix
    (import ../modules/programs.nix { inherit config pkgs enableLight enableSteam; })
    ../modules/redshift.nix
    ../modules/sound.nix
    (import ../modules/ssh.nix { inherit config pkgs; })
    (import ../modules/user.nix { inherit config pkgs; })
    (import ../modules/xserver/xserver.nix
      ({ inherit config pkgs; } // xserverConfig))
  ];

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
