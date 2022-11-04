{ config, lib, pkgs, ... }:

let
  authorizedSshKeyFiles = [ "id_rsa" "id_rsa_github" "hetzner_rsa" ];
  enableFirewall = true;
  enableNetworkmanager = true;
  enableLight = false;
  enableJellyfin = true;
  polybar-script = ''
    MONITOR=DP-2 polybar primary -c /etc/polybar/config.ini &
    MONITOR=HDMI-0 polybar secondary -c /etc/polybar/config.ini &
  '';
  noisetorchConfig = {
    enable = true;
    device-unit =
      "sys-devices-pci0000:00-0000:00:14.0-usb1-1\\x2d6-1\\x2d6:1.0-sound-card4-controlC4.device";
    device-id =
      "alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_REV8-00.analog-stereo";
  };
  threads = 2;
  xserverConfig = {
    compositorConfig = {
      enable = true;
      vSync = true;
    };
    videoDrivers = [ "nvidia" ];
    xautolockTimer = 20;
    xrandrHeads = [
      {
        output = "DP-2";
        primary = true;
      }
      {
        output = "HDMI-0";
      }
    ];
  };
in {
  imports = [
    ../modules/bluetooth.nix
    ../modules/boot-efi.nix
    (import ../modules/general.nix {
      inherit config pkgs enableFirewall enableNetworkmanager enableJellyfin
        noisetorchConfig polybar-script threads;
    })
    ../modules/packages.nix
    ../modules/printing.nix
    (import ../modules/programs.nix { inherit config pkgs enableLight; })
    ../modules/redshift.nix
    ../modules/sound.nix
    (import ../modules/ssh.nix { inherit config pkgs authorizedSshKeyFiles; })
    (import ../modules/user.nix { inherit config pkgs authorizedSshKeyFiles; })
    (import ../modules/xserver/xserver.nix
      ({ inherit config pkgs; } // xserverConfig))
  ];

  # Define hostname
  networking.hostName = "emma";

  networking = {
    useDHCP = false;
    interfaces.wlp4s0.useDHCP = true;
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
