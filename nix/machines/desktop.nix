{
  config,
  options,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../modules/bluetooth.nix
    ../modules/boot-efi.nix
    ../modules/nix.nix
    ../modules/hardware.nix
    ../modules/keyboard.nix
    ../modules/general.nix
    ../modules/networking.nix
    ../modules/packages.nix
    ../modules/overlays.nix
    ../modules/printing.nix
    ../modules/programs.nix
    ../modules/services/jellyfin.nix
    ../modules/services/noisetorch.nix
    ../modules/services/polybar.nix
    ../modules/redshift.nix
    ../modules/sound.nix
    ../modules/ssh.nix
    ../modules/user.nix
    ../modules/desktop/wayland/hyprland.nix
  ];

  # Local modules
  local = {
    desktop.hyprland = {
      enable = true;
      enableNvidiaDriver = true;
      xwayland.enable = true;
    };

    hardware.opentabletdriver.enable = true;

    networking = {
      firewall.enable = true;
      networkmanager.enable = true;
    };

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
      noisetorch = {
        enable = true;
        device-id = "alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_REV8-00.analog-stereo";
        device-unit = "sys-devices-pci0000:00-0000:00:14.0-usb1-1\\x2d6-1\\x2d6:1.0-sound-card4-controlC4.device";
      };
      polybar = {
        enable = true;
        startup-script = ''
          MONITOR=DP-2 polybar primary -c /etc/polybar/config.ini &
          DEFAULT_NETWORK_INTERFACE=wlan0 MONITOR=HDMI-0 polybar secondary -c /etc/polybar/config.ini &
        '';
      };
    };
  };

  hardware.nvidia = {
    # Required for hyprland
    modesetting.enable = true;

    powerManagement.enable = false;
    powerManagement.finegrained = false;

    open = false;

    nvidiaSettings = true;

    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  networking = {
    hostName = "emma";
    useDHCP = false;
    interfaces.wlan0.useDHCP = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
