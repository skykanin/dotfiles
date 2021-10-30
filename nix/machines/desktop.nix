{ config, pkgs, ... }:

let
  enableLight = false;
  xserverConfig = {
    compositorConfig = {
      enable = true;
      vSync = false;
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
        monitorConfig = ''
          Option "Rotate" "left"
        '';
      }
    ];
  };
in {
  imports = [
    ../modules/bluetooth.nix
    ../modules/boot-efi.nix
    ../modules/general.nix
    ../modules/packages.nix
    ../modules/printing.nix
    (import ../modules/programs.nix { inherit config pkgs enableLight; })
    ../modules/redshift.nix
    ../modules/sound.nix
    ../modules/ssh.nix
    ../modules/user.nix
    (import ../modules/xserver/xserver.nix
      ({ inherit config pkgs; } // xserverConfig))
  ];

  # Define hostname
  networking.hostName = "emma";

  # Detect other OS partitions  
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    version = 2;
    devices = [ "nodev" ];
    useOSProber = true;
  };

  environment.extraInit = ''
    xrandr --output DP-2 --mode 1920x1080 --rate 144 --left-of HDMI-0 --dpi 100
  '';
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?

}
