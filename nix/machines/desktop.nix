{ config, pkgs, ... }:

let
  authorizedSshKeyFiles = [ "id_rsa" "id_rsa_github" "hetzner_rsa" ];
  enableFirewall = false;
  enableLight = false;
  enablePlex = true;
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
    (import ../modules/general.nix {
      inherit config pkgs enableFirewall enablePlex;
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

  # Detect other OS partitions  
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    version = 2;
    devices = [ "nodev" ];
    useOSProber = true;
  };

  environment.extraInit = ''
    xrandr --output DP-2 --mode 2560x1440 --rate 169 --left-of HDMI-0 --output HDMI-0 --mode 1920x1080 --rotate left
  '';
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?

}
