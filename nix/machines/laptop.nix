{ config, pkgs, ... }:

let
  enableLight = true;
  xserverConfig = {
    compositorConfig = {
      enable = true;
      vSync = true;
    };
    videoDrivers = [ "amdgpu" "radeon" "nouveau" "modesetting" "fbdev" ];
    xautolockTimer = 10;
    xrandrHeads = [ ];
  };
in {
  imports = [ # Include the results of the hardware scan.
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
  networking.hostName = "daisy";

  environment.variables = { MESA_LOADER_DRIVER_OVERRIDE = "iris"; };

  # Suspend on lid close
  services.logind.lidSwitch = "suspend";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?

}
