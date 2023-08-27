{
  config,
  options,
  pkgs,
  ...
}: let
  xserverConfig = {
    compositorConfig = {
      enable = true;
      vSync = true;
    };
    videoDrivers = ["amdgpu" "radeon" "nouveau" "modesetting" "fbdev"];
    xautolockTimer = 10;
    xrandrHeads = [];
  };
in {
  imports = [
    # Include the results of the hardware scan.
    ../modules/bluetooth.nix
    ../modules/boot-efi.nix
    ../modules/nix.nix
    ../modules/hardware.nix
    ../modules/general.nix
    ../modules/networking.nix
    ../modules/packages.nix
    ../modules/printing.nix
    ../modules/programs.nix
    ../modules/overlays.nix
    ../modules/services/polybar.nix
    ../modules/redshift.nix
    ../modules/sound.nix
    ../modules/ssh.nix
    ../modules/user.nix
    (import ../modules/xserver/xserver.nix
      ({inherit config pkgs;} // xserverConfig))
  ];

  # Local modules
  local = {
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

    programs = {
      light.enable = true;
      steam.enable = true;
    };

    services = {
      polybar = {
        enable = true;
        startup-script = ''
          MONITOR=eDP-1 polybar primary -c /etc/polybar/config.ini &
        '';
      };
    };
  };

  networking.hostName = "daisy";

  environment.variables = {MESA_LOADER_DRIVER_OVERRIDE = "iris";};

  # Suspend on lid close
  services.logind.lidSwitch = "suspend";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
