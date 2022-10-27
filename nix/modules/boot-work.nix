{ config, pkgs, ... }:

{
  # Use the systemd-boot EFI boot loader.
  boot = {
    binfmt.emulatedSystems = [ "aarch64-linux" "riscv64-linux" ];
    extraModulePackages = [ config.boot.kernelPackages.nvidia_x11 ];
    initrd = {
      luks.devices."root" = {
        device = "/dev/disk/by-uuid/d6131608-2843-4008-8263-6f6e130c2bb9";
        preLVM = true;
        keyFile = "/keyfile0.bin";
        allowDiscards = true;
      };

      secrets = { "keyfile0.bin" = "/etc/secrets/initrd/keyfile0.bin"; };
    };

    # Use latest available kernel
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };

      systemd-boot = {
        configurationLimit = 20; # limit boot entries
        enable = true;
      };
    };
  };
}
