{ config, pkgs, ... }:

{
  # Use the systemd-boot EFI boot loader.
  boot.loader = { 
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      configurationLimit = 5; # limit boot entries
      enable = true;
    };
  };
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.supportedFilesystems = [ "btrfs" "ntfs" ];
}
