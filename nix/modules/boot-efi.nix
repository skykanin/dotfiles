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
  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
    options kvm_intel emulate_invalid_guest_state=0
    options kvm ignore_msrs=1
  '';
}
