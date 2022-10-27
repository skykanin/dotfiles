{ config, pkgs, ... }:

{
  # Use the systemd-boot EFI boot loader.
  boot = {
    binfmt.emulatedSystems = [ "aarch64-linux" "riscv64-linux" ];
    loader = { 
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot";
      systemd-boot = {
        configurationLimit = 15; # limit boot entries
        enable = true;
      };
    };
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = [ "btrfs" "ntfs" ];
    extraModprobeConfig = ''
      options kvm_intel nested=1
      options kvm_intel emulate_invalid_guest_state=0
      options kvm ignore_msrs=1
    '';
  };
}
