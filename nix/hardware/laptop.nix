# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/0c13b581-b9c9-4975-9b15-9aef105b22a0";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/42B6-4A87";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/067cba29-d7c3-486d-af5f-2bafc201ad51"; }
    ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}