{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/9d9ea324-965f-43af-bba6-c45a225f8fd5";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/75EB-A750";
      fsType = "vfat";
    };

  fileSystems."/mnt/media2" =
    { device = "/dev/disk/by-uuid/1ee97da3-3b08-47bb-973a-5cf118df9f88";
      fsType = "btrfs";
      options = [ "subvol=root" "usebackuproot" "nofail" ];
    };

  fileSystems."/mnt/media" =
    { device = "/dev/disk/by-uuid/FAFA6314FA62CC87";
      fsType = "ntfs";
    };

  fileSystems."/mnt/win" =
    { device = "/dev/disk/by-uuid/6CAAB456AAB41E90";
      fsType = "ntfs";
      options = ["rw" "uid=1000"];
    };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
