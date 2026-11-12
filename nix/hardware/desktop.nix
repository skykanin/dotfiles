{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "sd_mod"];
  boot.initrd.kernelModules = ["amdgpu"];
  boot.initrd.network.enable = true;
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  hardware = {
    enableRedistributableFirmware = true;
    enableAllFirmware = true;
    firmware = [pkgs.wireless-regdb];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/9dd1d47f-6a70-4170-818a-f3bbc4849f68";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/6170-440E";
      fsType = "vfat";
    };

    "/mnt/media" = {
      device = "/dev/disk/by-uuid/cca84990-1e86-400e-a347-8c2e14787d8f";
      fsType = "ext4";
      options = ["noatime" "nofail"];
    };

    "/mnt/media2" = {
      device = "/dev/disk/by-uuid/1ee97da3-3b08-47bb-973a-5cf118df9f88";
      fsType = "btrfs";
      options = ["subvol=root" "nofail"];
    };

    "/mnt/drive" = {
      device = "google-drive:/";
      fsType = "rclone";
      options = [
        "nodev"
        "nofail"
        "allow_other"
        "args2env"
        "config=${config.age.secrets."rclone.conf".path}"
      ];
    };
  };

  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 32 * 1024; # 32GB
    }
  ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
