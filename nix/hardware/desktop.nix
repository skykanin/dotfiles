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
      device = "/dev/disk/by-uuid/9d9ea324-965f-43af-bba6-c45a225f8fd5";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/75EB-A750";
      fsType = "vfat";
    };

    "/mnt/media" = {
      device = "/dev/disk/by-uuid/FE22F44322F40307";
      fsType = "ntfs";
      options = ["rw" "uid=1000"];
    };

    "/mnt/media2" = {
      device = "/dev/disk/by-uuid/1ee97da3-3b08-47bb-973a-5cf118df9f88";
      fsType = "btrfs";
      options = ["subvol=root" "usebackuproot" "nofail"];
    };

    "/mnt/win" = {
      device = "/dev/disk/by-uuid/6CAAB456AAB41E90";
      fsType = "ntfs";
      options = ["rw" "uid=1000"];
    };

    "/mnt/drive" = {
      device = "google-drive:/";
      fsType = "rclone";
      options = [
        "nodev"
        "nofail"
        "allow_other"
        "args2env"
        # This config is purposely not managed by nix so that the API access
        # token remains secret.
        #
        # TODO: Start using agenix to manage secrets in a pure way
        "config=/home/${config.local.user.name}/.config/rclone/rclone.conf"
      ];
    };
  };

  swapDevices = [];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
