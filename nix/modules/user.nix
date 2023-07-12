{ config, pkgs, authorizedSshKeys ? [ ], authorizedSshKeyFiles ? [ ], ... }:

let username = "skykanin";
in {
  users.users.skykanin = {
    isNormalUser = true;
    description = username;
    extraGroups = [
      "wheel"
      "audio"
      "docker"
      "libvirtd"
      "kvm"
      "video"
      "networkmanager"
      "postgres"
    ];
    initialPassword = "skykanin"; # Change with passwd later
    openssh.authorizedKeys = {
      keys = authorizedSshKeys;
    };
    shell = pkgs.fish;
  };
}
