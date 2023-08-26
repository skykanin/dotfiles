{ config, lib, pkgs, ... }:

let
  cfg = config.modules.user;
in {
  options.modules.user = with lib; {
    authorizedSshKeys = mkOption {
      type = types.listOf types.singleLineStr;
      default = [ ];
    };
    name = mkOption {
      type = types.str;
      default = "skykanin";
    };
    initialPassword = mkOption {
      type = types.str;
      default = "skykanin";
    };
  };

  config.users.users."${cfg.name}" = {
    isNormalUser = true;
    description = cfg.name;
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
    initialPassword = cfg.initialPassword; # Change with passwd later
    openssh.authorizedKeys = {
      keys = cfg.authorizedSshKeys;
    };
    shell = pkgs.fish;
  };
}
