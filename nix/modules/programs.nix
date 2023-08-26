{ config, lib, pkgs, ... }:

let
  cfg = config.module.programs;
in
{
  options.module.programs = with lib; {
    light.enable = mkEnableOption "Enable Light";
    steam.enable = mkEnableOption "Enable Steam";
  };

  config.programs = {
    fish = {
      enable = true;
      vendor = {
        completions.enable = true;
        config.enable = true;
        functions.enable = true;
      };
    };

    fuse.userAllowOther = true;

    gnupg = {
      agent = {
        enable = true;
        pinentryFlavor = "curses";
      };
      package = pkgs.gnupg;
    };

    steam = {
      enable = cfg.steam.enable;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    java = {
      enable = true;
      package = pkgs.jdk;
    };

    light.enable = cfg.light.enable;

    vim.defaultEditor = true;
  };
}
