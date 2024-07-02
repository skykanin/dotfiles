{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.programs;
in {
  options.local.programs = with lib; {
    light.enable = mkEnableOption "Enable Light";
    steam.enable = mkEnableOption "Enable Steam";
  };

  config.programs = {
    fuse.userAllowOther = true;

    light.enable = cfg.light.enable;

    gnupg.agent.pinentryPackage = pkgs.pinentry-curses;

    java = {
      enable = true;
      package = pkgs.openjdk21;
    };

    nh.enable = true;

    steam = lib.mkIf cfg.steam.enable {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    vim = {
      defaultEditor = true;
      package = lib.mkDefault pkgs.vim-with-conf;
    };
  };
}
