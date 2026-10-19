{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.programs;
in {
  options.local.programs = with lib; {
    steam.enable = mkEnableOption "Enable Steam";
  };

  config.programs = {
    command-not-found.enable = !config.programs.nix-index.enable;
    fuse.userAllowOther = true;

    gnupg.agent.pinentryPackage = pkgs.pinentry-gnome3;

    java = {
      enable = true;
      package = pkgs.openjdk21;
    };

    nix-index.enable = true;

    steam = lib.mkIf cfg.steam.enable {
      enable = true;
      extraPackages = [pkgs.SDL2];
      extraCompatPackages = [pkgs.proton-ge-bin];
      protontricks.enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      gamescopeSession.enable = true;
    };

    gamescope.enableWsi = true;

    vim = {
      enable = true;
      defaultEditor = true;
      package = lib.mkDefault pkgs.vim-with-conf;
    };
  };
}
