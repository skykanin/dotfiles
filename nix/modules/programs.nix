{ config, pkgs, enableLight, ... }:

{
  programs = {
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
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    java = {
      enable = true;
      package = pkgs.jdk;
    };

    light.enable = enableLight;

    vim.defaultEditor = true;
  };
}
