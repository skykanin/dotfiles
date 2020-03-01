{ config, pkgs, ... }:

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
    
    java = {
      enable = true;
      package = pkgs.openjdk11;
    };
  };
}
