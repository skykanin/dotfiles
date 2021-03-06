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
   
    gnupg = {
      agent = {
        enable = true;
        pinentryFlavor = "curses";
      };
      package = pkgs.gnupg;
    };
    
    java = {
      enable = true;
      package = pkgs.jdk;
      
    };

    vim.defaultEditor = true;
  };
}
