{ config, pkgs, ... }:
{
  services = {
    compton = {
      backend = "glx";
      enable = true;
      refreshRate = 0; # automatically detect monitor refresh rate
      activeOpacity = "1.0";
      inactiveOpacity = "1.0";      
    };
    
    xserver = {
      autoRepeatDelay = 280;
      autoRepeatInterval = 10;
      enable = true;
      layout = "no";
      libinput = {
        enable = true;
        accelProfile = "flat";
        naturalScrolling = true;
      };
      autorun = true;
      
      windowManager.default = "i3";
      windowManager.i3 = {
        enable = true;
        configFile = /home/skykanin/.config/i3/config;
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [
          rofi
          polybarFull
          betterlockscreen
        ];
      };
      
      desktopManager = {
        default = "none";
        xterm.enable = false;
      };
      
      displayManager = {
        lightdm = {
          autoLogin = {
            enable = true;
            user = "skykanin";
          };
          enable = true;
          greeter.enable = false;
        };
      };
    };
  };
}
