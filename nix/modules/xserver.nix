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
      autoRepeatDelay = 200;
      autoRepeatInterval = 10;
      autorun = true;
      
      enable = true;
      layout = "no";
      libinput = {
        enable = true;
        accelProfile = "flat";
        naturalScrolling = true;
      };


      desktopManager = {
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
	defaultSession = "none+i3";
      };
      
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

      xautolock = {
        enable = true;
        locker = "${pkgs.betterlockscreen}/bin/betterlockscreen -l dim";
        time = 10;
      };
    };
  };
}
