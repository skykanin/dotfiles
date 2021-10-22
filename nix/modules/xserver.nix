{ config, pkgs, ... }: {
  services = {
    picom = {
      backend = "glx";
      enable = true;
      refreshRate = 0; # automatically detect monitor refresh rate
      activeOpacity = 1.0;
      inactiveOpacity = 1.0;
      menuOpacity = 0.9;
      experimentalBackends = true;
      settings = {
        # blur-background = true;
        # blur-background-frame = true;
        # blur-background-fixed = true;
        blur = {
          method = "gaussian";
          size = 30;
          deviation = 5.0;
        };
        mark-overdir-focused = true;
      };
    };

    xserver = {
      autoRepeatDelay = 200;
      autoRepeatInterval = 10;
      autorun = true;

      enable = true;
      layout = "us";
      libinput = {
        enable = true;
        touchpad.accelProfile = "adaptive";
      };

      desktopManager = { xterm.enable = false; };

      displayManager = {

        autoLogin = {
          enable = true;
          user = "skykanin";
        };

        lightdm = {
          enable = true;
          greeter.enable = false;
        };
        defaultSession = "none+i3";
        
        startx.enable = false;
      };

      windowManager.i3 = {
        enable = true;
        configFile = "/home/skykanin/.config/i3/config";
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [ i3lock-color rofi polybarFull ];
      };

      xautolock = {
        enable = false;
        locker = "${pkgs.betterlockscreen}/bin/betterlockscreen -l dim";
        time = 30;
      };

      xkbOptions = "caps:escape,eurosign:e,compose:ralt";
    };
  };
}
