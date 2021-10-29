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

      xautolock =
        let lock-script = pkgs.writeShellScript "lock-script" ''
          ${pkgs.i3lock-color}/bin/i3lock-color -k -u -c 00000000 --date-str='%A, %B %Y' \
            --time-color=ff005acc --date-color=ff005acc --time-size=60 --date-size=20
        '';
        in {
        enable = true;
        locker = toString lock-script;
        nowlocker = toString lock-script;
        time = 10;
      };

      xkbOptions = "caps:escape,eurosign:e,compose:ralt";
    };
  };
}
