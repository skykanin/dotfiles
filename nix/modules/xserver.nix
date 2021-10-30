# Configuration function for setting up xserver options
{ config, pkgs, compositorConfig, videoDrivers, xautolockTimer, xrandrHeads, ...
}: {
  services = {
    picom = import ./compositor.nix compositorConfig;
    xserver = {
      autoRepeatDelay = 200;
      autoRepeatInterval = 10;
      autorun = true;

      enable = true;
      layout = "us";
      libinput = {
        enable = true;
        touchpad = {
          accelProfile = "adaptive";
          naturalScrolling = false;
        };
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

      exportConfiguration = true;

      inherit videoDrivers;

      wacom.enable = true;

      windowManager.i3 = {
        enable = true;
        configFile = "/home/skykanin/.config/i3/config";
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [ i3lock-color rofi polybarFull ];
      };

      xautolock = import ./xautolock.nix {
        inherit pkgs;
        enable = true;
        time = 20;
      };
      xkbOptions = "caps:escape,eurosign:e,compose:ralt";

      inherit xrandrHeads;
    };
  };
}
