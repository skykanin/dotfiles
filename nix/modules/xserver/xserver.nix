# Configuration function for setting up xserver options
{ config, pkgs, compositorConfig, videoDrivers, xautolockTimer, xrandrHeads, ...
}: {
  services = {
    autorandr.enable = true;
    picom = import ./compositor.nix compositorConfig;
    xserver = {
      autoRepeatDelay = 200;
      autoRepeatInterval = 10;
      autorun = true;

      enable = true;
      layout = "us";
      libinput = {
        enable = true;
        mouse = {
          accelProfile = "flat";
          disableWhileTyping = true;
        };
        touchpad = {
          accelProfile = "adaptive";
          naturalScrolling = false;
        };
      };

      desktopManager = { xterm.enable = true; };

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
        #startx.enable = true;
      };

      exportConfiguration = true;

      inherit videoDrivers;

      wacom.enable = true;

      windowManager.i3 = {
        enable = true;
        configFile = "/home/skykanin/.config/i3/config";
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [ rofi polybarFull ];
      };

      xautolock = import ./xautolock.nix {
        inherit pkgs;
        enable = true;
        time = xautolockTimer;
      };
      xkbOptions = "caps:escape,eurosign:e,compose:rctrl,compose:ralt";

      inherit xrandrHeads;
    };
  };
}
