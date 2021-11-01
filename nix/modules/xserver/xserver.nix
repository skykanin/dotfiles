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
        mouse.disableWhileTyping = true;
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
        defaultSession = "none+xmonad";
        #startx.enable = true;
      };

      exportConfiguration = true;

      inherit videoDrivers;

      wacom.enable = true;

      windowManager.xmonad = {
        enable = true;
        extraPackages = xmonadPackage: [ xmonadPackage.xmonad-contrib ];
        ghcArgs = [ "-Werror" "-Wall" "-Wno-missing-signatures" ];
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
