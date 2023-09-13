# Configuration function for setting up xserver options
{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.services.xserver;
in {
  options.local.services.xserver = with lib; {
    compositor = {
      enable = mkEnableOption "Enable xserver compositor";
      vSync = mkEnableOption "Enable vSync compositor";
    };

    xautolock = {
      enable = mkEnableOption "Enable autolock";
      time = mkOption {
        type = types.ints.positive;
        description = "Minutes before screen is locked";
        default = 20;
      };
    };
  };
  config.services = {
    autorandr.enable = true;
    picom =
      lib.mkIf cfg.compositor.enable
      (import ./compositor.nix {inherit (cfg.compositor) vSync;});
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

      desktopManager.xterm.enable = true;

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
      };

      exportConfiguration = true;

      wacom.enable = true;

      windowManager.i3 = {
        enable = true;
        configFile = "/home/skykanin/.config/i3/config";
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [i3lock rofi polybarFull];
      };

      xautolock = lib.mkIf cfg.xautolock.enable (import ./xautolock.nix {
        inherit (pkgs) i3lock writeShellScript;
        time = cfg.xautolock.time;
      });

      xkbOptions = "caps:escape,eurosign:e,compose:rctrl,compose:ralt";
    };
  };
}
