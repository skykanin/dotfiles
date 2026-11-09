{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.desktop.greetd;
in {
  options.local.desktop.greetd.enable =
    lib.mkEnableOption "Enable greetd login service";

  config = lib.mkIf cfg.enable {
    services.displayManager.regreet = {
      enable = true;

      iconTheme = {
        package = pkgs.numix-icon-theme;
        name = "Numix Circle";
      };

      settings = {
        background = {
          path = "/home/skykanin/Pictures/wallpapers/planets.jpg";
          fit = "Contain";
        };

        env.ELECTRON_OZONE_PLATFORM_HINT = "wayland";
      };
    };

    services.greetd = {
      enable = true;
      settings = {
        default_session = let
          cfg-rgt = config.services.displayManager.regreet;
          niri-config = pkgs.writeText "regreet-niri-config.kdl" ''
            ${builtins.readFile ../../../../dots/niri/outputs.kdl}
            hotkey-overlay { skip-at-startup; }
            spawn-at-startup "sh" "-c" "${lib.getExe cfg-rgt.package}; niri msg action quit --skip-confirmation"
            cursor { xcursor-size 20; }
          '';
        in {
          command = "${lib.getExe config.programs.niri.package} -c ${niri-config}";
        };
      };
    };
  };
}
