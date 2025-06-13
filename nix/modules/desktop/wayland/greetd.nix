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
    programs.regreet = {
      enable = true;

      theme = {
        package = pkgs.tokyonight-gtk-theme;
        name = "Tokyonight-Dark";
      };

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
          cfg-rgt = config.programs.regreet;
          sway-config =
            pkgs.writeTextFile
            {
              name = "regreet-sway-config";
              text = ''
                # Notice that `swaymsg exit` will run after ReGreet.
                exec "${lib.getExe cfg-rgt.package}; swaymsg exit"

                bindsym Mod4+shift+e exec swaynag \
                -t warning \
                -m 'What do you want to do?' \
                -b 'Poweroff' 'systemctl poweroff' \
                -b 'Reboot' 'systemctl reboot'

                output DP-1 {
                    resolution 2560x1440@169.83Hz
                    position 2560 0
                    render_bit_depth 8
                }

                include /etc/sway/config.d/*
              '';
            };
        in {
          command = "${lib.getExe pkgs.sway} --config ${sway-config}";
        };
      };
    };
  };
}
