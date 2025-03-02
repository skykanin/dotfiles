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

      # cageArgs = [ "-s" "-m" "last" ];

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
        default_session = let cfg-rgt = config.programs.regreet; in {
          command = "${lib.getExe pkgs.gamescope} -H 1440 -O DP-1 -f --backend drm -- ${lib.getExe cfg-rgt.package}";
          user = "greeter";
        };
      };
    };
  };
}
