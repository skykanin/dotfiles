{
  config,
  lib,
  ...
}: let
  cfg = config.local.desktop.greetd;
in {
  options.local.desktop.greetd.enable =
    lib.mkEnableOption "Enable greetd login service";

  config = lib.mkIf cfg.enable {
    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          command = let wm =
            if config.local.desktop.hyprland.enable
            then config.programs.hyprland.package
            else config.programs.sway.package;
            in "${lib.getExe wm} 2> ~/sway.log";
          user = config.local.user.name;
        };
      };
    };
  };
}
