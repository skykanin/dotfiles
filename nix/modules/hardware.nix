{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.hardware;
in {
  options.local.hardware = with lib; {
    opengl.enable =
      (mkEnableOption "Enable OpenGL") // {default = true;};
    opentabletdriver.enable =
      mkEnableOption "Enable opentable driver";
  };

  config.hardware = {
    opengl = {
      driSupport = true;
      enable = cfg.opengl.enable;
    };
    opentabletdriver.enable = cfg.opentabletdriver.enable;
  };
}
