{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [wev];

  services.keyd = {
    enable = true;
    keyboards.default = {
      ids = ["*"];
      settings.main = {
        delete = "pageup";
        home = "pagedown";
      };
    };
  };
}
