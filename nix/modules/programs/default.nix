{
  config,
  lib,
  pkgs,
  ...
}: {
  config.programs = {
    direnv.enable = true;

    gnupg.agent.enable = true;

    fish = {
      enable = true;
      vendor = {
        completions.enable = true;
        config.enable = true;
        functions.enable = true;
      };
    };


  };
}
