{
  config,
  lib,
  pkgs,
  ...
}: {
  config.programs = {
    direnv.enable = true;

    fish = {
      enable = true;
      vendor = {
        completions.enable = true;
        config.enable = true;
        functions.enable = true;
      };
    };

    gnupg.agent.enable = true;

    vim.package = lib.mkDefault pkgs.vim-with-conf;
  };
}
