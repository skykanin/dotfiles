{
  config,
  lib,
  pkgs,
  ...
}:
  let inherit (pkgs.stdenv) hostPlatform; in {
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
      # See fish issue with nix-darwin: https://github.com/LnL7/nix-darwin/issues/122#issuecomment-1659465635
      loginShellInit =
        if hostPlatform.isDarwin
        then let
          # This naive quoting is good enough in this case. There shouldn't be any
          # double quotes in the input string, and it needs to be double quoted in case
          # it contains a space (which is unlikely!)
          dquote = str: "\"" + str + "\"";
          makeBinPathList = map (path: path + "/bin");
        in ''
          fish_add_path --move --prepend --path ${lib.concatMapStringsSep " " dquote (makeBinPathList config.environment.profiles)}
          set fish_user_paths $fish_user_paths
        ''
        else "";
    };

    nh = {
      enable = true;
      clean = {
        enable = true;
      } // (if hostPlatform.isDarwin then
        {
          interval = [
            # Monthly interval
            {
              Day = 1;
              Hour = 0;
              Minute = 0;
            }
          ];
        }
        else {
          dates = "monthly";
        });
      flake = "${if hostPlatform.isDarwin then "/Users/nvj" else "/home/skykanin"}/dotfiles/nix";
    };
  };
}
