{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.nix;
in {
  options.local.nix = with lib; {
    gc.automatic = mkOption {
      type = types.bool;
      default = false;
    };
    max-jobs = mkOption {
      type = types.either types.ints.positive (types.enum ["auto"]);
      default = "auto";
    };

    extra-substituters = mkOption {
      type = types.listOf types.singleLineStr;
      default = [];
    };

    extra-trusted-public-keys = mkOption {
      type = types.listOf types.singleLineStr;
      default = [];
    };
  };

  config.nix = {
    nixPath = ["nixpkgs=${config.nix.registry.self.flake}"];
    registry.self.flake = inputs.nixpkgs;
    optimise.automatic = true;
    settings = {
      max-jobs = cfg.max-jobs;
      sandbox = pkgs.stdenv.isLinux;
      substituters =
        [
          "https://cache.nixos.org"
          "https://nix-community.cachix.org"
        ]
        ++ cfg.extra-substituters;
      trusted-public-keys =
        [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ]
        ++ cfg.extra-trusted-public-keys;

      trusted-substituters = [
        "https://hydra.iohk.io"
        "https://hydra.nixos.org"
      ];
      trusted-users = ["root" "skykanin"];

      # lix specific configuration
      repl-overlays = [./repl-overlays.nix];
    };
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = false
      keep-derivations = false
      warn-dirty = false
    '';
    gc =
      {
        inherit (cfg.gc) automatic;
        options = "--delete-older-than 14d";
      }
      // lib.optionalAttrs pkgs.stdenv.isLinux {
        persistent = true;
        dates = "weekly";
      };

    package = pkgs.lixPackageSets.stable.lix;
  };
}
