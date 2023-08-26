{ config, lib, pkgs, ... }:

let
  cfg = config.local.nix;
in
{
  options.local.nix = with lib; {
    max-jobs = mkOption {
      type = types.ints.positive;
      default = 2;
    };

    extra-substituters = mkOption {
      type = types.listOf types.singleLineStr;
      default = [ ];
    };

    extra-trusted-public-keys = mkOption {
      type = types.listOf types.singleLineStr;
      default = [ ];
    };
  };

  config = {
    nix = {
      settings = {
        auto-optimise-store = true;
        max-jobs = cfg.max-jobs;
        substituters = [
          "https://cache.nixos.org"
          "https://nix-community.cachix.org"
        ] ++ cfg.extra-substituters;
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ] ++ cfg.extra-trusted-public-keys;

        trusted-substituters = [
          "https://hydra.iohk.io"
          "https://hydra.nixos.org"
        ];
        trusted-users = [ "root" "skykanin" ];
      };
      extraOptions = ''
        experimental-features = nix-command flakes repl-flake
        keep-outputs = false
        keep-derivations = false
        warn-dirty = false
      '';
      gc = {
        automatic = true;
        persistent = true;
        dates = "weekly";
        options = "--delete-older-than 14d";
      };
      package = pkgs.nixVersions.stable;
    };
    nixpkgs.config.allowUnfree = true;
  };
}
