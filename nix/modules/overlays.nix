{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.overlays;
  importOverlay = package:
    import ../overlays/${package}/default.nix;
in {
  config = {
    nixpkgs.overlays = map importOverlay [
      "alejandra"
      "cbqn"
      "discocss"
    ];
  };
}
