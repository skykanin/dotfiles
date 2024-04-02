{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.overlays;
  # Import packages that don't exist in nixpkgs
  importCustomPackages = path: _final: prev: {
    inherit (prev.callPackage path {});
  };
  listNixFilesRecursive = path:
    builtins.filter (lib.hasSuffix ".nix") (map toString (lib.filesystem.listFilesRecursive path));
in {
  config.nixpkgs.overlays =
    map import (listNixFilesRecursive ../overlays)
    ++ map importCustomPackages (listNixFilesRecursive ../packages);
}
