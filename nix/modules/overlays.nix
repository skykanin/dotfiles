{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.overlays;
  # Import overlays
  importOverlay = package:
    import ../overlays/${package}/default.nix;
  # Import packages that don't exist in nixpkgs
  importCustomPackages = {
    attrname,
    package,
  }: _final: prev: {
    "${attrname}" = prev.callPackage ../packages/${package}.nix {};
  };
in {
  config = {
    nixpkgs.overlays =
      map importOverlay [
        "alejandra"
        "discocss"
        "obs"
        "vim"
      ]
      ++ map importCustomPackages [
        {
          attrname = "httpie-desktop";
          package = "httpie";
        }
      ];
  };
}
