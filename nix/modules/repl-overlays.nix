info: final: prev: let
  optionalAttrs = predicate: attrs:
    if predicate
    then attrs
    else {};
in
  {
    currentSystem = info.currentSystem;
    nixpkgs = __getFlake "nixpkgs";
  }
  // optionalAttrs (prev ? legacyPackages && prev.legacyPackages ? ${info.currentSystem})
  {
    pkgs = prev.legacyPackages.${info.currentSystem};
  }
