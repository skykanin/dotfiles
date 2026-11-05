final: prev: {
  # Temporary patch while waiting for patch to land in nixos-unstable: https://track.nix.ee/?pr=582675
  opencode = prev.opencode.overrideAttrs (old: {
    node_modules = old.node_modules.overrideAttrs {
      outputHash = "sha256-prx9dX7kUvs/E3zopkaPk5LXh3xJAy5sZeum/W8L7cA=";
    };
  });
}
