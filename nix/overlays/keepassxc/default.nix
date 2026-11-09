final: prev: {
  # shortcut: drop once nixos-unstable includes https://track.nix.ee/?pr=584023
  keepassxc = prev.keepassxc.overrideAttrs (o: {
    outputs = ["out"];
    postInstall =
      builtins.replaceStrings
      ["mkdir -p $doc/share/doc" "mv $out/share/keepassxc/docs/ $doc/share/doc/keepassxc"] ["" ""]
      o.postInstall;
  });
}
