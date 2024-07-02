{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.etc."cachix.nix".source = toString ./cachix.nix;

  documentation = {
    enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
  };

  fonts.packages = with pkgs; [
    bqn386
    jetbrains-mono
    noto-fonts
    uiua386
    unifont
    victor-mono
  ];
}
