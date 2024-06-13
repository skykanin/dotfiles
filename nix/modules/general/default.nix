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

  fonts = {
    ${
      if pkgs.stdenv.isDarwin
      then "fonts"
      else "packages"
    } = with pkgs; [
      bqn386
      jetbrains-mono
      noto-fonts
      uiua386
      unifont
      victor-mono
    ];

    fontDir.enable =
      if pkgs.stdenv.isDarwin
      then true
      else config.services.flatpak.enable;
  };
}
