{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../modules/nix.nix
    ../modules/general/default.nix
    ../modules/packages.nix
    ../modules/programs/default.nix
    ../modules/overlays.nix
  ];

  environment.systemPackages = with pkgs; [
    docker_24
    # firefox is broken on darwin so we use the -bin version from an overlay
    firefox-devedition-bin
    fluxcd
    k9s
    spotify
  ];

  programs.vim.enable = true;

  services.nix-daemon.enable = true;
}
