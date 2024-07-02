{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../modules/emacs.nix
    ../modules/general/default.nix
    ../modules/nix.nix
    ../modules/packages.nix
    ../modules/programs/default.nix
  ];

  environment.systemPackages = with pkgs; [
    # The nix-darwin emacs service doesn't add emacs to system packages
    # so we have to do it manually
    config.services.emacs.package
    google-cloud-sdk
    kubectx
    kubeseal
    # librewolf is broken on darwin so we use a version from an overlay
    librewolf
    fluxcd
    iterm2
    k9s
    spotify
    vim-with-conf
  ];

  homebrew = {
    enable = true;
    # TODO:
    # - Add docker module to nix-darwin
    # - Make nix signal package work on aarch64-darwin
    # - Package naisdevice-tenant in nix
    casks = ["docker" "naisdevice-tenant" "signal"];
    taps = ["nais/tap"];
  };

  nix.nixPath = [
    "nixpkgs=https://nixos.org/channels/nixpkgs-unstable"
  ];

  services.nix-daemon.enable = true;

  system.defaults.dock.persistent-apps = [
    "/Applications/Nix Apps/LibreWolf.app"
    "/Applications/Nix Apps/Spotify.app"
    "/Applications/Microsoft Outlook.app"
    "/Applications/Microsoft Teams.app"
    "/Applications/Nix Apps/Slack.app"
    "/Applications/Nix Apps/Discord.app"
    "/Applications/Nix Apps/KeePassXC.app"
    "/System/Applications/System Settings.app"
    "/Applications/Nix Apps/iTerm2.app"
    "/Applications/Nix Apps/Emacs.app"
    "/Applications/IntelliJ IDEA Ultimate alias"
  ];
}
