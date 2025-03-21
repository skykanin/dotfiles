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

  environment = {
    variables.EDITOR = "vim";

    systemPackages = with pkgs; [
      coreutils-full
      # The nix-darwin emacs service doesn't add emacs to system packages
      # so we have to do it manually
      config.services.emacs.package
      discord
      parinfer-rust-emacs # emacs dep
      (google-cloud-sdk.withExtraComponents [google-cloud-sdk.components.gke-gcloud-auth-plugin])
      helm-chart-releaser
      kubectl
      kubectx
      kubernetes-helm
      kubeseal
      # librewolf is broken on darwin so we use a version from an overlay
      librewolf
      fluxcd
      iterm2
      k9s
      spotify
      vim-with-conf
    ];
  };

  homebrew = {
    enable = true;
    # TODO:
    # - Add docker module to nix-darwin
    # - Make nix signal package work on aarch64-darwin
    casks = ["docker" "signal"];
    taps = ["helm/tap"];
  };

  nix.nixPath = [
    "nixpkgs=https://nixos.org/channels/nixpkgs-unstable"
  ];

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

  ids.gids.nixbld = 30000;
  system.stateVersion = 6;
}
