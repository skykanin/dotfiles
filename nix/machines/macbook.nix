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
      colima
      # The nix-darwin emacs service doesn't add emacs to system packages
      config.services.emacs.package
      coreutils-full
      discord
      docker
      docker-credential-helpers
      fluxcd
      (google-cloud-sdk.withExtraComponents [google-cloud-sdk.components.gke-gcloud-auth-plugin])
      helm-chart-releaser
      k9s
      kubectl
      kubectx
      kubernetes-helm
      kubeseal
      orion-browser
      parinfer-rust-emacs # emacs dep
      rectangle
      spotify
      vim-with-conf
    ];
  };

  security.pam.services.sudo_local.touchIdAuth = true;

  homebrew = {
    enable = true;
    # TODO: Make nix signal package work on aarch64-darwin
    taps = ["helm/tap" "nais/tap"];
    casks = [
      "cameracontroller"
      "firefox"
      "naisdevice-tenant"
      "slack"
      "signal"
    ];

    onActivation = {
      cleanup = "uninstall";
    };
  };

  nix.nixPath = [
    "nixpkgs=https://nixos.org/channels/nixpkgs-unstable"
  ];

  system.defaults.dock.persistent-apps = [
    "/Applications/Firefox.app"
    "/Applications/Nix Apps/Spotify.app"
    "/Applications/Calendar.app"
    "/Applications/Microsoft Outlook.app"
    "/Applications/Microsoft Teams.app"
    "/Applications/Slack.app"
    "/Applications/Nix Apps/Discord.app"
    "/Applications/Nix Apps/KeePassXC.app"
    "/System/Applications/System Settings.app"
    "/Applications/Nix Apps/Ghostty.app"
    "/Applications/Nix Apps/Emacs.app"
    "/Applications/IntelliJ IDEA Ultimate alias"
  ];

  ids.gids.nixbld = 30000;
  system.stateVersion = 6;
}
