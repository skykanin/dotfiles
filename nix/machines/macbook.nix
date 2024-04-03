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
    ../modules/emacs.nix
  ];

  environment.systemPackages = with pkgs; [
    docker_24
    # firefox is broken on darwin so we use the -bin version from an overlay
    firefox-devedition-bin
    fluxcd
    iterm2
    k9s
    spotify
  ];

  programs.vim.enable = true;

  services.nix-daemon.enable = true;

  system.defaults.dock.persistent-apps = [
    "/Applications/Nix Apps/Firefox Developer Edition.app"
    "/Applications/Nix Apps/Spotify.app"
    "/Applications/Microsoft Outlook.app"
    "/Applications/Microsoft Teams (work or school).app"
    "/Applications/Nix Apps/Slack.app"
    "/Applications/Nix Apps/Discord.app"
    "/Applications/Nix Apps/KeePassXC.app"
    "/System/Applications/System Settings.app"
    "/Applications/Nix Apps/iTerm2.app"
    "/Applications/Nix Apps/Emacs.app"
    "/Applications/IntelliJ IDEA Ultimate alias"
  ];
}
