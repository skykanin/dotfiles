{ config, pkgs, ... }:

with pkgs;
let
  stable = import <stable> { config.allowUnfree = true; };
  in
{
  environment.systemPackages = with pkgs; [
    adwaita-qt
    # android-studio
    arc-icon-theme
    stable.arc-theme
    ant-theme
    autorandr
    bat
    betterlockscreen
    clojure
    curl
    colorz
    direnv
    # discord
    docker
    (import /home/skykanin/dotfiles/nix/emacs.nix { inherit pkgs; }) # Emacs with my pkgs
    feh
    firefox-devedition-bin
    fish
    flameshot
    ghc
    gnome3.nautilus
    gnome3.networkmanagerapplet
    git
    jetbrains.idea-ultimate
    joker
    kitty
    my-leiningen
    lxappearance
    mpv
    neofetch
    nodejs-14_x
    obs-studio
    pamixer
    pavucontrol
    polybarFull
    playerctl
    plex
    python3
    pywal
    qbittorrent
    qdirstat
    rofi
    slack
    spotifywm
    steam
    weechat
    wget
    xdg_utils
    xorg.xbacklight
    youtube-dl
  ];

  # Nixpkgs overlays
  nixpkgs.overlays = [
    (self: super: {
      my-leiningen = super.leiningen.override {
        jdk = pkgs.openjdk11;
      };
    })
  ];
}
